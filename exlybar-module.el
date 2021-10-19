;;; exlybar-module.el --- Exlybar module lifecycle and display fns  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((xelb "0.18") (fontsloth "0.12.0") (emacs "27.1"))
;; Keywords: window-manager, status-bar, exwm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Part of exlybar.

;; This provides a base implementation for exlybar modules to manage display
;; refresh and lifecycle.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'fontsloth-layout)
(require 'xcb-render)

(require 'exlybar-module-types)
(require 'exlybar-render)

(defcustom exlybar-default-module-text-color
  (exlybar-render-create-color
   :red #xeeee :green #xffff :blue #xffff :alpha #xffff)
  "The default text color for modules."
  :type 'xcb:render:COLOR
  :group 'exlybar)

(cl-defgeneric exlybar-module-init ((m exlybar-module))
  "Initialize module M.
This default primary method gives M a graphics context, a pixmap, a glyphset,
and a cache. The xcb ids are stored in the module xcb alist."
  (pcase-let* ((c exlybar--connection)
               (pmap (xcb:generate-id c))
               (gc (xcb:generate-id c))
               ((cl-struct exlybar-module name width colors) m))
    (message "create module %s pixmap %s" name
             (exlybar-render-create-pixmap c pmap width exlybar-height))
    (message "create module %s gc %s" name
             (xcb:+request-checked+request-check c
                 (make-instance 'xcb:CreateGC
                                :cid gc
                                :drawable exlybar--window
                                :value-mask (logior xcb:GC:Background
                                                    xcb:GC:Foreground
                                                    xcb:GC:GraphicsExposures)
                                :background (exlybar-module-rgb-background-color colors)
                                :foreground (exlybar-module-rgb-background-color colors)
                                :graphics-exposures 0)))
    (message "fill module %s rectangle %s" name
             (exlybar-render-fill-rectangle c gc pmap width exlybar-height))
    (push `(pixmap . ,pmap) (exlybar-module-xcb m))
    (push `(gc . ,gc) (exlybar-module-xcb m))
    (push `(gs . ,(exlybar-render-create-glyphset c)) (exlybar-module-xcb m))
    (setf (exlybar-module-cache m) (make-hash-table :test 'equal))))

(defun exlybar-module--draw-text (m)
  "Draw module M's text into its pixmap."
  (pcase-let* (((cl-struct
                 exlybar-module name cache text-layout fonts xcb) m)
               ((map ('pixmap pixmap) ('gs gs)) xcb)
               (fonts (seq-map #'fontsloth-load-font fonts)))
    (dolist (pos text-layout)
      (let* ((key (fontsloth-layout-glyph-position-key pos))
             (font-idx
              (fontsloth-layout-glyph-raster-config-font-index key)))
        (unless (map-elt cache key)
          (exlybar-render-load-glyph
           exlybar--connection gs (elt fonts font-idx) pos)
          (map-put! (exlybar-module-cache m) key t))))
    (exlybar-render-draw-text exlybar--connection pixmap gs text-layout
                              exlybar-default-module-text-color)))

(cl-defgeneric exlybar-module-layout-text ((m exlybar-module))
  "Give module M a text layout.
This default primary method uses a result from fontsloth-layout to set
`exlybar-module-text-layout' and updates the module width accordingly."
  (let* ((text (exlybar-module-text m))
         (font-names (exlybar-module-fonts m))
         (fonts (seq-map #'fontsloth-load-font font-names))
         (layouts
          (cl-loop for txt+font in text collect
                   (let* ((l (fontsloth-layout-create
                              :x (exlybar-module-lpad m)))
                          (txt (car txt+font)) (font-name (cdr txt+font))
                          (font-index (seq-position font-names font-name))
                          (font (elt fonts font-index))
                          (px (fontsloth-font-compute-px font exlybar-height)))
                     (fontsloth-layout-reset
                      l (fontsloth-layout-settings-create))
                     (fontsloth-layout-append
                      l fonts (fontsloth-layout-text-style-create
                               :text txt :px px :font-index font-index))
                     (fontsloth-layout-finalize l)
                     l))))
    (let ((output (apply #'fontsloth-layout-concat-layouts layouts)))
      (setf (exlybar-module-width m)
            (+ (exlybar-module-rpad m)
               (round (fontsloth-layout-current-pos (car (last layouts)))))
            (exlybar-module-text-layout m)
            output))))

(cl-defmethod exlybar-module-init :before ((m exlybar-module))
  "Before init update module M's text-layout."
  (message "super init before")
  (exlybar-module-layout-text m))

(cl-defmethod exlybar-module-init :after ((m exlybar-module))
  "After init draw module M's text."
  (exlybar-module--draw-text m))

(cl-defgeneric exlybar-module-refresh ((m exlybar-module))
  "Refresh module M.
This default primary method redraws the text if it has changed."
  (message "module refresh primary %s" (exlybar-module-name m))
  (when (exlybar-module-needs-refresh? m)
    (message "module %s changed, redrawing text, xcb %s"
             (exlybar-module-name m) (exlybar-module-xcb m))
    (exlybar-module--draw-text m)))

(cl-defmethod exlybar-module-refresh :before ((m exlybar-module))
  "When refreshing, redo text layout and make a new pixmap."
  (message "running super before refresh")
  (pcase-let ((c exlybar--connection)
              ((cl-struct exlybar-module name width xcb) m))
    (when (exlybar-module-needs-refresh? m)
      (exlybar-module-layout-text m)
      (xcb:+request c
          (make-instance 'xcb:FreePixmap :pixmap (map-elt xcb 'pixmap)))
      (let ((pmap (xcb:generate-id c)))
        (exlybar-render-create-pixmap c pmap width exlybar-height)
        (map-put! (exlybar-module-xcb m) 'pixmap pmap)
        (exlybar-render-fill-rectangle
         c (map-elt xcb 'gc) pmap width exlybar-height)))))

(cl-defmethod exlybar-module-refresh :after ((m exlybar-module))
  "After refresh update M's needs-refresh?."
  (setf (exlybar-module-needs-refresh? m) nil))

(cl-defgeneric exlybar-module-exit ((m exlybar-module))
  "Tear down module M."
  (message "exiting module %s" (exlybar-module-name m))
  (pcase-let (((map ('pixmap pmap) ('gc gc) ('gs gs)) (exlybar-module-xcb m)))
    (when pmap
      (xcb:+request exlybar--connection
          (make-instance 'xcb:FreePixmap :pixmap pmap)))
    (when gc
      (xcb:+request exlybar--connection
          (make-instance 'xcb:FreeGC :gc gc)))
    (when gs
      (message "trying to free glyphset %s "
               (xcb:+request-checked+request-check exlybar--connection
                   (make-instance 'xcb:render:FreeGlyphSet :glyphset gs)))))
  (setf (exlybar-module-cache m) nil)
  (setf (exlybar-module-xcb m) nil))

(provide 'exlybar-module)

;;; exlybar-module.el ends here
