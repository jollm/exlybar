;;; exlybar-module.el --- Exlybar module lifecycle and display fns  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.21.3
;; Package-Requires: ((xelb "0.18") (fontsloth "0.15.3") (emacs "27.1"))
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

(require 'exlybar-color)
(require 'exlybar-module-types)
(require 'exlybar-render)

(defvar exlybar--connection)
(defvar exlybar--window)

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
    (exlybar-render-fill-rectangle c gc pmap width exlybar-height)
    (push `(pixmap . ,pmap) (exlybar-module-xcb m))
    (push `(gc . ,gc) (exlybar-module-xcb m))
    (push `(gs . ,(exlybar-render-create-glyphset c)) (exlybar-module-xcb m))
    (setf (exlybar-module-cache m) (make-hash-table :test 'equal))))

(defun exlybar-module--load-glyphs (text-layout gs cache)
  "Load glyphs from TEXT-LAYOUT into GS unless found in CACHE."
  (dolist (pos text-layout)
    (when (fontsloth-layout-glyph-position-p pos)
      (let* ((key (fontsloth-layout-glyph-position-key pos))
             (font-idx
              (fontsloth-layout-glyph-position-user-data pos))
             (font (fontsloth-load-font (exlybar-font-find font-idx))))
        (unless (map-elt cache key)
          (exlybar-render-load-glyph
           exlybar--connection gs font pos)
          (map-put! cache key t))))))

(defun exlybar-module--draw-text (m)
  "Draw module M's text into its pixmap.

If any color codes are present, the resulting text will be colorized
accordingly. Currently only commands :push, :pop, and :fg are supported."
  (pcase-let* (((cl-struct
                 exlybar-module cache text-layout xcb) m)
               ((map ('pixmap pixmap) ('gs gs)) xcb))
    (exlybar-module--load-glyphs text-layout gs cache)
    (cl-flet ((draw (layout color)
                (exlybar-render-draw-text
                 exlybar--connection pixmap gs layout color)))
      (cl-loop for pos in text-layout
               with fgidx = 0 with fg-stack = nil with next = nil do
               (if (fontsloth-layout-glyph-position-p pos)
                   (push pos next)
                 (cl-case (car pos)
                   (:push (push fgidx fg-stack))
                   (:pop (draw (nreverse next) (exlybar-color-find fgidx t))
                         (setq fgidx (pop fg-stack))
                         (setq next nil))
                   (:fg (draw (nreverse next) (exlybar-color-find fgidx t))
                        (setq fgidx (cadr pos))
                        (setq next nil))))
               finally (draw (nreverse next) (exlybar-color-find fgidx t))))))

(defun exlybar-module--create-layout (text font fidx px x)
  "Create a `fontsloth-layout' for TEXT, FONT, at size PX and offset x.
FIDX is the index into `exlybar-color-map-fg'."
  (let* ((l (fontsloth-layout-create)))
    (fontsloth-layout-reset
     l (fontsloth-layout-settings-create :x x
                                         :y (aref exlybar-font-y-delta fidx)))
    (fontsloth-layout-append
     l `(,font) (fontsloth-layout-text-style-create
                 :text text :px px :font-index 0
                 :user-data fidx))
    (fontsloth-layout-finalize l)
    l))

(defun exlybar-module--layout-format (format spec lpad)
  "Collect all `fonsloth-layout's necessary to render FORMAT.
SPEC is a `format-spec' spec for the parts of FORMAT that are not color codes
LPAD is the module left padding"
  (cl-loop for part in (exlybar-color-parse-string format)
           with fidx = 0 with font-stack = nil with ls = nil
           for prev-x = lpad
           then (if ls (max prev-x (fontsloth-layout-current-pos (car ls)))
                  prev-x) collect
           (if (stringp part)
               (let* ((txt (format-spec part spec))
                      (font (fontsloth-load-font (exlybar-font-find fidx)))
                      (px (+ (aref exlybar-font-px-size fidx)
                             (aref exlybar-font-px-delta fidx)))
                      (l (exlybar-module--create-layout
                          txt font fidx px prev-x)))
                 (push l ls) l)
             (cl-case (car part)
               (:push (push fidx font-stack) part)
               (:pop (setq fidx (pop font-stack)) part)
               (:font (setq fidx (cadr part)) part)
               (t part)))))

(defsubst exlybar-module--collect-layout-output (layouts)
  "Collect all layout output from LAYOUTS interspersed with any color codes."
  (let ((current-pos))
    `(,(cl-loop for l in layouts
                if (fontsloth-layout-p l)
                nconc (progn
                        (setq current-pos (fontsloth-layout-current-pos l))
                        (fontsloth-layout-output l))
                else collect l)
      ,current-pos)))

(cl-defgeneric exlybar-module-layout-text ((m exlybar-module))
  "Give module M a text layout.
This default primary method uses a result from fontsloth-layout to set
`exlybar-module-text-layout' and updates the module width accordingly."
  (pcase-let* (((cl-struct
                 exlybar-module format format-fn (format-spec spec) lpad) m)
               (format (if format-fn (funcall format-fn m) format))
               (layouts (exlybar-module--layout-format format spec lpad)))
    (cl-multiple-value-bind (output current-pos)
        (exlybar-module--collect-layout-output layouts)
      ;; (message "text layout width is %s" current-pos)
      (setf (exlybar-module-width m)
            (+ (exlybar-module-rpad m) (round current-pos))
            (exlybar-module-text-layout m)
            output))))

(cl-defmethod exlybar-module-init :before ((m exlybar-module))
  "Before init update module M's text-layout."
  ;; (message "super init before")
  (exlybar-module-layout-text m))

(cl-defmethod exlybar-module-init :after ((m exlybar-module))
  "After init draw module M's text."
  (exlybar-module--draw-text m))

(cl-defgeneric exlybar-module-refresh ((m exlybar-module))
  "Refresh module M.
This default primary method redraws the text if it has changed."
  ;; (message "module refresh primary %s" (exlybar-module-name m))
  (when (exlybar-module-needs-refresh? m)
    ;; (message "module %s changed, redrawing text, xcb %s"
    ;;          (exlybar-module-name m) (exlybar-module-xcb m))
    (exlybar-module--draw-text m)))

(cl-defmethod exlybar-module-refresh :before ((m exlybar-module))
  "When refreshing, redo text layout and make a new pixmap."
  ;; (message "running super before refresh")
  (when (exlybar-module-needs-refresh? m)
    (exlybar-module-layout-text m)
    (let ((c exlybar--connection)
          (xcb (exlybar-module-xcb m))
          (width (exlybar-module-width m)))
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
  (setf (exlybar-module-cache m) nil
        (exlybar-module-text m) nil
        (exlybar-module-xcb m) nil))

(provide 'exlybar-module)

;;; exlybar-module.el ends here
