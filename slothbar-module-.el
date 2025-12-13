;;; slothbar-module-.el --- Slothbar module lifecycle and display fns  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.30.2
;; Homepage: https://codeberg.org/agnes-li/slothbar
;; Keywords: frames, hardware

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

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

;; Part of slothbar.

;; This provides a base implementation for slothbar modules to manage display
;; refresh and lifecycle.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'fontsloth-layout)
(require 'map)
(require 'pcase)
(require 'xcb-render)

(require 'slothbar)
(require 'slothbar-color)
(require 'slothbar-font)
(require 'slothbar-log)
(require 'slothbar-module)
(require 'slothbar-render)

(defvar slothbar--connection)
(defvar slothbar--window)

(cl-defmethod slothbar-module-init ((m slothbar-module))
  "Initialize module M.
This default primary method gives M a graphics context, a pixmap, a glyphset,
and a cache.  The xcb ids are stored in the module xcb alist."
  (pcase-let* ((c slothbar--connection)
               (pmap (xcb:generate-id c))
               (gc (xcb:generate-id c))
               ((cl-struct slothbar-module name width colors) m))
    (slothbar--log-debug* "slothbar-module-init %s" name)
    (let ((epixmap (slothbar-render-create-pixmap c pmap width slothbar-height))
          (egc
           (xcb:+request-checked+request-check c
               (make-instance 'xcb:CreateGC
                              :cid gc
                              :drawable slothbar--window
                              :value-mask (logior xcb:GC:Background
                                                  xcb:GC:Foreground
                                                  xcb:GC:GraphicsExposures)
                              :background (slothbar-module-rgb-background-color
                                           colors)
                              :foreground (slothbar-module-rgb-background-color
                                           colors)
                              :graphics-exposures 0))))
      (slothbar--log-debug* "create %s pixmap, errors %s" name epixmap)
      (slothbar--log-debug* "create %s gc, errors %s" name egc))
    (slothbar-render-fill-rectangle c gc pmap width slothbar-height)
    (push `(pixmap . ,pmap) (slothbar-module-xcb m))
    (push `(gc . ,gc) (slothbar-module-xcb m))
    (push `(gs . ,(slothbar-render-create-glyphset c)) (slothbar-module-xcb m))
    (setf (slothbar-module-cache m) (make-hash-table :test 'equal))))

(defun slothbar-module--load-glyphs (text-layout gs cache)
  "Load glyphs from TEXT-LAYOUT into GS unless found in CACHE."
  (dolist (pos text-layout)
    (when (fontsloth-layout-glyph-position-p pos)
      (pcase-let* (((cl-struct
                     fontsloth-layout-glyph-position
                     key (parent char-code) (user-data font-idx)) pos)
                   (cache-key (map-elt cache char-code)))
        (unless (equal cache-key key)
          (when-let ((font-path (slothbar-font-find font-idx)))
            (slothbar-render-load-glyph
             slothbar--connection gs
             (fontsloth-load-font font-path)
             pos)
            (map-put! cache char-code key)))))))

(defun slothbar-module--draw-text (m)
  "Draw module M's text into its pixmap.

If any color codes are present, the resulting text will be colorized
accordingly.  Currently only commands :push, :pop, and :fg are supported."
  (pcase-let* (((cl-struct
                 slothbar-module cache text-layout xcb) m)
               ((map ('pixmap pixmap) ('gs gs)) xcb))
    (when (and pixmap gs)
      (slothbar-module--load-glyphs text-layout gs cache)
      (cl-flet ((draw (layout color)
                  (slothbar-render-draw-text
                   slothbar--connection pixmap gs layout color)))
        (cl-loop for pos in text-layout
                 with fgidx = 0 with fg-stack = nil with next = nil do
                 (if (fontsloth-layout-glyph-position-p pos)
                     (push pos next)
                   (cl-case (car pos)
                     (:push (push fgidx fg-stack))
                     (:pop (draw (nreverse next) (slothbar-color-find fgidx t))
                           (setq fgidx (pop fg-stack))
                           (setq next nil))
                     (:fg (draw (nreverse next) (slothbar-color-find fgidx t))
                          (setq fgidx (cadr pos))
                          (setq next nil))))
                 finally (draw (nreverse next) (slothbar-color-find fgidx t)))))))

(defun slothbar-module--create-layout (text font fidx px x)
  "Create a `fontsloth-layout' for TEXT, FONT, at size PX and offset x.
FIDX is the index into `slothbar-color-map-fg'."
  (let* ((l (fontsloth-layout-create)))
    (fontsloth-layout-reset
     l (fontsloth-layout-settings-create :x x
                                         :y (aref slothbar-font-y-delta fidx)))
    (fontsloth-layout-append
     l `(,font) (fontsloth-layout-text-style-create
                 :text text :px px :font-index 0
                 ;; placing the font object directly in user-data would make
                 ;; pretty-printing slothbar-modules too costly
                 :user-data fidx))
    (fontsloth-layout-finalize l)
    l))

(defun slothbar-module--layout-format (format spec lpad)
  "Collect all `fonsloth-layout's necessary to render FORMAT.
SPEC is a `format-spec' spec for the parts of FORMAT that are not color codes
LPAD is the module left padding"
  (cl-loop for part in (slothbar-color-parse-string format)
           with fidx = 0 with font-stack = nil with ls = nil
           for prev-x = lpad
           then (if ls (max prev-x (fontsloth-layout-current-pos (car ls)))
                  prev-x) collect
           (if (stringp part)
               (if-let ((font-path (slothbar-font-find fidx)))
                   (let* ((txt (format-spec part spec t))
                          (font (fontsloth-load-font font-path))
                          (px (+ (aref slothbar-font-px-size fidx)
                                 (aref slothbar-font-px-delta fidx)))
                          (l (slothbar-module--create-layout
                              txt font fidx px prev-x)))
                     (push l ls) l)
                 (let ((empty-layout (fontsloth-layout-create
                                      :current-pos (1+ prev-x))))
                   (push empty-layout ls) empty-layout))
             (cl-case (car part)
               (:push (push fidx font-stack) part)
               (:pop (setq fidx (pop font-stack)) part)
               (:font (setq fidx (cadr part)) part)
               (t part)))))

(defsubst slothbar-module--collect-layout-output (layouts)
  "Collect all layout output from LAYOUTS interspersed with any color codes."
  (let ((current-pos))
    `(,(cl-loop for l in layouts
                if (fontsloth-layout-p l)
                nconc (progn
                        (setq current-pos (fontsloth-layout-current-pos l))
                        (fontsloth-layout-output l))
                else collect l)
      ,current-pos)))

(cl-defmethod slothbar-module-layout ((m slothbar-module))
  "Give module M a layout.
This default primary method uses a result from fontsloth-layout to set
`slothbar-module-text-layout' and updates the module width accordingly."
  (pcase-let* (((cl-struct
                 slothbar-module format format-fn (format-spec spec) lpad) m)
               (format (if format-fn (funcall format-fn m) format))
               (layouts (slothbar-module--layout-format format spec lpad)))
    (cl-multiple-value-bind (output current-pos)
        (slothbar-module--collect-layout-output layouts)
      ;; (message "text layout width is %s" current-pos)
      (setf (slothbar-module-width m)
            (+ (slothbar-module-rpad m) (round current-pos))
            (slothbar-module-text-layout m)
            output))))

(cl-defmethod slothbar-module-init :before ((m slothbar-module))
  "Before init update module M's layout."
  ;; (message "super init before")
  (slothbar-module-layout m))

(declare-function cl-type-of "data.c" (object))

(cl-defmethod slothbar-module-init :after ((m slothbar-module))
  "After init start module M's update timer and draw its text."
  (when (cl-find-method #'slothbar-module-update-status '() `(,(cl-type-of m)))
    (unless (slothbar-module-update-timer m)
      (setf (slothbar-module-update-timer m)
            (run-at-time nil 10 #'slothbar-module-update-status m))))
  (when (slothbar-module-format m)
    (slothbar-module--draw-text m)))

(cl-defmethod slothbar-module-refresh ((m slothbar-module))
  "Refresh module M.
This default primary method redraws the text if it has changed."
  ;; (message "module refresh primary %s" (slothbar-module-name m))
  (when (slothbar-module-needs-refresh? m)
    ;; (message "module %s changed, redrawing text, xcb %s"
    ;;          (slothbar-module-name m) (slothbar-module-xcb m))
    (slothbar-module--draw-text m)))

(cl-defmethod slothbar-module-refresh :before ((m slothbar-module))
  "When refreshing, redo layout and make a new pixmap."
  ;; (message "running super before refresh")
  (when (slothbar-module-needs-refresh? m)
    (slothbar-module-layout m)
    (pcase-let* ((c slothbar--connection)
                 ((cl-struct slothbar-module width colors xcb) m))
      (when (map-elt xcb 'pixmap)
        (xcb:+request c
            (make-instance 'xcb:FreePixmap :pixmap (map-elt xcb 'pixmap)))
        (xcb:+request-checked+request-check c
            (make-instance 'xcb:ChangeGC
                           :gc (map-elt xcb 'gc)
                           :value-mask (logior xcb:GC:Background
                                               xcb:GC:Foreground)
                           :background (slothbar-module-rgb-background-color
                                        colors)
                           :foreground (slothbar-module-rgb-background-color
                                        colors)))
        (let ((pmap (xcb:generate-id c)))
          (slothbar-render-create-pixmap c pmap width slothbar-height)
          (map-put! (slothbar-module-xcb m) 'pixmap pmap)
          (slothbar-render-fill-rectangle
           c (map-elt xcb 'gc) pmap width slothbar-height))))))

(cl-defmethod slothbar-module-refresh :after ((m slothbar-module))
  "After refresh update M's needs-refresh?."
  (setf (slothbar-module-needs-refresh? m) nil))

(cl-defmethod slothbar-module-reposition ((m slothbar-module) x y)
  "Tell module M about its layout X and Y."
  (ignore m) (ignore x) (ignore y))

(cl-defmethod slothbar-module-exit :before ((m slothbar-module))
  "Cancel the update timer."
  (when (slothbar-module-update-timer m)
    (cancel-timer (slothbar-module-update-timer m)))
  (setf (slothbar-module-update-timer m) nil))

(cl-defmethod slothbar-module-exit ((m slothbar-module))
  "Tear down module M."
  (slothbar--log-debug* "slothbar-module-exit %s" (slothbar-module-name m))
  (pcase-let (((map ('pixmap pmap) ('gc gc) ('gs gs)) (slothbar-module-xcb m)))
    (when pmap
      (xcb:+request slothbar--connection
          (make-instance 'xcb:FreePixmap :pixmap pmap)))
    (when gc
      (xcb:+request slothbar--connection
          (make-instance 'xcb:FreeGC :gc gc)))
    (when gs
      (let ((egs (xcb:+request-checked+request-check slothbar--connection
                     (make-instance 'xcb:render:FreeGlyphSet :glyphset gs))))
        (slothbar--log-debug* "trying to free glyphset, errors %s" egs))))
  (setf (slothbar-module-cache m) nil
        (slothbar-module-text m) nil
        (slothbar-module-xcb m) nil))

(defsubst slothbar-module--find-by-name (module-name)
  "Find all instances of modules with names equivalent to MODULE-NAME."
  (cl-remove-if-not
   (lambda (m)
     (equal module-name
            (when (slothbar-module-p m)
              (slothbar-module-name m))))
   slothbar--modules))

(defun slothbar-module--refresh-all-by-name (module-name)
  "Refresh all instances of modules with names equivalent to MODULE-NAME.

This is intended for refreshing modules on user actions known to change
status."
  (when (slothbar-enabled-p)
    (let ((ms (slothbar-module--find-by-name module-name)))
      (dolist (m ms)
        (slothbar-module-update-status m))
      (slothbar-refresh-modules))))

(provide 'slothbar-module-)

;;; slothbar-module-.el ends here
