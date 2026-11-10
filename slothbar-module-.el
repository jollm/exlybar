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
(require 'pcase)

(require 'slothbar-backend)
(require 'slothbar-color)
(require 'slothbar-font)
(require 'slothbar-log)
(require 'slothbar-module)

(cl-defmethod slothbar-module-init ((m slothbar-module))
  "Initialize module M.
This default primary method gives M a cache and asks the active display
backend for the pixel resources it needs to draw."
  (slothbar--log-debug* "slothbar-module-init %s" (slothbar-module-name m))
  (setf (slothbar-module-cache m) (make-hash-table :test 'equal))
  (let ((backend (slothbar-module--backend m)))
    (setf (slothbar-module-backend m) backend)
    (when backend
      (slothbar-backend-add-module backend m))))

(defun slothbar-module--create-layout (text font fidx px x)
  "Create a `fontsloth-layout' for TEXT, FONT, at size PX and offset x.
FIDX is the index into `slothbar-color-map-fg'."
  (let* ((l (fontsloth-layout-create)))
    (fontsloth-layout-reset
     l (fontsloth-layout-settings-create
        :x x
        :y (+ (aref slothbar-font-y-delta fidx)
              (slothbar-font--vertical-offset font px))))
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
                   (let* ((txt (format-spec part spec 'ignore))
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

(defun slothbar-module--update-status-safe (m)
  "Update M, reporting failures instead of signaling from a timer."
  (condition-case err
      (slothbar-module-update-status m)
    (error
     (if (fboundp 'slothbar--report-module-failure)
         (slothbar--report-module-failure m "update" err)
       (slothbar--log-error "Slothbar module %s update failed: %S"
                            (slothbar-module-name m) err)))))

(cl-defmethod slothbar-module-init :after ((m slothbar-module))
  "After init start module M's update timer and draw its text."
  (when (cl-find-method #'slothbar-module-update-status '() `(,(cl-type-of m)))
    (unless (slothbar-module-update-timer m)
      (setf (slothbar-module-update-timer m)
            (run-at-time nil 10 #'slothbar-module--update-status-safe m))))
  (when (slothbar-module-format m)
    (when-let ((backend (slothbar-module--backend m)))
      (slothbar-backend-draw-module backend m)))
  (setf (slothbar-module-initialized m) t))

(cl-defmethod slothbar-module-refresh :before ((m slothbar-module))
  "Lay out module M when it needs a refresh."
  (when (slothbar-module-needs-refresh? m)
    (slothbar-module-layout m)))

(cl-defmethod slothbar-module-refresh ((m slothbar-module))
  "Refresh module M.
This default primary method redraws M through the display backend."
  (when (slothbar-module-needs-refresh? m)
    (when-let ((backend (slothbar-module--backend m)))
      (slothbar-backend-draw-module backend m))))

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
  (setf (slothbar-module-update-timer m) nil
        (slothbar-module-initialized m) nil))

(cl-defmethod slothbar-module-exit ((m slothbar-module))
  "Tear down module M."
  (slothbar--log-debug* "slothbar-module-exit %s" (slothbar-module-name m))
  (when-let ((backend (slothbar-module--backend m)))
    (slothbar-backend-remove-module backend m))
  (setf (slothbar-module-backend m) nil
        (slothbar-module-cache m) nil
        (slothbar-module-text m) nil))

(defvar slothbar--bars)

(defsubst slothbar-module--find-by-name (module-name)
  "Find all instances of modules with names equivalent to MODULE-NAME.

Search every active bar, not just the legacy global module list."
  (let ((modules (if (bound-and-true-p slothbar--bars)
                     (cl-loop for bar in slothbar--bars
                              append (slothbar-module-list bar))
                   slothbar--modules)))
    (cl-remove-if-not
     (lambda (m)
       (equal module-name
              (when (slothbar-module-p m)
                (slothbar-module-name m))))
     modules)))

(defun slothbar-module--refresh-all-by-name (module-name)
  "Refresh all instances of modules with names equivalent to MODULE-NAME.

This is intended for refreshing modules on user actions known to change
status."
  (when (slothbar-enabled-p)
    (let ((ms (slothbar-module--find-by-name module-name)))
      (dolist (m ms)
        (slothbar-module--update-status-safe m))
      (slothbar-refresh-modules))))

(provide 'slothbar-module-)

;;; slothbar-module-.el ends here
