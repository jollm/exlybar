;;; slothbar-module.el --- Base types and generics for modules  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.32.0
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

;; slothbar-module.el:
;; Provides a base type for slothbar modules.

;;; Code:

(require 'cl-lib)
(require 'cl-print)

(require 'slothbar-backend)
(require 'slothbar-color)
(require 'slothbar-util)

(defvar slothbar-module-min-width 10
  "Let modules have a min width so an empty module is visible.")

;; TODO: allow changing backgrounds colors using color codes
(cl-defstruct
    (slothbar-module-rgb
     (:constructor slothbar-module-rgb-create)
     (:copier nil))
  "Container for a background and a foreground color."
  (background-color (slothbar-color-from-name
                     (slothbar-util--find-background-color))
                    :type 'slothbar-color-rgb)
  (foreground-color (slothbar-color-from-name
                     (slothbar-util--find-foreground-color))
                    :type 'slothbar-color-rgb))

(cl-defstruct
    (slothbar-module
     (:constructor slothbar-module-create)
     (:copier nil))
  "This is a base type for slothbar modules."
  (name nil :type 'string)
  (text nil :type 'string)
  (format "" :type 'string)
  (format-fn nil :type 'function)
  (format-spec nil :type 'string)
  (text-layout nil :type 'list)
  (lpad 14 :type 'fixed)
  (rpad 14 :type 'fixed)
  (colors (slothbar-module-rgb-create) :type 'slothbar-module-rgb)
  (icon nil :type '(or character alist)
        :documentation "Either a single character or an alist as expected by
`slothbar-color-choose-icon'.")
  (animation nil :type 'function)
  (cache nil :type 'hash-table)
  (width slothbar-module-min-width :type 'fixed)
  (initialized nil :type 'boolean)
  (needs-refresh? nil :type 'boolean)
  (update-timer nil :type 'timer)
  (keymap nil :documentation "Canonical action keymap for this module.")
  (actions nil :documentation "Syntactic sugar for `keymap'.
A plist or alist of action keywords to commands.")
  backend
  bar)

(defun slothbar-module--print-text (text)
  "Return a short printable representation of TEXT."
  (let ((text (or text "")))
    (if (> (length text) 40)
        (concat (substring text 0 40) "...")
      text)))

(cl-defmethod cl-print-object ((rgb slothbar-module-rgb) stream)
  "Print RGB compactly in Help and `cl-prin1'."
  (princ (format "#<slothbar-module-rgb bg=%s fg=%s>"
                 (cl-prin1-to-string
                  (slothbar-module-rgb-background-color rgb))
                 (cl-prin1-to-string
                  (slothbar-module-rgb-foreground-color rgb)))
         stream))

(cl-defmethod cl-print-object ((module slothbar-module) stream)
  "Print MODULE compactly in Help and `cl-prin1'."
  (slothbar--print-object-fields
   stream "#<slothbar-module"
   (list (format "name=%s" (or (slothbar-module-name module) "anonymous"))
         (format "initialized=%S" (slothbar-module-initialized module))
         (format "width=%s" (slothbar-module-width module))
         (format "text=%S" (slothbar-module--print-text
                            (slothbar-module-text module))))))

(defun slothbar-module--backend (m)
  "Return M's owning display backend, or the current backend."
  (or (slothbar-module-backend m) slothbar--backend))

(defun slothbar-module--bar (m)
  "Return M's owning `slothbar' bar, or the current bar."
  (or (slothbar-module-bar m) slothbar--bar))

(cl-defgeneric slothbar-module-init (m) "Initialize module M.")

(cl-defgeneric slothbar-module-layout (m)
  "Give module M a layout.")

(cl-defgeneric slothbar-module-refresh (m) "Refresh module M.")

(cl-defgeneric slothbar-module-reposition (m x y)
  "Tell module M about its layout X and Y.")

(cl-defgeneric slothbar-module-update-status (m)
  "Refresh any backend status information required by module M.")

(cl-defgeneric slothbar-module-exit (m) "Tear down module M.")

(cl-defgeneric slothbar-module-supported-p (m backend)
  "Return non-nil when module M can run on display BACKEND.
Modules that depend on X-only facilities override this to consult
`slothbar-backend-supports-p'.")

(cl-defgeneric slothbar-module-singleton-id (m)
  "Return a symbol when module M may only be instantiated once globally.
Return nil for modules that may appear any number of times.  Bars that
would create a second instance of the same id skip it during module
construction.")

(cl-defmethod slothbar-module-singleton-id ((_m slothbar-module))
  "Ordinary modules are not singletons."
  nil)

(defvar slothbar-current-module nil
  "Module currently handling a pointer action.")

(defvar slothbar-current-action nil
  "Action keyword currently being handled.")

(defvar slothbar-current-action-x nil
  "Module-local x coordinate of the current action.")

(defvar slothbar-current-action-y nil
  "Module-local y coordinate of the current action.")

(defconst slothbar-module--action-events
  '((:left . mouse-1)
    (:middle . mouse-2)
    (:right . mouse-3)
    (:scroll-up . wheel-up)
    (:scroll-down . wheel-down))
  "Pointer action keywords and their Emacs event names.")

(defun slothbar-module--action-event (action)
  "Return the Emacs event for ACTION, or nil."
  (cond
   ((assq action slothbar-module--action-events)
    (cdr (assq action slothbar-module--action-events)))
   ((and (symbolp action) (not (keywordp action)))
    action)
   (t nil)))

(defun slothbar-module--actions-keymap (actions)
  "Return a sparse keymap built from ACTIONS.
ACTIONS may already be a keymap, a plist of action keywords to commands,
or an alist of the same."
  (cond
   ((keymapp actions) actions)
   ((and (listp actions) (consp (car actions)))
    (let ((map (make-sparse-keymap)))
      (dolist (pair actions map)
        (let ((event (slothbar-module--action-event (car pair))))
          (unless event
            (error "Unknown module action %S" (car pair)))
          (define-key map (vector event) (cdr pair))))))
   ((listp actions)
    (let ((map (make-sparse-keymap)))
      (cl-loop for (action command) on actions by #'cddr do
               (let ((event (slothbar-module--action-event action)))
                 (unless event
                   (error "Unknown module action %S" action))
                 (define-key map (vector event) command)))
      map))
   (t
    (error "Invalid module actions %S" actions))))

(defun slothbar-module--ensure-keymap (m)
  "Return M's keymap, normalizing its :actions sugar if needed."
  (or (slothbar-module-keymap m)
      (let ((actions (slothbar-module-actions m)))
        (when actions
          (let ((map (slothbar-module--actions-keymap actions)))
            (setf (slothbar-module-keymap m) map)
            map)))))

(cl-defgeneric slothbar-module-action (m action x y)
  "Handle ACTION at module-local X,Y on M.
ACTION is a keyword such as `:left' or `:scroll-up'.  A binding in M's
keymap takes precedence.  Unbound click actions fall back to
`slothbar-module-click'.")

(cl-defgeneric slothbar-module-click (m x y button)
  "Handle a pointer click at X,Y with BUTTON on module M.
X and Y are relative to M's top-left corner.  BUTTON is one of `:left',
`:middle', or `:right'.")

(cl-defgeneric slothbar-module-hover (m x y)
  "Handle the pointer hovering at X,Y over module M.
X and Y are relative to M's top-left corner, or nil when the pointer leaves
M.")

(cl-defmethod slothbar-module-supported-p
  ((_m slothbar-module) (_backend slothbar-backend))
  "By default every module supports every backend."
  t)

(cl-defmethod slothbar-module-click
  ((_m slothbar-module) (_x number) (_y number) (_button symbol))
  "By default a module ignores clicks."
  nil)

(cl-defmethod slothbar-module-hover
  ((_m slothbar-module) (_x t) (_y t))
  "By default a module ignores hover."
  nil)

(cl-defmethod slothbar-module-action ((m slothbar-module) action x y)
  "Handle ACTION at X,Y using M's keymap or click fallback."
  (let* ((event (slothbar-module--action-event action))
         (keymap (slothbar-module--ensure-keymap m))
         (binding (and event keymap
                       (lookup-key keymap (vector event)))))
    (cond
     ((commandp binding)
      (let ((slothbar-current-module m)
            (slothbar-current-action action)
            (slothbar-current-action-x x)
            (slothbar-current-action-y y))
        (call-interactively binding)))
     ((functionp binding)
      (funcall binding m x y))
     ((and (null binding) (memq action '(:left :middle :right)))
      (slothbar-module-click m x y action))
     (t nil))))

(provide 'slothbar-module)

;;; slothbar-module.el ends here
