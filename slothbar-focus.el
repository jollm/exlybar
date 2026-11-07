;;; slothbar-focus.el --- An slothbar focused-window module  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; A module showing the title and application id of the focused window.

;;; Code:

(require 'subr-x)

(require 'slothbar)
(require 'slothbar-backend)
(require 'slothbar-log)
(require 'slothbar-module-)

(defgroup slothbar-focus nil
  "Focused-window module."
  :group 'slothbar)

(defcustom slothbar-focus-refresh-interval 1
  "Seconds between focus status updates."
  :type 'natnum
  :group 'slothbar-focus)

(defcustom slothbar-focus-truncate-to-width 60
  "Maximum displayed title width, or nil for no truncation."
  :type '(choice (const :tag "None" nil) natnum)
  :group 'slothbar-focus)

(defcustom slothbar-focus-format "%t"
  "Format string for the focused-window module.
%t is the title and %a the application id."
  :type 'string
  :group 'slothbar-focus)

(defun slothbar-focus--truncate (string)
  "Truncate STRING to `slothbar-focus-truncate-to-width'."
  (if (and string slothbar-focus-truncate-to-width)
      (truncate-string-to-width
       string slothbar-focus-truncate-to-width nil nil t)
    string))

(defun slothbar-focus--format-values (focus)
  "Build a `format-spec' alist from FOCUS."
  (let* ((title (slothbar-focus--truncate (plist-get focus :title)))
         (app-id (plist-get focus :app-id))
         (title (or title ""))
         (app-id (or app-id title)))
    `((?t . ,title)
      (?a . ,app-id))))

(defun slothbar-focus--render (format focus)
  "Render FORMAT for FOCUS, or return an empty string when FOCUS is empty."
  (if (and focus
           (or (plist-get focus :title)
               (plist-get focus :app-id)))
      (format-spec format (slothbar-focus--format-values focus) 'ignore)
    ""))

(defun slothbar-focus-format-format (m)
  "Return M's format string, or an empty string when no window is focused."
  (if (or (null (slothbar-module-text m))
          (string-empty-p (slothbar-module-text m)))
      ""
    (slothbar-module-format m)))

(cl-defstruct
    (slothbar-focus
     (:include slothbar-module
               (name "focus")
               (format "%t")
               (format-fn #'slothbar-focus-format-format))
     (:constructor slothbar-focus--create)
     (:copier nil))
  "A focused-window module.")

(defun slothbar-focus-create (&rest args)
  "Create a focused-window module using `slothbar-focus-format'."
  (let ((args (copy-sequence args)))
    (unless (plist-member args :format)
      (setq args (append (list :format slothbar-focus-format) args)))
    (apply #'slothbar-focus--create args)))

(defvar slothbar-focus--subscribers 0
  "Number of active focus modules sharing the backend hook.")

(defun slothbar-focus--on-change (backend)
  "Refresh focus modules after a focused-window change on BACKEND."
  (when (eq backend slothbar--backend)
    (run-at-time
     0 nil
     (lambda ()
       (when slothbar--backend
         (slothbar-module--refresh-all-by-name "focus"))))))

(defun slothbar-focus--subscribe ()
  "Register a focus module as a backend-hook subscriber."
  (when (zerop slothbar-focus--subscribers)
    (add-hook 'slothbar-backend-focused-window-change-hook
              #'slothbar-focus--on-change))
  (cl-incf slothbar-focus--subscribers))

(defun slothbar-focus--unsubscribe ()
  "Unregister a focus module from the backend hook."
  (setq slothbar-focus--subscribers
        (max 0 (1- slothbar-focus--subscribers)))
  (when (zerop slothbar-focus--subscribers)
    (remove-hook 'slothbar-backend-focused-window-change-hook
                 #'slothbar-focus--on-change)
    (when slothbar-focus--refresh-timer
      (cancel-timer slothbar-focus--refresh-timer)
      (setq slothbar-focus--refresh-timer nil))))

(cl-defmethod slothbar-module-supported-p
  ((_m slothbar-focus) (backend slothbar-backend))
  "The focused-window module needs backend focus support."
  (slothbar-backend-supports-p backend 'focused-window))

(cl-defmethod slothbar-module-update-status ((m slothbar-focus))
  "Update M from the backend's focused window."
  (let* ((backend (slothbar-module--backend m))
         (focus (and backend
                     (slothbar-backend-focused-window backend)))
         (spec (slothbar-focus--format-values focus))
         (text (slothbar-focus--render (slothbar-module-format m) focus))
         (changed (not (equal text (slothbar-module-text m)))))
    (setf (slothbar-module-format-spec m) spec
          (slothbar-module-text m) text)
    (when changed
      (setf (slothbar-module-needs-refresh? m) t)
      (when (and backend (slothbar-enabled-p))
        (slothbar-refresh-modules nil (slothbar-module--bar m))))
    text))

(cl-defmethod slothbar-module-init :before ((m slothbar-focus))
  "Configure M's initial focus state."
  (slothbar-module-update-status m)
  (slothbar-focus--subscribe))

(cl-defmethod slothbar-module-init ((m slothbar-focus))
  "Initialize M and start its focus update timer."
  (cl-call-next-method)
  (when (slothbar-module-update-timer m)
    (cancel-timer (slothbar-module-update-timer m)))
  (setf (slothbar-module-update-timer m)
        (run-at-time nil slothbar-focus-refresh-interval
                     #'slothbar-module-update-status m)))

(cl-defmethod slothbar-module-exit :before ((_m slothbar-focus))
  "Stop listening for focus changes when no focus modules remain."
  (slothbar-focus--unsubscribe))

(provide 'slothbar-focus)
;;; slothbar-focus.el ends here
