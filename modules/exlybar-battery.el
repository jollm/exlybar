;;; exlybar-battery.el --- An exlybar battery module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for battery status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'battery)
(require 'cl-lib)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-battery nil
  "An Exlybar battery module."
  :group 'exlybar)

(defcustom exlybar-battery-icons
  '((10 . ?) (35 . ?) (60 . ?) (85 . ?) (100 . ?))
  "Icons for exlybar-battery. See `exlybar-choose-icon' for how it is used."
  :type 'alist
  :group 'exlybar-battery)

(defcustom exlybar-battery-color-zones '(49 29 10 t)
  "Battery percentages indicating icon color changes.
See `exlybar-zone-color'"
  :type 'list
  :group 'exlybar-battery)

(cl-defstruct (exlybar-battery
               (:include exlybar-module (name "battery")
                         (format "^6^[^f1%i^] %b%p%% ^[^2|^] %t ^[^2|^] %r")
                         (format-fn 'exlybar-battery-format-format))
               (:constructor exlybar-battery-create)
               (:copier nil)))

(defun exlybar-battery--format-fn-spec (zone-color)
  "Build the `format-spec' spec used by the format-fn."
  `((?i . ,(format "%s%%i" zone-color))))

(defun exlybar-battery-format-format (m)
  "This is the default format-fn that is applied to format."
  (let* ((status (or (map-elt (exlybar-module-cache m) 'status)
                     (funcall battery-status-function)))
         (pct (string-to-number (map-elt status ?p)))
         (zone-color
          (apply #'exlybar-zone-color pct exlybar-battery-color-zones)))
    (format-spec (exlybar-module-format m)
                 (exlybar-battery--format-fn-spec zone-color) t)))

(defun exlybar-battery--format-spec (status)
  "Build the `format-spec' spec used to generate module text."
  (let* ((pct (string-to-number (map-elt status ?p)))
         (icon (exlybar-choose-icon pct exlybar-battery-icons)))
    (map-insert status ?i (string icon))))

(defvar exlybar-battery--update-timer nil
  "A variable to hold the update timer.")

(defun exlybar-battery--do-update (m)
  "Poll the battery status and check whether to update M's text."
  (let* ((status (funcall battery-status-function))
         (txt (format-spec (exlybar-module-format m) status t)))
    (when (exlybar-module-cache m)
      (map-put! (exlybar-module-cache m) 'status status))
    (unless (equal txt (exlybar-module-text m))
      (setf (exlybar-module-format-spec m)
            (exlybar-battery--format-spec status)
            (exlybar-module-text m) txt
            (exlybar-module-needs-refresh? m) t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-battery))
  "Set the M's icon and update the text."
  (exlybar-battery--do-update m))

(cl-defmethod exlybar-module-init :after ((m exlybar-battery))
  "Run the update timer."
  (unless exlybar-battery--update-timer
    (setq exlybar-battery--update-timer
          (run-at-time nil 10 #'exlybar-battery--do-update m))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-battery))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-battery--update-timer
    (cancel-timer exlybar-battery--update-timer))
  (setq exlybar-battery--update-timer nil))

(provide 'exlybar-battery)
;;; exlybar-battery.el ends here
