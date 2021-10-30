;;; exlybar-backlight.el --- An exlybar backlight module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.21.3
;; Package-Requires: ((backlight "1.4") (cl-lib "0.5") (emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for backlight status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'backlight)
(require 'cl-lib)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-backlight nil
  "An Exlybar backlight module."
  :group 'exlybar)

(defcustom exlybar-backlight-progress-increment 10
  "The percent step increment for the backlight module progress bar."
  :type 'integer
  :group 'exlybar-backlight)

(defcustom exlybar-backlight-color-zones '(20 40 80)
  "Backlight percentages indicating progress color changes.
See `exlybar-zone-color'"
  :type 'list
  :group 'exlybar-backlight)

(cl-defstruct (exlybar-backlight
               (:include exlybar-module (name "backlight") (icon ?)
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn 'exlybar-backlight-format-format))
               (:constructor exlybar-backlight-create)
               (:copier nil)))

(defvar exlybar-backlight--update-timer nil
  "A variable to hold the update timer.")

(defun exlybar-backlight-current-progress ()
  "Build a progress bar corresponding to the current state."
  (exlybar-progress-bar
   (backlight--current-percentage)
   exlybar-backlight-progress-increment exlybar-backlight-color-zones))

(defun exlybar-backlight--format-fn-spec ()
  "Build the `format-spec' spec used by the format-fn."
  `((?p . ,(exlybar-backlight-current-progress))))

(defun exlybar-backlight-format-format (m)
  "This is the default format-fn that is applied to format."
  (format-spec (exlybar-module-format m)
               (exlybar-backlight--format-fn-spec) t))

(defun exlybar-backlight--format-spec (icon)
  "Build the `format-spec' spec used to generate module text."
  `((?i . ,(string icon))))

(defun exlybar-backlight--do-update (m)
  "Poll the battery status and check whether to update M's text."
  (let* ((status (exlybar-backlight--format-spec (exlybar-module-icon m)))
         (txt (number-to-string (backlight--current-percentage))))
    (unless (equal txt (exlybar-module-text m))
      (setf (exlybar-module-format-spec m) status
            (exlybar-module-text m) txt
            (exlybar-module-needs-refresh? m) t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-backlight))
  "Set the M's icon and update the text."
  (exlybar-backlight--do-update m))

(cl-defmethod exlybar-module-init :after ((m exlybar-backlight))
  "Run the update timer."
  (unless exlybar-backlight--update-timer
    (setq exlybar-backlight--update-timer
          (run-at-time nil 10 #'exlybar-backlight--do-update m))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-backlight))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-backlight--update-timer
    (cancel-timer exlybar-backlight--update-timer))
  (setq exlybar-backlight--update-timer nil))

(defadvice backlight--set-brightness
    (after exlybar-backlight-after-set-brightness activate)
  "Refresh the module if the brightness is adjusted in Emacs."
  (when (exlybar-enabled-p)
    (let ((m (seq-find (lambda (m)
                         (equal "backlight"
                                (when (exlybar-module-p m)
                                  (exlybar-module-name m))))
                       exlybar-modules)))
      (when m
        (exlybar-backlight--do-update m)
        (exlybar-refresh-modules)))))

(provide 'exlybar-backlight)
;;; exlybar-backlight.el ends here
