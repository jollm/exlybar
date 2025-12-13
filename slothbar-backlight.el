;;; slothbar-backlight.el --- An slothbar backlight module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

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

;; This is an implementation of `slothbar-module' for backlight status
;; information.

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'backlight)
(require 'cl-lib)

(require 'slothbar-color)
(require 'slothbar-module-)

(defgroup slothbar-backlight nil
  "An Slothbar backlight module."
  :group 'slothbar)

(defcustom slothbar-backlight-progress-increment 10
  "The percent step increment for the backlight module progress bar."
  :type 'integer
  :group 'slothbar-backlight)

(defcustom slothbar-backlight-color-zones '(20 40 80 nil nil)
  "Backlight percentages indicating progress color changes.
See `slothbar-color-zone'"
  :type '(list (integer :tag "Med") (integer :tag "Hi") (integer :tag "Crit")
               (boolean :tag "Reverse?") (boolean :tag "Local?"))
  :group 'slothbar-backlight)

(cl-defstruct (slothbar-backlight
               (:include slothbar-module (name "backlight") (icon ?)
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn #'slothbar-backlight-format-format))
               (:constructor slothbar-backlight-create)
               (:copier nil)))

(defun slothbar-backlight-current-progress ()
  "Build a progress bar corresponding to the current state."
  (slothbar-color-progress-bar
   (backlight--current-percentage)
   slothbar-backlight-progress-increment slothbar-backlight-color-zones))

(defun slothbar-backlight--format-fn-spec ()
  "Build the `format-spec' spec used by the format-fn."
  `((?p . ,(slothbar-backlight-current-progress))))

(defun slothbar-backlight-format-format (m)
  "This is the default format-fn applied to module M's format."
  (format-spec (slothbar-module-format m)
               (slothbar-backlight--format-fn-spec) t))

(defun slothbar-backlight--format-spec (icon)
  "Build the `format-spec' with ICON used to generate module text."
  `((?i . ,(string icon))))

(cl-defmethod slothbar-module-update-status ((m slothbar-backlight))
  "Get the backlight status and check whether to update M's text."
  (let* ((status (slothbar-backlight--format-spec (slothbar-module-icon m)))
         (txt (number-to-string (and (backlight--read-current-brightness)
                                     (backlight--current-percentage)))))
    (unless (equal txt (slothbar-module-text m))
      (setf (slothbar-module-format-spec m) status
            (slothbar-module-text m) txt
            (slothbar-module-needs-refresh? m) t))))

(defun slothbar-backlight--set-brightness-advice (&rest _)
  "A function to update status when the brightness is changed in Emacs."
  (slothbar-module--refresh-all-by-name "backlight"))

(cl-defmethod slothbar-module-init :before ((m slothbar-backlight))
  "Set the M's icon and update the text."
  (advice-add 'backlight--set-brightness
              :after #'slothbar-backlight--set-brightness-advice)
  (slothbar-module-update-status m))

(cl-defmethod slothbar-module-exit :after ((_ slothbar-backlight))
  "Tear down module M."
  (advice-remove 'backlight--set-brightness
                 #'slothbar-backlight--set-brightness-advice))

(provide 'slothbar-backlight)
;;; slothbar-backlight.el ends here
