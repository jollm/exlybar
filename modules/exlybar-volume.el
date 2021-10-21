;;; exlybar-volume.el --- An exlybar volume module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((volume "20201002.1022") (emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for volume status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'volume)
(require 'cl-lib)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-volume nil
  "An Exlybar volume module."
  :group 'exlybar)

(defcustom exlybar-volume-progress-increment 10
  "The percent step increment for the volume module progress bar."
  :group 'exlybar-volume)

(defcustom exlybar-volume-icons '((33 . ?) (67 . ?) (110 . ?))
  "Icons for exlybar-volume. See `exlybar-choose-icon' for how it is used."
  :type 'alist
  :group 'exlybar-volume)

(defcustom exlybar-volume-color-zones '(20 50 80)
  "Volume percentages indicating progress color changes.
See `exlybar-zone-color'"
  :type 'list
  :group 'exlybar-volume)

(cl-defstruct (exlybar-volume
               (:include exlybar-module (name "volume") (icon ?)
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn 'exlybar-volume-format-format))
               (:constructor exlybar-volume-create)
               (:copier nil)))

(defvar exlybar-volume--update-timer nil
  "A variable to hold the update timer.")

(defun exlybar-volume-current-progress (pct)
  "Build a progress bar corresponding to the current PCT."
  (exlybar-progress-bar
   pct exlybar-volume-progress-increment exlybar-volume-color-zones))

(defun exlybar-volume--format-fn-spec (pct)
  "Build the `format-spec' spec used by the format-fn given PCT."
  `((?p . ,(exlybar-volume-current-progress pct))))

(defun exlybar-volume-format-format (m)
  "This is the default format-fn that is applied to format."
  (format-spec (exlybar-module-format m)
               (exlybar-volume--format-fn-spec (volume-get)) t))

(defun exlybar-volume--format-spec (pct)
  "Build the `format-spec' spec used to generate module text given PCT."
  `((?i . ,(string (exlybar-choose-icon pct exlybar-volume-icons)))))

(defun exlybar-volume--do-update (m)
  "Poll the battery status and check whether to update M's text."
  (let* ((pct (volume-get))
         (status (exlybar-volume--format-spec pct))
         (txt (number-to-string pct)))
    (unless (equal txt (exlybar-volume-text m))
      (setf (exlybar-module-format-spec m) status
            (exlybar-module-text m) txt
            (exlybar-module-needs-refresh? m) t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-volume))
  "Set the M's icon and update the text."
  (exlybar-volume--do-update m))

(cl-defmethod exlybar-module-init :after ((m exlybar-volume))
  "Run the update timer."
  (unless exlybar-volume--update-timer
    (setq exlybar-volume--update-timer
          (run-at-time nil 10 #'exlybar-volume--do-update m))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-volume))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-volume--update-timer
    (cancel-timer exlybar-volume--update-timer))
  (setq exlybar-volume--update-timer nil))

(defadvice volume-update
    (after exlybar-volume-after-volume-update activate)
  "Refresh the module if the brightness is adjusted in Emacs."
  (when (exlybar-enabled-p)
    (let ((m (seq-find (lambda (m)
                         (equal "volume"
                                (when (exlybar-module-p m)
                                  (exlybar-module-name m))))
                       exlybar-modules)))
      (when m
        (exlybar-volume--do-update m)
        (exlybar-refresh-modules)))))

(provide 'exlybar-volume)
;;; exlybar-volume.el ends here
