;;; slothbar-volume.el --- An slothbar volume module  -*- lexical-binding: t -*-

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

;; This is an implementation of `slothbar-module' for volume status
;; information.

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'volume)
(require 'cl-lib)
(require 'f)

(require 'slothbar-color)
(require 'slothbar-module-)

(defgroup slothbar-volume nil
  "An Slothbar volume module."
  :group 'slothbar)

(defcustom slothbar-volume-progress-increment 10
  "The percent step increment for the volume module progress bar."
  :type 'integer
  :group 'slothbar-volume)

(defcustom slothbar-volume-color-zones '(20 50 80 nil nil)
  "Volume percentages indicating progress color changes.
See `slothbar-color-zone'"
  :type '(list (integer :tag "Med") (integer :tag "Hi") (integer :tag "Crit")
               (boolean :tag "Reverse?") (boolean :tag "Local?"))
  :group 'slothbar-volume)

(cl-defstruct (slothbar-volume
               (:include slothbar-module (name "volume") (icon '((33 . ?) (67 . ?) (110 . ?)))
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn #'slothbar-volume-format-format))
               (:constructor slothbar-volume-create)
               (:copier nil))
  (channel nil :type 'string
           :documentation "Channel name corresponding to `volume-channels'. When nil, it will be
`volume-default-channel'."))

(defun slothbar-volume-current-progress (pct)
  "Build a progress bar corresponding to the current PCT."
  (slothbar-color-progress-bar
   pct slothbar-volume-progress-increment slothbar-volume-color-zones))

(defun slothbar-volume--format-fn-spec (pct)
  "Build the `format-spec' spec used by the format-fn given PCT."
  `((?p . ,(slothbar-volume-current-progress pct))))

(defun slothbar-volume-format-format (m)
  "This is the default format-fn that is applied to module M's format."
  (let ((default-directory (f-full "~")))
    (format-spec (slothbar-module-format m)
		 (slothbar-volume--format-fn-spec (slothbar-volume--get-status m)) t)))

(defun slothbar-volume--get-status (m)
  "Return volume status for the given module M."
  (let* ((channel (slothbar-volume-channel m))
         (volume-amixer-current-channel (or channel (volume-default-channel))))
    (volume-get)))

(defun slothbar-volume--format-spec (m pct)
  "Build M's `format-spec' spec used to generate module text given PCT."
  `((?i . ,(string (slothbar-color-choose-icon pct (slothbar-module-icon m))))))

(cl-defmethod slothbar-module-update-status ((m slothbar-volume))
  "Query the volume backend and check whether to update M's text."
  (let* ((default-directory (f-full "~"))
	 (pct (slothbar-volume--get-status m))
         (status (slothbar-volume--format-spec m pct))
         (txt (number-to-string pct)))
    (unless (equal txt (slothbar-volume-text m))
      (setf (slothbar-module-format-spec m) status
            (slothbar-module-text m) txt
            (slothbar-module-needs-refresh? m) t))))

(defun slothbar-volume--refresh-advice (&rest _)
  "Refresh any module instances if the volume is adjusted in Emacs."
  (slothbar-module--refresh-all-by-name "volume"))

(cl-defmethod slothbar-module-init :before ((m slothbar-volume))
  "Set the M's icon and update the text."
  (advice-add 'volume-update :after #'slothbar-volume--refresh-advice)
  (advice-add 'volume-set :after #'slothbar-volume--refresh-advice)
  (slothbar-module-update-status m))

(cl-defmethod slothbar-module-exit :after ((_ slothbar-volume))
  "Tear down module M."
  (advice-remove 'volume-update #'slothbar-volume--refresh-advice)
  (advice-remove 'volume-set #'slothbar-volume--refresh-advice))

(provide 'slothbar-volume)
;;; slothbar-volume.el ends here
