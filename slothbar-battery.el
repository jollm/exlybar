;;; slothbar-battery.el --- An slothbar battery module  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jo Gay <jo.gay@mailfence.com>

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

;; This is an implementation of `slothbar-module' for battery status
;; information.

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'battery)
(require 'cl-lib)
(require 'f)
(require 'map)

(require 'slothbar-log)
(require 'slothbar-color)
(require 'slothbar-module-)

(defgroup slothbar-battery nil
  "An Slothbar battery module."
  :group 'slothbar)

(defcustom slothbar-battery-icons
  '((10 . ?) (35 . ?) (60 . ?) (85 . ?) (101 . ?))
  "Icons for slothbar-battery discharge thresholds.
See `slothbar-color-choose-icon' for how it is used."
  :type 'alist
  :group 'slothbar-battery)

(defcustom slothbar-battery-charge-icon ?
  "Icon for when the battery is charging."
  :type 'character
  :group 'slothbar-battery)

(defcustom slothbar-battery-charge-color-command "^1~"
  "Icon and percentage color command for when the battery is charging."
  :type 'string
  :group 'slothbar-battery)

(defcustom slothbar-battery-color-zones '(49 29 10 t t)
  "Battery percentages indicating icon color changes.
See `slothbar-color-zone'"
  :type '(list (integer :tag "Med") (integer :tag "Hi") (integer :tag "Crit")
               (boolean :tag "Reverse?") (boolean :tag "Local?"))
  :group 'slothbar-battery)

(cl-defstruct (slothbar-battery
               (:include slothbar-module (name "battery")
                         (format
                          "^6^[^f1%i^] %b%p% ^[^2|^] %t ^[^2|^] %r"
                          :documentation
                          "%i is for the battery icon.
For the rest, see `battery-status-function'. Note that for %r,
some versions of battery-status-function include the trailing W
and some do not. `battery-linux-sysfs' where available appears
more precise than e.g. `battery-upower'.")
                         (format-fn
                          #'slothbar-battery-format-format
                          :documentation
                          "Pre-format %i in format to use zone colors.
The color is decided based on battery percentage. See `slothbar-color-zone'."))
               (:constructor slothbar-battery-create)
               (:copier nil)))

(defun slothbar-battery--format-fn-spec (zone-color charging?)
  "Build the `format-spec' spec used by the format-fn.

ZONE-COLOR the color code as determined by `slothbar-color-zone'
CHARGING? t if battery status indicates charging, otherwise nil"
  (let ((icon-fmt (if charging? "^[^f3%s%%i^]" "%s%%i")))
    `((?i . ,(format icon-fmt zone-color))
      (?p . ,(format "%s%%p" zone-color)))))

(defun slothbar-battery-format-format (m)
  "This is the default format-fn that is applied to module M's format."
  (let* ((default-directory (f-full "~")) ; ensure status checks don't remote
         (status (or (map-elt (slothbar-module-cache m) 'status)
                     (funcall battery-status-function)))
         (pct (if-let ((pct (map-elt status ?p))) (string-to-number pct) 100))
         (charging? (equal "+" (map-elt status ?b)))
         (zone-color (if charging? slothbar-battery-charge-color-command
                       (apply #'slothbar-color-zone
                              pct slothbar-battery-color-zones))))
    (format-spec (slothbar-module-format m)
                 (slothbar-battery--format-fn-spec zone-color charging?) t)))

(defun slothbar-battery--format-spec (status)
  "Given STATUS, build the `format-spec' spec used to generate module text."
  (let* ((pct (if-let ((pct (map-elt status ?p))) (string-to-number pct) 100))
         (charging? (equal "+" (map-elt status ?b)))
         (icon (if charging? slothbar-battery-charge-icon
                 (slothbar-color-choose-icon pct slothbar-battery-icons))))
    (map-insert status ?i (string icon))))

(cl-defmethod slothbar-module-update-status ((m slothbar-battery))
  "Poll the battery status and check whether to update M's text."
  (condition-case err
      (let* ((default-directory (f-full "~")) ; ensure status checks don't remote
             (status (funcall battery-status-function))
             (txt (format-spec (slothbar-module-format m) status t)))
        (if status
            (progn
              (when (slothbar-module-cache m)
                (map-put! (slothbar-module-cache m) 'status status))
              (unless (equal txt (slothbar-module-text m))
                (setf (slothbar-module-format-spec m)
                      (slothbar-battery--format-spec status)
                      (slothbar-module-text m) txt
                      (slothbar-module-needs-refresh? m) t)))
          (slothbar--log-info "slothbar-battery: nil status from %s"
                             battery-status-function)))
    (t
     (message "error in battery update %s" (error-message-string err)))))

(cl-defmethod slothbar-module-init :after ((m slothbar-battery))
  "Set M's icon and update the text."
  (slothbar-module-update-status m))

(provide 'slothbar-battery)
;;; slothbar-battery.el ends here
