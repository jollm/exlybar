;;; slothbar-date.el --- An slothbar date/time module  -*- lexical-binding: t -*-

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

;; This is an implementation of `slothbar-module' for date/time status
;; information.

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'time-date)
(require 'solar)

(require 'slothbar-color)
(require 'slothbar-module-)

(defgroup slothbar-date nil
  "An Slothbar date module."
  :group 'slothbar)

(defsubst slothbar-date--equinox-solstice-day (k)
  "Day of the kth equinox/solstice for the current year.

K=0, spring equinox; K=1, summer solstice; K=2, fall equinox;
K=3, winter solstice.  RESULT is a Gregorian local day.
Accurate to within a minute between 1951 and 2050.

See `solar-equinoxes/solstices'"
  (pcase-let* ((year (string-to-number (format-time-string "%Y")))
               (`(,m ,d ,_) (solar-equinoxes/solstices k year)))
    (+ d (cl-loop for m from (1- m) downto 1
                  sum (date-days-in-month year m)))))

(defcustom slothbar-date-color-zones
  `(,(slothbar-date--equinox-solstice-day 0)
    ,(slothbar-date--equinox-solstice-day 1)
    ,(slothbar-date--equinox-solstice-day 2) nil nil)
  "Days of the year indicating seasonal color changes.
See `slothbar-color-zone'"
  :type '(list (float :tag "Med") (float :tag "Hi") (float :tag "Crit")
               (boolean :tag "Reverse?") (boolean :tag "Local?"))
  :group 'slothbar-date)

(defcustom slothbar-date-color-spring "^7"
  "Color for spring dates."
  :type 'string
  :group 'slothbar-date)
(defcustom slothbar-date-color-summer "^5"
  "Color for summer dates."
  :type 'string
  :group 'slothbar-date)
(defcustom slothbar-date-color-fall "^8"
  "Color for fall dates."
  :type 'string
  :group 'slothbar-date)
(defcustom slothbar-date-color-winter "^9"
  "Color for winter dates."
  :type 'string
  :group 'slothbar-date)

(cl-defstruct (slothbar-date
               (:include slothbar-module (name "date") (icon ?)
                         (format (concat "^2^[^f1%i^] ^["
                                         slothbar-date-color-winter
                                         "%ζ%a, %h %e.^] %l:%M %#p %Z"))
                         (format-fn #'slothbar-date-format-format))
               (:constructor slothbar-date-create)
               (:copier nil)))

(defun slothbar-date--zone-color ()
  "Decide the seasonal zone color using solar equinox/solstice calculations."
  (let* ((winter-solstice (slothbar-date--equinox-solstice-day 3))
         (day (string-to-number (format-time-string "%j")))
         (day (if (< winter-solstice day) (- winter-solstice day) day)))
    (let ((slothbar-color-zone-med slothbar-date-color-spring)
          (slothbar-color-zone-hi slothbar-date-color-summer)
          (slothbar-color-zone-crit slothbar-date-color-fall))
      (apply #'slothbar-color-zone day slothbar-date-color-zones))))

(defun slothbar-date--format-fn-spec (zone-color)
  "Build the `format-spec' spec used by the format-fn.

ZONE-COLOR the color code as determined by `slothbar-color-zone'"
  `((?i . ,(format (concat slothbar-date-color-winter "%s%%i") zone-color))
    (?ζ . ,(slothbar-date--zone-color))))

(defun slothbar-date-format-format (m)
  "This is the default format-fn that is applied to module M's format."
  (format-time-string
   (format-spec (slothbar-module-format m)
                (slothbar-date--format-fn-spec (slothbar-date--zone-color)) t)))

(defsubst slothbar-date--format-spec (icon)
  "Build the `format-spec' spec used to generate module text given ICON."
  `((?i . ,(string icon))))

(cl-defmethod slothbar-module-update-status ((m slothbar-date))
  "Poll the wifi status and check whether to update the M's text."
  (let* ((date (format-time-string (slothbar-module-format m))))
    (unless (equal date (slothbar-module-text m))
      (setf
       (slothbar-module-format-spec m)
       (slothbar-date--format-spec (slothbar-module-icon m))
       (slothbar-module-text m) date
       (slothbar-module-needs-refresh? m) t))))

(cl-defmethod slothbar-module-init :before ((m slothbar-date))
  "Set the M's icon and update the text."
  (slothbar-module-update-status m))

(provide 'slothbar-date)
;;; slothbar-date.el ends here
