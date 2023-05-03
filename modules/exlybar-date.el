;;; exlybar-date.el --- An exlybar date/time module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.3
;; Package-Requires: ((cl-lib "0.5") (emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for date/time status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'cl-lib)
(require 'time-date)
(require 'solar)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-date nil
  "An Exlybar date module."
  :group 'exlybar)

(defsubst exlybar-date--equinox/solstice-day (k)
  "Day of the kth equinox/solstice for the current year.

K=0, spring equinox; K=1, summer solstice; K=2, fall equinox;
K=3, winter solstice.  RESULT is a Gregorian local day.
Accurate to within a minute between 1951 and 2050.

See `solar-equinoxes/solstices'"
  (pcase-let* ((year (string-to-number (format-time-string "%Y")))
               (`(,m ,d ,_) (solar-equinoxes/solstices k year)))
    (+ d (cl-loop for m from (1- m) downto 1
                  sum (date-days-in-month year m)))))

(defcustom exlybar-date-color-zones
  `(,(exlybar-date--equinox/solstice-day 0)
    ,(exlybar-date--equinox/solstice-day 1)
    ,(exlybar-date--equinox/solstice-day 2) nil nil)
  "Days of the year indicating seasonal color changes.
See `exlybar-zone-color'"
  :type '(list float float float boolean boolean)
  :group 'exlybar-date)

(defcustom exlybar-date-color-spring "^7"
  "Color for spring dates."
  :type 'string
  :group 'exlybar-date)
(defcustom exlybar-date-color-summer "^5"
  "Color for summer dates."
  :type 'string
  :group 'exlybar-date)
(defcustom exlybar-date-color-fall "^8"
  "Color for fall dates."
  :type 'string
  :group 'exlybar-date)
(defcustom exlybar-date-color-winter "^9"
  "Color for winter dates."
  :type 'string
  :group 'exlybar-date)

(cl-defstruct (exlybar-date
               (:include exlybar-module (name "date") (icon ?)
                         (format (concat "^2^[^f1%i^] ^["
                                         exlybar-date-color-winter
                                         "%ζ%a, %h %e.^] %l:%M %#p %Z"))
                         (format-fn 'exlybar-date-format-format))
               (:constructor exlybar-date-create)
               (:copier nil)))

(defun exlybar-date--zone-color ()
  "Decide the seasonal zone color using solar equinox/solstice calculations."
  (let* ((winter-solstice (exlybar-date--equinox/solstice-day 3))
         (day (string-to-number (format-time-string "%j")))
         (day (if (< winter-solstice day) (- winter-solstice day) day)))
    (let ((exlybar-color-zone-med exlybar-date-color-spring)
          (exlybar-color-zone-hi exlybar-date-color-summer)
          (exlybar-color-zone-crit exlybar-date-color-fall))
      (apply #'exlybar-zone-color day exlybar-date-color-zones))))

(defun exlybar-date--format-fn-spec (zone-color)
  "Build the `format-spec' spec used by the format-fn."
  `((?i . ,(format (concat exlybar-date-color-winter "%s%%i") zone-color))
    (?ζ . ,(exlybar-date--zone-color))))

(defun exlybar-date-format-format (m)
  "This is the default format-fn that is applied to format."
  (format-time-string
   (format-spec (exlybar-module-format m)
                (exlybar-date--format-fn-spec (exlybar-date--zone-color)) t)))

(defsubst exlybar-date--format-spec (icon)
  "Build the `format-spec' spec used to generate module text given ICON."
  `((?i . ,(string icon))))

(defvar exlybar-date--update-timer nil "A variable to hold the update timer.")

(defun exlybar-date--do-update (m)
  "Poll the wifi status and check whether to update the M's text."
  (let* ((date (format-time-string (exlybar-module-format m))))
    (unless (equal date (exlybar-module-text m))
      (setf
       (exlybar-module-format-spec m)
       (exlybar-date--format-spec (exlybar-module-icon m))
       (exlybar-module-text m) date
       (exlybar-module-needs-refresh? m) t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-date))
  "Set the M's icon and update the text."
  (exlybar-date--do-update m))

(cl-defmethod exlybar-module-init :after ((m exlybar-date))
  "Run the update timer."
  (unless exlybar-date--update-timer
    (setq exlybar-date--update-timer
          (run-at-time nil 10 #'exlybar-date--do-update m))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-date))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-date--update-timer
    (cancel-timer exlybar-date--update-timer))
  (setq exlybar-date--update-timer nil))

(provide 'exlybar-date)
;;; exlybar-date.el ends here
