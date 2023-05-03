;;; exlybar-module-helpers.el --- Exlybar module helper fns -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.3
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

;; This provides helper fns for modules.

;;; Code:

(require 'cl-lib)

(require 'exlybar-color)

;; TODO change these to keyword params
(cl-defun exlybar-zone-color
    (amount &optional (med 20) (hi 50) (crit 90) reverse local?)
  "Return a color command based on the magnitude of the argument. If
the limits for the levels aren't specified, they default to sensible
values for a percentage. With reverse, lower numbers are more
critical. With local? t, the color code is made local."
  (cl-flet ((past (n) (funcall (if reverse #'<= #'>=) amount n)))
    (let ((zone (cond ((past crit) exlybar-color-zone-crit)
                      ((past hi) exlybar-color-zone-hi)
                      ((past med) exlybar-color-zone-med)
                      (t ""))))
      (if (and (not (seq-empty-p zone)) local?) (s-append "~" zone)
        zone))))

(cl-defun exlybar-progress-bar
    (percent increment colorize
             &key (fill ?—) (blank ? ) (right ?\]) (left ?\[))
  "Given PERCENT and INCREMENT return a string of RIGHT FILL* BLANK* LEFT.
If RIGHT or LEFT are nil, they are respectively excluded.
COLORIZE t to use default zone color codes, nil for no color
codes, or a list of arguments excluding amount to pass to
`exlybar-zone-color'"
  (let* ((progress (/ percent increment))
         (steps (/ 100 increment))
         (bar (when left `(,left)))
         (zone-color
          (apply #'exlybar-zone-color percent
                 (when (consp colorize) colorize)))
         (has-zone? (and colorize (not (seq-empty-p zone-color)))))
    (when has-zone?
      (push ?^ bar) (push ?\[ bar)
      (cl-loop for c across zone-color do (push c bar)))
    (cl-loop with end-zone? = nil
             for should-end-zone? = (and has-zone? (not end-zone?))
             repeat steps do
             (if (< 0 progress)
                 (progn (cl-decf progress) (push fill bar))
               (progn (when should-end-zone?
                        (push ?^ bar) (push ?\] bar)
                        (setq end-zone? t))
                      (push blank bar)))
             finally (when should-end-zone?
                       (push ?^ bar) (push ?\] bar)))
    (when right (push right bar))
    (apply #'string (nreverse bar))))

(defsubst exlybar-choose-icon (val icons)
  "Return first (cdr icon) for which (< VAL (car icon)) is t.
ICONS is an alist of the form ((ival1 . icon1) ... (ivaln . iconn)). ivals are
expected to be in ascending order."
  (cdr (seq-find (pcase-lambda (`(,p . ,_)) (< val p)) icons)))

(provide 'exlybar-module-helpers)
;;; exlybar-module-helpers.el ends here
