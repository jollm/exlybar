;;; exlybar-module-helpers.el --- Exlybar module helper fns -*- lexical-binding: t -*-

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

;; This provides helper fns for modules.

;;; Code:

(require 'cl-lib)

(cl-defun exlybar-progress-bar
    (percent increment &key (fill ?—) (blank ? ) (right ?\]) (left ?\[))
  "Given PERCENT and INCREMENT return a string of RIGHT FILL* BLANK* LEFT.
If RIGHT or LEFT are nil, they are respectively excluded."
  (let* ((progress (/ percent increment))
         (steps (/ 100 increment))
         (bar (when left `(,left))))
    (cl-loop repeat steps do
             (if (< 0 progress)
                 (progn (cl-decf progress) (push fill bar))
               (push blank bar)))
    (when right (push right bar))
    (apply #'string (nreverse bar))))

(defsubst exlybar-choose-icon (val icons)
  "Return first (cdr icon) for which (< VAL (car icon)) is t.
ICONS is an alist of the form ((ival1 . icon1) ... (ivaln . iconn)). ivals are
expected to be in ascending order."
  (cdr (seq-find (pcase-lambda (`(,p . ,i)) (< val p)) icons)))

(provide 'exlybar-module-helpers)
;;; exlybar-module-helpers.el ends here