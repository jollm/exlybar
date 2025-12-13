;;; slothbar-color.el --- Support color code formats similar to stumpwm's -*- lexical-binding: t -*-

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

;; Support color codes in format strings like those in stumpwm.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 's)
(require 'seq)

(require 'slothbar-util)
(require 'slothbar-log)
;; FIXME: store colors without relying on an xcb type
(require 'slothbar-render)

;;; foreground colors

(defcustom slothbar-color-fg
  '(dark
    (:red #xeeee :green #xffff :blue #xffff :alpha #xeeee)
    light
    (:red #x4f4f :green #x2020 :blue #x4f4f :alpha #xeeee))
  "The default text color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-notice
  '(dark
    (:red #xc3c3 :green #xe8e8 :blue #x8d8d :alpha #xeeee)
    light
    (:red #x0a0a :green #x7a7a :blue #x1a1a :alpha #xeeee))
  "The default notice color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-diminish
  '(dark
    (:red #x6767 :green #x6e6e :blue #x9595 :alpha #xeeee)
    light
    (:red #x6f6f :green #x6868 :blue #x7777 :alpha #xeeee))
  "The default diminish color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-warning
  '(dark
    (:red #xffff :green #xcbcb :blue #x6b6b :alpha #xeeee)
    light
    (:red #xa0a0 :green #x5959 :blue #x0000 :alpha #xeeee))
  "The default warning color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-critical
  '(dark
    (:red #xffff :green #x5353 :blue #x7070 :alpha #xeeee)
    light
    (:red #xa7a7 :green #x3030 :blue #x8080 :alpha #xeeee))
  "The default critical color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-blueish
  '(dark
    (:red #x8282 :green #xaaaa :blue #xffff :alpha #xeeee)
    light
    (:red #x2626 :green #x5f5f :blue #xbfbf :alpha #xeeee))
  "A blueish color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-amaranthish
  '(dark
    (:red #xc7c7 :green #x9292 :blue #xeaea :alpha #xeeee)
    light
    (:red #xe5e5 :green #x2b2b :blue #x5050 :alpha #xeeee))
  "A purplish color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-pinkish
  '(dark
    (:red #xffff :green #x6e6e :blue #xb4b4 :alpha #xeeee)
    light
    (:red #xffff :green #x6e6e :blue #xb4b4 :alpha #xeeee))
  "A pinkish color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

(defcustom slothbar-color-orangeish
  '(dark
    (:red #xf7f7 :green #x8c8c :blue #x6c6c :alpha #xeeee)
    light
    (:red #xb2b2 :green #x1f1f :blue #x0000 :alpha #xeeee))
  "An orangeish color."
  :type '(plist :value-type
                (list symbol natnum symbol natnum symbol natnum symbol natnum))
  :group 'slothbar)

;;; background colors

;; TODO: use this
;; (defcustom slothbar-color-bg
;;   (slothbar-util--color->pixel
;;    (slothbar-util--find-background-color))
;;   "The default background color.
;; Currently unused."
;;   :type 'natnum
;;   :group 'slothbar)

;;; color maps

(defun slothbar-color--gen-color-map ()
  "Generate the ^0-^9 color map."
  (let ((color-key (or (frame-parameter nil 'background-mode) 'dark)))
    (cl-loop with colors = (list slothbar-color-fg
                                 slothbar-color-notice
                                 slothbar-color-diminish
                                 slothbar-color-warning
                                 slothbar-color-critical
                                 slothbar-color-blueish
                                 slothbar-color-amaranthish
                                 slothbar-color-pinkish
                                 slothbar-color-orangeish
                                 slothbar-color-fg)
             with vec = (make-vector (length colors) nil)
             for i from 0
             for color-map in colors
             do (aset vec i
                      (apply #'slothbar-render-create-color
                             (plist-get color-map color-key)))
             finally return vec)))

(defcustom slothbar-color-map-fg (slothbar-color--gen-color-map)
  "The color map corresponding to color codes ^0-^9."
  :type '(vector sexp sexp sexp sexp sexp sexp sexp sexp sexp sexp)
  :group 'slothbar)

(defcustom slothbar-color-zone-crit "^4"
  "A color for critical zone.
See `slothbar-color-zone'"
  :type 'string
  :group 'slothbar)

(defcustom slothbar-color-zone-hi "^3"
  "A color for hi zone.
See `slothbar-color-zone'"
  :type 'string
  :group 'slothbar)

(defcustom slothbar-color-zone-med "^5"
  "A color for med zone.
See `slothbar-color-zone'"
  :type 'string
  :group 'slothbar)

(defsubst slothbar-color-find (color-index fg)
  "Find a color given COLOR-INDEX.
FG t if a foreground color, nil if a background color"
  (if fg (aref slothbar-color-map-fg color-index)
    (slothbar-util--color->pixel
     (slothbar-util--find-background-color))))

;; TODO change these to keyword params
(cl-defun slothbar-color-zone
    (amount &optional (med 20) (hi 50) (crit 90) reverse? local?)
  "Return a color command based on the magnitude of the AMOUNT, an integer.

If the limits MED, HI, and CRIT for the levels aren't specified, they
default to sensible values for a percentage.  See
`slothbar-color-zone-med', `slothbar-color-zone-hi', and
`slothbar-color-zone-crit' to customize these defaults.

With REVERSE? t, lower numbers are more critical.  With LOCAL? t, the
color code is made local."
  (cl-flet ((past (n) (funcall (if reverse? #'<= #'>=) amount n)))
    (let ((zone (cond ((past crit) slothbar-color-zone-crit)
                      ((past hi) slothbar-color-zone-hi)
                      ((past med) slothbar-color-zone-med)
                      (t ""))))
      (if (and (not (seq-empty-p zone)) local?) (s-append "~" zone)
        zone))))

(cl-defun slothbar-color-progress-bar
    (percent increment colorize
             &key (fill ?—) (blank ? ) (right ?\]) (left ?\[))
  "Given PERCENT and INCREMENT return a string of RIGHT FILL* BLANK* LEFT.
If RIGHT or LEFT are nil, they are respectively excluded.
COLORIZE t to use default zone color codes, nil for no color
codes, or a list of arguments excluding amount to pass to
`slothbar-color-zone'"
  (let* ((progress (/ percent increment))
         (steps (/ 100 increment))
         (bar (when left `(,left)))
         (zone-color
          (apply #'slothbar-color-zone percent
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

(defsubst slothbar-color-choose-icon (val icons)
  "Return first (cdr icon) for which (< VAL (car icon)) is t.

ICONS is an alist of the form ((ival1 . icon1) ... (ivaln . iconn)).
ivals are expected to be in ascending order."
  (cdr (seq-find (pcase-lambda (`(,p . ,_)) (< val p)) icons)))

(defvar slothbar-color--pattern
  "\\^[][nrRbB>^;]\\|\\^[0-9*]\\{1,2\\}~?\\|\\^f[0-9]\\|\\^(.*?)"
  "This is the same color code pattern used in stumpwm.
Note that not yet all of these options are supported for slothbar.")

(defun slothbar-color-parse (color)
  "Parse a possible colorcode into a list of the appropriate modifiers.
If COLOR isn't a colorcode a list containing COLOR is returned."
  (if (and (> (length color) 1)
           (= (elt color 0) ?^))
      (let ((foreground (elt color 1))
            (background (if (> (length color) 2)
                            (elt color 2)
                          :reset))
            (till-next (if (> (length color) 3)
                           (elt color 3)
                         (when (and (> (length color) 2)
                                    (= (elt color 2) ?~))
                           (elt color 2)))))
        (cl-case foreground
          ;; Normalize colors
          (?n '((:bg :reset)
                 (:fg :reset)
                 (:reverse nil)))
          (?R '((:reverse t)))
          (?r '((:reverse nil)))
          (?B '((:bright t)))
          (?b '((:bright nil)))
          (?\[ '((:push)))
          (?\] '((:pop)))
          (?> '((:>)))
          (?f `((:font ,(string-to-number (string background)))))
          (?^ '("^"))
          (?\; nil)
          (?\( (list (car (read-from-string color 1))))
          ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?*)
           `(,@(when till-next '((:~)))
             (:bg ,(if (characterp background)
                       (string-to-number (string background))
                     :reset))
             (:fg ,(string-to-number (string foreground)))
             (:reverse nil)))))
    (list color)))

(defun slothbar-color-parse-string (string)
  "Parse a color-coded STRING into a list of strings and color modifiers."
  ;; these two fns attempt to replicate cl ppcre:split
  (cl-labels ((expand-positions (pos length)
                (if pos
                    (cl-loop for (p . rest) on pos with res = nil do
                             (push p res)
                             (let ((nleft (if rest (caar rest) length)))
                               (when (< (cdr p) nleft)
                                 (push `(,(cdr p) . ,nleft) res)))
                             finally return (nreverse res))
                  `((0 . ,length))))
              (split-retain (s regex)
                (let* ((pos (s-matched-positions-all regex s))
                       (pos (expand-positions pos (length string))))
                  ;; pretend below is:
                  ;; (cl-loop for pos in pos collect
                  ;;          (seq-subseq string (car pos) (cdr pos)))
                  ;; below should be better than the above given what is known
                  ;; about expand-positions
                  (cl-loop for p in pos for acc = nil
                           with chars = (cl-loop for c across string collect c)
                           with res = nil do
                           (cl-loop repeat (- (cdr p) (car p)) do
                                    (push (pop chars) acc))
                           (push (apply #'string (nreverse acc)) res)
                           finally return (nreverse res)))))
    (let ((substrings
           (split-retain string slothbar-color--pattern)))
      (cl-loop for substring in substrings
               with resolve~? = nil append
               (let ((p (slothbar-color-parse substring)))
                 (if (stringp (car p)) p
                   (if (eq :~ (caar p))
                       (progn (setq resolve~? t) (cons '(:push) (cdr p)))
                     (if resolve~?
                         (progn (setq resolve~? nil) (cons '(:pop) p))
                       p))))))))

(provide 'slothbar-color)
;;; slothbar-color.el ends here
