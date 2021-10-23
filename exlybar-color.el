;;; exlybar-color.el --- support color code formats similar to stumpwm's -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (s "1.12.0") (emacs "27.1"))
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

;; Support color codes in format strings like those in stumpwm.

;;; Code:

(require 'cl-lib)
(require 's)

;; FIXME: store colors without relying on an xcb type
(require 'exlybar-render)

;;; foreground colors

(defcustom exlybar-color-fg
  (exlybar-render-create-color
   :red #xeeee :green #xffff :blue #xffff :alpha #xeeee)
  "The default text color."
  :type 'xcb:render:COLOR
  :group 'exlybar)

(defcustom exlybar-color-notice
  (exlybar-render-create-color
   :red #xc3c3 :green #xe8e8 :blue #x8d8d :alpha #xeeee)
  "The default notice color."
  :type 'xcb:render:COLOR
  :group 'exlybar)

(defcustom exlybar-color-diminish
  (exlybar-render-create-color
   :red #x6767 :green #x6e6e :blue #x9595 :alpha #xeeee)
  "The default diminish color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

(defcustom exlybar-color-warning
  (exlybar-render-create-color
   :red #xffff :green #xcbcb :blue #x6b6b :alpha #xeeee)
  "The default warning color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

(defcustom exlybar-color-critical
  (exlybar-render-create-color
   :red #xffff :green #x5353 :blue #x7070 :alpha #xeeee)
  "The default critical color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

(defcustom exlybar-color-blueish
  (exlybar-render-create-color
   :red #x8282 :green #xaaaa :blue #xffff :alpha #xeeee)
  "A blueish color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

(defcustom exlybar-color-amaranthish
  (exlybar-render-create-color
   :red #xc7c7 :green #x9292 :blue #xeaea :alpha #xeeee)
  "A purplish color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

(defcustom exlybar-color-pinkish
  (exlybar-render-create-color
   :red #xffff :green #x6e6e :blue #xb4b4 :alpha #xeeee)
  "A pinkish color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

(defcustom exlybar-color-orangeish
  (exlybar-render-create-color
   :red #xf7f7 :green #x8c8c :blue #x6c6c :alpha #xeeee)
  "An orangeish color."
  :type 'xcb:render:COLOR'
  :group 'exlybar)

;;; background colors

;; TODO: use this
(defcustom exlybar-color-bg
  (exlybar--color->pixel
   (exlybar--find-background-color))
  "The default background color.
Currently unused."
  :type 'natnum
  :group 'exlybar)

;;; color maps

(defcustom exlybar-color-map-fg
  (vector
   exlybar-color-fg
   exlybar-color-notice
   exlybar-color-diminish
   exlybar-color-warning
   exlybar-color-critical
   exlybar-color-blueish
   exlybar-color-amaranthish
   exlybar-color-pinkish
   exlybar-color-orangeish
   exlybar-color-fg)
  "The color map corresponding to color codes ^0-^9."
  :type 'vector
  :group 'exlybar)

(defcustom exlybar-color-zone-crit "^4"
  "A color for critical zone.
See `exlybar-zone-color'"
  :type 'string
  :group 'exlybar)

(defcustom exlybar-color-zone-hi "^3"
  "A color for hi zone.
See `exlybar-zone-color'"
  :type 'string
  :group 'exlybar)

(defcustom exlybar-color-zone-med "^5"
  "A color for med zone.
See `exlybar-zone-color'"
  :type 'string
  :group 'exlybar)

;;; fonts

(defcustom exlybar-font-variable
  "/usr/share/fonts/TTF/IBMPlexSerif-Regular.ttf"
  "The TTF font path for a text font."
  :type 'string
  :group 'exlybar)

(defcustom exlybar-font-icon
  "/usr/share/fonts/TTF/fontawesome.ttf"
  "The TTF font path for an icon font."
  :type 'string
  :group 'exlybar)

(defcustom exlybar-font-mono
  "/usr/share/fonts/TTF/IBMPlexMono-Regular.ttf"
  "The TTF font path for a text font."
  :type 'string
  :group 'exlybar)

(defcustom exlybar-font-all-the-icons
  "/usr/share/fonts/TTF/all-the-icons.ttf"
  "The TTF font path for all the icons font to add extra icons."
  :type 'string
  :group 'exlybar)

(defcustom exlybar-font-map
  (vector
   exlybar-font-variable
   exlybar-font-icon
   exlybar-font-mono
   exlybar-font-all-the-icons
   exlybar-font-variable
   exlybar-font-variable
   exlybar-font-variable
   exlybar-font-variable
   exlybar-font-variable
   exlybar-font-variable)
  "The font map corresponding to color codes ^f0-^f9."
  :type 'vector
  :group 'exlybar)

(defun exlybar-font--precompute-px-sizes (height)
  "Given a HEIGHT, compute pixel sizes for all fonts in the font map."
  (apply
   #'vector
     (cl-loop for f across exlybar-font-map collect
              (fontsloth-font-compute-px (fontsloth-load-font f) height))))

(defvar exlybar-font-px-size (exlybar-font--precompute-px-sizes exlybar-height)
  "Precomputed font px size map.

There should be no need to recompute pixel sizes unless either the height or
the fonts change.")

(defun exlybar-font--watch-px-size (sym nval oper where)
  "Update `exlybar-font-px-size' when a relevant change occurs."
  (message "watch px size called %s %s %s %s" sym nval oper where)
  (when (and (not where) (eq 'set oper))
    (let ((height (cl-case sym
                    (exlybar-height (when (/= (symbol-value sym) nval) nval))
                    (exlybar-font-map exlybar-height))))
      (when height
        (setq exlybar-font-px-size
              (exlybar-font--precompute-px-sizes height))))))

(add-variable-watcher 'exlybar-height #'exlybar-font--watch-px-size)
(add-variable-watcher 'exlybar-font-map #'exlybar-font--watch-px-size)

(defsubst exlybar-color-find (color-index fg)
  "Find a color given COLOR-INDEX.
FG t if a foreground color, nil if a background color"
  (if fg (aref exlybar-color-map-fg color-index)
    exlybar-color-bg))

(defsubst exlybar-font-find (font-index)
  "Find a font in `exlybar-font-map' given FONT-INDEX."
  (aref exlybar-font-map font-index))

(defvar exlybar--color-pattern
  "\\^[][nrRbB>^;]\\|\\^[0-9*]\\{1,2\\}~?\\|\\^f[0-9]\\|\\^(.*?)"
  "This is the same color code pattern used in stumpwm.
Note that not yet all of these options are supported for exlybar.")

(defun exlybar-color-parse (color)
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

(defun exlybar-color-parse-string (string)
  "Parse a color-coded string into a list of strings and color modifiers."
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
           (split-retain string exlybar--color-pattern)))
      (cl-loop for substring in substrings
               with resolve~? = nil append
               (let ((p (exlybar-color-parse substring)))
                 (if (stringp (car p)) p
                   (if (eq :~ (caar p))
                       (progn (setq resolve~? t) (cons '(:push) (cdr p)))
                     (if resolve~?
                         (progn (setq resolve~? nil) (cons '(:pop) p))
                       p))))))))

(provide 'exlybar-color)
;;; exlybar-color.el ends here
