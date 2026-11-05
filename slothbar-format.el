;;; slothbar-format.el --- Backend-neutral format compilation  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Compile keyword-and-option token lists into `format-spec' format strings.
;; The compiler is display-backend neutral and knows nothing about sysinfo or
;; any other module.  A caller supplies an alist mapping keywords to the single
;; `format-spec' characters used by its format.

;;; Code:

(require 'cl-lib)

(defconst slothbar-format--option-keys
  '(:width :precision :pad :truncate :case :prefix :suffix :char)
  "Options accepted in a token list.")

(defun slothbar-format--escape (string)
  "Return STRING with literal percent signs escaped for `format-spec'."
  (unless (stringp string)
    (error "Expected a string, got %S" string))
  (string-replace "%" "%%" string))

(defun slothbar-format--field-character (keyword fields options)
  "Resolve KEYWORD to a `format-spec' character.
FIELDS is an alist of keyword to character.  OPTIONS may override the
mapping with :char."
  (let ((character (or (plist-get options :char)
                       (alist-get keyword fields))))
    (setq character
          (cond
           ((characterp character)
            character)
           ((and (stringp character) (= (length character) 1))
            (aref character 0))
           ((null character)
            (error "Unknown format field %S" keyword))
           (t
            (error "Format field %S must map to one character, got %S"
                   keyword character))))
    (when (eq character ?%)
      (error "Format field %S cannot use %% as its character" keyword))
    character))

(defun slothbar-format--pad-flag (pad)
  "Return the `format-spec' flag for PAD."
  (cond
   ((null pad) "")
   ((memq pad '(left :left)) "-")
   ((memq pad '(right :right)) "")
   ((memq pad '(zero :zero)) "0")
   (t (error "Invalid :pad value %S" pad))))

(defun slothbar-format--truncate-flag (truncate)
  "Return the `format-spec' flag for TRUNCATE."
  (cond
   ((null truncate) "")
   ((memq truncate '(left :left)) "<")
   ((memq truncate '(right :right)) ">")
   (t (error "Invalid :truncate value %S" truncate))))

(defun slothbar-format--case-flag (case)
  "Return the `format-spec' flag for CASE."
  (cond
   ((null case) "")
   ((memq case '(upper :upper)) "^")
   ((memq case '(lower :lower)) "_")
   (t (error "Invalid :case value %S" case))))

(defun slothbar-format--validate-options (options)
  "Validate OPTIONS and return them as a property list."
  (unless (zerop (% (length options) 2))
    (error "Format options must be a property list, got %S" options))
  (let (plist)
    (cl-loop for (key value) on options by #'cddr do
             (unless (memq key slothbar-format--option-keys)
               (error "Unknown format option %S" key))
             (setq plist (plist-put plist key value)))
    plist))

(defun slothbar-format--spec (keyword options fields)
  "Build one `format-spec' specification for KEYWORD.
OPTIONS is a property list.  FIELDS maps keywords to characters."
  (let* ((plist (slothbar-format--validate-options options))
         (character (slothbar-format--field-character keyword fields plist))
         (width (plist-get plist :width))
         (precision (plist-get plist :precision))
         (prefix (or (plist-get plist :prefix) ""))
         (suffix (or (plist-get plist :suffix) "")))
    (when (and width (not (and (integerp width) (>= width 0))))
      (error "Invalid :width value %S" width))
    (when (and precision
               (not (and (integerp precision) (>= precision 0))))
      (error "Invalid :precision value %S" precision))
    (concat
     (slothbar-format--escape prefix)
     "%"
     (slothbar-format--truncate-flag (plist-get plist :truncate))
     (slothbar-format--pad-flag (plist-get plist :pad))
     (slothbar-format--case-flag (plist-get plist :case))
     (if width (number-to-string width) "")
     (if precision (concat "." (number-to-string precision)) "")
     (char-to-string character)
     (slothbar-format--escape suffix))))

(defun slothbar-format--token (token fields)
  "Compile one TOKEN using FIELDS."
  (cond
   ((stringp token)
    (slothbar-format--escape token))
   ((keywordp token)
    (slothbar-format--spec token nil fields))
   ((consp token)
    (let ((keyword (car token))
          (rest (cdr token)))
      (unless (keywordp keyword)
        (error "Format token must start with a keyword, got %S" token))
      (cond
       ((null rest)
        (slothbar-format--spec keyword nil fields))
       ((integerp rest)
        (slothbar-format--spec keyword (list :width rest) fields))
       ((and (listp rest) (keywordp (car rest)))
        (slothbar-format--spec keyword rest fields))
       (t
        (error "Invalid format token %S" token)))))
   (t
    (error "Invalid format token %S" token))))

(defun slothbar-format-compile (tokens fields)
  "Compile TOKENS into a `format-spec' format string.

TOKENS is a list whose elements are strings, keywords, or sublists of the
form \(KEYWORD . OPTIONS).  Strings are literal.  Percent signs in them are
escaped.  A keyword alone uses FIELDS to choose its `format-spec' character.
A sublist may be \(KEYWORD . WIDTH) as a width shorthand, or a property list
of options:

  :width      non-negative field width
  :precision  non-negative string precision
  :pad        `left' (pad right), `right' (pad left), or `zero'
  :truncate   `left' or `right'
  :case       `upper' or `lower'
  :prefix     literal text before the field
  :suffix     literal text after the field
  :char       explicit character, bypassing FIELDS

FIELDS is an alist mapping keywords to single characters.

Example:

  (slothbar-format-compile
   (list (list :cpu-usage :width 4 :pad :zero :suffix \"%\")
         \" \"
         (list :load :width 5 :precision 2))
   (list (cons :cpu-usage ?c) (cons :load ?l)))
  => \"%04c%% %5.2l\""
  (unless (listp tokens)
    (setq tokens (list tokens)))
  (mapconcat (lambda (token)
               (slothbar-format--token token fields))
             tokens
             ""))

(provide 'slothbar-format)
;;; slothbar-format.el ends here
