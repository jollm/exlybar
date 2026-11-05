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
  '(:width :precision :pad :truncate :case :prefix :suffix :char :decimals)
  "Options accepted in a token list.")

(cl-defstruct
    (slothbar-format-slot
     (:constructor slothbar-format-slot-create)
     (:copier nil))
  "One value slot in a compiled format plan."
  char
  keyword
  options)

(cl-defstruct
    (slothbar-format-plan
     (:constructor slothbar-format-plan-create)
     (:copier nil))
  "A format string and the value slots needed to render it."
  format
  slots)

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

(defun slothbar-format--spec (keyword options fields &optional character)
  "Build one `format-spec' specification for KEYWORD.
OPTIONS is a property list; FIELDS maps keywords to characters.
CHARACTER overrides the character resolved from FIELDS."
  (let* ((plist (slothbar-format--validate-options options))
         (character (or character
                        (slothbar-format--field-character keyword fields plist)))
         (width (plist-get plist :width))
         (precision (plist-get plist :precision))
         (prefix (or (plist-get plist :prefix) ""))
         (suffix (or (plist-get plist :suffix) "")))
    (when (and width (not (and (integerp width) (>= width 0))))
      (error "Invalid :width value %S" width))
    (when (and precision
               (not (and (integerp precision) (>= precision 0))))
      (error "Invalid :precision value %S" precision))
    (when (eq character ?%)
      (error "Format field %S cannot use %% as its character" keyword))
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

(defun slothbar-format--token-keyword-options (token)
  "Return (KEYWORD . OPTIONS) for a field TOKEN."
  (cond
   ((keywordp token)
    (cons token nil))
   ((consp token)
    (let ((keyword (car token))
          (rest (cdr token)))
      (unless (keywordp keyword)
        (error "Format token must start with a keyword, got %S" token))
      (cond
       ((null rest)
        (cons keyword nil))
       ((integerp rest)
        (cons keyword (list :width rest)))
       ((and (listp rest) (keywordp (car rest)))
        (cons keyword rest))
       (t
        (error "Invalid format token %S" token)))))
   (t
    (error "Invalid format token %S" token))))

(defun slothbar-format--candidate-characters ()
  "Return characters available for format value slots."
  (delete-dups
   (delq ?%
         (append (number-sequence ?a ?z)
                 (number-sequence ?A ?Z)
                 (number-sequence ?0 ?9)
                 (number-sequence ?! ?~)))))

(defun slothbar-format--allocate-character (preferred used)
  "Return a free format character, preferring PREFERRED.
USED is a list of characters already assigned."
  (or (and preferred (not (memq preferred used)) preferred)
      (cl-find-if (lambda (character) (not (memq character used)))
                  (slothbar-format--candidate-characters))
      (error "No free format characters remain")))

(defun slothbar-format--plan-token (token fields used)
  "Return (STRING . SLOT) for TOKEN.
SLOT is nil for literal strings.  USED is the characters already assigned."
  (cond
   ((stringp token)
    (cons (slothbar-format--escape token) nil))
   ((or (keywordp token) (consp token))
    (let* ((keyword-options (slothbar-format--token-keyword-options token))
           (keyword (car keyword-options))
           (options (cdr keyword-options))
           (plist (slothbar-format--validate-options options))
           (decimals (plist-get plist :decimals)))
      (unless (or (null decimals)
                  (and (integerp decimals) (>= decimals 0)))
        (error "Invalid :decimals value %S" decimals))
      (let* ((preferred (slothbar-format--field-character
                         keyword fields plist))
             (character (slothbar-format--allocate-character preferred used))
             (spec (slothbar-format--spec keyword plist fields character)))
        (cons spec
              (slothbar-format-slot-create
               :char character
               :keyword keyword
               :options plist)))))
   (t
    (error "Invalid format token %S" token))))

(defun slothbar-format-compile-plan (tokens fields)
  "Compile TOKENS into a `slothbar-format-plan'.
FIELDS maps keywords to their preferred `format-spec' characters.  Repeated
fields receive distinct characters so callers can format each occurrence
independently, including with different :decimals values.  A consumer that
applies :decimals before rendering should treat :width as the width of that
rounded value string, including its sign and decimal point but not :prefix or
:suffix."
  (unless (listp tokens)
    (setq tokens (list tokens)))
  (let ((used nil)
        (strings nil)
        (slots nil))
    (dolist (token tokens)
      (let* ((result (slothbar-format--plan-token token fields used))
             (string (car result))
             (slot (cdr result)))
        (push string strings)
        (when slot
          (push (slothbar-format-slot-char slot) used)
          (push slot slots))))
    (slothbar-format-plan-create
     :format (mapconcat #'identity (nreverse strings) "")
     :slots (nreverse slots))))

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
  :decimals   numeric formatter hint for plan consumers; ignored by this
              string-only entry point

FIELDS is an alist mapping keywords to single characters.

Use `slothbar-format-compile-plan' when a token's :decimals option should
reach the value formatter; `slothbar-format-compile' only builds the string.

For plan consumers such as sysinfo, :decimals rounds the value before
`format-spec' sees it.  :width then applies to that formatted value string,
counting the sign, decimal point, and digits, but not :prefix or :suffix.
Width is a minimum; use :truncate or :precision to cut characters.  :precision
is string truncation, not a decimal-place setting.

Example:

  (slothbar-format-compile
   (list (list :cpu-usage :width 4 :pad :zero :suffix \"%\")
         \" \"
         (list :load :width 5 :precision 2))
   (list (cons :cpu-usage ?c) (cons :load ?l)))
  => \"%04c%% %5.2l\""
  (slothbar-format-plan-format
   (slothbar-format-compile-plan tokens fields)))

(provide 'slothbar-format)
;;; slothbar-format.el ends here
