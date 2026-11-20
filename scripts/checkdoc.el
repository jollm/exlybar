;;; checkdoc.el --- Run checkdoc over Slothbar sources  -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch runner used by `maak checkdoc'.  It checks every top-level
;; slothbar*.el file and exits non-zero if checkdoc reports anything.

;;; Code:

(require 'cl-lib)
(require 'checkdoc)

(defvar slothbar-checkdoc-errors 0
  "Number of checkdoc findings seen in this run.")

(defun slothbar-checkdoc--count (&rest _)
  "Count one checkdoc finding."
  (cl-incf slothbar-checkdoc-errors))

(advice-add 'checkdoc-error :before #'slothbar-checkdoc--count)

(dolist (file (directory-files "." t "\\`slothbar.*\\.el\\'"))
  (checkdoc-file file))

(princ (format "checkdoc findings: %d\n" slothbar-checkdoc-errors))
(kill-emacs (if (zerop slothbar-checkdoc-errors) 0 1))

;;; checkdoc.el ends here
