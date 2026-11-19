;;; package-lint.el --- Run package-lint for Slothbar  -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch runner used by `maak package-lint'.

;;; Code:

(unless (require 'package-lint nil t)
  (error "package-lint is not installed; install it before running this task"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-refresh-contents)

(package-initialize)

(setq package-lint-main-file "slothbar.el")

(package-lint-batch-and-exit)

;;; package-lint.el ends here
