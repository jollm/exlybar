;;; exlybar-log.el --- Exlybar logging -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.0
;; Homepage: https://github.com/jollm/exlybar
;; Keywords: convenience, window-manager, status-bar, exwm

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

;; Part of exlybar.

;; This module provides common functionality for exlybar.

;;; Code:

(require 'log4e)

(log4e:deflogger "exlybar" "%t [%l] %m" "%H:%M:%S")

(defun exlybar-log-enable ()
  "Enable exlybar logging."
  (interactive)
  (exlybar--log-enable-logging)
  (exlybar--log-open-log))

(defun exlybar-log-disable ()
  "Disable exlybar logging."
  (interactive)
  (exlybar--log-disable-logging))

(defun exlybar-log-set-level (level)
  "Set exlybar logging LEVEL to trace, debug, info, warn, error, or fatal."
  (interactive
   (let ((levels '("trace" "debug" "info" "warn" "error" "fatal")))
     (list (completing-read "Level: " levels nil t "info"))))
  (exlybar--log-set-level (intern level)))

(provide 'exlybar-log)

;;; exlybar-log.el ends here
