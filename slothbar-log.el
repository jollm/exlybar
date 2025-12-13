;;; slothbar-log.el --- Slothbar logging -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

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

;; Part of slothbar.

;; This module provides common functionality for slothbar.

;;; Code:

(require 'log4e)

(log4e:deflogger "slothbar" "%t [%l] %m" "%H:%M:%S")

(defun slothbar-log-enable ()
  "Enable slothbar logging."
  (interactive)
  (slothbar--log-enable-logging)
  (slothbar--log-open-log))

(defun slothbar-log-disable ()
  "Disable slothbar logging."
  (interactive)
  (slothbar--log-disable-logging))

(defun slothbar-log-set-level (level)
  "Set slothbar logging LEVEL to trace, debug, info, warn, error, or fatal."
  (interactive
   (let ((levels '("trace" "debug" "info" "warn" "error" "fatal")))
     (list (completing-read "Level: " levels nil t "info"))))
  (slothbar--log-set-level (intern level)))

(provide 'slothbar-log)

;;; slothbar-log.el ends here
