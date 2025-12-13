;;; slothbar-util.el --- Utility code useful in multiple places  -*- lexical-binding: t -*-

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

;; slothbar-util.el:
;; Provides utility code that may be useful in multiple places.

;;; Code:

(require 'xcb)

(declare-function x-display-visual-class "xfns.c" (&optional terminal))

(defun slothbar-util--color->pixel (color)
  "Convert COLOR to PIXEL (index in TrueColor colormap)."
  (when (and color
             (eq (x-display-visual-class) 'true-color))
    (let ((rgb (color-values color)))
      (logior (ash (ash (pop rgb) -8) 16)
              (ash (ash (pop rgb) -8) 8)
              (ash (pop rgb) -8)))))

(defun slothbar-util--find-background-color ()
  "Attempt to guess a reasonable background color."
  ;; TODO update this after a theme is loaded or changed
  ;; also, is it possible to get current theme background directly?
  (frame-parameter (selected-frame) 'background-color))

(defun slothbar-util--find-foreground-color ()
  "Attempt to guess a reasonable foreground color."
  ;; TODO update this after a theme is loaded or changed
  ;; also, is it possible to get current theme foreground directly?
  (frame-parameter (selected-frame) 'foreground-color))

(defvar slothbar-connection)

(defun slothbar-util--find-root-window-id ()
  "Attempt to find a root window."
  (when (and (boundp 'slothbar--connection) slothbar--connection)
    (slot-value (car (slot-value
                      (xcb:get-setup slothbar--connection) 'roots))
                'root)))

(provide 'slothbar-util)

;;; slothbar-util.el ends here
