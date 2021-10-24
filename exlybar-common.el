;;; exlybar-common.el --- Exlybar common vars and fns -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.19.4
;; Package-Requires: ((xelb "0.18") (emacs "27.1"))
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

;; Part of exlybar.

;; This module provides common functionality for exlybar.

;;; Code:

(require 'xcb)

(defcustom exlybar-width (display-pixel-width)
  "Exlybar width.

Defaults to the width obtained from `display-pixel-width'"
  :type 'integer
  :group 'exlybar)

(defcustom exlybar-height 20
  "Exlybar height."
  :type 'integer
  :group 'exlybar)

(defcustom exlybar-modules nil
  "List of exlybar modules with optional layout instructions."
  :type 'list
  :group 'exlybar)

(defvar exlybar--connection)

(defun exlybar--color->pixel (color)
  "Convert COLOR to PIXEL (index in TrueColor colormap)."
  (when (and color
             (eq (x-display-visual-class) 'true-color))
    (let ((rgb (x-color-values color)))
      (logior (lsh (lsh (pop rgb) -8) 16)
              (lsh (lsh (pop rgb) -8) 8)
              (lsh (pop rgb) -8)))))

(defun exlybar--find-background-color ()
  "Attempt to guess a reasonable background color."
  ;; TODO update this after a theme is loaded or changed
  ;; also, is it possible to get current theme background directly?
  (frame-parameter (selected-frame) 'background-color))

(defun exlybar--find-foreground-color ()
  "Attempt to guess a reasonable foreground color."
  ;; TODO update this after a theme is loaded or changed
  ;; also, is it possible to get current theme foreground directly?
  (frame-parameter (selected-frame) 'foreground-color))

(defun exlybar--find-root-window-id ()
  "Attempt to find a root window."
  (when exlybar--connection
    (slot-value (car (slot-value
                      (xcb:get-setup exlybar--connection) 'roots))
                'root)))

(provide 'exlybar-common)

;;; exlybar-common.el ends here
