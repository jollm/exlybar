;;; exlybar-module-types.el --- base type for modules  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "27.1"))
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

;; exlybar-module-types.el:
;; Provides a base type for exlybar modules.

;;; Code:

(require 'cl-lib)

(require 'exlybar-common)

(defvar exlybar-module-min-width 10
  "Let modules have a min width so an empty module is visible.")

(cl-defstruct
    (exlybar-module-rgb
     (:constructor exlybar-module-rgb-create)
     (:copier nil))
  "Container for a background and a foreground color."
  (background-color (exlybar--color->pixel
                     (exlybar--find-background-color)) :type 'number)
  (foreground-color (exlybar--color->pixel
                     (exlybar--find-foreground-color)) :type 'number))

(cl-defstruct
    (exlybar-module
     (:constructor exlybar-module-create)
     (:copier nil))
  "This is a base type for exlybar modules."
  (name nil :type 'string)
  (text nil :type 'list)
  (fonts `(,exlybar-icon-font ,exlybar-text-font) :type 'list)
  (text-layout nil :type 'list)
  (colors (exlybar-module-rgb-create) :type 'exlybar-module-rgb)
  (icon nil :type 'character)
  (animation nil :type 'function)
  (cache nil :type 'alist)
  (width exlybar-module-min-width :type 'fixed)
  (prev-width 0 :type 'fixed)
  (xcb nil :type 'list))

(provide 'exlybar-module-types)

;;; exlybar-module-types.el ends here
