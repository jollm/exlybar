;;; slothbar-module.el --- Base types and generics for modules  -*- lexical-binding: t -*-

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

;; slothbar-module.el:
;; Provides a base type for slothbar modules.

;;; Code:

(require 'cl-lib)

(require 'slothbar-util)

(defvar slothbar-module-min-width 10
  "Let modules have a min width so an empty module is visible.")

;; TODO: allow changing backgrounds colors using color codes
(cl-defstruct
    (slothbar-module-rgb
     (:constructor slothbar-module-rgb-create)
     (:copier nil))
  "Container for a background and a foreground color."
  (background-color (slothbar-util--color->pixel
                     (slothbar-util--find-background-color)) :type 'number)
  (foreground-color (slothbar-util--color->pixel
                     (slothbar-util--find-foreground-color)) :type 'number))

(cl-defstruct
    (slothbar-module
     (:constructor slothbar-module-create)
     (:copier nil))
  "This is a base type for slothbar modules."
  (name nil :type 'string)
  (text nil :type 'string)
  (format "" :type 'string)
  (format-fn nil :type 'function)
  (format-spec nil :type 'string)
  (text-layout nil :type 'list)
  (lpad 14 :type 'fixed)
  (rpad 14 :type 'fixed)
  (colors (slothbar-module-rgb-create) :type 'slothbar-module-rgb)
  (icon nil :type '(or character alist)
        :documentation "Either a single character or an alist as expected by
`slothbar-color-choose-icon'.")
  (animation nil :type 'function)
  (cache nil :type 'hash-table)
  (width slothbar-module-min-width :type 'fixed)
  (xcb nil :type 'list)
  (needs-refresh? nil :type 'boolean)
  (update-timer nil :type 'timer))

(cl-defgeneric slothbar-module-init (m) "Initialize module M.")

(cl-defgeneric slothbar-module-layout (m)
  "Give module M a layout.")

(cl-defgeneric slothbar-module-refresh (m) "Refresh module M.")

(cl-defgeneric slothbar-module-reposition (m x y)
  "Tell module M about its layout X and Y.")

(cl-defgeneric slothbar-module-update-status (m)
  "Refresh any backend status information required by module M.")

(cl-defgeneric slothbar-module-exit (m) "Tear down module M.")

(provide 'slothbar-module)

;;; slothbar-module.el ends here
