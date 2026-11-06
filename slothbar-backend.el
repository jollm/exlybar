;;; slothbar-backend.el --- Display backend interface for slothbar  -*- lexical-binding: t -*-

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

;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Part of slothbar.
;;
;; This module defines the display backend protocol.  The core owns module
;; construction, layout, timers, and damage computation; a backend owns the
;; connection, the bar surface, and per-module pixel resources.
;;
;; Backends are loaded lazily by `slothbar-backend-create' so an X-only
;; configuration does not need Wayland libraries and vice versa.

;;; Code:

(require 'cl-lib)

(cl-defstruct
    (slothbar-backend
     (:constructor slothbar-backend--make)
     (:copier nil))
  "Base type for slothbar display backends."
  (kind nil :type 'symbol))

(defvar slothbar--backend nil
  "The active `slothbar-backend', or nil when slothbar is stopped.")

(cl-defgeneric slothbar-backend-open (backend)
  "Open BACKEND's connection and bar surface.")

(cl-defgeneric slothbar-backend-close (backend)
  "Tear down BACKEND's surface and connection.")

(cl-defgeneric slothbar-backend-create-bar (backend bar)
  "Create BACKEND's per-bar surface for BAR.")

(cl-defmethod slothbar-backend-create-bar ((_backend t) (_bar t))
  "Default: backends without per-bar surfaces need no setup."
  nil)

(cl-defgeneric slothbar-backend-destroy-bar (backend bar)
  "Destroy BACKEND's per-bar surface for BAR.")

(cl-defmethod slothbar-backend-destroy-bar ((_backend t) (_bar t))
  "Default: backends without per-bar surfaces need no teardown."
  nil)

(cl-defgeneric slothbar-backend-show (backend bar)
  "Map BACKEND's BAR and reserve its space.")

(cl-defgeneric slothbar-backend-hide (backend bar)
  "Unmap BACKEND's BAR and release its reserved space.")

(cl-defgeneric slothbar-backend-present (backend bar layout damage)
  "Draw LAYOUT with BACKEND and update the bar.
LAYOUT is a list of ((X Y) . MODULE) placements as produced by
`slothbar-layout-coordinate'.  DAMAGE is a list of (LEFT RIGHT) segments
that must be repainted, or nil if nothing is known to be stale.")

(cl-defgeneric slothbar-backend-add-module (backend module)
  "Give BACKEND the pixel resources MODULE needs to draw.")

(cl-defgeneric slothbar-backend-remove-module (backend module)
  "Release BACKEND's pixel resources for MODULE.")

(cl-defgeneric slothbar-backend-draw-module (backend module)
  "Draw MODULE's text layout with BACKEND.")

(cl-defgeneric slothbar-backend-reset-module-cache (backend module)
  "Discard BACKEND's render cache for MODULE.")

(cl-defgeneric slothbar-backend-supports-p (backend feature)
  "Return non-nil when BACKEND supports FEATURE.
Known features include `system-tray', `ewmh', `randr', `layer-shell',
`buffer-scale', and `ext-workspace'.")

(cl-defgeneric slothbar-backend-ext-workspace-manager (backend)
  "Return BACKEND's live ext-workspace manager, or nil.")

(cl-defgeneric slothbar-backend-output (backend)
  "Return BACKEND's selected output object, or nil.")

(cl-defgeneric slothbar-backend-focused-window (backend)
  "Return the focused window for BACKEND as a plist, or nil.
The plist contains :title and :app-id when known.")

(defvar slothbar-backend-workspace-change-hook nil
  "Hook run when a backend's workspace information changes.
Functions are called with the backend as the sole argument.")

(defvar slothbar-backend-pointer-button-hook nil
  "Hook run when a pointer button is pressed on the bar.
Functions are called with BACKEND, X, Y, and BUTTON.  BUTTON is one of
`:left', `:middle', or `:right'.  X and Y are surface-local logical
coordinates.")

(defvar slothbar-backend-pointer-motion-hook nil
  "Hook run when the pointer moves over the bar.
Functions are called with BACKEND, X, and Y.  X and Y are nil when the
pointer leaves the bar.")

(defvar slothbar-backend-focused-window-change-hook nil
  "Hook run when the backend's focused window changes.
Functions are called with the backend as the sole argument.")

(declare-function slothbar-backend-xcb-create "slothbar-backend-xcb" ())
(declare-function slothbar-backend-wayland-create "slothbar-backend-wayland" ())

(defun slothbar-backend--wayland-available-p ()
  "Return non-nil when a Wayland socket and the elwl client are available."
  (let* ((display (getenv "WAYLAND_DISPLAY"))
         (runtime (getenv "XDG_RUNTIME_DIR"))
         (socket (and display
                      (if (string-prefix-p "/" display)
                          display
                        (and runtime (expand-file-name display runtime))))))
    (and socket
         (file-exists-p socket)
         (condition-case nil
             (progn (require 'elwl) t)
           (error nil)))))

(defun slothbar-backend-create (&optional kind)
  "Create a backend of KIND, defaulting to `auto'.
KIND is `auto', `xcb', or `wayland'.  The concrete backend module is loaded on
demand."
  (pcase (or kind 'auto)
    ('auto (if (slothbar-backend--wayland-available-p)
               (slothbar-backend-create 'wayland)
             (slothbar-backend-create 'xcb)))
    ('xcb
     (require 'slothbar-backend-xcb)
     (slothbar-backend-xcb-create))
    ('wayland
     (require 'slothbar-backend-wayland)
     (slothbar-backend-wayland-create))
    (_ (error "Unsupported slothbar backend: %s" kind))))

(provide 'slothbar-backend)
;;; slothbar-backend.el ends here
