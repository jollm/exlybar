;;; slothbar-tray.el --- An slothbar system tray module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.32.0
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
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Backend-neutral system tray module.  The actual tray protocol lives in a
;; transport module, selected lazily from the active display backend.  Today
;; the only transport is XEmbed.  Future Wayland support can add a
;; StatusNotifier transport without loading xcb.

;; Only one tray instance can be active across all bars.  Later instances are
;; skipped during module construction.

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'cl-lib)

(require 'slothbar-backend)
(require 'slothbar-module-)

(defgroup slothbar-tray nil
  "System tray module."
  :group 'slothbar)

(defcustom slothbar-tray-icon-gap 2
  "Gap between tray icons."
  :type 'integer
  :group 'slothbar-tray)

(cl-defstruct (slothbar-tray
               (:include slothbar-module
                         (name "tray")
                         (format nil)
                         (lpad 4)
                         (rpad 4))
               (:constructor slothbar-tray-create)
               (:copier nil))
  "A system tray module."
  transport)

(defun slothbar-tray--sni-available-p ()
  "Return non-nil when a StatusNotifier host can be started."
  (and (require 'dbus nil t)
       (getenv "DBUS_SESSION_BUS_ADDRESS")))

(cl-defmethod slothbar-module-supported-p
  ((_m slothbar-tray) (backend slothbar-backend))
  "System tray requires an available tray transport."
  (or (slothbar-backend-supports-p backend 'system-tray)
      (slothbar-tray--sni-available-p)))

(cl-defmethod slothbar-module-singleton-id ((_m slothbar-tray))
  "There can be only one tray across all bars."
  'system-tray)

(declare-function slothbar-tray-sni-init "slothbar-tray-sni" (m))
(declare-function slothbar-tray-sni-layout "slothbar-tray-sni" (m))
(declare-function slothbar-tray-sni-refresh "slothbar-tray-sni" (m))
(declare-function slothbar-tray-sni-reposition "slothbar-tray-sni" (m x y))
(declare-function slothbar-tray-sni-exit "slothbar-tray-sni" (m))
(declare-function slothbar-tray-sni-click "slothbar-tray-sni" (m x y button))
(declare-function slothbar-tray-xembed-init "slothbar-tray-xembed" (m))
(declare-function slothbar-tray-xembed-layout "slothbar-tray-xembed" (m))
(declare-function slothbar-tray-xembed-refresh "slothbar-tray-xembed" (m))
(declare-function slothbar-tray-xembed-reposition "slothbar-tray-xembed" (m x y))
(declare-function slothbar-tray-xembed-exit "slothbar-tray-xembed" (m))

(defun slothbar-tray--transport-for-module (m)
  "Return and load the tray transport appropriate for M, or nil."
  (let ((backend (slothbar-module--backend m)))
    (cond
     ((and backend
           (slothbar-backend-supports-p backend 'system-tray))
      (require 'slothbar-tray-xembed)
      'xembed)
     ((slothbar-tray--sni-available-p)
      (require 'slothbar-tray-sni)
      'sni)
     (t nil))))

(cl-defmethod slothbar-module-init :before ((m slothbar-tray))
  "Set up M's tray transport before layout."
  (let ((transport (slothbar-tray--transport-for-module m)))
    (unless transport
      (error "No system tray transport for the active backend"))
    (setf (slothbar-tray-transport m) transport)
    (pcase transport
      ('xembed (slothbar-tray-xembed-init m))
      ('sni (slothbar-tray-sni-init m)))))

(cl-defmethod slothbar-module-init ((m slothbar-tray))
  "Initialize M.  The transport owns the protocol connection."
  m)

(cl-defmethod slothbar-module-layout ((m slothbar-tray))
  "Lay out M through its tray transport."
  (pcase (slothbar-tray-transport m)
    ('xembed (slothbar-tray-xembed-layout m))
    ('sni (slothbar-tray-sni-layout m))))

(cl-defmethod slothbar-module-refresh ((m slothbar-tray))
  "Refresh M through its tray transport."
  (pcase (slothbar-tray-transport m)
    ('xembed (slothbar-tray-xembed-refresh m))
    ('sni (slothbar-tray-sni-refresh m))))

(cl-defmethod slothbar-module-reposition ((m slothbar-tray) x y)
  "Reposition M through its tray transport."
  (pcase (slothbar-tray-transport m)
    ('xembed (slothbar-tray-xembed-reposition m x y))
    ('sni (slothbar-tray-sni-reposition m x y))))

(cl-defmethod slothbar-module-exit :before ((m slothbar-tray))
  "Tear down M's tray transport."
  (pcase (slothbar-tray-transport m)
    ('xembed (slothbar-tray-xembed-exit m))
    ('sni (slothbar-tray-sni-exit m)))
  (setf (slothbar-tray-transport m) nil))

(cl-defmethod slothbar-module-click ((m slothbar-tray) x y button)
  "Dispatch a tray click through the active transport."
  (pcase (slothbar-tray-transport m)
    ('sni (slothbar-tray-sni-click m x y button))
    (_ nil)))

(provide 'slothbar-tray)
;;; slothbar-tray.el ends here
