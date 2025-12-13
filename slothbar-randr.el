;;; slothbar-randr.el --- Slothbar randr event listeners -*- lexical-binding: t -*-

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

;; Part of slothbar.

;; This module provides randr awareness for slothbar.

;;; Code:

(require 'xcb-randr)

(require 'slothbar)
(require 'slothbar-util)

(defgroup slothbar-randr nil
  "Slothbar RandR."
  :group 'slothbar-randr)

(defcustom slothbar-randr-screen-change-hook nil
  "Normal hook run when screen changes."
  :type 'hook
  :group 'slothbar-randr)

;;;###autoload
(define-minor-mode slothbar-randr-mode
  "Toggle slothbar randr support."
  :global t
  :group 'slothbar-randr
  (slothbar--global-minor-mode-body randr))

(defvar slothbar-randr--last-timestamp 0 "Used for debouncing events.")

(defvar slothbar-randr--prev-screen-change-seqnum nil
  "The most recent ScreenChangeNotify sequence number.")

(defvar slothbar-randr--compatibility-mode nil
  "Non-nil when the server does not support RandR 1.5 protocol.")

(defun slothbar-randr--on-ScreenChangeNotify (data _synthetic)
  "Handle `ScreenChangeNotify' event with DATA.

Run `slothbar-randr-screen-change-hook' (usually user scripts to
configure RandR)."
  (let ((evt (make-instance 'xcb:randr:ScreenChangeNotify)))
    (xcb:unmarshal evt data)
    (let ((seqnum (slot-value evt '~sequence)))
      (unless (equal seqnum slothbar-randr--prev-screen-change-seqnum)
        (setq slothbar-randr--prev-screen-change-seqnum seqnum)
        (run-hooks 'slothbar-randr-screen-change-hook)))))

(defun slothbar-randr--on-Notify (data _synthetic)
  "Handle `CrtcChangeNotify' and `OutputChangeNotify' events with DATA.

Refresh when any CRTC/output changes."
  (let ((evt (make-instance 'xcb:randr:NotifyData)))
    (xcb:unmarshal evt data)
    (let ((notify (or (slot-value evt 'cc)
                      (slot-value evt 'oc))))
      (when notify
        (with-slots (timestamp) notify
          (when (> timestamp slothbar-randr--last-timestamp)
            (run-at-time 0 nil #'slothbar-refresh)
            (setq slothbar-randr--last-timestamp timestamp)))))))

(defun slothbar-randr--on-ConfigureNotify (data _synthetic)
  "Handle `ConfigureNotify' event with DATA.

Refresh when any RandR 1.5 monitor changes."
  (let ((evt (make-instance 'xcb:ConfigureNotify)))
    (xcb:unmarshal evt data)
    (with-slots (window) evt
      (when (eq window slothbar--window)
        (slothbar-refresh)))))

(defun slothbar-randr--init ()
  "Initialize RandR extension and slothbar RandR module.

This is adapted from the EXWM RandR module."
  (when (= 0 (slot-value (xcb:get-extension-data slothbar--connection 'xcb:randr)
                         'present))
    (error "[slothbar] RandR extension is not supported by the server"))
  (with-slots (major-version minor-version)
      (xcb:+request-unchecked+reply slothbar--connection
          (make-instance 'xcb:randr:QueryVersion
                         :major-version 1 :minor-version 5))
    (cond ((and (= major-version 1) (= minor-version 5))
           (setq slothbar-randr--compatibility-mode nil))
          ((and (= major-version 1) (>= minor-version 2))
           (setq slothbar-randr--compatibility-mode t))
          (t
           (error "[slothbar] The server only support RandR version up to %d.%d"
                  major-version minor-version)))
    ;; Listen for `ScreenChangeNotify' to notify external tools to
    ;; configure RandR and `CrtcChangeNotify/OutputChangeNotify' to
    ;; refresh the workspace layout.
    (xcb:+event slothbar--connection 'xcb:randr:ScreenChangeNotify
                #'slothbar-randr--on-ScreenChangeNotify)
    (xcb:+event slothbar--connection 'xcb:randr:Notify
                #'slothbar-randr--on-Notify)
    (xcb:+event slothbar--connection 'xcb:ConfigureNotify
                #'slothbar-randr--on-ConfigureNotify)
    (xcb:+request slothbar--connection
        (make-instance 'xcb:randr:SelectInput
                       :window slothbar--window
                       :enable (logior
                                xcb:randr:NotifyMask:ScreenChange
                                xcb:randr:NotifyMask:CrtcChange
                                xcb:randr:NotifyMask:OutputChange)))
    (xcb:flush slothbar--connection)))

(defun slothbar-randr--exit ()
  "Perform cleanup."
  (setq slothbar-randr--last-timestamp 0
        slothbar-randr--prev-screen-change-seqnum nil
        slothbar-randr--compatibility-mode nil))

(provide 'slothbar-randr)
;;; slothbar-randr.el ends here
