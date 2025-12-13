;;; slothbar-ewmh.el --- Helpers for extended window manager hints  -*- lexical-binding: t -*-

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

;; A set of helper functions for receiving and process events related
;; to X extended window manager hints.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)

(require 'slothbar)
(require 'slothbar-log)
(require 'slothbar-util)

(defgroup slothbar-ewmh nil
  "Slothbar ewmh."
  :group 'slothbar-ewmh)

;;;###autoload
(define-minor-mode slothbar-ewmh-mode
  "Toggle slothbar ewmh support."
  :global t
  :group 'slothbar-ewmh
  (slothbar--global-minor-mode-body ewmh))

(defvar slothbar-ewmh--supported nil
  "Corresponds to property _NET_SUPPORTED.")
(defvar slothbar-ewmh--num-desktops 0
  "Corresponds to property _NET_NUMBER_OF_DESKTOPS.")
(defvar slothbar-ewmh--desktop-names nil
  "Corresponds to property _NET_DESKTOP_NAMES.")
(defvar slothbar-ewmh--current-desktop nil
  "Corresponds to property _NET_CURRENT_DESKTOP.")
(defvar slothbar-ewmh--client-list nil
  "Corresponds to property _NET_CLIENT_LIST.")
(defvar slothbar-ewmh--client-desktop-map (make-hash-table :size 20)
  "A map from client id to desktop number.")
(defvar slothbar-ewmh--active-window nil
  "Corresponds to property _NET_ACTIVE_WINDOW.")
(defvar slothbar-ewmh--urgents nil
  "Tracks clients with property _NET_WM_STATE_DEMANDS_ATTENTION.")
(defvar slothbar-ewmh--urgent-desktops nil
  "Tracks desktop numbers with urgent clients.")

(defun slothbar-ewmh--watch-client-list (_ nval oper where)
  "Ensure that the PropertyChange event mask is included for NVAL clients.

Only when OPER eq \\='set and WHERE is nil."
  (when (and (eq 'set oper) (not where))
    (cl-loop for client across nval
             do (slothbar--ensure-event-mask
                 client xcb:EventMask:PropertyChange))))

(add-variable-watcher 'slothbar-ewmh--client-list
                      #'slothbar-ewmh--watch-client-list)

(defvar slothbar-ewmh--on-change-hook nil
  "An hook to run whenever an ewmh tracking variable changes.

The hook function should expect two arguments: the symbol of the
tracking variable and the new value of the variable.")

(defun slothbar-ewmh--watch-all (sym nval oper where)
  "Watcher for tracking variables.

SYM the symbol of the variable
NVAL the new value of the variable
OPER the operation performed on the variable
WHERE the context of the operation"
  (when (and (eq 'set oper) (not where))
    (run-at-time
     0 nil
     (lambda ()
       (run-hook-with-args 'slothbar-ewmh--on-change-hook sym nval)))))

(cl-loop for var in '(slothbar-ewmh--num-desktops
                      slothbar-ewmh--desktop-names
                      slothbar-ewmh--current-desktop
                      slothbar-ewmh--client-desktop-map
                      slothbar-ewmh--active-window
                      slothbar-ewmh--urgent-desktops)
         do (add-variable-watcher var #'slothbar-ewmh--watch-all))

(defsubst slothbar-ewmh--supported-p (prop)
  "Non-nil if PROP is supported by the window manager."
  (cl-find prop slothbar-ewmh--supported))

(cl-defun slothbar-ewmh--get-window-property
    (win prop-class &optional (slot 'value))
  "Get PROP-CLASS for WIN if any and return the value of SLOT.

PROP-CLASS the xcb class representing the value to get
SLOT optional and defaults to \\='value"
  (slot-value (xcb:+request-unchecked+reply
                  slothbar--connection
                  (make-instance prop-class :window win))
              slot))

(defsubst slothbar-ewmh--get-root-win-prop (prop-class)
  "Get the root window property represented by PROP-CLASS.

The root window is determined by `slothbar-util--find-root-window-id'."
  (slothbar-ewmh--get-window-property
   (slothbar-util--find-root-window-id) prop-class))

(defsubst slothbar-ewmh--get-desktop-names ()
  "Get the window manager desktop names.

If the _NET_DESKTOP_NAMES property is not supported, the names will be
the stringified desktop numbers."
  (if-let ((names (slothbar-ewmh--get-root-win-prop 'xcb:ewmh:get-_NET_DESKTOP_NAMES)))
      (string-split names "\0" t)
    (cl-loop for i from 0 below slothbar-ewmh--num-desktops
             collect (number-to-string i))))

(defun slothbar-ewmh--window-urgent-p (win)
  "Non-nil if WIN has property _NET_WM_STATE_DEMANDS_ATTENTION."
  (let ((state (slothbar-ewmh--get-window-property
                win 'xcb:ewmh:get-_NET_WM_STATE)))
    (cl-find xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION state)))

(defun slothbar-ewmh--urgent-desktops ()
  "Return the list of urgent desktop numbers."
  (cl-loop for win in slothbar-ewmh--urgents
           collect (map-elt slothbar-ewmh--client-desktop-map win)))

(defun slothbar-ewmh--map-client-desktops (client-vec)
  "Given CLIENT-VEC, return an alist mapping clients to desktop numbers."
  (cl-loop for client across client-vec
           for desktop = (slot-value (xcb:+request-unchecked+reply
                                         slothbar--connection
                                         (make-instance
                                          'xcb:ewmh:get-_NET_WM_DESKTOP
                                          :window client))
                                     'value)
           collect `(,client . ,desktop) into result
           finally return (map-into result 'hash-table)))

(defun slothbar-ewmh--update-current-desktop ()
  "Update the value of `slothbar-ewmh--current-desktop'."
  (slothbar--log-debug* "current_desktop notify")
  (setq slothbar-ewmh--current-desktop
        (slothbar-ewmh--get-root-win-prop
         'xcb:ewmh:get-_NET_CURRENT_DESKTOP)))

(defun slothbar-ewmh--update-active-window ()
  "Update the value of `slothbar-ewmh--active-window'."
  (slothbar--log-debug* "active_window notify")
  (when-let ((active-win (slothbar-ewmh--get-root-win-prop
                          'xcb:ewmh:get-_NET_ACTIVE_WINDOW)))
    (setq slothbar-ewmh--active-window active-win)
    (when-let ((active-desktop (map-elt slothbar-ewmh--client-desktop-map active-win)))
      (setq slothbar-ewmh--current-desktop active-desktop))))

(defun slothbar-ewmh--update-client-list ()
  "Update the value of `slothbar-ewmh--client-list'."
  (slothbar--log-debug* "client_list notify")
  (setq slothbar-ewmh--client-list
        (slothbar-ewmh--get-root-win-prop
         'xcb:ewmh:get-_NET_CLIENT_LIST)
        slothbar-ewmh--client-desktop-map
        (slothbar-ewmh--map-client-desktops
         slothbar-ewmh--client-list)))

(defun slothbar-ewmh--update-desktop-names ()
  "Update the value of `slothbar-ewmh--desktop-names'."
  (slothbar--log-debug* "desktop_names notify")
  (setq slothbar-ewmh--desktop-names (slothbar-ewmh--get-desktop-names)))

(defun slothbar-ewmh--update-num-desktops ()
  "Update the value of `slothbar-ewmh--num-desktops'."
  (slothbar--log-debug* "num_desktops notify")
  (setq slothbar-ewmh--num-desktops
        (slothbar-ewmh--get-root-win-prop
         'xcb:ewmh:get-_NET_NUMBER_OF_DESKTOPS)))

(defun slothbar-ewmh--update-wm-state (id)
  "Update the urgency list and mapping for window ID."
  (slothbar--log-debug* "wm_state notify for win %s" id)
  (if (slothbar-ewmh--window-urgent-p id)
      (add-to-list 'slothbar-ewmh--urgents id)
    (setq slothbar-ewmh--urgents
          (remq id slothbar-ewmh--urgents)))
  (setq slothbar-ewmh--urgent-desktops
        (slothbar-ewmh--urgent-desktops)))

(defun slothbar-ewmh--update-wm-desktop (id)
  "Update the client to desktop mapping for win ID."
  (slothbar--log-debug* "wm_desktop notify for win %s" id)
  (when-let* ((reply (xcb:+request-unchecked+reply
                        slothbar--connection
                        (make-instance
                         'xcb:ewmh:get-_NET_WM_DESKTOP
                         :window id)))
              (desktop (slot-value reply 'value)))
    (map-put! slothbar-ewmh--client-desktop-map id desktop)))

(defun slothbar-ewmh--on-PropertyNotify (data _synthetic)
  "A listener for ewmh related events.

DATA is the marshalled event data"
  (when (slothbar-enabled-p)
    (let ((obj (make-instance 'xcb:PropertyNotify))
          atom id)
      (xcb:unmarshal obj data)
      (setq id (slot-value obj 'window)
            atom (slot-value obj 'atom))
      (cond ((= xcb:Atom:_NET_CURRENT_DESKTOP atom)
             (slothbar-ewmh--update-current-desktop))
            ((= xcb:Atom:_NET_ACTIVE_WINDOW atom)
             (slothbar-ewmh--update-active-window))
            ((= xcb:Atom:_NET_CLIENT_LIST atom)
             (slothbar-ewmh--update-client-list))
            ((= xcb:Atom:_NET_CLIENT_LIST_STACKING atom)
             (slothbar--log-debug* "client_list_stacking notify for win %s" id))
            ((= xcb:Atom:_NET_DESKTOP_NAMES atom)
             (slothbar-ewmh--update-desktop-names))
            ((= xcb:Atom:_NET_NUMBER_OF_DESKTOPS atom)
             (slothbar-ewmh--update-num-desktops)
             (unless (slothbar-ewmh--supported-p xcb:Atom:_NET_DESKTOP_NAMES)
               (slothbar-ewmh--update-desktop-names)))
            ((= xcb:Atom:_NET_WM_STATE atom)
             (slothbar-ewmh--update-wm-state id))
            ((= xcb:Atom:_NET_WM_DESKTOP atom)
             (slothbar-ewmh--update-wm-desktop id))
            ((= xcb:Atom:WM_HINTS atom)
             ;; TODO: is this needed?
             )))))

(defun slothbar-ewmh--check-conforming ()
  "Return non-nil if the WM appears ewmh conforming."
  (when-let* ((reply (xcb:+request-unchecked+reply
                        slothbar--connection
                        (make-instance
                         'xcb:ewmh:get-_NET_SUPPORTING_WM_CHECK
                         :window (slothbar-util--find-root-window-id))))
              (window (slot-value reply 'value))
              (child-reply (xcb:+request-unchecked+reply
                               slothbar--connection
                               (make-instance
                                'xcb:ewmh:get-_NET_SUPPORTING_WM_CHECK
                                :window window)))
              (child-check (slot-value child-reply 'value)))
    ;; according to the standard this should also check _NET_WM_NAME
    ;; of the child, however, not all window managers set that
    (and (= window child-check))))

(defun slothbar-ewmh--init ()
  "Initialize the global mode `slothbar-ewmh-mode'."
  (if (not (or (slothbar-ewmh--check-conforming)
               (y-or-n-p (format "WM %s may not support ewmh, try anyway? "
                                 (getenv "DESKTOP_SESSION")))))
      (slothbar-ewmh-mode -1)
    (setq slothbar-ewmh--supported
          (slothbar-ewmh--get-root-win-prop 'xcb:ewmh:get-_NET_SUPPORTED)

          slothbar-ewmh--num-desktops
          (slothbar-ewmh--get-root-win-prop
           'xcb:ewmh:get-_NET_NUMBER_OF_DESKTOPS)

          slothbar-ewmh--desktop-names (slothbar-ewmh--get-desktop-names)

          slothbar-ewmh--current-desktop
          (slothbar-ewmh--get-root-win-prop 'xcb:ewmh:get-_NET_CURRENT_DESKTOP)

          slothbar-ewmh--client-list
          (slothbar-ewmh--get-root-win-prop 'xcb:ewmh:get-_NET_CLIENT_LIST)

          slothbar-ewmh--client-desktop-map
          (slothbar-ewmh--map-client-desktops slothbar-ewmh--client-list)

          slothbar-ewmh--urgents
          (seq-filter #'slothbar-ewmh--window-urgent-p
                      slothbar-ewmh--client-list)

          slothbar-ewmh--urgent-desktops (slothbar-ewmh--urgent-desktops))
    (unless (memq #'slothbar-ewmh--on-PropertyNotify
                  (plist-get (slot-value slothbar--connection 'event-plist)
                             (xcb:-error-or-event-class->number
                              slothbar--connection 'xcb:PropertyNotify)))
      (xcb:+event slothbar--connection 'xcb:PropertyNotify
                  #'slothbar-ewmh--on-PropertyNotify))))

(defun slothbar-ewmh--exit ()
  "Exit the global mode `slothbar-ewmh-mode'."
  (when (slothbar-enabled-p)
    (cl-loop for client across slothbar-ewmh--client-list
             do (slothbar--clear-event-mask client))
    (let* ((event-plist (slot-value slothbar--connection 'event-plist))
           (event-number (xcb:-error-or-event-class->number
                          slothbar--connection 'xcb:PropertyNotify))
           (listeners (plist-get event-plist event-number)))
      (setf (slot-value slothbar--connection 'event-plist)
            (plist-put event-plist event-number
                       (remq #'slothbar-ewmh--on-PropertyNotify listeners)
                       #'equal)))))

(provide 'slothbar-ewmh)
;;; slothbar-ewmh.el ends here
