;;; exlybar.el --- Emacs polybar-like thing -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((xelb "0.18") (fontsloth "0.12.0") (emacs "27.1"))
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

;; This module uses xelb to build polybar like modules for displaying status
;; information

;; To use this module, load and enable it as follows:
;;   (use-package exlybar
;;     :config (exlybar))

;;; Code:

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)

(require 'exlybar-common)

(defgroup exlybar nil
  "Exlybar is a status bar that displays as a dock window in X."
  :group 'display)

(defvar exlybar--connection nil "The X connection.")
(defvar exlybar--window nil "The parent window.")
(defvar exlybar--gc nil "The graphics context.")

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

(defvar exlybar--enabled nil "t if exlybar is enabled.")

(require 'exlybar-layout)

(defsubst exlybar-enabled-p ()
  "Return t if exlybar is enabled."
  exlybar--enabled)

(defun exlybar--refresh ()
  "Refresh the bar."
  (xcb:+request exlybar--connection
      (make-instance 'xcb:UnmapWindow
                     :window exlybar--window))
  (message "configure window errors: %s"
           (xcb:+request-checked+request-check exlybar--connection
               (make-instance 'xcb:ConfigureWindow
                              :window exlybar--window
                              :value-mask (logior xcb:ConfigWindow:X
                                                  xcb:ConfigWindow:Width)
                              :x 0
                              :width exlybar-width)))
  (xcb:+request exlybar--connection
      (make-instance 'xcb:MapWindow
                     :window exlybar--window))
  (xcb:flush exlybar--connection))

(defun exlybar--on-DestroyNotify (data _synthetic)
  "DestroyNotify.
DATA the event data"
  (message "received destroynotify %s" data))

(defun exlybar--on-ReparentNotify (data _synthetic)
  "ReparentNotify.
DATA the event data"
  (message "received reparentnotify %s" data))

(defun exlybar--on-ResizeRequest (data _synthetic)
  "ResizeRequest.
DATA the event data"
  (message "received resizerequest %s" data))

(defun exlybar--on-PropertyNotify (data _synthetic)
  "PropertyNotify.
DATA the event data"
  (message "received propertynotify %s" data))

(defun exlybar--on-ClientMessage (data _synthetic)
  "Handle client messages.
DATA the event data"
  (message "received clientmessage %s" data))

(defun exlybar--on-KeyPress (data _synthetic)
  "Forward all KeyPress events to Emacs frame.
DATA the event data"
  ;; This function a workspace frame has the input focus and the pointer is
  ;; over a tray icon.
  (let ((dest (frame-parameter (selected-frame) 'exwm-outer-id))
        (obj (make-instance 'xcb:KeyPress)))
    (xcb:unmarshal obj data)
    (setf (slot-value obj 'event) dest)
    (xcb:+request exlybar--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination dest
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal obj exlybar--connection)))
    (message "key press %s" obj))
  (xcb:flush exlybar--connection))

(defun exlybar--selectively-clear-areas (prev-extents new-extents)
  "Clear old areas that the new extents do not cover.
PREV-EXTENTS the previous layout extents
NEW-EXTENTS the new layout extents"
  (let ((to-clear (exlybar-layout-subtract-extents new-extents prev-extents)))
    (pcase-dolist (`(,l ,r) to-clear)
      (xcb:+request exlybar--connection
          (make-instance 'xcb:ClearArea
                         :exposures 0
                         :window exlybar--window
                         :x l :y 0
                         :width (- r l) :height exlybar-height)))))

(defun exlybar--copy-areas (layout)
  "Copy a LAYOUT's modules' pixmaps into their respective areas."
  (dolist (m layout)
    (pcase-let ((`((,x ,y) ,(cl-struct exlybar-module width xcb)) m))
      (xcb:+request exlybar--connection
          (make-instance 'xcb:CopyArea
                          :src-drawable (alist-get 'pixmap xcb)
                          :dst-drawable exlybar--window
                          :gc (alist-get 'gc xcb)
                          :src-x 0 :src-y 0 :dst-x x :dst-y y
                          :width width :height exlybar-height)))))

(defvar exlybar--prev-extents nil "Held by `exlybar-refresh-modules'.")
(defun exlybar-refresh-modules ()
  "Ask the modules to refresh and see whether the layout has changed."
  ;; (message "refreshing modules")
  ;; refresh modules to update to latest dimensions
  (let ((prev-extents
         (exlybar-layout-extents
          (exlybar-layout-coordinate (exlybar-layout exlybar-modules) 0 0))))
    ;; (message "prev extents %s" prev-extents)
    (dolist (m exlybar-modules)
      (when (exlybar-module-p m)
        (exlybar-module-refresh m)))
    (let* ((new-layout
            (exlybar-layout-coordinate (exlybar-layout exlybar-modules) 0 0))
           (new-extents (exlybar-layout-extents new-layout)))
      ;; (message "prev extents %s new extents %s" prev-extents new-extents)
      (when (not (equal prev-extents new-extents))
        ;; (message "layout has changed")
        (exlybar--selectively-clear-areas prev-extents new-extents))
      (exlybar--copy-areas new-layout)))
  (xcb:flush exlybar--connection))

(defvar exlybar--module-refresh-timer nil)
(defun exlybar--start-module-refresh-timer ()
  "Start a timer to periodically refresh the modules."
  (setq exlybar--module-refresh-timer
        (run-at-time nil 10 #'exlybar-refresh-modules)))

(defun exlybar--on-Expose (data _synthetic)
  "Can draw things after Expose.
DATA the event data"
  ;; (message "received expose %s" data)
  (unless exlybar--module-refresh-timer
    (exlybar--start-module-refresh-timer)))

(defun exlybar ()
  "Initialize the connection, window, graphics context, and modules."
  (cl-assert (not exlybar--connection))
  (cl-assert (not exlybar--window))
  (setq exlybar--connection (xcb:connect))
  ;; apparently ewmh initializes icccm automatically
  (xcb:ewmh:init exlybar--connection)
  ;; (xcb:icccm:init exlybar--connection)
  (set-process-query-on-exit-flag (slot-value exlybar--connection
                                              'process)
                                  nil)

  ;; initialize the bar window
  (let ((id (xcb:generate-id exlybar--connection))
        (background-pixel (exlybar--color->pixel
                           (exlybar--find-background-color)))
        frame parent depth y)
    (setq exlybar--window id)
    (message "Exlybar window: %s" exlybar--window)
    (setq frame (selected-frame)
          y 0)
    (setq parent (exlybar--find-root-window-id)
          depth (slot-value (xcb:+request-unchecked+reply
                                exlybar--connection
                                (make-instance 'xcb:GetGeometry
                                               :drawable parent))
                            'depth))
    (xcb:+request exlybar--connection
        (make-instance 'xcb:CreateWindow
                       :depth depth
                       :wid id
                       :parent parent
                       :override-redirect 1
                       :x 0
                       :y y
                       :width 1
                       :height exlybar-height
                       :border-width 1
                       :class xcb:WindowClass:InputOutput
                       :visual 0
                       :value-mask (logior xcb:CW:BackPixmap
                                           (if background-pixel
                                               xcb:CW:BackPixel 0)
                                           xcb:CW:EventMask)
                       :background-pixmap xcb:BackPixmap:ParentRelative
                       :background-pixel background-pixel
                       :event-mask (logior xcb:EventMask:Exposure
                                           xcb:EventMask:KeyPress
                                           xcb:EventMask:PointerMotion
                                           xcb:EventMask:PropertyChange
                                           xcb:EventMask:SubstructureNotify
                                           )))
    ;; Set WM_NAME and WM_CLASS.
    (xcb:+request exlybar--connection
        (make-instance 'xcb:icccm:set-WM_NAME
                       :window id
                       :data "exlybar"))
    (xcb:+request exlybar--connection
        (make-instance 'xcb:icccm:set-WM_CLASS
                       :window id
                       :instance-name "exlybar"
                       :class-name "Exlybar"))
    ;; dock the window
    (xcb:+request exlybar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_WINDOW_TYPE
                       :window id
                       :data `(,xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK)))
    ;; state is sticky and above
    (xcb:+request exlybar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window id
                       :data `(,xcb:Atom:_NET_WM_STATE_STICKY
                               ,xcb:Atom:_NET_WM_STATE_ABOVE)))
    ;; configure geometry
    (xcb:+request exlybar--connection
        (make-instance 'xcb:ConfigureWindow
                       :window id
                       :value-mask (logior xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       :width (* 8 exlybar-width)
                       :height exlybar-height))
    ;; configure struts
    (xcb:+request exlybar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STRUT
                       :window id
                       :left 0
                       :right 0
                       :top exlybar-height
                       :bottom 0))
    (xcb:+request exlybar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STRUT_PARTIAL
                       :window id
                       :left 0
                       :right 0
                       :top exlybar-height
                       :bottom 0
                       :left-start-y 0
                       :left-end-y 0
                       :right-start-y 0
                       :right-end-y 0
                       :top-start-x 0
                       :top-end-x exlybar-width
                       :bottom-start-x 0
                       :bottom-end-x 0))
    ;; create gc
    (setq exlybar--gc (xcb:generate-id exlybar--connection))
    (message "create gc errors: %s"
             (xcb:+request-checked+request-check exlybar--connection
                 (make-instance 'xcb:CreateGC
                                :cid exlybar--gc
                                :drawable exlybar--window
                                :value-mask (logior xcb:GC:Background
                                                    xcb:GC:Foreground)
                                :background (exlybar--color->pixel
                                             (exlybar--find-background-color))
                                :foreground (exlybar--color->pixel
                                             (exlybar--find-foreground-color)))))
    ;; initialize modules
    (dolist (m exlybar-modules)
      (when (exlybar-module-p m)
        (exlybar-module-init m)))
    (xcb:flush exlybar--connection)
    ;; Attach event listeners.
    (xcb:+event exlybar--connection 'xcb:DestroyNotify
                #'exlybar--on-DestroyNotify)
    (xcb:+event exlybar--connection 'xcb:ReparentNotify
                #'exlybar--on-ReparentNotify)
    (xcb:+event exlybar--connection 'xcb:ResizeRequest
                #'exlybar--on-ResizeRequest)
    (xcb:+event exlybar--connection 'xcb:PropertyNotify
                #'exlybar--on-PropertyNotify)
    (xcb:+event exlybar--connection 'xcb:ClientMessage
                #'exlybar--on-ClientMessage)
    (xcb:+event exlybar--connection 'xcb:KeyPress
                #'exlybar--on-KeyPress)
    (xcb:+event exlybar--connection 'xcb:Expose
                #'exlybar--on-Expose)
    (exlybar--refresh)
    (setq exlybar--enabled t)))

(defun exlybar-exit ()
  "Exit the exlybar."
  ;; exit modules
  (when exlybar--module-refresh-timer
    (cancel-timer exlybar--module-refresh-timer)
    (setq exlybar--module-refresh-timer nil))
  (dolist (m exlybar-modules)
    (when (exlybar-module-p m)
      (exlybar-module-exit m)))
  (when exlybar--connection
    (xcb:+request exlybar--connection
        (make-instance 'xcb:UnmapWindow
                       :window exlybar--window))
    (xcb:+request exlybar--connection
        (make-instance 'xcb:FreeGC
                       :gc exlybar--gc))
    (xcb:disconnect exlybar--connection)
    (setq exlybar--connection nil
          exlybar--window nil
          exlybar--enabled nil)))

(provide 'exlybar)

;;; exlybar.el ends here
