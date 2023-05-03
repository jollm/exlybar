;;; exlybar.el --- Emacs polybar-like thing -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.3
;; Homepage: https://github.com/jollm/exlybar
;; Package-Requires: ((backlight "1.4") (dash "2.1.0") (f "0.20.0") (fontsloth "20211101.1") (log4e "0.3.3") (s "1.12.0") (volume "20201002.1022") (xelb "0.18") (emacs "28.0"))
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
;; information.

;; *Please see the website for a detailed README.*

;; To use this module, load and enable it as follows:
;;   (use-package exlybar
;;     :config (exlybar))

;;; Code:

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)

(require 'exlybar-common)
(require 'exlybar-log)

;; ensure modules if present
(defconst exlybar--modules-directory
  (when load-file-name
    (file-name-concat (file-name-directory load-file-name) "modules")))

(with-eval-after-load "exlybar"
  (when (and exlybar--modules-directory (file-directory-p exlybar--modules-directory))
    (unless (seq-contains-p load-path exlybar--modules-directory)
      (add-to-list 'load-path exlybar--modules-directory))))

(defgroup exlybar nil
  "Exlybar is a status bar that displays as a dock window in X."
  :group 'display)

(defvar exlybar--connection nil "The X connection.")
(defvar exlybar--window nil "The parent window.")
(defvar exlybar--gc nil "The graphics context.")

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
  (let ((ecw (xcb:+request-checked+request-check exlybar--connection
                 (make-instance 'xcb:ConfigureWindow
                                :window exlybar--window
                                :value-mask (logior xcb:ConfigWindow:X
                                                    xcb:ConfigWindow:Width
                                                    xcb:ConfigWindow:Height)
                                :x 0
                                :width exlybar-width
                                :height exlybar-height))))
    (exlybar--log-debug* "exlybar-refresh: configure window errors: %s" ecw))
  ;; configure struts
  (xcb:+request exlybar--connection
      (make-instance 'xcb:ewmh:set-_NET_WM_STRUT
                     :window exlybar--window
                     :left 0
                     :right 0
                     :top exlybar-height
                     :bottom 0))
  (xcb:+request exlybar--connection
      (make-instance 'xcb:ewmh:set-_NET_WM_STRUT_PARTIAL
                     :window exlybar--window
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
  (xcb:+request exlybar--connection
      (make-instance 'xcb:MapWindow
                     :window exlybar--window))
  (xcb:flush exlybar--connection))

(defun exlybar--on-DestroyNotify (data _synthetic)
  "DestroyNotify.
DATA the event data"
  (exlybar--log-trace* "received destroynotify %s" data))

(defun exlybar--on-ReparentNotify (data _synthetic)
  "ReparentNotify.
DATA the event data"
  (exlybar--log-trace* "received reparentnotify %s" data))

(defun exlybar--on-ResizeRequest (data _synthetic)
  "ResizeRequest.
DATA the event data"
  (exlybar--log-trace* "received resizerequest %s" data))

(defun exlybar--on-PropertyNotify (data _synthetic)
  "PropertyNotify.
DATA the event data"
  (exlybar--log-trace* "received propertynotify %s" data))

(defun exlybar--on-ClientMessage (data _synthetic)
  "Handle client messages.
DATA the event data"
  (exlybar--log-trace* "received clientmessage %s" data))

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
    (exlybar--log-trace* "key press %s" obj))
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
      (when (alist-get 'pixmap xcb)
        (xcb:+request exlybar--connection
            (make-instance 'xcb:CopyArea
                           :src-drawable (alist-get 'pixmap xcb)
                           :dst-drawable exlybar--window
                           :gc (alist-get 'gc xcb)
                           :src-x 0 :src-y 0 :dst-x x :dst-y y
                           :width width :height exlybar-height)))
      (exlybar-module-reposition (cadr m) x y))))

(defvar exlybar--geometry-changed? nil "Held by `exlybar--on-Expose'.")
(cl-defun exlybar-refresh-modules (&optional modules)
  "Ask the modules to refresh and see whether the layout has changed.
MODULES optional modules to refresh and compare with prev-extents"
  (when exlybar--geometry-changed?
    (dolist (m exlybar-modules)
      (when (exlybar-module-p m)
        (setf (exlybar-module-needs-refresh? m) t
              (exlybar-module-cache m) (make-hash-table :test 'equal))))
    (exlybar--refresh))
  ;; (message "refreshing modules")
  ;; refresh modules to update to latest dimensions
  (let ((prev-extents
         (exlybar-layout-extents
          (exlybar-layout-coordinate (exlybar-layout exlybar-modules) 0 0)))
        (exlybar-modules (or modules exlybar-modules)))
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

(defun exlybar--watch-modules (sym nval oper where)
  "Watcher for `exlybar-modules' to refresh modules with NVAL."
  (ignore sym)
  (when (and exlybar--enabled (not where) (eq 'set oper))
    ;; exit modules that have been removed
    (dolist (m exlybar-modules)
      (when (exlybar-module-p m)
        (unless (seq-contains nval m #'eq)
          (exlybar-module-exit m))))
    ;; check for uninitialized modules
    (dolist (m nval)
      (when (exlybar-module-p m)
        (unless (exlybar-module-xcb m)
          (exlybar-module-init m))))
    (exlybar-refresh-modules nval)))

(add-variable-watcher 'exlybar-modules #'exlybar--watch-modules)

(defvar exlybar--module-refresh-timer nil)
(defun exlybar--start-module-refresh-timer ()
  "Start a timer to periodically refresh the modules."
  (setq exlybar--module-refresh-timer
        (run-at-time nil 10 #'exlybar-refresh-modules)))

(defun exlybar--on-Expose (data _synthetic)
  "Can draw things after Expose.
DATA the event data"
  (exlybar--log-debug* "exlybar received expose %s" data)
  (ignore data)
  (unless (or (not exlybar--enabled) exlybar--module-refresh-timer)
    (exlybar--start-module-refresh-timer))
  (when (and exlybar--enabled exlybar--geometry-changed?)
    (setq exlybar--geometry-changed? nil)
    (run-at-time 0 nil #'exlybar-refresh-modules)))

(defun exlybar--watch-height (sym nval oper where)
  "Watcher for `exlybar-height' to refresh modules when height changes."
  (ignore sym)
  (ignore nval)
  (when (and exlybar--enabled (not where) (eq 'set oper))
    (setq exlybar--geometry-changed? t)
    (run-at-time 0 nil #'exlybar-refresh-modules)))

(add-variable-watcher 'exlybar-height #'exlybar--watch-height)

;;;###autoload
(defun exlybar ()
  "Start exlybar.
Initialize the connection, window, graphics context, and modules."
  (interactive)
  (run-hook-with-args 'exlybar-before-init-hook)
  (cl-assert (not exlybar--connection))
  (cl-assert (not exlybar--window))
  (exlybar--log-enable-logging)
  (setq exlybar-font-px-size (exlybar-font--precompute-px-sizes exlybar-height exlybar-font-map))
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
        (y 0)
        parent depth)
    (setq exlybar--window id)
    (exlybar--log-debug* "Exlybar window id: %s" exlybar--window)
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
    ;; create gc
    (setq exlybar--gc (xcb:generate-id exlybar--connection))
    (let ((egc
           (xcb:+request-checked+request-check exlybar--connection
               (make-instance 'xcb:CreateGC
                              :cid exlybar--gc
                              :drawable exlybar--window
                              :value-mask (logior xcb:GC:Background
                                                  xcb:GC:Foreground)
                              :background (exlybar--color->pixel
                                           (exlybar--find-background-color))
                              :foreground (exlybar--color->pixel
                                           (exlybar--find-foreground-color))))))
      (exlybar--log-debug* "exlybar init create gc errors: %s" egc))
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
    (setq exlybar--enabled t)
    (run-hook-with-args 'exlybar-after-init-hook)))

;;;###autoload
(defun exlybar-exit ()
  "Exit exlybar."
  (interactive)
  (run-hook-with-args 'exlybar-before-exit-hook)
  ;; exit modules
  (when exlybar--module-refresh-timer
    (cancel-timer exlybar--module-refresh-timer)
    (setq exlybar--module-refresh-timer nil))
  (setq exlybar--enabled nil)
  (dolist (m exlybar-modules)
    (when (exlybar-module-p m)
      (exlybar-module-exit m)))
  (when exlybar--connection
    (when exlybar--window
      (xcb:+request exlybar--connection
          (make-instance 'xcb:UnmapWindow
                         :window exlybar--window)))
    (when exlybar--gc
      (xcb:+request exlybar--connection
          (make-instance 'xcb:FreeGC
                         :gc exlybar--gc)))
    (xcb:disconnect exlybar--connection)
    (setq exlybar--connection nil
          exlybar--window nil
          exlybar--gc nil)
    (run-hook-with-args 'exlybar-after-exit-hook)))

(provide 'exlybar)

;;; exlybar.el ends here
