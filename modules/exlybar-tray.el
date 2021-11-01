;;; exlybar-tray.el --- An exlybar system tray module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.21.3
;; Homepage: https://github.com/jollm/exlybar
;; Package-Requires: ((xelb "0.18") (emacs "27.1"))
;; Keywords: multimedia, window-manager, status-bar, exwm

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

;; This is an implementation of `exlybar-module' for a system tray.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'cl-lib)

(require 'xcb-icccm)
(require 'xcb-xembed)
(require 'xcb-systemtray)

(cl-defstruct (exlybar-tray--icon
               (:constructor exlybar-tray--icon-create))
  "Attributes of a system tray icon."
  (width 0 :type 'natnum)
  (height 0 :type 'natnum)
  (visible nil :type 'boolean))

(cl-defstruct (exlybar-tray
               (:include exlybar-module
                         (name "tray")
                         (format nil)
                         (lpad 4)
                         (rpad 4))
               (:constructor exlybar-tray-create)
               (:copier nil))
  "A system tray module.")

;;; we need this extension to handle client message
(defclass xcb:systemtray:-ClientMessage
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)
   (type :initform xcb:Atom:MANAGER)
   (time :initarg :time :type xcb:TIMESTAMP)      ;new slot
   (selection :initarg :selection :type xcb:ATOM) ;new slot
   (owner :initarg :owner :type xcb:WINDOW))      ;new slot
  :documentation "A systemtray client message.")

(defgroup exlybar-tray nil
  "System tray module."
  :group 'exlybar)

(defcustom exlybar-tray-icon-gap 2
  "Gap between icons."
  :type 'integer
  :group 'exlybar-tray)

(defvar exlybar-tray--embedder-window nil "The embedder window.")

(defcustom exlybar-tray-background-color nil
  "Background color of system tray module.

This should be a color, or nil for transparent background."
  :type '(choice (const :tag "Transparent" nil)
                 (color))
  :group 'exlybar-tray
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Change the background color for embedder.
         (when (and exlybar--connection
                    exlybar-tray--embedder-window)
           (let ((background-pixel (exlybar--color->pixel value)))
             (xcb:+request exlybar--connection
                 (make-instance 'xcb:ChangeWindowAttributes
                                :window exlybar-tray--embedder-window
                                :value-mask (logior xcb:CW:BackPixmap
                                                    (if background-pixel
                                                        xcb:CW:BackPixel 0))
                                :background-pixmap
                                xcb:BackPixmap:ParentRelative
                                :background-pixel background-pixel))
             ;; Unmap & map to take effect immediately.
             (xcb:+request exlybar--connection
                 (make-instance 'xcb:UnmapWindow
                                :window exlybar-tray--embedder-window))
             (xcb:+request exlybar--connection
                 (make-instance 'xcb:MapWindow
                                :window exlybar-tray--embedder-window))
             (xcb:flush exlybar--connection)))))

;; GTK icons require at least 16 pixels to show normally.
(defconst exlybar-tray--icon-min-size 16 "Minimum icon size.")

(defvar exlybar-tray--connection nil "The X connection.")

(defvar exlybar-tray--list nil "The icon list.")

(defvar exlybar-tray--selection-owner-window nil
  "The selection owner window.")

(defvar exlybar-tray--module nil "The tray module (there can only be one).")

(defvar xcb:Atom:_NET_SYSTEM_TRAY_S0 "This is the tray selection atom.")

(defun exlybar-tray--embed (icon)
  "Embed ICON."
  (message "Try to embed #x%x" icon)
  (let ((info (xcb:+request-unchecked+reply exlybar-tray--connection
                  (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                 :window icon)))
        width* height* visible)
    (when info
      (message "Embed #x%x" icon)
      (with-slots (width height)
          (xcb:+request-unchecked+reply exlybar-tray--connection
              (make-instance 'xcb:GetGeometry :drawable icon))
        (setq height* exlybar-height
              width* (round (* width (/ (float height*) height))))
        (when (< width* exlybar-tray--icon-min-size)
          (setq width* exlybar-tray--icon-min-size
                height* (round (* height (/ (float width*) width)))))
        (message "Resize from %dx%d to %dx%d"
                 width height width* height*))
      ;; Add this icon to save-set.
      (xcb:+request exlybar-tray--connection
          (make-instance 'xcb:ChangeSaveSet
                         :mode xcb:SetMode:Insert
                         :window icon))
      ;; Reparent to the embedder.
      (xcb:+request exlybar-tray--connection
          (make-instance 'xcb:ReparentWindow
                         :window icon
                         :parent exlybar-tray--embedder-window
                         :x 0
                         ;; Vertically centered.
                         :y (/ (- exlybar-height height*) 2)))
      ;; Resize the icon.
      (xcb:+request exlybar-tray--connection
          (make-instance 'xcb:ConfigureWindow
                         :window icon
                         :value-mask (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height
                                             xcb:ConfigWindow:BorderWidth)
                         :width width*
                         :height height*
                         :border-width 0))
      ;; Set event mask.
      (xcb:+request exlybar-tray--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window icon
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior xcb:EventMask:ResizeRedirect
                                             xcb:EventMask:KeyPress
                                             xcb:EventMask:PropertyChange)))
      (setq visible (slot-value info 'flags))
      (message "embed has flags %s" (slot-value info 'flags))
      ;; TODO: should we check for the MAPPED flag at this point?
      (if ;visible
          nil
          (setq visible
                (/= 0 (logand (slot-value info 'flags) xcb:xembed:MAPPED)))
        ;; Default to visible.
        (setq visible t))
      (when visible
        (message "Map the window")
        (xcb:+request exlybar-tray--connection
            (make-instance 'xcb:MapWindow :window icon)))
      (xcb:+request exlybar-tray--connection
          (make-instance 'xcb:xembed:SendEvent
                         :destination icon
                         :event
                         (xcb:marshal
                          (make-instance 'xcb:xembed:EMBEDDED-NOTIFY
                                         :window icon
                                         :time xcb:Time:CurrentTime
                                         :embedder
                                         exlybar-tray--embedder-window
                                         :version 0)
                          exlybar-tray--connection)))
      (push `(,icon . ,(exlybar-tray--icon-create
                        :width width*
                        :height height*
                        :visible visible))
            exlybar-tray--list)
      (setf (exlybar-module-needs-refresh? exlybar-tray--module) t)
      (exlybar-refresh-modules))))

(cl-defun exlybar-tray--unembed (icon &optional (should-refresh? t))
  "Unembed ICON.
SHOULD-REFRESH? optional (defaults to t) nil to forgo module refresh"
  (message "Unembed #x%x" icon)
  (xcb:+request exlybar-tray--connection
      (make-instance 'xcb:UnmapWindow :window icon))
  (xcb:+request exlybar-tray--connection
      (make-instance 'xcb:ReparentWindow
                     :window icon
                     :parent (exlybar--find-root-window-id)
                     :x 0 :y 0))
  (setq exlybar-tray--list
        (assq-delete-all icon exlybar-tray--list))
  (setf (exlybar-module-needs-refresh? exlybar-tray--module) t)
  (when should-refresh? (exlybar-refresh-modules)))

;;; xcb event handlers

(defun exlybar-tray--on-DestroyNotify (data _synthetic)
  "Unembed icons on DestroyNotify given DATA."
  (message "received destroy notify for tray")
  (let ((obj (make-instance 'xcb:DestroyNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window) obj
      (when (assoc window exlybar-tray--list)
        (exlybar-tray--unembed window)))))

(defun exlybar-tray--on-ReparentNotify (data _synthetic)
  "Unembed icons on ReparentNotify given DATA."
  (message "tray received reparentnotify")
  (let ((obj (make-instance 'xcb:ReparentNotify)))
    (xcb:unmarshal obj data)
    (with-slots (window parent) obj
      (when (and (/= parent exlybar-tray--embedder-window)
                 (assoc window exlybar-tray--list))
        (exlybar-tray--unembed window)))))

(defun exlybar-tray--resize-icon (window width height)
  "Resize icon window WINDOW given WIDTH and HEIGHT."
  (when-let ((attr (cdr (assoc window exlybar-tray--list))))
    (setf (exlybar-tray--icon-height attr) exlybar-height
          (exlybar-tray--icon-width attr)
          (round (* width (/ (float (exlybar-tray--icon-height attr))
                             height))))
    (when (< (exlybar-tray--icon-width attr) exlybar-tray--icon-min-size)
      (setf (exlybar-tray--icon-width attr) exlybar-tray--icon-min-size
            (exlybar-tray--icon-height attr)
            (round (* height (/ (float (exlybar-tray--icon-width attr))
                                width)))))
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:ConfigureWindow
                       :window window
                       :value-mask (logior xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       ;; Vertically centered.
                       :y (/ (- exlybar-height
                                (exlybar-tray--icon-height attr)) 2)
                       :width (exlybar-tray--icon-width attr)
                       :height (exlybar-tray--icon-height attr)))))

(defun exlybar-tray--on-ResizeRequest (data _synthetic)
  "Resize the tray icon on ResizeRequest given DATA."
  (let ((obj (make-instance 'xcb:ResizeRequest))
        attr)
    (xcb:unmarshal obj data)
    (message "tray received resize request for icon %s" obj)
    (with-slots (window width height) obj
      (exlybar-tray--resize-icon window width height))
    (setf (exlybar-module-needs-refresh? exlybar-tray--module) t)
    (exlybar-refresh-modules)))

(defun exlybar-tray--on-PropertyNotify (data _synthetic)
  "Map/Unmap the tray icon on PropertyNotify given DATA."
  (message "tray received propertynotify")
  (let ((obj (make-instance 'xcb:PropertyNotify))
        attr info visible)
    (xcb:unmarshal obj data)
    (with-slots (window atom state) obj
      (when (and (eq state xcb:Property:NewValue)
                 (eq atom xcb:Atom:_XEMBED_INFO)
                 (setq attr (cdr (assoc window exlybar-tray--list))))
        (setq info (xcb:+request-unchecked+reply exlybar-tray--connection
                       (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                      :window window)))
        (when info
          (setq visible (/= 0 (logand (slot-value info 'flags)
                                      xcb:xembed:MAPPED)))
          (if visible
              (xcb:+request exlybar-tray--connection
                  (make-instance 'xcb:MapWindow :window window))
            (xcb:+request exlybar-tray--connection
                (make-instance 'xcb:UnmapWindow :window window)))
          (setf (exlybar-tray--icon-visible attr) visible)
          (setf (exlybar-module-needs-refresh? exlybar-tray--module) t)
          (exlybar-refresh-modules))))))

(defun exlybar-tray--on-ClientMessage (data _synthetic)
  "Handle client messages given DATA."
  (let ((obj (make-instance 'xcb:ClientMessage))
        opcode data32)
    (xcb:unmarshal obj data)
    (with-slots (window type data) obj
      (when (eq type xcb:Atom:_NET_SYSTEM_TRAY_OPCODE)
        (setq data32 (slot-value data 'data32)
              opcode (elt data32 1))
        (message "opcode: %s" opcode)
        (cond ((= opcode xcb:systemtray:opcode:REQUEST-DOCK)
               (unless (assoc (elt data32 2) exlybar-tray--list)
                 (exlybar-tray--embed (elt data32 2))))
              ;; Not implemented (rarely used nowadays).
              ((or (= opcode xcb:systemtray:opcode:BEGIN-MESSAGE)
                   (= opcode xcb:systemtray:opcode:CANCEL-MESSAGE)))
              (t
               (message "Unknown opcode message: %s" obj)))))))

;;; module lifecycle

(cl-defmethod exlybar-module-init ((m exlybar-tray))
  "Initialize `exlybar-tray' module M.
This overrides the default module init because system tray is special."
  (message "initializing tray %s" m)
  (cl-assert (not exlybar-tray--connection))
  (cl-assert (not exlybar-tray--list))
  (cl-assert (not exlybar-tray--selection-owner-window))
  (cl-assert (not exlybar-tray--embedder-window))
  ;; Create a new connection.
  (setq exlybar-tray--connection (xcb:connect))
  (set-process-query-on-exit-flag
   (slot-value exlybar-tray--connection 'process) nil)
  ;; Initialize XELB modules.
  (xcb:xembed:init exlybar-tray--connection t)
  (xcb:systemtray:init exlybar-tray--connection t)
  ;; Acquire the manager selection _NET_SYSTEM_TRAY_S0.
  (with-slots (owner)
      (xcb:+request-unchecked+reply exlybar-tray--connection
          (make-instance 'xcb:GetSelectionOwner
                         :selection xcb:Atom:_NET_SYSTEM_TRAY_S0))
    (when (/= owner xcb:Window:None)
      ;; (xcb:disconnect exwm-systemtray--connection)
      ;; (setq exwm-systemtray--connection nil)
      (warn "[exlybar-tray] Other system tray detected")
      (cl-return-from exlybar-module-init)))
  (let ((id (xcb:generate-id exlybar-tray--connection))
        (root (exlybar--find-root-window-id)))
    (setq exlybar-tray--selection-owner-window id)
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:CreateWindow
                       :depth 0
                       :wid id
                       :parent root
                       :x 0
                       :y 0
                       :width 1
                       :height 1
                       :border-width 0
                       :class xcb:WindowClass:InputOnly
                       :visual 0
                       :value-mask xcb:CW:OverrideRedirect
                       :override-redirect 1))
    ;; Get the selection ownership.
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:SetSelectionOwner
                       :owner id
                       :selection xcb:Atom:_NET_SYSTEM_TRAY_S0
                       :time xcb:Time:CurrentTime))
    ;; Send a client message to announce the selection.
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination root
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:systemtray:-ClientMessage
                                              :window root
                                              :time xcb:Time:CurrentTime
                                              :selection
                                              xcb:Atom:_NET_SYSTEM_TRAY_S0
                                              :owner id)
                               exlybar-tray--connection)))
    ;; Set _NET_WM_NAME.
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id
                       :data "exlybar: exlybar-tray--selection-owner-window"))
    ;; Set the _NET_SYSTEM_TRAY_ORIENTATION property.
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:xembed:set-_NET_SYSTEM_TRAY_ORIENTATION
                       :window id
                       :data xcb:systemtray:ORIENTATION:HORZ)))
  ;; Create the embedder.
  (let* ((id (xcb:generate-id exlybar-tray--connection))
         (background-pixel
          (exlybar--color->pixel exlybar-tray-background-color))
        (parent exlybar--window)
        (depth (slot-value (xcb:+request-unchecked+reply
                               exlybar-tray--connection
                               (make-instance 'xcb:GetGeometry
                                              :drawable parent))
                           'depth))
        (y 0))
    (setq exlybar-tray--embedder-window id)
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:CreateWindow
                       :depth depth
                       :wid id
                       :parent parent
                       :x 0
                       :y y
                       :width 1
                       :height exlybar-height
                       :border-width 0
                       :class xcb:WindowClass:InputOutput
                       :visual 0
                       :value-mask (logior xcb:CW:BackPixmap
                                           (if background-pixel
                                               xcb:CW:BackPixel 0)
                                           xcb:CW:EventMask)
                       :background-pixmap xcb:BackPixmap:ParentRelative
                       :background-pixel background-pixel
                       :event-mask xcb:EventMask:SubstructureNotify))
    ;; Set _NET_WM_NAME.
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                       :window id
                       :data "exlybar: exlybar-tray--embedder-window")))
  (xcb:flush exlybar-tray--connection)
  ;; Attach event listeners.
  (xcb:+event exlybar-tray--connection 'xcb:DestroyNotify
              #'exlybar-tray--on-DestroyNotify)
  (xcb:+event exlybar-tray--connection 'xcb:ReparentNotify
              #'exlybar-tray--on-ReparentNotify)
  (xcb:+event exlybar-tray--connection 'xcb:ResizeRequest
              #'exlybar-tray--on-ResizeRequest)
  (xcb:+event exlybar-tray--connection 'xcb:PropertyNotify
              #'exlybar-tray--on-PropertyNotify)
  (xcb:+event exlybar-tray--connection 'xcb:ClientMessage
              #'exlybar-tray--on-ClientMessage)
  ;; we don't need xcb since it is all managed here
  (push '(unused . unused) (exlybar-module-xcb m)))

(cl-defmethod exlybar-module-init :before ((m exlybar-tray))
  "Before initialize `exlybar-tray' module M."
  (setf (exlybar-module-cache m) (make-hash-table :test 'eq))
  (puthash 'prev-height exlybar-height (exlybar-module-cache m))
  (setq exlybar-tray--module m))

(cl-defmethod exlybar-module-layout ((m exlybar-tray))
  "Layout `exlybar-tray' module M.
This overrides the default module layout because system tray is special."
  (message "doing layout for tray")
  (when exlybar-tray--embedder-window
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:UnmapWindow
                       :window exlybar-tray--embedder-window)))
  (let ((x (+ (exlybar-module-lpad m) exlybar-tray-icon-gap)))
    (dolist (pair exlybar-tray--list)
      (unless (eq exlybar-height
                  (gethash 'prev-height (exlybar-module-cache m)))
        (message "tray new height %s, resizing icon %s"
                 exlybar-height (cdr pair))
        (exlybar-tray--resize-icon
         (car pair)
         (exlybar-tray--icon-width (cdr pair))
         (exlybar-tray--icon-height (cdr pair))))
      (when (exlybar-tray--icon-visible (cdr pair))
        (setq x (+ x (exlybar-tray--icon-width (cdr pair))
                   exlybar-tray-icon-gap))))
    (puthash 'prev-height exlybar-height (exlybar-module-cache m))
    (message "setting new width to %s" x)
    (setf (exlybar-module-width m) (+ (- (exlybar-module-rpad m)
                                         exlybar-tray-icon-gap) x))
    (unless (eq x (gethash 'prev-width (exlybar-module-cache m)))
      (setq exlybar-tray--should-map? t))
    (puthash 'prev-width x (exlybar-module-cache m))))

(defvar exlybar-tray--should-map? nil
  "Set to t during refresh if reposition should map the embedder window.")

(cl-defmethod exlybar-module-refresh ((m exlybar-tray))
  "Refresh `exlybar-tray' module M.
This overrides the default module refresh because system tray is special."
  (let ((x exlybar-tray-icon-gap))
    (dolist (pair exlybar-tray--list)
      (when (exlybar-tray--icon-visible (cdr pair))
        (xcb:+request exlybar-tray--connection
            (make-instance 'xcb:ConfigureWindow
                           :window (car pair)
                           :value-mask xcb:ConfigWindow:X
                           :x x))
        (setq x (+ x (exlybar-tray--icon-width (cdr pair))
                   exlybar-tray-icon-gap))
        (setq exlybar-tray--should-map? t)))))

(cl-defmethod exlybar-module-reposition ((m exlybar-tray) x y)
  "Reposition `exlybar-tray' M to X,Y."
  (message "tray reposition %s,%s %s" x y (exlybar-module-width m))
  (xcb:+request exlybar-tray--connection
      (make-instance 'xcb:ConfigureWindow
                     :window exlybar-tray--embedder-window
                     :value-mask (logior xcb:ConfigWindow:X
                                         xcb:ConfigWindow:Width
                                         xcb:ConfigWindow:Height)
                     :x x
                     :width (exlybar-module-width m)
                     :height exlybar-height))
  (when exlybar-tray--should-map?
    (xcb:+request exlybar-tray--connection
        (make-instance 'xcb:MapWindow :window exlybar-tray--embedder-window))
    (setq exlybar-tray--should-map? nil)
    (xcb:flush exlybar-tray--connection)))

(cl-defmethod exlybar-module-exit ((m exlybar-tray))
  "Exit `exlybar-tray' module M.
This overrides the default module exit because system tray is special."
  (when exlybar-tray--connection
    ;; Hide & reparent out the embedder before disconnection to prevent
    ;; embedded icons from being reparented to an Emacs frame (which is the
    ;; parent of the embedder).
    (when exlybar-tray--embedder-window
      (xcb:+request exlybar-tray--connection
          (make-instance 'xcb:UnmapWindow
                         :window exlybar-tray--embedder-window)))
    ;; XXX: there is a race condition if the reparent doesn't occur prior to
    ;; connection close.
    ;; Using a checked request and waiting for check appears to be enough time.
    (when exlybar-tray--embedder-window
      (xcb:+request-checked+request-check exlybar-tray--connection
          (make-instance 'xcb:ReparentWindow
                         :window exlybar-tray--embedder-window
                         :parent (exlybar--find-root-window-id)
                         :x 0
                         :y 0)))
    (xcb:disconnect exlybar-tray--connection)
    (setq exlybar-tray--connection nil
          exlybar-tray--list nil
          exlybar-tray--selection-owner-window nil
          exlybar-tray--embedder-window nil
          exlybar-tray--module nil))
  (cl-call-next-method))

(provide 'exlybar-tray)
;;; exlybar-tray.el ends here
