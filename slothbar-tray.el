;;; slothbar-tray.el --- An slothbar system tray module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

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

;; This is an implementation of `slothbar-module' for a system tray.

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'cl-lib)

(require 'xcb-icccm)
(require 'xcb-xembed)
(require 'xcb-systemtray)

(require 'slothbar-log)
(require 'slothbar-module-)

(cl-defstruct (slothbar-tray--icon
               (:constructor slothbar-tray--icon-create))
  "Attributes of a system tray icon."
  (width 0 :type 'natnum)
  (height 0 :type 'natnum)
  (visible nil :type 'boolean))

(cl-defstruct (slothbar-tray
               (:include slothbar-module
                         (name "tray")
                         (format nil)
                         (lpad 4)
                         (rpad 4))
               (:constructor slothbar-tray-create)
               (:copier nil))
  "A system tray module.")

;;; We need this extension to handle client message
(defclass slothbar-tray--ClientMessage
  (xcb:icccm:--ClientMessage xcb:ClientMessage)
  ((format :initform 32)
   (type :initform 'xcb:Atom:MANAGER)
   (time :initarg :time :type xcb:TIMESTAMP)      ;new slot
   (selection :initarg :selection :type xcb:ATOM) ;new slot
   (owner :initarg :owner :type xcb:WINDOW))      ;new slot
  :documentation "A systemtray client message.")

(defgroup slothbar-tray nil
  "System tray module."
  :group 'slothbar)

(defcustom slothbar-tray-icon-gap 2
  "Gap between icons."
  :type 'integer
  :group 'slothbar-tray)

(defvar slothbar-tray--embedder-window nil "The embedder window.")

(defcustom slothbar-tray-background-color nil
  "Background color of system tray module.

This should be a color, or nil for transparent background."
  :type '(choice (const :tag "Transparent" nil)
                 (color))
  :group 'slothbar-tray
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Change the background color for embedder.
         (when (and slothbar--connection
                    slothbar-tray--embedder-window)
           (let ((background-pixel (slothbar-util--color->pixel value)))
             (xcb:+request slothbar--connection
                 (make-instance 'xcb:ChangeWindowAttributes
                                :window slothbar-tray--embedder-window
                                :value-mask (logior xcb:CW:BackPixmap
                                                    (if background-pixel
                                                        xcb:CW:BackPixel 0))
                                :background-pixmap
                                xcb:BackPixmap:ParentRelative
                                :background-pixel background-pixel))
             ;; Unmap & map to take effect immediately.
             (xcb:+request slothbar--connection
                 (make-instance 'xcb:UnmapWindow
                                :window slothbar-tray--embedder-window))
             (xcb:+request slothbar--connection
                 (make-instance 'xcb:MapWindow
                                :window slothbar-tray--embedder-window))
             (xcb:flush slothbar--connection)))))

;; GTK icons require at least 16 pixels to show normally.
(defconst slothbar-tray--icon-min-size 16 "Minimum icon size.")

(defvar slothbar-tray--connection nil "The X connection.")

(defvar slothbar-tray--list nil "The icon list.")

(defvar slothbar-tray--selection-owner-window nil
  "The selection owner window.")

(defvar slothbar-tray--module nil "The tray module (there can only be one).")

(defvar xcb:Atom:_NET_SYSTEM_TRAY_S0)

(defun slothbar-tray--embed (icon)
  "Embed ICON."
  (slothbar--log-debug* "tray try to embed #x%x" icon)
  (let ((info (xcb:+request-unchecked+reply slothbar-tray--connection
                  (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                 :window icon)))
        width* height* visible)
    (when info
      (slothbar--log-debug* "Embedding #x%x" icon)
      (with-slots (width height)
          (xcb:+request-unchecked+reply slothbar-tray--connection
              (make-instance 'xcb:GetGeometry :drawable icon))
        (setq height* slothbar-height
              width* (round (* width (/ (float height*) height))))
        (when (< width* slothbar-tray--icon-min-size)
          (setq width* slothbar-tray--icon-min-size
                height* (round (* height (/ (float width*) width)))))
        (slothbar--log-debug*
         "Resize icon from %dx%d to %dx%d" width height width* height*))
      ;; Add this icon to save-set.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:ChangeSaveSet
                         :mode xcb:SetMode:Insert
                         :window icon))
      ;; Reparent to the embedder.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:ReparentWindow
                         :window icon
                         :parent slothbar-tray--embedder-window
                         :x 0
                         ;; Vertically centered.
                         :y (/ (- slothbar-height height*) 2)))
      ;; Resize the icon.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:ConfigureWindow
                         :window icon
                         :value-mask (logior xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height
                                             xcb:ConfigWindow:BorderWidth)
                         :width width*
                         :height height*
                         :border-width 0))
      ;; Set event mask.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window icon
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior xcb:EventMask:ResizeRedirect
                                             xcb:EventMask:KeyPress
                                             xcb:EventMask:PropertyChange)))
      (setq visible (slot-value info 'flags))
      (slothbar--log-debug* "embed has flags %s" (slot-value info 'flags))
      ;; TODO: should we check for the MAPPED flag at this point?
      (if ;visible
          nil
          (setq visible
                (/= 0 (logand (slot-value info 'flags) xcb:xembed:MAPPED)))
        ;; Default to visible.
        (setq visible t))
      (when visible
        (slothbar--log-debug* "Mapping the icon window")
        (xcb:+request slothbar-tray--connection
            (make-instance 'xcb:MapWindow :window icon)))
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:xembed:SendEvent
                         :destination icon
                         :event
                         (xcb:marshal
                          (make-instance 'xcb:xembed:EMBEDDED-NOTIFY
                                         :window icon
                                         :time xcb:Time:CurrentTime
                                         :embedder
                                         slothbar-tray--embedder-window
                                         :version 0)
                          slothbar-tray--connection)))
      (push `(,icon . ,(slothbar-tray--icon-create
                        :width width*
                        :height height*
                        :visible visible))
            slothbar-tray--list)
      (setf (slothbar-module-needs-refresh? slothbar-tray--module) t)
      (slothbar-refresh-modules))))

(cl-defun slothbar-tray--unembed (icon &optional (should-refresh? t))
  "Unembed ICON.
SHOULD-REFRESH? optional (defaults to t) nil to forgo module refresh"
  (slothbar--log-debug* "Unembed #x%x" icon)
  (xcb:+request slothbar-tray--connection
      (make-instance 'xcb:UnmapWindow :window icon))
  (xcb:+request slothbar-tray--connection
      (make-instance 'xcb:ReparentWindow
                     :window icon
                     :parent (slothbar-util--find-root-window-id)
                     :x 0 :y 0))
  (setq slothbar-tray--list
        (assq-delete-all icon slothbar-tray--list))
  (setf (slothbar-module-needs-refresh? slothbar-tray--module) t)
  (when should-refresh? (slothbar-refresh-modules)))

;;; xcb event handlers

(defun slothbar-tray--on-DestroyNotify (data _synthetic)
  "Unembed icons on DestroyNotify given DATA."
  (let ((obj (make-instance 'xcb:DestroyNotify)))
    (xcb:unmarshal obj data)
    (slothbar--log-debug* "received destroynotify for tray icon %s" obj)
    (with-slots (window) obj
      (when (assoc window slothbar-tray--list)
        (slothbar-tray--unembed window)))))

(defun slothbar-tray--on-ReparentNotify (data _synthetic)
  "Unembed icons on ReparentNotify given DATA."
  (let ((obj (make-instance 'xcb:ReparentNotify)))
    (xcb:unmarshal obj data)
    (slothbar--log-debug* "received reparentnotify for tray icon %s" obj)
    (with-slots (window parent) obj
      (when (and (/= parent slothbar-tray--embedder-window)
                 (assoc window slothbar-tray--list))
        (slothbar-tray--unembed window)))))

(defun slothbar-tray--resize-icon (window width height)
  "Resize icon window WINDOW given WIDTH and HEIGHT."
  (when-let ((attr (cdr (assoc window slothbar-tray--list))))
    (setf (slothbar-tray--icon-height attr) slothbar-height
          (slothbar-tray--icon-width attr)
          (round (* width (/ (float (slothbar-tray--icon-height attr))
                             height))))
    (when (< (slothbar-tray--icon-width attr) slothbar-tray--icon-min-size)
      (setf (slothbar-tray--icon-width attr) slothbar-tray--icon-min-size
            (slothbar-tray--icon-height attr)
            (round (* height (/ (float (slothbar-tray--icon-width attr))
                                width)))))
    (xcb:+request slothbar-tray--connection
        (make-instance 'xcb:ConfigureWindow
                       :window window
                       :value-mask (logior xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       ;; Vertically centered.
                       :y (/ (- slothbar-height
                                (slothbar-tray--icon-height attr)) 2)
                       :width (slothbar-tray--icon-width attr)
                       :height (slothbar-tray--icon-height attr)))))

(defun slothbar-tray--on-ResizeRequest (data _synthetic)
  "Resize the tray icon on ResizeRequest given DATA."
  (let ((obj (make-instance 'xcb:ResizeRequest)))
    (xcb:unmarshal obj data)
    (slothbar--log-debug* "received resize request for tray icon %s" obj)
    (with-slots (window width height) obj
      (slothbar-tray--resize-icon window width height))
    (setf (slothbar-module-needs-refresh? slothbar-tray--module) t)
    (slothbar-refresh-modules)))

(defun slothbar-tray--on-PropertyNotify (data _synthetic)
  "Map/Unmap the tray icon on PropertyNotify given DATA."
  (let ((obj (make-instance 'xcb:PropertyNotify))
        attr info visible)
    (xcb:unmarshal obj data)
    (slothbar--log-debug* "received propertynotify for tray icon %s" obj)
    (with-slots (window atom state) obj
      (when (and (eq state xcb:Property:NewValue)
                 (eq atom xcb:Atom:_XEMBED_INFO)
                 (setq attr (cdr (assoc window slothbar-tray--list))))
        (setq info (xcb:+request-unchecked+reply slothbar-tray--connection
                       (make-instance 'xcb:xembed:get-_XEMBED_INFO
                                      :window window)))
        (when info
          (setq visible (/= 0 (logand (slot-value info 'flags)
                                      xcb:xembed:MAPPED)))
          (if visible
              (xcb:+request slothbar-tray--connection
                  (make-instance 'xcb:MapWindow :window window))
            (xcb:+request slothbar-tray--connection
                (make-instance 'xcb:UnmapWindow :window window)))
          (setf (slothbar-tray--icon-visible attr) visible)
          (setf (slothbar-module-needs-refresh? slothbar-tray--module) t)
          (slothbar-refresh-modules))))))

(defun slothbar-tray--on-ClientMessage (data _synthetic)
  "Handle client messages given DATA."
  (let ((obj (make-instance 'xcb:ClientMessage))
        opcode data32)
    (xcb:unmarshal obj data)
    (with-slots (window type data) obj
      (when (eq type xcb:Atom:_NET_SYSTEM_TRAY_OPCODE)
        (setq data32 (slot-value data 'data32)
              opcode (elt data32 1))
        (slothbar--log-debug* "tray icon clientmessage opcode: %s" opcode)
        (cond ((= opcode xcb:systemtray:opcode:REQUEST-DOCK)
               (unless (assoc (elt data32 2) slothbar-tray--list)
                 (slothbar-tray--embed (elt data32 2))))
              ;; Not implemented (rarely used nowadays).
              ((or (= opcode xcb:systemtray:opcode:BEGIN-MESSAGE)
                   (= opcode xcb:systemtray:opcode:CANCEL-MESSAGE)))
              (t
               (slothbar--log-error "Unknown icon opcode message: %s" obj)))))))

;;; module lifecycle

(cl-defmethod slothbar-module-init ((m slothbar-tray))
  "Initialize `slothbar-tray' module M.
This overrides the default module init because system tray is special."
  (cl-block 'other-tray
    (slothbar--log-debug* "initializing tray %s" m)
    (cl-assert (not slothbar-tray--connection))
    (cl-assert (not slothbar-tray--list))
    (cl-assert (not slothbar-tray--selection-owner-window))
    (cl-assert (not slothbar-tray--embedder-window))
    ;; Create a new connection.
    (setq slothbar-tray--connection (xcb:connect))
    (set-process-query-on-exit-flag
     (slot-value slothbar-tray--connection 'process) nil)
    ;; Initialize XELB modules.
    (xcb:xembed:init slothbar-tray--connection t)
    (xcb:systemtray:init slothbar-tray--connection t)
    ;; Acquire the manager selection _NET_SYSTEM_TRAY_S0.
    (with-slots (owner)
        (xcb:+request-unchecked+reply slothbar-tray--connection
            (make-instance 'xcb:GetSelectionOwner
                           :selection xcb:Atom:_NET_SYSTEM_TRAY_S0))
      (when (/= owner xcb:Window:None)
        (warn "[slothbar-tray] Other system tray detected")
        (slothbar-module-exit m)
        (cl-return-from 'other-tray)))
    (let ((id (xcb:generate-id slothbar-tray--connection))
          (root (slothbar-util--find-root-window-id)))
      (setq slothbar-tray--selection-owner-window id)
      (xcb:+request slothbar-tray--connection
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
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:SetSelectionOwner
                         :owner id
                         :selection xcb:Atom:_NET_SYSTEM_TRAY_S0
                         :time xcb:Time:CurrentTime))
      ;; Send a client message to announce the selection.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination root
                         :event-mask xcb:EventMask:StructureNotify
                         :event (xcb:marshal
                                 (make-instance 'slothbar-tray--ClientMessage
                                                :window root
                                                :time xcb:Time:CurrentTime
                                                :selection
                                                xcb:Atom:_NET_SYSTEM_TRAY_S0
                                                :owner id)
                                 slothbar-tray--connection)))
      ;; Set _NET_WM_NAME.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window id
                         :data "slothbar: slothbar-tray--selection-owner-window"))
      ;; Set the _NET_SYSTEM_TRAY_ORIENTATION property.
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:xembed:set-_NET_SYSTEM_TRAY_ORIENTATION
                         :window id
                         :data xcb:systemtray:ORIENTATION:HORZ)))
    ;; Create the embedder.
    (let* ((id (xcb:generate-id slothbar-tray--connection))
           (background-pixel
            (slothbar-util--color->pixel slothbar-tray-background-color))
          (parent slothbar--window)
          (depth (slot-value (xcb:+request-unchecked+reply
                                 slothbar-tray--connection
                                 (make-instance 'xcb:GetGeometry
                                                :drawable parent))
                             'depth))
          (y 0))
      (setq slothbar-tray--embedder-window id)
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:CreateWindow
                         :depth depth
                         :wid id
                         :parent parent
                         :x 0
                         :y y
                         :width 1
                         :height slothbar-height
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
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window id
                         :data "slothbar: slothbar-tray--embedder-window")))
    (xcb:flush slothbar-tray--connection)
    ;; Attach event listeners.
    (xcb:+event slothbar-tray--connection 'xcb:DestroyNotify
                #'slothbar-tray--on-DestroyNotify)
    (xcb:+event slothbar-tray--connection 'xcb:ReparentNotify
                #'slothbar-tray--on-ReparentNotify)
    (xcb:+event slothbar-tray--connection 'xcb:ResizeRequest
                #'slothbar-tray--on-ResizeRequest)
    (xcb:+event slothbar-tray--connection 'xcb:PropertyNotify
                #'slothbar-tray--on-PropertyNotify)
    (xcb:+event slothbar-tray--connection 'xcb:ClientMessage
                #'slothbar-tray--on-ClientMessage)
    ;; we don't need xcb since it is all managed here
    (push '(unused . unused) (slothbar-module-xcb m))))

(cl-defmethod slothbar-module-init :before ((m slothbar-tray))
  "Before initialize `slothbar-tray' module M."
  (setf (slothbar-module-cache m) (make-hash-table :test 'eq))
  (puthash 'prev-height slothbar-height (slothbar-module-cache m))
  (setq slothbar-tray--module m))

(defvar slothbar-tray--should-map? nil
  "Set to t during refresh if reposition should map the embedder window.")

(cl-defmethod slothbar-module-layout ((m slothbar-tray))
  "Layout `slothbar-tray' module M.
This overrides the default module layout because system tray is special."
  (slothbar--log-debug* "doing layout for tray %s" m)
  (when slothbar-tray--embedder-window
    (xcb:+request slothbar-tray--connection
        (make-instance 'xcb:UnmapWindow
                       :window slothbar-tray--embedder-window)))
  (when slothbar-tray--connection
    (let ((x (+ (slothbar-module-lpad m) slothbar-tray-icon-gap)))
      (dolist (pair slothbar-tray--list)
        (unless (eq slothbar-height
                    (gethash 'prev-height (slothbar-module-cache m)))
          (slothbar--log-debug*
           "tray new height %s, resizing icon %s" slothbar-height (cdr pair))
          (slothbar-tray--resize-icon
           (car pair)
           (slothbar-tray--icon-width (cdr pair))
           (slothbar-tray--icon-height (cdr pair))))
        (when (slothbar-tray--icon-visible (cdr pair))
          (setq x (+ x (slothbar-tray--icon-width (cdr pair))
                     slothbar-tray-icon-gap))))
      (puthash 'prev-height slothbar-height (slothbar-module-cache m))
      (slothbar--log-debug* "setting tray new width to %s" x)
      (setf (slothbar-module-width m) (+ (- (slothbar-module-rpad m)
                                           slothbar-tray-icon-gap) x))
      (unless (eq x (gethash 'prev-width (slothbar-module-cache m)))
        (setq slothbar-tray--should-map? t))
      (puthash 'prev-width x (slothbar-module-cache m)))))

(cl-defmethod slothbar-module-refresh ((_ slothbar-tray))
  "Refresh `slothbar-tray' module.
This overrides the default module refresh because system tray is special."
  (when slothbar-tray--connection
    (let ((x slothbar-tray-icon-gap))
      (dolist (pair slothbar-tray--list)
        (when (slothbar-tray--icon-visible (cdr pair))
          (xcb:+request slothbar-tray--connection
              (make-instance 'xcb:ConfigureWindow
                             :window (car pair)
                             :value-mask xcb:ConfigWindow:X
                             :x x))
          (setq x (+ x (slothbar-tray--icon-width (cdr pair))
                     slothbar-tray-icon-gap))
          (setq slothbar-tray--should-map? t))))))

(cl-defmethod slothbar-module-reposition ((m slothbar-tray) x y)
  "Reposition `slothbar-tray' M to X,Y."
  (slothbar--log-debug* "tray reposition %s,%s %s" x y (slothbar-module-width m))
  (when slothbar-tray--connection
    (xcb:+request slothbar-tray--connection
        (make-instance 'xcb:ConfigureWindow
                       :window slothbar-tray--embedder-window
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       :x x
                       :width (slothbar-module-width m)
                       :height slothbar-height))
    (when slothbar-tray--should-map?
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:MapWindow :window slothbar-tray--embedder-window))
      (setq slothbar-tray--should-map? nil)
      (xcb:flush slothbar-tray--connection))))

(cl-defmethod slothbar-module-exit ((_ slothbar-tray))
  "Exit `slothbar-tray' module.
This overrides the default module exit because system tray is special."
  (when slothbar-tray--connection
    ;; Hide & reparent out the embedder before disconnection to prevent
    ;; embedded icons from being reparented to an Emacs frame (which is the
    ;; parent of the embedder).
    (when slothbar-tray--embedder-window
      (xcb:+request slothbar-tray--connection
          (make-instance 'xcb:UnmapWindow
                         :window slothbar-tray--embedder-window)))
    ;; XXX: there is a race condition if the reparent doesn't occur prior to
    ;; connection close.
    ;; Using a checked request and waiting for check appears to be enough time.
    (when slothbar-tray--embedder-window
      (xcb:+request-checked+request-check slothbar-tray--connection
          (make-instance 'xcb:ReparentWindow
                         :window slothbar-tray--embedder-window
                         :parent (slothbar-util--find-root-window-id)
                         :x 0
                         :y 0)))
    (xcb:disconnect slothbar-tray--connection)
    (setq slothbar-tray--connection nil
          slothbar-tray--list nil
          slothbar-tray--selection-owner-window nil
          slothbar-tray--embedder-window nil
          slothbar-tray--module nil))
  (cl-call-next-method))

(provide 'slothbar-tray)
;;; slothbar-tray.el ends here
