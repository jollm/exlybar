;;; slothbar-tray-sni.el --- StatusNotifier tray transport  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

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

;; StatusNotifier tray transport for Wayland (and other non-XEmbed backends).
;; This implements the watcher side of org.kde.StatusNotifierWatcher and
;; currently renders IconPixmap data.  Menus are not implemented yet.

;;; Code:

(require 'cl-lib)
(require 'dbus)

(require 'slothbar-log)
(require 'slothbar-module-)
(require 'slothbar-tray)

(defconst slothbar-tray-sni--watcher-service
  "org.kde.StatusNotifierWatcher")
(defconst slothbar-tray-sni--watcher-path
  "/StatusNotifierWatcher")
(defconst slothbar-tray-sni--watcher-interface
  "org.kde.StatusNotifierWatcher")
(defconst slothbar-tray-sni--item-interface
  "org.kde.StatusNotifierItem")

(cl-defstruct (slothbar-tray-sni-item
               (:constructor slothbar-tray-sni--item-create)
               (:copier nil))
  "A StatusNotifierItem registration."
  service
  path
  id
  title
  status
  icon-name
  icon-pixmap
  menu
  image)

(defvar slothbar-tray-sni--items nil
  "Registered StatusNotifierItems in display order.")

(defvar slothbar-tray-sni--module nil
  "The tray module using the SNI transport.")

(defvar slothbar-tray-sni--service-owned nil
  "Non-nil when this process owns the watcher service.")

(defvar slothbar-tray-sni--signal-items (make-hash-table :test 'equal)
  "Items whose change signals have already been registered.")

(defun slothbar-tray-sni--property (service path name)
  "Read property NAME from SERVICE at PATH, or return nil."
  (condition-case err
      (dbus-get-property :session service path
                         slothbar-tray-sni--item-interface name)
    (error
     (slothbar--log-debug* "SNI property %s for %s%s: %S"
                           name service path err)
     nil)))

(defun slothbar-tray-sni--bytes (value)
  "Return VALUE as a unibyte string."
  (cond
   ((stringp value) value)
   ((vectorp value) (vconcat value))
   ((listp value) (apply #'unibyte-string (append value nil)))
   (t nil)))

(defun slothbar-tray-sni--argb->rgba (bytes)
  "Convert big-endian ARGB32 BYTES to straight RGBA."
  (let* ((length (length bytes))
         (out (make-string length 0)))
    (cl-loop for i from 0 below length by 4
             for alpha = (aref bytes i)
             for red = (aref bytes (+ i 1))
             for green = (aref bytes (+ i 2))
             for blue = (aref bytes (+ i 3))
             do (aset out i red)
                (aset out (+ i 1) green)
                (aset out (+ i 2) blue)
                (aset out (+ i 3) alpha))
    out))

(defun slothbar-tray-sni--scale-image (width height bytes target-height)
  "Scale ARGB bytes WIDTH by HEIGHT to TARGET-HEIGHT.
Return (WIDTH HEIGHT RGBA) or nil."
  (when (and (integerp width) (integerp height)
             (> width 0) (> height 0)
             bytes
             (>= (length bytes) (* width height 4)))
    (let* ((scale (/ (float target-height) height))
           (new-width (max 1 (round (* width scale))))
           (new-height (max 1 target-height))
           (rgba (slothbar-tray-sni--argb->rgba bytes))
           (out (make-string (* new-width new-height 4) 0)))
      (dotimes (y new-height)
        (let ((sy (min (1- height) (floor (/ y scale)))))
          (dotimes (x new-width)
            (let* ((sx (min (1- width) (floor (/ x scale))))
                   (source (* (+ sx (* sy width)) 4))
                   (target (* (+ x (* y new-width)) 4)))
              (dotimes (n 4)
                (aset out (+ target n)
                      (aref rgba (+ source n))))))))
      (list new-width new-height out))))

(defun slothbar-tray-sni--decode-pixmap (pixmaps target-height)
  "Decode the first IconPixmap in PIXMAPS for TARGET-HEIGHT."
  (when-let* ((pixmap (car pixmaps))
              (width (nth 0 pixmap))
              (height (nth 1 pixmap))
              (data (nth 2 pixmap))
              (bytes (slothbar-tray-sni--bytes data)))
    (slothbar-tray-sni--scale-image width height bytes target-height)))

(defun slothbar-tray-sni--refresh-item (service path)
  "Re-read and refresh the item at SERVICE/PATH."
  (when (cl-find-if
         (lambda (item)
           (and (equal (slothbar-tray-sni-item-service item) service)
                (equal (slothbar-tray-sni-item-path item) path)))
         slothbar-tray-sni--items)
    (slothbar-tray-sni--add-item service path)))

(defun slothbar-tray-sni--register-signals (service path)
  "Register SNI change signals for SERVICE at PATH once."
  (let* ((key (cons service path))
         (owner (or (ignore-errors (dbus-get-name-owner :session service))
                    service)))
    (unless (gethash key slothbar-tray-sni--signal-items)
      (puthash key t slothbar-tray-sni--signal-items)
      (dolist (signal '("NewIcon" "NewStatus" "NewTitle"
                        "NewAttentionIcon" "NewOverlayIcon" "NewToolTip"))
        (dbus-register-signal
         :session owner path slothbar-tray-sni--item-interface signal
         (lambda (&rest _)
           (slothbar-tray-sni--refresh-item service path)))))))

(defun slothbar-tray-sni--add-item (service path)
  "Add or replace the item at SERVICE/PATH."
  (let* ((icon-pixmap
          (slothbar-tray-sni--property service path "IconPixmap"))
         (item (slothbar-tray-sni--item-create
                :service service
                :path path
                :id (slothbar-tray-sni--property service path "Id")
                :title (slothbar-tray-sni--property service path "Title")
                :status (slothbar-tray-sni--property service path "Status")
                :icon-name (slothbar-tray-sni--property service path "IconName")
                :icon-pixmap icon-pixmap
                :menu (slothbar-tray-sni--property service path "Menu")
                :image (slothbar-tray-sni--decode-pixmap
                        icon-pixmap slothbar-height))))
    (setq slothbar-tray-sni--items
          (cons item
                (cl-remove-if
                 (lambda (old)
                   (and (equal (slothbar-tray-sni-item-service old) service)
                        (equal (slothbar-tray-sni-item-path old) path)))
                 slothbar-tray-sni--items)))
    (slothbar-tray-sni--register-signals service path)
    (slothbar-tray-sni--update-watcher-property)
    (slothbar-tray-sni--refresh-module)))

(defun slothbar-tray-sni--update-watcher-property ()
  "Publish the current item list on the watcher."
  (when slothbar-tray-sni--service-owned
    (let ((values (mapcar (lambda (item)
                            (concat (slothbar-tray-sni-item-service item)
                                    (slothbar-tray-sni-item-path item)))
                          slothbar-tray-sni--items)))
      (condition-case err
          (dbus-set-property
           :session slothbar-tray-sni--watcher-service
           slothbar-tray-sni--watcher-path
           slothbar-tray-sni--watcher-interface
           "RegisteredStatusNotifierItems"
           (if values
               (cons :array (cons :string values))
             '(:array)))
        (error
         (slothbar--log-debug* "SNI watcher property update failed: %S" err))))))

(defun slothbar-tray-sni--register-item (item)
  "Handle a RegisterStatusNotifierItem request for ITEM."
  (let* ((event last-input-event)
         (sender (and (fboundp 'dbus-event-service-name)
                      (ignore-errors
                        (dbus-event-service-name event))))
         (path (if (and (stringp item) (string-prefix-p "/" item))
                   item
                 "/StatusNotifierItem"))
         (service (if (and (stringp item) (string-prefix-p "/" item))
                      sender
                    item)))
    (when service
      (run-at-time 0 nil
                   (lambda () (slothbar-tray-sni--add-item service path))))
    :ignore))

(defun slothbar-tray-sni--layout (m)
  "Lay out M's SNI icons."
  (let ((x (slothbar-module-lpad m))
        (gap slothbar-tray-icon-gap)
        (images nil))
    (dolist (item slothbar-tray-sni--items)
      (when-let* ((image (slothbar-tray-sni-item-image item)))
        (pcase-let ((`(,width ,height ,data) image))
          (push (list x 0 width height data) images)
          (setq x (+ x width gap)))))
    (setf (slothbar-module-images m) (nreverse images)
          (slothbar-module-width m)
          (+ x (slothbar-module-rpad m)))))

(defun slothbar-tray-sni--refresh-module ()
  "Redraw the tray module after an item change."
  (when slothbar-tray-sni--module
    (setf (slothbar-module-needs-refresh? slothbar-tray-sni--module) t)
    (when (slothbar-enabled-p)
      (slothbar-refresh-modules
       nil (slothbar-module--bar slothbar-tray-sni--module)))))

(defun slothbar-tray-sni-init (m)
  "Start the StatusNotifier watcher for M."
  (setq slothbar-tray-sni--module m
        slothbar-tray-sni--items nil)
  (unless (eq :primary-owner
              (dbus-register-service
               :session slothbar-tray-sni--watcher-service
               :do-not-queue))
    (error "Another StatusNotifier host owns %s"
           slothbar-tray-sni--watcher-service))
  (setq slothbar-tray-sni--service-owned t)
  (dbus-register-method
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "RegisterStatusNotifierItem"
   #'slothbar-tray-sni--register-item)
  (dbus-register-method
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "RegisterStatusNotifierHost"
   (lambda (_host) :ignore))
  (dbus-register-property
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "IsStatusNotifierHostRegistered" :readwrite t)
  (dbus-register-property
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "ProtocolVersion" :read :int32 0)
  (dbus-register-property
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "RegisteredStatusNotifierItems" :readwrite '(:array))
  (slothbar-tray-sni--layout m)
  m)

(defun slothbar-tray-sni-layout (m)
  "Lay out M through the SNI transport."
  (slothbar-tray-sni--layout m))

(defun slothbar-tray-sni-refresh (m)
  "Refresh M through the SNI transport."
  (slothbar-tray-sni--layout m))

(defun slothbar-tray-sni-reposition (_m _x _y)
  "SNI icons are composited into the bar.  No window reposition is needed.")

(defun slothbar-tray-sni--item-at-x (m x)
  "Return the SNI item at logical X in M, or nil."
  (let ((left (slothbar-module-lpad m))
        (gap slothbar-tray-icon-gap)
        (items (cl-remove-if-not #'slothbar-tray-sni-item-image
                                 slothbar-tray-sni--items)))
    (cl-loop for item in items
             for image = (slothbar-tray-sni-item-image item)
             for width = (nth 0 image)
             if (and (>= x left) (< x (+ left width)))
             return item
             do (setq left (+ left width gap)))))

(defun slothbar-tray-sni-click (m x y button)
  "Activate the SNI item at X,Y on M for BUTTON."
  (when-let* ((item (slothbar-tray-sni--item-at-x m x))
              (method (pcase button
                        (:left "Activate")
                        (:middle "SecondaryActivate")
                        (:right "ContextMenu")))
              (service (slothbar-tray-sni-item-service item))
              (path (slothbar-tray-sni-item-path item)))
    (condition-case err
        (progn
          (dbus-call-method :session service path
                            slothbar-tray-sni--item-interface
                            method (round x) (round y))
          t)
      (error
       (slothbar--log-debug* "SNI %s failed for %s: %S"
                             method service err)
       nil))))

(defun slothbar-tray-sni-exit (_m)
  "Stop the StatusNotifier watcher."
  (when slothbar-tray-sni--service-owned
    (ignore-errors
      (when (fboundp 'dbus-unregister-service)
        (dbus-unregister-service :session
                                 slothbar-tray-sni--watcher-service))))
  (setq slothbar-tray-sni--items nil
        slothbar-tray-sni--module nil
        slothbar-tray-sni--service-owned nil)
  (clrhash slothbar-tray-sni--signal-items))

(provide 'slothbar-tray-sni)
;;; slothbar-tray-sni.el ends here
