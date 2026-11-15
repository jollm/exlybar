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

(declare-function umbra-png-read "umbra-png" (path))
(declare-function umbra-pixmap-width "umbra-pixmap" (pixmap))
(declare-function umbra-pixmap-height "umbra-pixmap" (pixmap))
(declare-function umbra-pixmap-demultiplied-data "umbra-pixmap" (pixmap))

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
  category
  window-id
  item-is-menu
  ordering-index
  icon-name
  icon-pixmap
  attention-icon-name
  attention-icon-pixmap
  attention-image
  overlay-icon-name
  overlay-icon-pixmap
  overlay-image
  icon-theme-path
  menu
  tooltip
  label
  image)

(defvar slothbar-tray-sni--items nil
  "Registered StatusNotifierItems in display order.")

(defvar slothbar-tray-sni--module nil
  "The tray module using the SNI transport.")

(defvar slothbar-tray-sni--service-owned nil
  "Non-nil when this process owns the watcher service.")

(defvar slothbar-tray-sni--signal-items (make-hash-table :test 'equal)
  "Items whose change signals have already been registered.")

(defvar slothbar-tray-sni--signal-objects (make-hash-table :test 'equal)
  "Registered D-Bus signal objects keyed by item address.")

(defvar slothbar-tray-sni--secondary-host nil
  "Non-nil when using another StatusNotifier watcher as a host.")

(defvar slothbar-tray-sni--host-service
  (format "org.kde.StatusNotifierHost-slothbar-%d" (emacs-pid))
  "Service name advertised when registering as a secondary host.")

(defvar slothbar-tray-sni--host-service-owned nil
  "Non-nil when this process owns the secondary-host service name.")

(defvar slothbar-tray-sni--name-owner-signal nil
  "Signal registration for item owner changes.")

(defvar slothbar-tray-sni--watcher-signal-objects nil
  "Signal registrations on a secondary watcher.")

(defvar slothbar-tray-sni--icon-cache (make-hash-table :test 'equal)
  "Cache of resolved icon names to file paths.")

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

(defun slothbar-tray-sni--scale-rgba (width height rgba target-height)
  "Scale straight RGBA bytes WIDTH by HEIGHT to TARGET-HEIGHT.
Return (WIDTH HEIGHT RGBA) or nil."
  (when (and (integerp width) (integerp height)
             (> width 0) (> height 0)
             rgba
             (>= (length rgba) (* width height 4)))
    (let* ((scale (/ (float target-height) height))
           (new-width (max 1 (round (* width scale))))
           (new-height (max 1 target-height))
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

(defun slothbar-tray-sni--scale-image (width height bytes target-height)
  "Scale ARGB bytes WIDTH by HEIGHT to TARGET-HEIGHT.
Return (WIDTH HEIGHT RGBA) or nil."
  (when bytes
    (slothbar-tray-sni--scale-rgba
     width height (slothbar-tray-sni--argb->rgba bytes) target-height)))

(defun slothbar-tray-sni--icon-dirs (theme-paths)
  "Return icon directories for THEME-PATHS and the XDG data dirs.
THEME-PATHS may be a string or a list of strings."
  (let ((theme-paths
         (cond ((stringp theme-paths) (list theme-paths))
               ((listp theme-paths) theme-paths)
               (t nil))))
    (delete-dups
     (append
      (mapcar #'expand-file-name theme-paths)
      (list (expand-file-name "~/.local/share/icons")
            (expand-file-name "~/.icons"))
      (cl-loop for dir in (split-string
                           (or (getenv "XDG_DATA_DIRS")
                               "/usr/local/share:/usr/share")
                           ":" t)
               collect (expand-file-name "icons" dir))
      (list "/usr/share/icons" "/usr/local/share/icons")))))

(defun slothbar-tray-sni--find-icon (name theme-paths)
  "Find NAME's PNG in THEME-PATHS or the standard icon directories."
  (when (and (stringp name) (not (string-empty-p name)))
    (let* ((dirs (slothbar-tray-sni--icon-dirs theme-paths))
           (key (cons name dirs)))
      (or (gethash key slothbar-tray-sni--icon-cache)
          (let ((path
                 (cl-loop
                  for dir in dirs
                  when (file-directory-p dir)
                  thereis
                  (or (let ((direct (expand-file-name
                                     (concat name ".png") dir)))
                        (and (file-readable-p direct) direct))
                      (car (directory-files-recursively
                            dir (concat "\\`" (regexp-quote name) "\\.png\\'")
                            nil))))))
            (when path
              (puthash key path slothbar-tray-sni--icon-cache))
            path)))))

(defun slothbar-tray-sni--load-icon (name theme-paths target-height)
  "Load and scale NAME from THEME-PATHS for TARGET-HEIGHT."
  (when-let* ((path (slothbar-tray-sni--find-icon name theme-paths))
              (umbra (require 'umbra-png nil t)))
    (condition-case err
        (let* ((pixmap (umbra-png-read path))
               (width (umbra-pixmap-width pixmap))
               (height (umbra-pixmap-height pixmap))
               (rgba (umbra-pixmap-demultiplied-data pixmap)))
          (slothbar-tray-sni--scale-rgba width height rgba target-height))
      (error
       (slothbar--log-debug* "SNI icon %s failed: %S" path err)
       nil))))

(defun slothbar-tray-sni--resolve-icon (pixmap name theme-paths target-height)
  "Resolve ICON PIXMAP or NAME using THEME-PATHS for TARGET-HEIGHT."
  (or (slothbar-tray-sni--decode-pixmap pixmap target-height)
      (slothbar-tray-sni--load-icon name theme-paths target-height)))

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
      (let (objects)
        (dolist (signal '("NewIcon" "NewStatus" "NewTitle"
                          "NewAttentionIcon" "NewOverlayIcon" "NewToolTip"))
          (push (dbus-register-signal
                 :session owner path slothbar-tray-sni--item-interface signal
                 (lambda (&rest _)
                   (slothbar-tray-sni--refresh-item service path)))
                objects))
        (puthash key objects slothbar-tray-sni--signal-objects)))))

(defun slothbar-tray-sni--unregister-signals (service path)
  "Unregister SNI change signals for SERVICE at PATH."
  (let ((key (cons service path)))
    (dolist (object (gethash key slothbar-tray-sni--signal-objects))
      (ignore-errors (dbus-unregister-object object)))
    (remhash key slothbar-tray-sni--signal-objects)
    (remhash key slothbar-tray-sni--signal-items)))

(defun slothbar-tray-sni--remove-item (service path)
  "Remove the item at SERVICE/PATH."
  (setq slothbar-tray-sni--items
        (cl-remove-if
         (lambda (item)
           (and (equal (slothbar-tray-sni-item-service item) service)
                (equal (slothbar-tray-sni-item-path item) path)))
         slothbar-tray-sni--items))
  (slothbar-tray-sni--unregister-signals service path)
  (slothbar-tray-sni--update-watcher-property)
  (slothbar-tray-sni--refresh-module))

(defun slothbar-tray-sni--on-name-owner-changed (name _old new)
  "Remove items whose bus NAME lost its owner."
  (when (and (stringp name)
             (string-empty-p (or new ""))
             (string-prefix-p "org.kde.StatusNotifierItem" name))
    (let ((matches (cl-remove-if-not
                    (lambda (item)
                      (equal (slothbar-tray-sni-item-service item) name))
                    slothbar-tray-sni--items)))
      (dolist (item matches)
        (slothbar-tray-sni--remove-item
         (slothbar-tray-sni-item-service item)
         (slothbar-tray-sni-item-path item))))))

(defun slothbar-tray-sni--watcher-entry (entry)
  "Add ENTRY from a secondary watcher's item list."
  (when (stringp entry)
    (let* ((slash (string-match "/" entry))
           (service (if slash (substring entry 0 slash) entry))
           (path (if slash (substring entry slash) "/StatusNotifierItem")))
      (unless (string-empty-p service)
        (slothbar-tray-sni--add-item service path)))))

(defun slothbar-tray-sni--register-watcher-signals ()
  "Watch a secondary watcher's item registration signals."
  (setq slothbar-tray-sni--watcher-signal-objects
        (list
         (dbus-register-signal
          :session slothbar-tray-sni--watcher-service
          slothbar-tray-sni--watcher-path
          slothbar-tray-sni--watcher-interface
          "StatusNotifierItemRegistered"
          (lambda (entry) (slothbar-tray-sni--watcher-entry entry)))
         (dbus-register-signal
          :session slothbar-tray-sni--watcher-service
          slothbar-tray-sni--watcher-path
          slothbar-tray-sni--watcher-interface
          "StatusNotifierItemUnregistered"
          (lambda (entry)
            (let* ((slash (string-match "/" (or entry "")))
                   (service (if slash (substring entry 0 slash) entry))
                   (path (if slash (substring entry slash)
                           "/StatusNotifierItem")))
              (slothbar-tray-sni--remove-item service path)))))))

(defun slothbar-tray-sni--try-secondary-host ()
  "Try to register as a host with an existing StatusNotifier watcher."
  (condition-case err
      (progn
        (unless (eq :primary-owner
                    (dbus-register-service
                     :session slothbar-tray-sni--host-service
                     :do-not-queue))
          (error "Could not own host service %s"
                 slothbar-tray-sni--host-service))
        (setq slothbar-tray-sni--host-service-owned t)
        (dbus-call-method :session
                          slothbar-tray-sni--watcher-service
                          slothbar-tray-sni--watcher-path
                          slothbar-tray-sni--watcher-interface
                          "RegisterStatusNotifierHost"
                          slothbar-tray-sni--host-service)
        (setq slothbar-tray-sni--secondary-host t)
        (slothbar-tray-sni--register-watcher-signals)
        (dolist (entry
                 (condition-case nil
                     (dbus-get-property
                      :session slothbar-tray-sni--watcher-service
                      slothbar-tray-sni--watcher-path
                      slothbar-tray-sni--watcher-interface
                      "RegisteredStatusNotifierItems")
                   (error nil)))
          (slothbar-tray-sni--watcher-entry entry))
        t)
    (error
     (when slothbar-tray-sni--host-service-owned
       (ignore-errors
         (when (fboundp 'dbus-unregister-service)
           (dbus-unregister-service :session
                                    slothbar-tray-sni--host-service)))
       (setq slothbar-tray-sni--host-service-owned nil))
     (message "Slothbar tray: another StatusNotifier host owns %s; %s"
              slothbar-tray-sni--watcher-service
              (error-message-string err))
     nil)))

(defun slothbar-tray-sni--sort-items (items)
  "Sort ITEMS by XAyatanaOrderingIndex, preserving registration order."
  (cl-stable-sort
   items
   (lambda (a b)
     (let ((ai (slothbar-tray-sni-item-ordering-index a))
           (bi (slothbar-tray-sni-item-ordering-index b)))
       (cond ((and ai bi) (< ai bi))
             (ai t)
             (bi nil)
             (t nil))))))

(defun slothbar-tray-sni--add-item (service path)
  "Add or replace the item at SERVICE/PATH."
  (let* ((icon-pixmap
          (slothbar-tray-sni--property service path "IconPixmap"))
         (icon-name (slothbar-tray-sni--property service path "IconName"))
         (attention-name
          (slothbar-tray-sni--property service path "AttentionIconName"))
         (attention-pixmap
          (slothbar-tray-sni--property service path "AttentionIconPixmap"))
         (overlay-name
          (slothbar-tray-sni--property service path "OverlayIconName"))
         (overlay-pixmap
          (slothbar-tray-sni--property service path "OverlayIconPixmap"))
         (icon-theme-path
          (slothbar-tray-sni--property service path "IconThemePath"))
         (item (slothbar-tray-sni--item-create
                :service service
                :path path
                :id (slothbar-tray-sni--property service path "Id")
                :title (slothbar-tray-sni--property service path "Title")
                :status (slothbar-tray-sni--property service path "Status")
                :category (slothbar-tray-sni--property service path "Category")
                :window-id (slothbar-tray-sni--property service path "WindowId")
                :item-is-menu
                (slothbar-tray-sni--property service path "ItemIsMenu")
                :ordering-index
                (slothbar-tray-sni--property service path "XAyatanaOrderingIndex")
                :icon-name icon-name
                :icon-pixmap icon-pixmap
                :attention-icon-name attention-name
                :attention-icon-pixmap attention-pixmap
                :attention-image
                (slothbar-tray-sni--resolve-icon
                 attention-pixmap attention-name icon-theme-path slothbar-height)
                :overlay-icon-name overlay-name
                :overlay-icon-pixmap overlay-pixmap
                :overlay-image
                (slothbar-tray-sni--resolve-icon
                 overlay-pixmap overlay-name icon-theme-path slothbar-height)
                :icon-theme-path icon-theme-path
                :menu (slothbar-tray-sni--property service path "Menu")
                :tooltip (slothbar-tray-sni--property service path "ToolTip")
                :label (slothbar-tray-sni--property service path "XAyatanaLabel")
                :image (slothbar-tray-sni--resolve-icon
                        icon-pixmap icon-name icon-theme-path slothbar-height))))
    (setq slothbar-tray-sni--items
          (slothbar-tray-sni--sort-items
           (cons item
                 (cl-remove-if
                  (lambda (old)
                    (and (equal (slothbar-tray-sni-item-service old) service)
                         (equal (slothbar-tray-sni-item-path old) path)))
                  slothbar-tray-sni--items))))
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

(defun slothbar-tray-sni--base-image (item)
  "Return ITEM's active base icon image."
  (if (and (equal (slothbar-tray-sni-item-status item) "NeedsAttention")
           (slothbar-tray-sni-item-attention-image item))
      (slothbar-tray-sni-item-attention-image item)
    (slothbar-tray-sni-item-image item)))

(defun slothbar-tray-sni--layout (m)
  "Lay out M's SNI icons, including attention and overlay states."
  (let ((x (slothbar-module-lpad m))
        (gap slothbar-tray-icon-gap)
        (images nil))
    (dolist (item slothbar-tray-sni--items)
      (when-let* ((base (slothbar-tray-sni--base-image item)))
        (pcase-let ((`(,width ,height ,data) base))
          (push (list x 0 width height data) images)
          (when-let* ((overlay (slothbar-tray-sni-item-overlay-image item)))
            (pcase-let ((`(,owidth ,oheight ,odata) overlay))
              (push (list (max x (- (+ x width) owidth))
                          (max 0 (- slothbar-height oheight))
                          owidth oheight odata)
                    images)))
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
  (if (eq :primary-owner
          (dbus-register-service
           :session slothbar-tray-sni--watcher-service
           :do-not-queue))
      (progn
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
         "RegisteredStatusNotifierItems" :readwrite '(:array)))
    (unless (slothbar-tray-sni--try-secondary-host)
      (error "Another StatusNotifier host owns %s"
             slothbar-tray-sni--watcher-service)))
  (unless slothbar-tray-sni--name-owner-signal
    (setq slothbar-tray-sni--name-owner-signal
          (dbus-register-signal
           :session "org.freedesktop.DBus" "/org/freedesktop/DBus"
           "org.freedesktop.DBus" "NameOwnerChanged"
           #'slothbar-tray-sni--on-name-owner-changed)))
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
        (items (cl-remove-if-not #'slothbar-tray-sni--base-image
                                 slothbar-tray-sni--items)))
    (cl-loop for item in items
             for image = (slothbar-tray-sni--base-image item)
             for width = (nth 0 image)
             if (and (>= x left) (< x (+ left width)))
             return item
             do (setq left (+ left width gap)))))

(defun slothbar-tray-sni-click (m x y button)
  "Activate the SNI item at X,Y on M for BUTTON."
  (when-let* ((item (slothbar-tray-sni--item-at-x m x))
              (method (pcase button
                        (:left (if (slothbar-tray-sni-item-item-is-menu item)
                                   "ContextMenu"
                                 "Activate"))
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
  (when slothbar-tray-sni--host-service-owned
    (ignore-errors
      (when (fboundp 'dbus-unregister-service)
        (dbus-unregister-service :session
                                 slothbar-tray-sni--host-service)))
    (setq slothbar-tray-sni--host-service-owned nil))
  (when slothbar-tray-sni--name-owner-signal
    (ignore-errors
      (dbus-unregister-object slothbar-tray-sni--name-owner-signal)))
  (dolist (object slothbar-tray-sni--watcher-signal-objects)
    (ignore-errors (dbus-unregister-object object)))
  (maphash (lambda (_key objects)
             (dolist (object objects)
               (ignore-errors (dbus-unregister-object object))))
           slothbar-tray-sni--signal-objects)
  (setq slothbar-tray-sni--items nil
        slothbar-tray-sni--module nil
        slothbar-tray-sni--service-owned nil
        slothbar-tray-sni--secondary-host nil
        slothbar-tray-sni--name-owner-signal nil
        slothbar-tray-sni--watcher-signal-objects nil)
  (clrhash slothbar-tray-sni--signal-items)
  (clrhash slothbar-tray-sni--signal-objects)
  (clrhash slothbar-tray-sni--icon-cache))

(provide 'slothbar-tray-sni)
;;; slothbar-tray-sni.el ends here
