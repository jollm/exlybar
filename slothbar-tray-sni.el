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
(require 'subr-x)

(require 'slothbar-log)
(require 'slothbar-module-)
(require 'slothbar-dbusmenu)
(require 'slothbar-menu)
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

(defvar slothbar-tray-sni--modules nil
  "Tray modules sharing the SNI watcher.")

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

(defun slothbar-tray-sni--image-size (image)
  "Return IMAGE's dimensions as a short string."
  (if image
      (format "%dx%d" (nth 0 image) (nth 1 image))
    "none"))

(defun slothbar-tray-sni--image-stats (image)
  "Return dimensions and alpha statistics for IMAGE."
  (if (not image)
      "none"
    (let* ((width (nth 0 image))
           (height (nth 1 image))
           (data (nth 2 image))
           (pixels (* width height))
           (nonzero-alpha 0)
           (max-alpha 0)
           (nonzero-rgb 0))
      (dotimes (index pixels)
        (let* ((offset (* index 4))
               (alpha (aref data (+ offset 3))))
          (when (> alpha 0) (cl-incf nonzero-alpha))
          (when (> alpha max-alpha) (setq max-alpha alpha))
          (when (or (> (aref data offset) 0)
                    (> (aref data (1+ offset)) 0)
                    (> (aref data (+ offset 2)) 0))
            (cl-incf nonzero-rgb))))
      (format "%dx%d alpha>0=%d/%d max-alpha=%d rgb>0=%d"
              width height nonzero-alpha pixels max-alpha nonzero-rgb))))

(defun slothbar-tray-sni--describe-item (item)
  "Return a compact, non-payload ITEM description for debug logs."
  (format (concat "service=%s path=%s id=%S title=%S status=%s "
                  "category=%S window=%S icon=%S image=%s attention=%s "
                  "overlay=%s menu=%S is-menu=%S order=%S tooltip=%s label=%s")
          (slothbar-tray-sni-item-service item)
          (slothbar-tray-sni-item-path item)
          (slothbar-tray-sni-item-id item)
          (let ((title (slothbar-tray-sni-item-title item)))
            (if (stringp title)
                (truncate-string-to-width title 48 nil nil t)
              title))
          (slothbar-tray-sni-item-status item)
          (slothbar-tray-sni-item-category item)
          (slothbar-tray-sni-item-window-id item)
          (slothbar-tray-sni-item-icon-name item)
          (slothbar-tray-sni--image-size (slothbar-tray-sni-item-image item))
          (slothbar-tray-sni--image-size
           (slothbar-tray-sni-item-attention-image item))
          (slothbar-tray-sni--image-size
           (slothbar-tray-sni-item-overlay-image item))
          (slothbar-tray-sni-item-menu item)
          (slothbar-tray-sni-item-item-is-menu item)
          (slothbar-tray-sni-item-ordering-index item)
          (if (slothbar-tray-sni-item-tooltip item) "present" "none")
          (if (slothbar-tray-sni-item-label item) "present" "none")))

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
      (or (progn
            (when (gethash key slothbar-tray-sni--icon-cache)
              (slothbar--log-debug* "SNI icon cache hit %s -> %s"
                                    name (gethash key slothbar-tray-sni--icon-cache)))
            (gethash key slothbar-tray-sni--icon-cache))
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
            (if path
                (slothbar--log-debug* "SNI icon resolved %s -> %s" name path)
              (slothbar--log-debug* "SNI icon not found %s in %s"
                                    name (mapconcat #'identity dirs ":")))
            (when path
              (puthash key path slothbar-tray-sni--icon-cache))
            path)))))

(defun slothbar-tray-sni--load-icon (name theme-paths target-height)
  "Load and scale NAME from THEME-PATHS for TARGET-HEIGHT."
  (when-let* ((path (slothbar-tray-sni--find-icon name theme-paths))
              (umbra (progn
                       (unless (require 'umbra-png nil t)
                         (slothbar--log-debug*
                          "SNI icon %s found at %s but umbra-png is unavailable"
                          name path))
                       (require 'umbra-png nil t))))
    (condition-case err
        (let* ((pixmap (umbra-png-read path))
               (width (umbra-pixmap-width pixmap))
               (height (umbra-pixmap-height pixmap))
               (rgba (umbra-pixmap-demultiplied-data pixmap)))
          (slothbar--log-debug* "SNI icon %s decoded %dx%d -> target %d"
                                path width height target-height)
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
      (slothbar--log-debug* "SNI watch signals for %s%s" service path)
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
  (slothbar--log-debug* "SNI remove item service=%s path=%s" service path)
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
  "Remove items whose bus NAME lost its owner.
NAME can be a well-known item service or the unique sender name used
for an object-path registration."
  (when (and (stringp name)
             (string-empty-p (or new "")))
    (slothbar--log-debug* "SNI bus owner vanished: %s" name)
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
  (slothbar--log-debug* "SNI add/refresh item service=%s path=%s" service path)
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
    (slothbar--log-debug* "SNI item registered: %s"
                          (slothbar-tray-sni--describe-item item))
    (slothbar--log-debug* "SNI image stats: base=%s attention=%s overlay=%s"
                          (slothbar-tray-sni--image-stats
                           (slothbar-tray-sni-item-image item))
                          (slothbar-tray-sni--image-stats
                           (slothbar-tray-sni-item-attention-image item))
                          (slothbar-tray-sni--image-stats
                           (slothbar-tray-sni-item-overlay-image item)))
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
      (slothbar--log-debug* "SNI RegisterStatusNotifierItem item=%S sender=%S"
                            item sender)
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
          (+ x (slothbar-module-rpad m)))
    (slothbar--log-debug* "SNI layout items=%d images=%d width=%d"
                          (length slothbar-tray-sni--items)
                          (length (slothbar-module-images m))
                          (slothbar-module-width m))))

(defun slothbar-tray-sni--refresh-module ()
  "Redraw every tray module sharing the SNI watcher."
  (dolist (module slothbar-tray-sni--modules)
    (setf (slothbar-module-needs-refresh? module) t)
    (let ((bar (slothbar-module--bar module)))
      (slothbar--log-debug* "SNI refresh module enabled=%S bar=%S modules=%d"
                            (slothbar-enabled-p)
                            (and bar (slothbar-id bar))
                            (if bar (length (slothbar-module-list bar)) 0)))
    (when (slothbar-enabled-p)
      (slothbar-refresh-modules
       nil (slothbar-module--bar module)))))

(defun slothbar-tray-sni-init (m)
  "Start or reuse the StatusNotifier watcher for M."
  (unless (memq m slothbar-tray-sni--modules)
    (push m slothbar-tray-sni--modules))
  (if (or slothbar-tray-sni--service-owned
          slothbar-tray-sni--secondary-host)
      (slothbar--log-debug* "SNI init: reusing existing watcher for %s"
                            (slothbar-id (slothbar-module--bar m)))
    (slothbar--log-debug* "SNI init: trying to own %s"
                          slothbar-tray-sni--watcher-service)
    (setq slothbar-tray-sni--items nil)
    (if (eq :primary-owner
            (dbus-register-service
             :session slothbar-tray-sni--watcher-service
             :do-not-queue))
        (progn
          (slothbar--log-debug* "SNI init: primary watcher owner")
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
      (slothbar--log-debug* "SNI init: watcher occupied, trying secondary host")
      (unless (slothbar-tray-sni--try-secondary-host)
        (error "Another StatusNotifier host owns %s"
               slothbar-tray-sni--watcher-service)))
    (unless slothbar-tray-sni--name-owner-signal
      (setq slothbar-tray-sni--name-owner-signal
            (dbus-register-signal
             :session "org.freedesktop.DBus" "/org/freedesktop/DBus"
             "org.freedesktop.DBus" "NameOwnerChanged"
             #'slothbar-tray-sni--on-name-owner-changed))))
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

(defun slothbar-tray-sni--show-menu (m item x)
  "Open ITEM's dbusmenu popup on M at X."
  (when-let* ((path (slothbar-tray-sni-item-menu item))
              (service (slothbar-tray-sni-item-service item))
              (bar (slothbar-module--bar m)))
    (let* ((dbusmenu (slothbar-dbusmenu--make
                      :service service :path path))
           (menu (and (slothbar-dbusmenu-refresh dbusmenu)
                      (slothbar-menu-create dbusmenu))))
      (when menu
        (ignore-errors (slothbar-dbusmenu-about-to-show dbusmenu 0))
        (slothbar-menu-show menu bar (round x) 0)))))

(defun slothbar-tray-sni-click (m x y button)
  "Activate the SNI item at X,Y on M for BUTTON."
  (when-let* ((item (slothbar-tray-sni--item-at-x m x)))
    (if (and (slothbar-tray-sni-item-menu item)
             (or (eq button :right)
                 (and (eq button :left)
                      (slothbar-tray-sni-item-item-is-menu item))))
        (or (slothbar-tray-sni--show-menu m item x)
            (slothbar--log-debug* "SNI menu unavailable for %s"
                                  (slothbar-tray-sni-item-id item)))
      (when-let* ((method (pcase button
                            (:left "Activate")
                            (:middle "SecondaryActivate")
                            (:right "ContextMenu")))
                  (service (slothbar-tray-sni-item-service item))
                  (path (slothbar-tray-sni-item-path item)))
        (condition-case err
            (progn
              (slothbar--log-debug* "SNI %s %s%s at %d,%d"
                                    method service path (round x) (round y))
              (dbus-call-method :session service path
                                slothbar-tray-sni--item-interface
                                method (round x) (round y))
              t)
          (error
           (slothbar--log-debug* "SNI %s failed for %s: %S"
                                 method service err)
           nil))))))

(defun slothbar-tray-sni-exit (m)
  "Stop M's SNI transport, tearing down the watcher after the last module."
  (setq slothbar-tray-sni--modules
        (delq m slothbar-tray-sni--modules))
  (if slothbar-tray-sni--modules
      (slothbar--log-debug* "SNI exit: %d tray module(s) still active"
                            (length slothbar-tray-sni--modules))
    (slothbar--log-debug* "SNI exit: service-owned=%S secondary=%S items=%d"
                          slothbar-tray-sni--service-owned
                          slothbar-tray-sni--secondary-host
                          (length slothbar-tray-sni--items))
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
          slothbar-tray-sni--modules nil
          slothbar-tray-sni--service-owned nil
          slothbar-tray-sni--secondary-host nil
          slothbar-tray-sni--name-owner-signal nil
          slothbar-tray-sni--watcher-signal-objects nil)
    (clrhash slothbar-tray-sni--signal-items)
    (clrhash slothbar-tray-sni--signal-objects)
    (clrhash slothbar-tray-sni--icon-cache)))

(provide 'slothbar-tray-sni)
;;; slothbar-tray-sni.el ends here
