;;; slothbar-dbusmenu.el --- D-BusMenu client/model  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;;; Commentary:

;; Pure com.canonical.dbusmenu client and normalized tree model.  Rendering
;; and popup interaction live elsewhere.

;;; Code:

(require 'cl-lib)
(require 'dbus)
(require 'subr-x)

(require 'slothbar-log)

(defconst slothbar-dbusmenu--interface "com.canonical.dbusmenu")
(defconst slothbar-dbusmenu--properties-interface
  "org.freedesktop.DBus.Properties")

(cl-defstruct (slothbar-dbusmenu-item
               (:constructor slothbar-dbusmenu-item--make)
               (:copier nil))
  "One normalized dbusmenu node."
  id
  label
  enabled
  visible
  type
  toggle-type
  toggle-state
  children-display
  icon-name
  icon-data
  shortcut
  disposition
  accessible-desc
  children)

(cl-defstruct (slothbar-dbusmenu
               (:constructor slothbar-dbusmenu--make)
               (:copier nil))
  "A dbusmenu client for one service object."
  service
  path
  revision
  root
  signal-objects
  update-function)

(defun slothbar-dbusmenu--unwrap (value)
  "Unwrap one-element D-Bus variant/list wrappers from VALUE."
  (cond
   ((and (consp value) (null (cdr value)))
    (slothbar-dbusmenu--unwrap (car value)))
   ((and (consp value) (eq (car value) :variant))
    (slothbar-dbusmenu--unwrap (cadr value)))
   (t value)))

(defun slothbar-dbusmenu--properties (properties)
  "Convert D-Bus PROPERTIES to an alist."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (slothbar-dbusmenu--unwrap (cdr entry))))
          properties))

(defun slothbar-dbusmenu--property (properties name &optional default)
  "Read PROPERTIES NAME, returning DEFAULT when missing."
  (let ((entry (assoc name properties)))
    (if entry
        (slothbar-dbusmenu--unwrap (cdr entry))
      default)))

(defun slothbar-dbusmenu--children (children)
  "Normalize D-Bus CHILDREN nodes."
  (mapcar (lambda (child)
            (let ((child (slothbar-dbusmenu--unwrap child)))
              (slothbar-dbusmenu--node
               (if (and (consp child) (eq (car child) :variant))
                   (cadr child)
                 child))))
          children))

(defun slothbar-dbusmenu--node (node)
  "Normalize a D-Bus dbusmenu NODE."
  (let* ((node (slothbar-dbusmenu--unwrap node))
         (id (slothbar-dbusmenu--unwrap (nth 0 node)))
         (properties (slothbar-dbusmenu--properties (nth 1 node)))
         (children (slothbar-dbusmenu--children (nth 2 node))))
    (slothbar-dbusmenu-item--make
     :id id
     :label (slothbar-dbusmenu--property properties "label" "")
     :enabled (slothbar-dbusmenu--property properties "enabled" t)
     :visible (slothbar-dbusmenu--property properties "visible" t)
     :type (slothbar-dbusmenu--property properties "type" "standard")
     :toggle-type (slothbar-dbusmenu--property properties "toggle-type" "")
     :toggle-state (slothbar-dbusmenu--property properties "toggle-state" 0)
     :children-display
     (slothbar-dbusmenu--property properties "children-display" "")
     :icon-name (slothbar-dbusmenu--property properties "icon-name")
     :icon-data (slothbar-dbusmenu--property properties "icon-data")
     :shortcut (slothbar-dbusmenu--property properties "shortcut")
     :disposition (slothbar-dbusmenu--property properties "disposition")
     :accessible-desc
     (slothbar-dbusmenu--property properties "accessible-desc")
     :children children)))

(defun slothbar-dbusmenu-watch (menu callback)
  "Call CALLBACK when MENU's layout or item properties change."
  (unless (slothbar-dbusmenu-signal-objects menu)
    (setf (slothbar-dbusmenu-update-function menu) callback
          (slothbar-dbusmenu-signal-objects menu)
          (list
           (dbus-register-signal
            :session (slothbar-dbusmenu-service menu)
            (slothbar-dbusmenu-path menu)
            slothbar-dbusmenu--interface "LayoutUpdated"
            (lambda (&rest _)
              (when (slothbar-dbusmenu-update-function menu)
                (funcall (slothbar-dbusmenu-update-function menu)))))
           (dbus-register-signal
            :session (slothbar-dbusmenu-service menu)
            (slothbar-dbusmenu-path menu)
            slothbar-dbusmenu--interface "ItemsPropertiesUpdated"
            (lambda (&rest _)
              (when (slothbar-dbusmenu-update-function menu)
                (funcall (slothbar-dbusmenu-update-function menu))))))))
  menu)

(defun slothbar-dbusmenu-unwatch (menu)
  "Stop watching MENU's D-Bus signals."
  (dolist (object (slothbar-dbusmenu-signal-objects menu))
    (ignore-errors (dbus-unregister-object object)))
  (setf (slothbar-dbusmenu-signal-objects menu) nil
        (slothbar-dbusmenu-update-function menu) nil)
  menu)

(defun slothbar-dbusmenu-refresh (menu)
  "Fetch and normalize MENU's complete layout.
Return MENU.  On failure, leave the previous layout intact."
  (condition-case err
      (let* ((layout
              (dbus-call-method
               :session (slothbar-dbusmenu-service menu)
               (slothbar-dbusmenu-path menu)
               slothbar-dbusmenu--interface "GetLayout"
               :int32 0 :int32 -1 '(:array)))
             (layout (slothbar-dbusmenu--unwrap layout))
             (revision (slothbar-dbusmenu--unwrap (nth 0 layout)))
             (root (slothbar-dbusmenu--node (nth 1 layout))))
        (setf (slothbar-dbusmenu-revision menu) revision
              (slothbar-dbusmenu-root menu) root)
        menu)
    (error
     (slothbar--log-debug* "dbusmenu refresh failed for %s%s: %s"
                           (slothbar-dbusmenu-service menu)
                           (slothbar-dbusmenu-path menu)
                           (error-message-string err))
     nil)))

(defun slothbar-dbusmenu-about-to-show (menu id)
  "Ask MENU's service to prepare item ID for display."
  (dbus-call-method :session (slothbar-dbusmenu-service menu)
                    (slothbar-dbusmenu-path menu)
                    slothbar-dbusmenu--interface "AboutToShow"
                    :int32 id))

(defun slothbar-dbusmenu-event (menu id event &optional data timestamp)
  "Send EVENT for item ID on MENU.
DATA defaults to an integer variant and TIMESTAMP defaults to zero."
  (dbus-call-method :session (slothbar-dbusmenu-service menu)
                    (slothbar-dbusmenu-path menu)
                    slothbar-dbusmenu--interface "Event"
                    :int32 id event (or data '(:variant :int32 0))
                    :uint32 (or timestamp 0)))

(defun slothbar-dbusmenu-click (menu id &optional timestamp)
  "Send a clicked event for ID on MENU."
  (slothbar-dbusmenu-event menu id "clicked" nil timestamp))

(defun slothbar-dbusmenu-close (menu id &optional timestamp)
  "Notify MENU's service that item ID's menu has closed."
  (slothbar-dbusmenu-event menu id "closed" nil timestamp))

(defun slothbar-dbusmenu-find-item (menu id)
  "Return MENU's normalized item with ID, or nil."
  (cl-labels ((find-in (item)
                (cond
                 ((= id (slothbar-dbusmenu-item-id item)) item)
                 (t (cl-loop for child in (slothbar-dbusmenu-item-children item)
                             thereis (find-in child))))))
    (let ((root (slothbar-dbusmenu-root menu)))
      (and root (find-in root)))))

(defun slothbar-dbusmenu-visible-children (item)
  "Return ITEM's visible, standard menu children."
  (cl-remove-if-not
   (lambda (child)
     (and (slothbar-dbusmenu-item-visible child)
          (not (equal "separator"
                      (slothbar-dbusmenu-item-type child)))))
   (slothbar-dbusmenu-item-children item)))

(provide 'slothbar-dbusmenu)
;;; slothbar-dbusmenu.el ends here
