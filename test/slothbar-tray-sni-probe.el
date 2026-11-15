;;; slothbar-tray-sni-probe.el --- Phase 0 tray/SNI probe  -*- lexical-binding: t; -*-

;; This is a test-only spike for the Wayland tray work.  It is not part of
;; the slothbar package.

;;; Commentary:

;; Run under dbus-run-session through test/slothbar-tray-sni-probe.sh.
;; It checks the D-Bus primitives the StatusNotifier watcher will need,
;; decodes an ARGB32 IconPixmap sample, and verifies the elwl popup entry
;; point is available.

;;; Code:

(require 'cl-lib)
(require 'dbus)

(defun probe--check (label value)
  "Print LABEL as ok/failed and exit non-zero on failure."
  (princ (format "%s: %s\n" (if value "ok" "FAILED") label))
  (unless value (kill-emacs 1))
  value)

(defconst probe--service "org.kde.StatusNotifierWatcher")
(defconst probe--path "/StatusNotifierWatcher")
(defconst probe--interface "org.kde.StatusNotifierWatcher")
(defconst probe--item "org.kde.StatusNotifierItem-Probe")

(let ((items nil))
  (probe--check
   "own StatusNotifierWatcher"
   (eq :primary-owner
       (dbus-register-service :session probe--service
                              :do-not-queue)))
  (probe--check
   "register RegisterStatusNotifierItem"
   (dbus-register-method
    :session probe--service probe--path probe--interface
    "RegisterStatusNotifierItem"
    (lambda (item)
      (cl-pushnew item items :test #'equal)
      (dbus-set-property :session probe--service probe--path
                         probe--interface
                         "RegisteredStatusNotifierItems"
                         (cons :array (cons :string items)))
      :ignore)))
  (probe--check
   "register RegisterStatusNotifierHost"
   (dbus-register-method
    :session probe--service probe--path probe--interface
    "RegisterStatusNotifierHost"
    (lambda (_host) :ignore)))
  (probe--check
   "register watcher properties"
   (progn
     (dbus-register-property
      :session probe--service probe--path probe--interface
      "IsStatusNotifierHostRegistered" :readwrite t)
     (dbus-register-property
      :session probe--service probe--path probe--interface
      "ProtocolVersion" :read :int32 0)
     (dbus-register-property
      :session probe--service probe--path probe--interface
      "RegisteredStatusNotifierItems" :readwrite '(:array :string "probe"))
     t))
  (let ((activated nil))
    (probe--check
     "register a fake SNI item name"
     (eq :primary-owner
         (dbus-register-service :session probe--item :do-not-queue)))
    (probe--check
     "register fake SNI item properties"
     (progn
       (dbus-register-property :session probe--item "/StatusNotifierItem"
                               "org.kde.StatusNotifierItem"
                               "Id" :read "probe")
       (dbus-register-property :session probe--item "/StatusNotifierItem"
                               "org.kde.StatusNotifierItem"
                               "Title" :read "Probe Item")
       (dbus-register-property :session probe--item "/StatusNotifierItem"
                               "org.kde.StatusNotifierItem"
                               "Status" :read "Active")
       (dbus-register-property :session probe--item "/StatusNotifierItem"
                               "org.kde.StatusNotifierItem"
                               "IconName" :read "probe")
       (dbus-register-property :session probe--item "/StatusNotifierItem"
                               "org.kde.StatusNotifierItem"
                               "Menu" :read :object-path "/MenuBar")
       t))
    (probe--check
     "read fake SNI item properties"
     (equal (list (dbus-get-property
                   :session probe--item "/StatusNotifierItem"
                   "org.kde.StatusNotifierItem" "Id")
                  (dbus-get-property
                   :session probe--item "/StatusNotifierItem"
                   "org.kde.StatusNotifierItem" "Title")
                  (dbus-get-property
                   :session probe--item "/StatusNotifierItem"
                   "org.kde.StatusNotifierItem" "Status")
                  (dbus-get-property
                   :session probe--item "/StatusNotifierItem"
                   "org.kde.StatusNotifierItem" "IconName"))
            '("probe" "Probe Item" "Active" "probe")))
    (probe--check
     "register fake SNI item Activate"
     (dbus-register-method
      :session probe--item "/StatusNotifierItem"
      "org.kde.StatusNotifierItem" "Activate"
      (lambda (x y)
        (setq activated (list x y))
        :ignore)))
    (dbus-call-method :session probe--item "/StatusNotifierItem"
                      "org.kde.StatusNotifierItem" "Activate" 7 9)
    (probe--check "fake SNI item activation is delivered"
                  (equal activated '(7 9)))
    (probe--check
     "register fake dbusmenu GetLayout"
     (dbus-register-method
      :session probe--item "/MenuBar" "com.canonical.dbusmenu" "GetLayout"
      (lambda (_parent _depth _properties)
        (list :struct
              (list :uint32 1)
              (list :struct
                    (list :int32 0)
                    (list :array
                          (list :dict-entry
                                :string "label"
                                (list :variant :string "Probe")))
                    (list :array))))))
    (let ((layout (dbus-call-method
                   :session probe--item "/MenuBar"
                   "com.canonical.dbusmenu" "GetLayout"
                   0 -1 '(:array :string "label"))))
      (probe--check "dbusmenu layout is returned"
                    (and layout
                         (string-match-p "Probe" (format "%S" layout))))
      (princ (format "dbusmenu layout: %S\n" layout))))
  (dbus-call-method :session probe--service probe--path probe--interface
                    "RegisterStatusNotifierItem" probe--item)
  (let ((registered (dbus-get-property
                     :session probe--service probe--path probe--interface
                     "RegisteredStatusNotifierItems")))
    (probe--check "fake item is tracked by the watcher"
                  (member probe--item registered))
    (princ (format "registered items: %S\n" registered)))
  (probe--check "watcher host property is readable"
                (eq t (dbus-get-property
                       :session probe--service probe--path probe--interface
                       "IsStatusNotifierHostRegistered"))))

(defun probe--argb32->rgba (bytes)
  "Convert one big-endian ARGB32 BYTES string to an (R G B A) list."
  (let ((v (if (stringp bytes) (string-to-vector bytes) (vconcat bytes))))
    (list (aref v 1) (aref v 2) (aref v 3) (aref v 0))))

(probe--check "ARGB32 icon bytes convert to RGBA byte order"
              (equal (probe--argb32->rgba
                      (unibyte-string #x80 #xff #x00 #x00))
                     '(255 0 0 128)))

(probe--check
 "elwl exposes elwl-layer-surface-get-popup"
 (progn
   (require 'elwl-layer-shell nil t)
   (fboundp 'elwl-layer-surface-get-popup)))

(princ "all tray SNI Phase 0 probes passed\n")
