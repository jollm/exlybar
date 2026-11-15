;;; slothbar-tray-sni-smoke.el --- live StatusNotifier transport test  -*- lexical-binding: t; -*-

;; Run through test/slothbar-tray-sni-smoke.sh under dbus-run-session.
;; This is a test-only integration check for the production SNI transport.

;;; Code:

(require 'cl-lib)
(require 'dbus)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(require 'slothbar)
(require 'slothbar-tray)
(require 'slothbar-tray-sni)

(defconst slothbar-sni-test--item "org.kde.StatusNotifierItem-Test")

(defun slothbar-sni-test--check (label value)
  "Record LABEL as passing when VALUE is non-nil."
  (princ (format "%s: %s\n" (if value "ok" "FAILED") label))
  (unless value (kill-emacs 1))
  value)

(setq slothbar-height 20
      slothbar-tray-icon-gap 2)

(let ((activated nil)
      (secondary nil)
      (context nil))
  (dbus-register-service :session slothbar-sni-test--item :do-not-queue)
  (dolist (property '(("Id" . "test-item")
                      ("Title" . "Test Item")
                      ("Status" . "Active")
                      ("IconName" . "test-icon")))
    (dbus-register-property
     :session slothbar-sni-test--item "/StatusNotifierItem"
     "org.kde.StatusNotifierItem" (car property) :readwrite (cdr property)))
  (dbus-register-method
   :session slothbar-sni-test--item "/StatusNotifierItem"
   "org.kde.StatusNotifierItem" "Activate"
   (lambda (x y) (setq activated (list x y)) :ignore))
  (dbus-register-method
   :session slothbar-sni-test--item "/StatusNotifierItem"
   "org.kde.StatusNotifierItem" "SecondaryActivate"
   (lambda (x y) (setq secondary (list x y)) :ignore))
  (dbus-register-method
   :session slothbar-sni-test--item "/StatusNotifierItem"
   "org.kde.StatusNotifierItem" "ContextMenu"
   (lambda (x y) (setq context (list x y)) :ignore))

  (let ((module (slothbar-tray-create)))
    (slothbar-tray-sni-init module)
    (slothbar-sni-test--check "watcher service is owned"
                              slothbar-tray-sni--service-owned)
    (dbus-call-method :session
                      slothbar-tray-sni--watcher-service
                      slothbar-tray-sni--watcher-path
                      slothbar-tray-sni--watcher-interface
                      "RegisterStatusNotifierItem"
                      slothbar-sni-test--item)
    (sit-for 0.2)
    (let ((items slothbar-tray-sni--items))
      (slothbar-sni-test--check "item registration reached the watcher"
                                (= 1 (length items)))
      (slothbar-sni-test--check "item properties were read"
                                (and (equal "test-item"
                                            (slothbar-tray-sni-item-id
                                             (car items)))
                                     (equal "Test Item"
                                            (slothbar-tray-sni-item-title
                                             (car items)))))
      ;; No IconPixmap was supplied by the fake item.  Inject a synthetic
      ;; image to exercise layout, hit testing, and activation.
      (setf (slothbar-tray-sni-item-image (car items))
            (list 8 8 (make-string (* 8 8 4) 255)))
      (slothbar-tray-sni--layout module)
      (slothbar-sni-test--check "icon layout produced an image placement"
                                (= 1 (length (slothbar-module-images module))))
      (slothbar-tray-sni-click module
                              (+ (slothbar-module-lpad module) 1) 0 :left)
      (slothbar-sni-test--check "Activate reached the item"
                                (equal activated '(5 0)))
      (slothbar-tray-sni-click module
                              (+ (slothbar-module-lpad module) 1) 0 :middle)
      (slothbar-sni-test--check "SecondaryActivate reached the item"
                                (equal secondary '(5 0)))
      (dbus-set-property :session slothbar-sni-test--item
                         "/StatusNotifierItem"
                         "org.kde.StatusNotifierItem"
                         "Title" "Changed Title")
      (ignore-errors
        (dbus-send-signal
         :session nil "/StatusNotifierItem"
         "org.kde.StatusNotifierItem" "NewTitle"))
      (read-event nil nil 0.2)
      (slothbar-sni-test--check "NewTitle refreshes item properties"
                                (equal "Changed Title"
                                       (slothbar-tray-sni-item-title
                                        (car slothbar-tray-sni--items))))
      (dbus-unregister-service :session slothbar-sni-test--item)
      (read-event nil nil 0.2)
      (slothbar-sni-test--check "item removal clears the item list"
                                (null slothbar-tray-sni--items)))
    (slothbar-tray-sni-exit module)
    (slothbar-sni-test--check "watcher service was released"
                              (not slothbar-tray-sni--service-owned))))

(let ((host-registered nil))
  (dbus-register-service :session slothbar-tray-sni--watcher-service
                         :do-not-queue)
  (dbus-register-method
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "RegisterStatusNotifierHost"
   (lambda (host) (setq host-registered host) :ignore))
  (dbus-register-property
   :session slothbar-tray-sni--watcher-service
   slothbar-tray-sni--watcher-path
   slothbar-tray-sni--watcher-interface
   "RegisteredStatusNotifierItems" :readwrite '(:array))
  (let ((module (slothbar-tray-create)))
    (slothbar-tray-sni-init module)
    (slothbar-sni-test--check "secondary host mode was selected"
                              slothbar-tray-sni--secondary-host)
    (slothbar-sni-test--check "secondary host service was announced"
                              (and (stringp host-registered)
                                   (string-prefix-p
                                    "org.kde.StatusNotifierHost-slothbar-"
                                    host-registered)))
    (slothbar-tray-sni-exit module))
  (dbus-unregister-service :session slothbar-tray-sni--watcher-service))

(princ "all tray SNI integration checks passed\n")
(kill-emacs 0)
