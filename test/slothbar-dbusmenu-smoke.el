;;; slothbar-dbusmenu-smoke.el --- live dbusmenu client test  -*- lexical-binding: t -*-

;; Run through test/slothbar-dbusmenu-smoke.sh under dbus-run-session.

;;; Code:

(require 'cl-lib)
(require 'dbus)
(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))
(require 'slothbar-dbusmenu)

(defconst slothbar-dbusmenu-test--service "org.slothbar.DbusmenuTest")
(defconst slothbar-dbusmenu-test--path "/MenuBar")
(defvar slothbar-dbusmenu-test--seen-layout nil)
(defvar slothbar-dbusmenu-test--seen-show nil)
(defvar slothbar-dbusmenu-test--seen-event nil)

(defun slothbar-dbusmenu-test--check (label value)
  "Print LABEL as OK when VALUE is non-nil."
  (princ (format "%s: %s\n" (if value "ok" "FAILED") label))
  (unless value (kill-emacs 1))
  value)

(defun slothbar-dbusmenu-test--layout ()
  "Return a minimal dbusmenu layout."
  (list :struct
        (list :uint32 7)
        (list :struct
              (list :int32 0)
              (list :array
                    (list :dict-entry
                          :string "label" (list :variant :string "Root"))
                    (list :dict-entry
                          :string "children-display"
                          (list :variant :string "submenu")))
              (list :array
                    (list :variant
                          (list :struct
                                (list :int32 1)
                                (list :array
                                      (list :dict-entry
                                            :string "label"
                                            (list :variant :string "Quit"))
                                      (list :dict-entry
                                            :string "enabled"
                                            (list :variant :boolean t)))
                                (list :array)))))))

(dbus-register-service :session slothbar-dbusmenu-test--service
                       :do-not-queue)
(dbus-register-method
 :session slothbar-dbusmenu-test--service slothbar-dbusmenu-test--path
 "com.canonical.dbusmenu" "GetLayout"
 (lambda (parent depth properties)
   (setq slothbar-dbusmenu-test--seen-layout (list parent depth properties))
   (slothbar-dbusmenu-test--layout)))
(dbus-register-method
 :session slothbar-dbusmenu-test--service slothbar-dbusmenu-test--path
 "com.canonical.dbusmenu" "AboutToShow"
 (lambda (id)
   (setq slothbar-dbusmenu-test--seen-show id)
   t))
(dbus-register-method
 :session slothbar-dbusmenu-test--service slothbar-dbusmenu-test--path
 "com.canonical.dbusmenu" "Event"
 (lambda (id event data timestamp)
   (setq slothbar-dbusmenu-test--seen-event
         (list id event data timestamp))
   :ignore))

(let ((menu (slothbar-dbusmenu--make
             :service slothbar-dbusmenu-test--service
             :path slothbar-dbusmenu-test--path)))
  (slothbar-dbusmenu-refresh menu)
  (slothbar-dbusmenu-test--check "GetLayout was called"
                                 (equal '(0 -1 nil)
                                        slothbar-dbusmenu-test--seen-layout))
  (slothbar-dbusmenu-test--check "revision parsed"
                                 (= 7 (slothbar-dbusmenu-revision menu)))
  (let* ((root (slothbar-dbusmenu-root menu))
         (child (car (slothbar-dbusmenu-item-children root))))
    (slothbar-dbusmenu-test--check "root label parsed"
                                   (equal "Root" (slothbar-dbusmenu-item-label root)))
    (slothbar-dbusmenu-test--check "root is a submenu"
                                   (equal "submenu"
                                          (slothbar-dbusmenu-item-children-display
                                           root)))
    (slothbar-dbusmenu-test--check "child label parsed"
                                   (equal "Quit" (slothbar-dbusmenu-item-label child)))
    (slothbar-dbusmenu-test--check "child enabled flag parsed"
                                   (eq t (slothbar-dbusmenu-item-enabled child)))
    (slothbar-dbusmenu-test--check "visible children parsed"
                                   (= 1 (length
                                         (slothbar-dbusmenu-visible-children root))))
    (slothbar-dbusmenu-click menu 1 42)
    (slothbar-dbusmenu-test--check "AboutToShow called"
                                   (= 1 slothbar-dbusmenu-test--seen-show))
    (slothbar-dbusmenu-test--check "Event clicked dispatched"
                                   (and (equal 1 (nth 0 slothbar-dbusmenu-test--seen-event))
                                        (equal "clicked"
                                               (nth 1 slothbar-dbusmenu-test--seen-event))
                                        (= 42 (nth 3 slothbar-dbusmenu-test--seen-event))))))

(princ "all dbusmenu client checks passed\n")
(kill-emacs 0)
