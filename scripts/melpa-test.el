;;; melpa-test.el --- MELPA package build check for slothbar  -*- lexical-binding: t -*-

;; This is executed inside the Guix container created by
;; scripts/melpa-test.sh.  It builds a local MELPA recipe, verifies the
;; generated tarball, unpacks it, and loads the packaged X11-capable files.

;;; Code:

(require 'package)
(require 'seq)

(defvar slothbar-melpa-test--tmp (getenv "SLOTHBAR_TEST_TMP"))
(defvar slothbar-melpa-test--melpa (getenv "SLOTHBAR_TEST_MELPA"))
(defvar slothbar-melpa-test--repo (getenv "SLOTHBAR_TEST_REPO"))

(unless (and slothbar-melpa-test--tmp
             slothbar-melpa-test--melpa
             slothbar-melpa-test--repo)
  (error "SLOTHBAR_TEST_* environment variables are required"))

(setq package-build-directory slothbar-melpa-test--tmp
      package-build-recipes-dir
      (expand-file-name "recipes" slothbar-melpa-test--tmp)
      package-build-working-dir
      (expand-file-name "working" slothbar-melpa-test--tmp)
      package-build-archive-dir
      (expand-file-name "packages" slothbar-melpa-test--tmp)
      package-build-releases nil
      package-build-verbose nil)

(make-directory package-build-recipes-dir t)
(make-directory package-build-working-dir t)
(make-directory package-build-archive-dir t)

(load (expand-file-name "package-build.el"
                        (concat slothbar-melpa-test--melpa "/package-build"))
      nil t)

(package-build-archive "slothbar")

(let* ((tarball (car (last (directory-files
                            package-build-archive-dir t
                            "\\`slothbar-.*\\.tar\\'"))))
       (dest (expand-file-name "unpacked" slothbar-melpa-test--tmp))
       (package-dir nil))
  (unless tarball
    (error "MELPA recipe produced no slothbar tarball"))
  (make-directory dest t)
  (unless (equal 0 (call-process "tar" nil nil nil "-xf" tarball "-C" dest))
    (error "Could not unpack %s" tarball))
  (setq package-dir
        (car (directory-files dest t "\\`slothbar-[0-9]")))
  (unless package-dir
    (error "Unpacked slothbar package directory was not found"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents)
  (package-install-file tarball)
  (add-to-list 'load-path package-dir)
  (dolist (file '("slothbar.el"
                  "slothbar-backend.el"
                  "slothbar-backend-xcb.el"
                  "slothbar-backend-wayland.el"
                  "slothbar-module.el"
                  "slothbar-module-.el"
                  "slothbar-module-requires.el"
                  "slothbar-render-xcb.el"))
    (unless (file-exists-p (expand-file-name file package-dir))
      (error "Package is missing %s" file)))
  (require 'slothbar)
  (require 'slothbar-module-requires)
  (unless (featurep 'slothbar)
    (error "Packaged slothbar did not load"))
  (require 'slothbar-backend-wayland)
  (when slothbar-backend-wayland--elwl-available-p
    (error "MELPA test environment unexpectedly provides elwl"))
  (let ((guarded nil))
    (condition-case nil
        (slothbar-backend-wayland-create)
      (user-error (setq guarded t)))
    (unless guarded
      (error "Wayland backend did not fail cleanly without elwl")))
  (princ "MELPA TEST OK: package built, loaded, and Wayland guard verified\n"))

;;; melpa-test.el ends here
