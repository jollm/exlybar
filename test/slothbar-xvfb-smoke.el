;;; slothbar-xvfb-smoke.el --- live X backend smoke test under xmonad  -*- lexical-binding: t; -*-

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
;; this program. If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Run through `test/slothbar-xvfb-smoke.sh', which starts Xvfb and xmonad
;; with the EWMH hook enabled.  This exercises the X backend and the EWMH
;; workspaces source under a real window manager, without Xwayland.

;;; Code:

(require 'cl-lib)
(require 'slothbar)
(require 'slothbar-backend-xcb)
(require 'slothbar-ewmh)
(require 'slothbar-module-)
(require 'slothbar-workspaces)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(defvar slothbar-smoke--results
  (or (getenv "SLOTHBAR_SMOKE_RESULTS")
      (expand-file-name "slothbar-xvfb-smoke-results.txt"
                        temporary-file-directory))
  "File where this test appends its results.")

(with-temp-file slothbar-smoke--results (insert ""))

(defun slothbar-smoke--record (line)
  "Append LINE to the results file and echo it."
  (with-temp-buffer
    (insert line)
    (append-to-file (point-min) (point-max) slothbar-smoke--results))
  (message "%s" line))

(defun slothbar-smoke--check (label value)
  "Record LABEL as passing when VALUE is non-nil, else abort.
Return VALUE."
  (slothbar-smoke--record
   (format "%s: %s\n" (if value "ok" "FAILED") label))
  (unless value (kill-emacs 1))
  value)

(defun slothbar-smoke--wm-name ()
  "Return the running window manager's name, or nil."
  (with-temp-buffer
    (when (eq 0 (call-process "wmctrl" nil t nil "-m"))
      (let ((text (buffer-string)))
        (when (string-match "^Name:[ \t]*\\(.+\\)$" text)
          (string-trim (match-string 1 text)))))))

(let ((wm (slothbar-smoke--wm-name)))
  (slothbar-smoke--record (format "window manager: %S\n" wm))
  (slothbar-smoke--check "xmonad is running"
                         (and wm (string-match-p "xmonad" wm))))

(setq slothbar-height 20
      slothbar-width 1280
      slothbar-offset-x 0
      slothbar-offset-y 0
      slothbar-is-bottom nil
      slothbar-backend 'xcb)
(setq slothbar-font--color-code-map (slothbar-font-map-candidates))
(slothbar-smoke--check "system font found for ^f0"
                       (slothbar-font-find 0))
(setq slothbar-font-px-size
      (slothbar-font--precompute-px-sizes
       slothbar-height slothbar-font--color-code-map))

(let* ((backend (slothbar-backend-create 'xcb))
       (bar (slothbar-create :display-backend backend
                             :pixel-width slothbar-width
                             :pixel-height slothbar-height))
       (module (slothbar-workspaces-create)))
  (setq slothbar--backend backend
        slothbar--bar bar)
  (unwind-protect
      (progn
        (slothbar-backend-open backend)
        (slothbar-backend-create-bar backend bar)
        (slothbar-smoke--check "opened an xcb connection"
                               (slothbar-xcb-connection))
        (slothbar-smoke--check "created the bar window"
                               (slothbar-xcb-window))
        (slothbar-smoke--check "X backend advertises ewmh"
                               (slothbar-backend-supports-p backend 'ewmh))
        (slothbar-smoke--check "X backend advertises randr"
                               (slothbar-backend-supports-p backend 'randr))
        (slothbar-smoke--check "Xvfb exposes an output"
                               (slothbar-backend-output-names backend))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (slothbar-ewmh-mode 1))
        (slothbar-smoke--check "EWMH initialized"
                               slothbar-ewmh-mode)
        (slothbar-smoke--record
         (format "EWMH desktops=%s current=%s names=%S\n"
                 slothbar-ewmh--num-desktops
                 slothbar-ewmh--current-desktop
                 slothbar-ewmh--desktop-names))
        (slothbar-smoke--check "xmonad exposes EWMH desktops"
                               (> (or slothbar-ewmh--num-desktops 0) 0))
        (slothbar-smoke--check "EWMH desktop names are available"
                               (and slothbar-ewmh--desktop-names
                                    (> (length slothbar-ewmh--desktop-names)
                                       0)))
        (let ((list (slothbar-workspaces-generate-list-fn-ewmh)))
          (slothbar-smoke--record (format "workspaces=%S\n" list))
          (slothbar-smoke--check "workspaces list is non-empty"
                                 list)
          (slothbar-smoke--check "exactly one workspace is current"
                                 (= 1 (cl-count-if
                                       (lambda (ws)
                                         (memq :current (cadr ws)))
                                       list))))
        (setf (slothbar-module-backend module) backend
              (slothbar-module-bar module) bar)
        (slothbar-module-init module)
        (slothbar-smoke--check "workspaces module initialized"
                               (slothbar-module-initialized module))
        (slothbar-smoke--check "workspaces module has text"
                               (and (slothbar-module-text module)
                                    (not (string-empty-p
                                          (slothbar-module-text module)))))
        (slothbar-smoke--check "workspaces module has a surface"
                               (gethash module
                                        (slothbar-backend-xcb-module-surfaces
                                         backend)))
        (slothbar-module-exit module)
        (slothbar-smoke--check "workspaces module exit removed its surface"
                               (null (gethash
                                      module
                                      (slothbar-backend-xcb-module-surfaces
                                       backend))))
        (when slothbar-ewmh-mode
          (slothbar-ewmh-mode -1)))
    (ignore-errors
      (when slothbar-ewmh-mode
        (slothbar-ewmh-mode -1)))
    (ignore-errors (slothbar-backend-close backend))
    (setq slothbar--backend nil
          slothbar--bar nil)))
(princ "all xvfb/xmonad smoke checks passed\n")
(kill-emacs 0)
