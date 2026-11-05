;;; slothbar-xwayland-smoke.el --- live X backend smoke test  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

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

;;; Commentary:

;; Live smoke test for the xcb backend.  Run it through
;; test/slothbar-xwayland-smoke.sh, which boots headless sway and an
;; Xwayland server and sets DISPLAY.
;;
;; This must run in a GUI Emacs, not `--batch': xelb's EWMH/ICCCM
;; initialization calls `x-display-screens', which needs an X frame.
;;
;; Environment:
;;   SLOTHBAR_SMOKE_RESULTS   file to append results to
;;   SLOTHBAR_TEST_LOAD_PATH  colon-separated extra `load-path' entries

;;; Code:

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(let ((extra (getenv "SLOTHBAR_TEST_LOAD_PATH")))
  (when extra
    (dolist (dir (split-string extra path-separator t))
      (add-to-list 'load-path dir))))

(defvar slothbar-smoke--results
  (or (getenv "SLOTHBAR_SMOKE_RESULTS")
      (expand-file-name "slothbar-xwayland-smoke-results.txt"
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

(defun slothbar-smoke--pixmap-non-background (backend module)
  "Count pixels in MODULE's pixmap that differ from its first pixel."
  (let* ((surface (gethash
                   module
                   (slothbar-backend-xcb-module-surfaces backend)))
         (pixmap (slothbar-backend-xcb--module-surface-pixmap surface))
         (width (slothbar-module-width module))
         (height slothbar-height)
         (reply (xcb:+request-unchecked+reply
                    (slothbar-xcb-connection)
                  (make-instance 'xcb:GetImage
                                 :format xcb:ImageFormat:ZPixmap
                                 :drawable pixmap
                                 :x 0 :y 0
                                 :width width :height height
                                 :plane-mask #xffffffff)))
         (data (vconcat (slot-value reply 'data)))
         (bytes-per-pixel (/ (length data) (* width height)))
         (background (cl-loop for index below bytes-per-pixel
                              collect (aref data index)))
         (count 0))
    (dotimes (pixel-index (* width height))
      (let ((offset (* pixel-index bytes-per-pixel))
            (different nil))
        (dotimes (byte bytes-per-pixel)
          (unless (= (aref data (+ offset byte))
                     (nth byte background))
            (setq different t)))
        (when different (cl-incf count))))
    count))

(defun slothbar-smoke--run ()
  "Exercise the xcb backend and the core lifecycle against a live X server."
  (unless (frame-parameter nil 'display)
    (error "Run this test in a GUI Emacs with DISPLAY set, not --batch"))
  (require 'slothbar)
  (require 'slothbar-module-)
  (require 'slothbar-backend-xcb)
  (slothbar-smoke--record
   (format "display: %s\n" (frame-parameter nil 'display)))

  (setq slothbar-width 200
        slothbar-height 20
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

  ;; Direct backend and module lifecycle.
  (let* ((backend (slothbar-backend-create 'xcb))
         (module (slothbar-module-create
                  :name "smoke" :format "hello" :lpad 2 :rpad 2)))
    (setq slothbar--backend backend)
    (unwind-protect
        (progn
          (slothbar-backend-open backend)
          (slothbar-smoke--check "opened an xcb connection"
                                 (slothbar-xcb-connection))
          (slothbar-smoke--check "created the bar window"
                                 (slothbar-xcb-window))
          (slothbar-smoke--check "created a background pixmap"
                                 (slothbar-backend-xcb-pixmap backend))
          (slothbar-smoke--check "xcb supports the system tray"
                                 (slothbar-backend-supports-p
                                  backend 'system-tray))
          (slothbar-smoke--check "xcb supports ewmh"
                                 (slothbar-backend-supports-p
                                  backend 'ewmh))
          (slothbar-smoke--check "xcb supports randr"
                                 (slothbar-backend-supports-p
                                  backend 'randr))
          (slothbar-smoke--check "xcb supports pointer input"
                                 (slothbar-backend-supports-p
                                  backend 'pointer-input))
          (slothbar-smoke--check "xcb has no ext-workspace support"
                                 (not (slothbar-backend-supports-p
                                       backend 'ext-workspace)))
          (slothbar-smoke--check "xcb has no ext-workspace manager"
                                 (null (slothbar-backend-ext-workspace-manager
                                        backend)))
          (slothbar-module-init module)
          (slothbar-smoke--check "module initialized with backend resources"
                                 (slothbar-module-initialized module))
          (slothbar-smoke--check "module layout produced a width"
                                 (> (slothbar-module-width module) 0))
          (slothbar-smoke--check "module surface exists"
                                 (gethash
                                  module
                                  (slothbar-backend-xcb-module-surfaces
                                   backend)))
          ;; Regression: a refreshed module can keep its width while its text
          ;; shrinks.  The old glyphs must not survive the redraw.
          (let ((previous-width (slothbar-module-width module))
                (before (slothbar-smoke--pixmap-non-background
                         backend module)))
            (slothbar-smoke--check "initial glyphs are drawn" (> before 0))
            (aset module (cl-struct-slot-offset 'slothbar-module 'format)
                  "i")
            (slothbar-module-layout module)
            ;; Keep the pixmap width unchanged so only the redraw clear can
            ;; remove the old glyphs.
            (aset module (cl-struct-slot-offset 'slothbar-module 'width)
                  previous-width)
            (slothbar-backend-draw-module backend module)
            (slothbar-smoke--check
             "redraw clears previous glyphs"
             (< (slothbar-smoke--pixmap-non-background backend module)
                before)))
          (slothbar-backend-present
           backend
           (list (list (list 0 0) module))
           '((0 100)))
          (slothbar-backend-show backend)
          (slothbar-backend-hide backend)
          (slothbar-module-exit module)
          (slothbar-smoke--check
           "module exit removed its surface"
           (null (gethash module
                          (slothbar-backend-xcb-module-surfaces backend)))))
      (slothbar-backend-close backend)
      (setq slothbar--backend nil))
    (slothbar-smoke--check "closed the connection"
                           (null (slothbar-backend-xcb-connection backend))))

  ;; Full core lifecycle through slothbar--start and slothbar-exit.
  (setq slothbar-modules
        (list (lambda ()
                (slothbar-module-create
                 :name "live" :format "live" :lpad 2 :rpad 2))))
  (slothbar--start)
  (slothbar-smoke--check "core start enabled slothbar"
                         (slothbar-enabled-p))
  (slothbar-smoke--check "core start created a backend"
                         slothbar--backend)
  (slothbar-smoke--check "core start initialized the module"
                         (cl-some #'slothbar-module-initialized
                                  slothbar--modules))
  ;; Let Expose and other X events reach the handlers.
  (sit-for 0.3)
  (slothbar-exit)
  (slothbar-smoke--check "core exit disabled slothbar"
                         (not (slothbar-enabled-p)))
  (slothbar-smoke--check "core exit cleared the backend"
                         (null slothbar--backend)))

(condition-case err
    (progn
      (slothbar-smoke--run)
      (slothbar-smoke--record "all xwayland smoke checks passed\n")
      (kill-emacs 0))
  (error
   (slothbar-smoke--record (format "ERROR: %S\n" err))
   (kill-emacs 1)))

;;; slothbar-xwayland-smoke.el ends here
