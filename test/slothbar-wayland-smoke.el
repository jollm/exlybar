;;; slothbar-wayland-smoke.el --- live Wayland backend smoke test  -*- lexical-binding: t -*-

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

;; Live smoke test for the Wayland backend.  Run it through
;; test/slothbar-wayland-smoke.sh, which starts headless sway and sets
;; WAYLAND_DISPLAY, XDG_RUNTIME_DIR, FD_FAIRY_MODULE, and SLOTHBAR_TEST_FONT.
;;
;; This runs in batch Emacs: the bar is its own Wayland client and does not
;; need an Emacs GUI frame.

;;; Code:

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(let ((extra (getenv "SLOTHBAR_TEST_LOAD_PATH")))
  (when extra
    (dolist (dir (split-string extra path-separator t))
      (add-to-list 'load-path dir))))

(defvar slothbar-wayland-results
  (or (getenv "SLOTHBAR_SMOKE_RESULTS")
      (expand-file-name "slothbar-wayland-smoke-results.txt"
                        temporary-file-directory))
  "File where this test appends its results.")

(with-temp-file slothbar-wayland-results (insert ""))

(defun slothbar-wayland-record (line)
  "Append LINE to the results file and echo it."
  (with-temp-buffer
    (insert line)
    (append-to-file (point-min) (point-max) slothbar-wayland-results))
  (message "%s" line))

(defun slothbar-wayland-check (label value)
  "Record LABEL as passing when VALUE is non-nil, else abort."
  (slothbar-wayland-record
   (format "%s: %s\n" (if value "ok" "FAILED") label))
  (unless value (kill-emacs 1))
  value)

(defun slothbar-wayland--run ()
  "Exercise the Wayland backend and the core lifecycle against sway."
  (require 'slothbar)
  (require 'slothbar-module-)
  (require 'slothbar-backend-wayland)
  (let ((font (getenv "SLOTHBAR_TEST_FONT")))
    (unless (and font (file-exists-p font))
      (error "SLOTHBAR_TEST_FONT must name an existing font file"))
    (let ((map (make-vector 10 nil)))
      (aset map 0 font)
      (setq slothbar-font--color-code-map map)))

  (setq slothbar-width 300
        slothbar-height 20
        slothbar-offset-x 0
        slothbar-offset-y 0
        slothbar-margin-y 0
        slothbar-is-bottom nil
        slothbar-preferred-display nil
        slothbar-backend 'wayland)
  (slothbar-wayland-check "system font found for ^f0"
                          (slothbar-font-find 0))
  (setq slothbar-font-px-size
        (slothbar-font--precompute-px-sizes
         slothbar-height slothbar-font--color-code-map))

  ;; Direct backend and module lifecycle.
  (let* ((backend (slothbar-backend-create 'wayland))
         (module (slothbar-module-create
                  :name "smoke" :format "hello" :lpad 2 :rpad 2)))
    (setq slothbar--backend backend)
    (unwind-protect
        (progn
          (slothbar-backend-open backend)
          (slothbar-wayland-check
           "layer configure supplied a width"
           (> (slothbar-backend-wayland-logical-width backend) 0))
          (slothbar-wayland-check
           "surface and layer surface exist"
           (and (slothbar-backend-wayland-surface backend)
                (slothbar-backend-wayland-layer-surface backend)))
          (slothbar-wayland-check
           "layer configure was acknowledged"
           (slothbar-backend-wayland-configured-serial backend))
          (slothbar-wayland-check
           "Wayland has no system tray support"
           (not (slothbar-backend-supports-p backend 'system-tray)))
          (slothbar-wayland-check
           "Wayland has no ewmh support"
           (not (slothbar-backend-supports-p backend 'ewmh)))
          (slothbar-wayland-check
           "Wayland has no randr support"
           (not (slothbar-backend-supports-p backend 'randr)))
          ;; X-only modules are filtered out during construction.
          (require 'slothbar-tray)
          (require 'slothbar-workspaces)
          (let ((slothbar-modules
                 (list #'slothbar-tray-create
                       #'slothbar-workspaces-create)))
            (slothbar--construct-modules)
            (slothbar-wayland-check
             "X-only modules are gated on Wayland"
             (null slothbar--modules)))
          (require 'slothbar-ewmh)
          (require 'slothbar-randr)
          (slothbar-ewmh-mode 1)
          (slothbar-wayland-check
           "ewmh mode disables itself on Wayland"
           (not slothbar-ewmh-mode))
          (slothbar-randr-mode 1)
          (slothbar-wayland-check
           "randr mode disables itself on Wayland"
           (not slothbar-randr-mode))
          (let* ((scale (slothbar-backend-wayland-scale backend))
                 (width (slothbar-backend-wayland-logical-width backend))
                 (height (slothbar-backend-wayland-logical-height backend)))
            (slothbar-wayland-record
             (format "width=%s height=%s scale=%s output=%S\n"
                     width height scale
                     (and (slothbar-backend-wayland-output backend)
                          (elwl-output-name
                           (slothbar-backend-wayland-output backend)))))
            (slothbar-wayland-check "output scale is positive"
                                    (>= scale 1))
            (slothbar-wayland-check "two shm buffers"
                                    (= (length
                                        (slothbar-backend-wayland-buffers
                                         backend))
                                       2))
            (slothbar-wayland-check
             "frame dimensions follow scale"
             (and (= (slothbar-backend-wayland-frame-width backend)
                     (* width scale))
                  (= (slothbar-backend-wayland-frame-height backend)
                     (* height scale)))))
          (slothbar-module-init module)
          (slothbar-backend-present
           backend (list (list (list 0 0) module)) '((0 100)))
          (slothbar-wayland-check
           "module draw operations were recorded"
           (gethash module
                    (slothbar-backend-wayland-module-ops backend)))
          (slothbar-wayland-check
           "glyphs were rasterized"
           (> (hash-table-count
               (slothbar-backend-wayland-glyph-cache backend))
              0))
          (slothbar-wayland-check
           "frame was composed"
           (slothbar-backend-wayland-frame backend))
          (let* ((frame (slothbar-backend-wayland-frame backend))
                 (width (slothbar-backend-wayland-frame-width backend))
                 (height (slothbar-backend-wayland-frame-height backend))
                 (found nil)
                 (index 0))
            (while (and (not found) (< index (* width height)))
              (let ((offset (* index 4)))
                (when (or (> (aref frame offset) 0)
                          (> (aref frame (1+ offset)) 0)
                          (> (aref frame (+ offset 2)) 0))
                  (setq found t)))
              (cl-incf index))
            (slothbar-wayland-check
             "frame contains glyph pixels" found))
          (slothbar-backend-show backend)
          (elwl-dispatch (slothbar-backend-wayland-connection backend) 0.1)
          (slothbar-wayland-check
           "frame was presented"
           (slothbar-backend-wayland-presented backend))
          (slothbar-backend-hide backend)
          (slothbar-module-exit module)
          (slothbar-backend-close backend)
          (setq slothbar--backend nil))
      (ignore-errors (slothbar-backend-close backend))
      (setq slothbar--backend nil)))

  ;; Full core lifecycle through slothbar--start and slothbar-exit.
  (setq slothbar-modules
        (list (lambda ()
                (slothbar-module-create
                 :name "live" :format "live" :lpad 2 :rpad 2))))
  (slothbar--start)
  (slothbar-wayland-check "core start enabled slothbar"
                          (slothbar-enabled-p))
  (slothbar-wayland-check
   "core start created a Wayland backend"
   (and slothbar--backend
        (eq (slothbar-backend-kind slothbar--backend) 'wayland)))
  (slothbar-wayland-check
   "core start initialized the module"
   (cl-some #'slothbar-module-initialized slothbar--modules))
  (sit-for 0.3)
  (slothbar-exit)
  (slothbar-wayland-check "core exit disabled slothbar"
                          (not (slothbar-enabled-p)))
  (slothbar-wayland-check "core exit cleared the backend"
                          (null slothbar--backend))
  (let ((auto (slothbar-backend-create 'auto)))
    (slothbar-wayland-check
     "auto selects the Wayland backend when available"
     (eq (slothbar-backend-kind auto) 'wayland))))

(condition-case err
    (progn
      (slothbar-wayland--run)
      (slothbar-wayland-record "all wayland smoke checks passed\n")
      (kill-emacs 0))
  (error
   (slothbar-wayland-record (format "ERROR: %S\n" err))
   (kill-emacs 1)))

;;; slothbar-wayland-smoke.el ends here
