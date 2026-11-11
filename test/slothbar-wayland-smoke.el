;;; slothbar-wayland-smoke.el --- live Wayland backend smoke test  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.31.0
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

;; Define the backend accessors before any `setf' forms are expanded.
(require 'slothbar-backend-wayland)

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

(defun slothbar-wayland--swaymsg (&rest args)
  "Run swaymsg with ARGS against the harness sway instance."
  (let ((sock (or (getenv "SWAYSOCK")
                  (car (file-expand-wildcards
                        (expand-file-name "sway-ipc.*.sock"
                                          (or (getenv "XDG_RUNTIME_DIR")
                                              "/tmp")))))))
    (unless sock
      (error "No sway IPC socket found"))
    (setenv "SWAYSOCK" sock)
    (apply #'call-process (or (getenv "SWAYMSG") "swaymsg")
           nil nil nil "-s" sock args)))

(defun slothbar-wayland--wait-for (predicate &optional seconds)
  "Wait up to SECONDS for PREDICATE while processing Wayland events."
  (let ((backend (and (boundp 'slothbar--backend) slothbar--backend)))
    (if (and backend
             (eq (slothbar-backend-kind backend) 'wayland))
        (slothbar-backend-wayland--wait-for backend predicate (or seconds 5))
      (let ((deadline (+ (float-time) (or seconds 5))))
        (while (and (not (funcall predicate)) (< (float-time) deadline))
          (sit-for 0.1))
        (funcall predicate)))))

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
         (bar (slothbar-create :display-backend backend
                               :pixel-width slothbar-width
                               :pixel-height slothbar-height))
         state
         (module (slothbar-module-create
                  :name "smoke" :format "hello" :lpad 2 :rpad 2)))
    (setq slothbar--backend backend
          slothbar--bar bar)
    (unwind-protect
        (progn
          (slothbar-backend-open backend)
          (setq state (slothbar-backend-create-bar backend bar))
          (slothbar-wayland-check
           "layer configure supplied a width"
           (> (slothbar-backend-wayland-surface-logical-width state) 0))
          (slothbar-wayland-check
           "surface and layer surface exist"
           (and (slothbar-backend-wayland-surface-surface state)
                (slothbar-backend-wayland-surface-layer-surface state)))
          (slothbar-wayland-check
           "layer configure was acknowledged"
           (slothbar-backend-wayland-surface-configured-serial state))
          (slothbar-wayland-check
           "Wayland has no system tray support"
           (not (slothbar-backend-supports-p backend 'system-tray)))
          (slothbar-wayland-check
           "Wayland has no ewmh support"
           (not (slothbar-backend-supports-p backend 'ewmh)))
          (slothbar-wayland-check
           "Wayland has no randr support"
           (not (slothbar-backend-supports-p backend 'randr)))
          (let ((pointer (slothbar-backend-wayland-pointer backend)))
            (slothbar-wayland-record
             (format "pointer=%S\n" pointer))
            (when pointer
              (slothbar-wayland-check
               "Wayland supports pointer input"
               (slothbar-backend-supports-p backend 'pointer-input))))
          (let ((clicks nil)
                (surface (slothbar-backend-wayland-surface-surface state)))
            (setf (slothbar-backend-wayland-pointer-surface backend) surface
                  (slothbar-backend-wayland-pointer-x backend) 12
                  (slothbar-backend-wayland-pointer-y backend) 7)
            (let ((function (lambda (_backend _bar _x _y _button)
                              (push (list _x _y _button) clicks))))
              (add-hook 'slothbar-backend-pointer-button-hook function)
              (unwind-protect
                  (progn
                    (slothbar-backend-wayland--pointer-button
                     backend '((state . pressed) (button . 272)))
                    (slothbar-wayland-check
                     "pointer button hook receives clicks" clicks)
                    (slothbar-wayland-check
                     "pointer click coordinates and button are preserved"
                     (equal (car clicks) '(12 7 :left))))
                (remove-hook 'slothbar-backend-pointer-button-hook function)
                (setf (slothbar-backend-wayland-pointer-surface backend) nil
                      (slothbar-backend-wayland-pointer-x backend) nil
                      (slothbar-backend-wayland-pointer-y backend) nil))))
          (slothbar-wayland-check
           "Wayland advertises focused-window support"
           (slothbar-backend-supports-p backend 'focused-window))
          (slothbar-wayland-check
           "foreign-toplevel manager is bound"
           (slothbar-backend-wayland-foreign-toplevel-manager backend))
          (let ((focused (slothbar-backend-focused-window backend)))
            (slothbar-wayland-record
             (format "focused-window=%S\n" focused)))
          (let ((manager (slothbar-backend-ext-workspace-manager backend)))
            (slothbar-wayland-check
             "Wayland advertises ext-workspace"
             (slothbar-backend-supports-p backend 'ext-workspace))
            (slothbar-wayland-check
             "ext-workspace manager is bound" manager)
            (slothbar-wayland-check
             "ext-workspace initial sync completed"
             (elwl-workspace-manager-done manager))
            (let ((groups (elwl-workspace-manager-groups manager)))
              (slothbar-wayland-record
               (format "ext-workspace groups=%d workspaces=%d\n"
                       (length groups)
                       (length (elwl-workspace-manager-workspaces manager))))
              (slothbar-wayland-check
               "ext-workspace lists a group" groups)
              (let* ((output (slothbar-output bar))
                     (group (cl-find-if
                             (lambda (group)
                               (and output
                                    (memq (elwl-output-object output)
                                          (elwl-workspace-group-outputs group))))
                             groups)))
                (slothbar-wayland-check
                 "workspace group includes the bar output"
                 group))
              (slothbar-wayland-check
               "ext-workspace lists a workspace"
               (> (length (elwl-workspace-manager-workspaces manager)) 0))))
          (require 'slothbar-workspaces)
          (let ((workspaces (slothbar-workspaces-create)))
            (slothbar-wayland-check
             "workspaces module supports ext-workspace"
             (slothbar-module-supported-p workspaces backend))
            (slothbar-module-init workspaces)
            (let ((list (funcall slothbar-workspaces-generate-list-fn)))
              (slothbar-wayland-record
               (format "ext-workspace list=%S\n" list))
              (slothbar-wayland-check
               "ext-workspace generate list is non-empty" list)
              (slothbar-wayland-check
               "active ext-workspace maps to :current"
               (equal (cadr (car list)) '(:current))))
            (let ((regions (slothbar-workspaces--click-regions workspaces)))
              (slothbar-wayland-record
               (format "workspace click regions=%S\n" regions))
              (slothbar-wayland-check
               "workspace click extents were found" regions)
              (let ((slothbar--last-layout
                     (list (list (list 0 0) workspaces))))
                (slothbar-wayland-check
                 "workspace click dispatches to the module"
                 (slothbar--click (nth 1 (car regions)) 5 :left))))
            (slothbar-module-exit workspaces))
          ;; X-only modules are filtered out during construction.
          (require 'slothbar-tray)
          (require 'slothbar-workspaces)
          (let ((slothbar-modules
                 (list #'slothbar-tray-create
                       #'slothbar-workspaces-create)))
            (slothbar--construct-modules)
            (slothbar-wayland-check
             "X-only modules are gated on Wayland"
             (equal (mapcar #'slothbar-module-name slothbar--modules)
                    '("workspaces"))))
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
          (let* ((scale (slothbar-backend-wayland-surface-scale state))
                 (width (slothbar-backend-wayland-surface-logical-width state))
                 (height (slothbar-backend-wayland-surface-logical-height state)))
            (slothbar-wayland-record
             (format "width=%s height=%s scale=%s output=%S\n"
                     width height scale
                     (and (slothbar-output bar)
                          (elwl-output-name
                           (slothbar-output bar)))))
            (slothbar-wayland-check "output scale is positive"
                                    (>= scale 1))
            (slothbar-wayland-check "two shm buffers"
                                    (= (length
                                        (slothbar-backend-wayland-surface-buffers
                                         state))
                                       2))
            (slothbar-wayland-check
             "frame dimensions follow scale"
             (and (= (slothbar-backend-wayland-surface-frame-width state)
                     (* width scale))
                  (= (slothbar-backend-wayland-surface-frame-height state)
                     (* height scale)))))
          (slothbar-module-init module)
          (slothbar-backend-present
           backend bar (list (list (list 0 0) module)) '((0 100)))
          (slothbar-wayland-check
           "module draw operations were recorded"
           (gethash module
                    (slothbar-backend-wayland-surface-module-ops state)))
          (slothbar-wayland-check
           "glyphs were rasterized"
           (> (hash-table-count
               (slothbar-backend-wayland-glyph-cache backend))
              0))
          (slothbar-wayland-check
           "frame was composed"
           (slothbar-backend-wayland-surface-frame state))
          (let* ((frame (slothbar-backend-wayland-surface-frame state))
                 (width (slothbar-backend-wayland-surface-frame-width state))
                 (height (slothbar-backend-wayland-surface-frame-height state))
                 (stride (slothbar-backend-wayland-surface-frame-stride state))
                 (background (list (aref frame 0)
                                   (aref frame 1)
                                   (aref frame 2)))
                 (found nil)
                 (min-y nil)
                 (max-y nil))
            (dotimes (y height)
              (dotimes (x width)
                (let ((offset (+ (* y stride) (* x 4)))
                      (different nil))
                  (dotimes (byte 3)
                    (unless (= (aref frame (+ offset byte))
                               (nth byte background))
                      (setq different t)))
                  (when different
                    (setq found t
                          min-y (if min-y (min min-y y) y)
                          max-y (if max-y (max max-y y) y))))))
            (slothbar-wayland-check
             "frame contains glyph pixels" found)
            (slothbar-wayland-check
             "glyphs are not clipped at the top"
             (and min-y (> min-y 0)))
            (slothbar-wayland-check
             "glyphs are not clipped at the bottom"
             (and max-y (< max-y (1- height)))))
          ;; Partial damage must leave pixels outside the damaged span alone.
          (let* ((module-b
                  (slothbar-module-create
                   :name "smoke-b" :format "world" :lpad 2 :rpad 2))
                 (layout (list (list (list 0 0) module)
                               (list (list 100 0) module-b))))
            (slothbar-module-init module-b)
            (slothbar-backend-present backend bar layout nil)
            (let* ((before (copy-sequence
                            (slothbar-backend-wayland-surface-frame state)))
                   (scale (slothbar-backend-wayland-surface-scale state))
                   (stride (slothbar-backend-wayland-surface-frame-stride state))
                   (height (slothbar-backend-wayland-surface-frame-height state))
                   (cut (* 100 scale 4)))
              (aset module (cl-struct-slot-offset 'slothbar-module 'format) "i")
              (slothbar-module-layout module)
              (slothbar-backend-draw-module backend module)
              (slothbar-backend-present backend bar layout '((0 100)))
              (let ((after (slothbar-backend-wayland-surface-frame state)))
                (slothbar-wayland-check
                 "partial damage leaves untouched pixels unchanged"
                 (cl-loop for y below height
                          for start = (+ (* y stride) cut)
                          always
                          (equal (substring before start
                                             (+ start (- stride cut)))
                                 (substring after start
                                            (+ start (- stride cut))))))
                (slothbar-wayland-check
                 "partial damage changes the damaged region"
                 (cl-loop for y below height
                          for start = (* y stride)
                          thereis
                          (not (equal (substring before start (+ start cut))
                                      (substring after start (+ start cut))))))))
            (slothbar-module-exit module-b))
          ;; Two independent bars must keep independent damage state.
          (let* ((bar-b (slothbar-create
                         :display-backend backend
                         :pixel-width slothbar-width
                         :pixel-height slothbar-height))
                 (module-a
                  (slothbar-module-create
                   :name "two-bar-a" :format "alpha" :lpad 2 :rpad 2))
                 (module-b
                  (slothbar-module-create
                   :name "two-bar-b" :format "bravo" :lpad 2 :rpad 2)))
            (setf (slothbar-module-backend module-a) backend
                  (slothbar-module-bar module-a) bar
                  (slothbar-module-backend module-b) backend
                  (slothbar-module-bar module-b) bar-b)
            (let ((state-b (slothbar-backend-create-bar backend bar-b)))
              (slothbar-module-init module-a)
              (slothbar-module-init module-b)
              (slothbar-backend-present
               backend bar (list (list (list 0 0) module-a)) nil)
              (slothbar-backend-present
               backend bar-b (list (list (list 0 0) module-b)) nil)
              (let ((before-a
                     (copy-sequence
                      (slothbar-backend-wayland-surface-frame state)))
                    (before-b
                     (copy-sequence
                      (slothbar-backend-wayland-surface-frame state-b))))
                (aset module-a
                      (cl-struct-slot-offset 'slothbar-module 'format)
                      "beta")
                (slothbar-module-layout module-a)
                (slothbar-backend-draw-module backend module-a)
                (slothbar-backend-present
                 backend bar (list (list (list 0 0) module-a)) '((0 10)))
                (slothbar-wayland-check
                 "two-bar damage changes the receiving bar"
                 (not (equal
                       before-a
                       (slothbar-backend-wayland-surface-frame state))))
                (slothbar-wayland-check
                 "two-bar damage leaves the other bar unchanged"
                 (equal
                  before-b
                  (slothbar-backend-wayland-surface-frame state-b))))
              ;; Pointer events map the receiving surface back to its bar.
              (let ((button-events nil)
                    (motion-events nil))
                (let ((button-hook
                       (lambda (_backend bar x y button)
                         (push (list bar x y button) button-events)))
                      (motion-hook
                       (lambda (_backend bar x y)
                         (push (list bar x y) motion-events))))
                  (add-hook 'slothbar-backend-pointer-button-hook button-hook)
                  (add-hook 'slothbar-backend-pointer-motion-hook motion-hook)
                  (unwind-protect
                      (progn
                        (setf (slothbar-backend-wayland-pointer-surface backend)
                              (slothbar-backend-wayland-surface-surface state)
                              (slothbar-backend-wayland-pointer-x backend) 11
                              (slothbar-backend-wayland-pointer-y backend) 7)
                        (slothbar-backend-wayland--pointer-button
                         backend '((state . pressed) (button . 272)))
                        (setf (slothbar-backend-wayland-pointer-surface backend)
                              (slothbar-backend-wayland-surface-surface state-b)
                              (slothbar-backend-wayland-pointer-x backend) 21
                              (slothbar-backend-wayland-pointer-y backend) 9)
                        (slothbar-backend-wayland--pointer-button
                         backend '((state . pressed) (button . 273)))
                        (setf (slothbar-backend-wayland-pointer-surface backend)
                              (slothbar-backend-wayland-surface-surface state-b)
                              (slothbar-backend-wayland-pointer-x backend) 23
                              (slothbar-backend-wayland-pointer-y backend) 10)
                        (slothbar-backend-wayland--pointer-motion backend 23 10)
                        (setf (slothbar-backend-wayland-pointer-surface backend)
                              (slothbar-backend-wayland-surface-surface state)
                              (slothbar-backend-wayland-pointer-x backend) 13
                              (slothbar-backend-wayland-pointer-y backend) 8)
                        (slothbar-backend-wayland--pointer-motion backend 13 8)
                        (slothbar-wayland-check
                         "multi-bar pointer buttons route to the receiving bar"
                         (equal (list (nth 1 button-events)
                                      (nth 0 button-events))
                                (list (list bar 11 7 :left)
                                      (list bar-b 21 9 :right))))
                        (slothbar-wayland-check
                         "multi-bar pointer motion routes to the receiving bar"
                         (equal motion-events
                                (list (list bar 13 8)
                                      (list bar-b 23 10)))))
                    (remove-hook 'slothbar-backend-pointer-button-hook
                                 button-hook)
                    (remove-hook 'slothbar-backend-pointer-motion-hook
                                 motion-hook)
                    (setf (slothbar-backend-wayland-pointer-surface backend) nil
                          (slothbar-backend-wayland-pointer-x backend) nil
                          (slothbar-backend-wayland-pointer-y backend) nil))))
              (slothbar-module-exit module-a)
              (slothbar-module-exit module-b)
              (slothbar-backend-destroy-bar backend bar-b)))
          (slothbar-backend-show backend bar)
          (slothbar-wayland-check
           "presented buffer was submitted in-flight"
           (cl-some #'elwl-shm-buffer-in-flight-p
                    (slothbar-backend-wayland-surface-buffers state)))
          (elwl-dispatch (slothbar-backend-wayland-connection backend) 0.1)
          (slothbar-wayland-check
           "frame was presented"
           (slothbar-backend-wayland-surface-presented state))
          (slothbar-backend-hide backend bar)
          (slothbar-module-exit module)
          (slothbar-backend-destroy-bar backend bar)
          (slothbar-backend-close backend)
          (setq slothbar--backend nil
                slothbar--bar nil))
      (ignore-errors (slothbar-backend-close backend))
      (setq slothbar--backend nil
            slothbar--bar nil)))

  ;; Wayland output hotplug: one bar per output via :each.
  (setq slothbar-bars
        (list (list :id "each"
                    :output :each
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "hotplug" :format "hotplug"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (slothbar-wayland-check
   "hotplug: one bar per connected output"
   (= 2 (length slothbar--bars)))
  (slothbar-wayland-check
   "hotplug: both headless outputs are known"
   (equal (sort (copy-sequence
                 (slothbar-backend-output-names slothbar--backend))
                #'string<)
          '("HEADLESS-1" "HEADLESS-2")))
  (slothbar-wayland--swaymsg "output" "HEADLESS-2" "disable")
  (slothbar-wayland-check
   "disabling a Wayland output removes its bar"
   (slothbar-wayland--wait-for
    (lambda () (= 1 (length slothbar--bars))) 5))
  (slothbar-wayland--swaymsg "output" "HEADLESS-2" "enable")
  (slothbar-wayland-check
   "re-enabling a Wayland output restores its bar"
   (slothbar-wayland--wait-for
    (lambda () (= 2 (length slothbar--bars))) 5))
  (slothbar-exit)
  (slothbar-wayland-check
   "hotplug: exit cleared the bars"
   (null slothbar--bars))
  (setq slothbar-bars nil)

  ;; Two `:each' entries must produce two bars on every output.
  (setq slothbar-bars
        (list (list :id "each-a"
                    :output :each
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "each-a" :format "a"
                             :lpad 2 :rpad 2))))
              (list :id "each-b"
                    :output :each
                    :width 300
                    :height 24
                    :offset-x 0
                    :offset-y 24
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "each-b" :format "b"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (let ((bar-outputs
         (lambda ()
           (sort (mapcar (lambda (bar)
                           (slothbar-backend-output-name
                            slothbar--backend (slothbar-output bar)))
                         slothbar--bars)
                 #'string<))))
    (slothbar-wayland-check
     "two :each entries make two bars per output"
     (and (= 4 (length slothbar--bars))
          (equal (funcall bar-outputs)
                 '("HEADLESS-1" "HEADLESS-1" "HEADLESS-2" "HEADLESS-2"))))
    (slothbar-wayland--swaymsg "output" "HEADLESS-2" "disable")
    (slothbar-wayland-check
     "disabling one output removes both of its :each bars"
     (slothbar-wayland--wait-for
      (lambda ()
        (and (= 2 (length slothbar--bars))
             (equal (funcall bar-outputs)
                    '("HEADLESS-1" "HEADLESS-1"))))
      5))
    (slothbar-wayland--swaymsg "output" "HEADLESS-2" "enable")
    (slothbar-wayland-check
     "re-enabling one output restores both :each bars"
     (slothbar-wayland--wait-for
      (lambda ()
        (and (= 4 (length slothbar--bars))
             (equal (funcall bar-outputs)
                    '("HEADLESS-1" "HEADLESS-1" "HEADLESS-2" "HEADLESS-2"))))
      5)))
  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; Geometry refresh must resize the Wayland surface and frame.
  (setq slothbar-bars
        (list (list :id "geometry"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "geometry" :format "geometry"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (let* ((bar (car slothbar--bars))
         (state (slothbar-surface bar))
         (scale (slothbar-backend-wayland-surface-scale state)))
    (slothbar-wayland-check
     "geometry refresh starts at the configured height"
     (= 20 (slothbar-backend-wayland-surface-logical-height state)))
    (let ((config (copy-sequence (slothbar-config bar))))
      (setf (slothbar-config bar)
            (plist-put config :height 30)))
    (slothbar-refresh)
    (slothbar-wayland-check
     "geometry refresh updates the bar height"
     (slothbar-wayland--wait-for
      (lambda () (= 30 (slothbar-pixel-height bar))) 3))
    (slothbar-wayland-check
     "geometry refresh resizes the Wayland surface"
     (slothbar-wayland--wait-for
      (lambda ()
        (= 30 (slothbar-backend-wayland-surface-logical-height state)))
      3))
    (slothbar-wayland-check
     "geometry refresh rebuilds the frame at the new scale"
     (= (* 30 scale)
        (slothbar-backend-wayland-surface-frame-height state)))
    ;; Changing the output scale must update the surface and frame.
    (slothbar-wayland--swaymsg "output" "HEADLESS-1" "scale" "3")
    (slothbar-wayland-check
     "output scale change updates the surface scale"
     (slothbar-wayland--wait-for
      (lambda ()
        (= 3 (slothbar-backend-wayland-surface-scale state)))
      5))
    (slothbar-wayland-check
     "output scale change rebuilds the frame dimensions"
     (and (= (* 3 (slothbar-backend-wayland-surface-logical-width state))
             (slothbar-backend-wayland-surface-frame-width state))
          (= (* 3 (slothbar-backend-wayland-surface-logical-height state))
             (slothbar-backend-wayland-surface-frame-height state))))
    (slothbar-wayland--swaymsg "output" "HEADLESS-1" "scale" "2")
    (slothbar-wayland-check
     "output scale restores the surface scale"
     (slothbar-wayland--wait-for
      (lambda ()
        (= 2 (slothbar-backend-wayland-surface-scale state)))
      5))
    (sit-for 0.5))
  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; A scale change must fan out to every bar on that output.
  (setq slothbar-bars
        (list (list :id "scale-a"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "scale-a" :format "scale-a"
                             :lpad 2 :rpad 2))))
              (list :id "scale-b"
                    :output "HEADLESS-1"
                    :width 300
                    :height 24
                    :offset-x 0
                    :offset-y 24
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "scale-b" :format "scale-b"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (let* ((bar-a (car slothbar--bars))
         (bar-b (cadr slothbar--bars))
         (state-a (slothbar-surface bar-a))
         (state-b (slothbar-surface bar-b)))
    (slothbar-wayland-check
     "two-bar scale test starts at scale 2"
     (and (= 2 (slothbar-backend-wayland-surface-scale state-a))
          (= 2 (slothbar-backend-wayland-surface-scale state-b))))
    (slothbar-wayland--swaymsg "output" "HEADLESS-1" "scale" "3")
    (slothbar-wayland-check
     "output scale change updates both bars"
     (slothbar-wayland--wait-for
      (lambda ()
        (let ((a (car slothbar--bars))
              (b (cadr slothbar--bars)))
          (and a b
               (= 3 (slothbar-backend-wayland-surface-scale
                     (slothbar-surface a)))
               (= 3 (slothbar-backend-wayland-surface-scale
                     (slothbar-surface b))))))
      5))
    (slothbar-wayland-check
     "output scale change rebuilds both frames"
     (and (= (* 3 (slothbar-backend-wayland-surface-logical-width state-a))
             (slothbar-backend-wayland-surface-frame-width state-a))
          (= (* 3 (slothbar-backend-wayland-surface-logical-height state-a))
             (slothbar-backend-wayland-surface-frame-height state-a))
          (= (* 3 (slothbar-backend-wayland-surface-logical-width state-b))
             (slothbar-backend-wayland-surface-frame-width state-b))
          (= (* 3 (slothbar-backend-wayland-surface-logical-height state-b))
             (slothbar-backend-wayland-surface-frame-height state-b))))
    (slothbar-wayland--swaymsg "output" "HEADLESS-1" "scale" "2")
    (slothbar-wayland-check
     "output scale restores both bars"
     (slothbar-wayland--wait-for
      (lambda ()
        (let ((a (car slothbar--bars))
              (b (cadr slothbar--bars)))
          (and a b
               (= 2 (slothbar-backend-wayland-surface-scale
                     (slothbar-surface a)))
               (= 2 (slothbar-backend-wayland-surface-scale
                     (slothbar-surface b))))))
      5))
    (sit-for 0.5))
  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; Runtime reconfiguration must update one bar in place, not its neighbor.
  (setq slothbar-bars
        (list (list :id "runtime-a"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "runtime-a-old" :format "a"
                             :lpad 2 :rpad 2))))
              (list :id "runtime-b"
                    :output "HEADLESS-1"
                    :width 300
                    :height 24
                    :offset-x 0
                    :offset-y 24
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "runtime-b" :format "b"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (let* ((bar-a (car slothbar--bars))
         (bar-b (cadr slothbar--bars))
         (surface-a (slothbar-surface bar-a))
         (surface-b (slothbar-surface bar-b))
         (configs slothbar-bars)
         (new-a (copy-sequence (car configs))))
    (setq slothbar-bars
          (list (plist-put
                 (plist-put new-a :height 30)
                 :modules
                 (list (lambda ()
                         (slothbar-module-create
                          :name "runtime-a-new" :format "aa"
                          :lpad 2 :rpad 2))))
                (cadr configs)))
    (slothbar-wayland-check
     "runtime reconfiguration applies through the bar watcher"
     (slothbar-wayland--wait-for
      (lambda ()
        (and (= 30 (slothbar-pixel-height bar-a))
             (equal (mapcar #'slothbar-module-name
                            (slothbar-module-list bar-a))
                    '("runtime-a-new"))))
      5))
    (slothbar-wayland-check
     "runtime reconfiguration keeps both bar identities"
     (and (= 2 (length slothbar--bars))
          (eq bar-a (car slothbar--bars))
          (eq bar-b (cadr slothbar--bars))))
    (slothbar-wayland-check
     "runtime reconfiguration recreates only the changed surface"
     (and (not (eq surface-a (slothbar-surface bar-a)))
          (eq surface-b (slothbar-surface bar-b))))
    (slothbar-wayland-check
     "runtime reconfiguration updates the changed bar height"
     (= 30 (slothbar-pixel-height bar-a)))
    (slothbar-wayland-check
     "runtime reconfiguration leaves the other bar height alone"
     (= 24 (slothbar-pixel-height bar-b)))
    (slothbar-wayland-check
     "runtime reconfiguration replaces the changed bar's modules"
     (equal (mapcar #'slothbar-module-name
                    (slothbar-module-list bar-a))
            '("runtime-a-new")))
    (slothbar-wayland-check
     "runtime reconfiguration leaves the other bar's modules alone"
     (equal (mapcar #'slothbar-module-name
                    (slothbar-module-list bar-b))
            '("runtime-b")))
    (slothbar-wayland-check
     "runtime reconfiguration rebuilds the changed frame height"
     (= (* 30 (slothbar-backend-wayland-surface-scale
               (slothbar-surface bar-a)))
        (slothbar-backend-wayland-surface-frame-height
         (slothbar-surface bar-a)))))

  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; Live theme and font-scale fan-out to two bars.
  (setq slothbar-bars
        (list (list :id "theme-a"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "theme-a" :format "theme-a"
                             :lpad 2 :rpad 2))))
              (list :id "theme-b"
                    :output "HEADLESS-1"
                    :width 300
                    :height 24
                    :offset-x 0
                    :offset-y 24
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "theme-b" :format "theme-b"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (let* ((bar-a (car slothbar--bars))
         (bar-b (cadr slothbar--bars))
         (module-a (car (slothbar-module-list bar-a)))
         (module-b (car (slothbar-module-list bar-b)))
         (state-a (slothbar-surface bar-a))
         (state-b (slothbar-surface bar-b))
         (frame-a (copy-sequence (slothbar-backend-wayland-surface-frame state-a)))
         (frame-b (copy-sequence (slothbar-backend-wayland-surface-frame state-b)))
         (font-a (copy-sequence (slothbar-font-px-sizes bar-a)))
         (font-b (copy-sequence (slothbar-font-px-sizes bar-b)))
         (old-scale slothbar-font-px-scale)
         (new-bg (slothbar-color-from-plist
                  '(:red #x1111 :green #x2222 :blue #x3333 :alpha #xffff)))
         (new-fg (slothbar-color-from-plist
                  '(:red #xdddd :green #xeeee :blue #xffff :alpha #xffff))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'slothbar--background-color)
                     (lambda () new-bg))
                    ((symbol-function 'slothbar--foreground-color)
                     (lambda () new-fg)))
            (slothbar--on-theme-change nil)
            (slothbar-wayland-check
             "theme change updates both bars' module colors"
             (and (eq (slothbar-module-rgb-background-color
                       (slothbar-module-colors module-a))
                      new-bg)
                  (eq (slothbar-module-rgb-background-color
                       (slothbar-module-colors module-b))
                      new-bg)))
            (slothbar-wayland-check
             "theme change redraws both bars"
             (slothbar-wayland--wait-for
              (lambda ()
                (and (not (equal frame-a
                                 (slothbar-backend-wayland-surface-frame state-a)))
                     (not (equal frame-b
                                 (slothbar-backend-wayland-surface-frame state-b)))))
              3)))
          (let ((theme-frame-a
                 (copy-sequence (slothbar-backend-wayland-surface-frame state-a)))
                (theme-frame-b
                 (copy-sequence (slothbar-backend-wayland-surface-frame state-b))))
            (setq slothbar-font-px-scale 0.75)
            (slothbar-wayland-check
             "font scale changes both bars' computed sizes"
             (slothbar-wayland--wait-for
              (lambda ()
                (and (not (equal font-a (slothbar-font-px-sizes bar-a)))
                     (not (equal font-b (slothbar-font-px-sizes bar-b)))))
              3))
            (slothbar-wayland-check
             "font scale redraws both bars"
             (slothbar-wayland--wait-for
              (lambda ()
                (and (not (equal theme-frame-a
                                 (slothbar-backend-wayland-surface-frame state-a)))
                     (not (equal theme-frame-b
                                 (slothbar-backend-wayland-surface-frame state-b)))))
              3))))
      (slothbar-exit)
      (setq slothbar-bars nil)
      (setq slothbar-font-px-scale old-scale)
      (sit-for 0.1)))

  ;; Full core lifecycle through slothbar--start and slothbar-exit.
  (setq slothbar-bars
        (list (list :id "one"
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "live-one" :format "one"
                             :lpad 2 :rpad 2))))
              (list :id "two"
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "live-two" :format "two"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (slothbar-wayland-check
   "core start created two bars"
   (= 2 (length slothbar--bars)))
  (slothbar-wayland-check
   "every bar has a Wayland surface"
   (cl-every (lambda (bar) (slothbar-surface bar)) slothbar--bars))
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
  (slothbar-wayland-check "core exit cleared the bars"
                          (null slothbar--bars))
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
