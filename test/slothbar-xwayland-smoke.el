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
;;   SLOTHBAR_EXPECT_NO_ELWL  when "1", assert that elwl is unavailable

;;; Code:

(require 'cl-lib)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(when (equal (getenv "SLOTHBAR_EXPECT_NO_ELWL") "1")
  (setq load-path
        (cl-remove-if
         (lambda (dir)
           (or (string-match-p "elwl" dir)
               (file-exists-p (expand-file-name "elwl.el" dir))))
         load-path)))

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

(defun slothbar-smoke--swaymsg (&rest args)
  "Run swaymsg with ARGS against the harness sway instance."
  (let ((sock (or (getenv "SWAYSOCK")
                  (car (file-expand-wildcards
                        (expand-file-name "sway-ipc.*.sock"
                                          (or (getenv "XDG_RUNTIME_DIR")
                                              "/tmp")))))))
    (unless sock
      (error "No sway IPC socket found"))
    (setenv "SWAYSOCK" sock)
    (apply #'call-process "swaymsg" nil nil nil "-s" sock args)))

(defun slothbar-smoke--wait-for (predicate &optional seconds)
  "Wait up to SECONDS for PREDICATE, processing X events."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (sit-for 0.1))
    (funcall predicate)))

(defun slothbar-smoke--drawable-data (drawable width height)
  "Return DRAWABLE's pixels as a vector of WIDTH by HEIGHT."
  (let ((reply (xcb:+request-unchecked+reply
                   (slothbar-xcb-connection)
                 (make-instance 'xcb:GetImage
                                :format xcb:ImageFormat:ZPixmap
                                :drawable drawable
                                :x 0 :y 0
                                :width width :height height
                                :plane-mask #xffffffff))))
    (vconcat (slot-value reply 'data))))

(defun slothbar-smoke--set-bar-config (bar config)
  "Set BAR's config to CONFIG without relying on a preloaded setf."
  (aset bar (cl-struct-slot-offset 'slothbar 'config) config))

(defun slothbar-smoke--window-geometry (window)
  "Return WINDOW's X/Y/WIDTH/HEIGHT geometry."
  (let ((reply (xcb:+request-unchecked+reply
                   (slothbar-xcb-connection)
                 (make-instance 'xcb:GetGeometry :drawable window))))
    (list (slot-value reply 'x)
          (slot-value reply 'y)
          (slot-value reply 'width)
          (slot-value reply 'height))))

(defun slothbar-smoke--module-pixmap-data (_backend module)
  "Return MODULE's live X pixmap bytes."
  (let* ((table (slothbar-backend-xcb--module-surface-table module))
         (surface (and table (gethash module table)))
         (pixmap (slothbar-backend-xcb--module-surface-pixmap surface))
         (width (max 1 (slothbar-module-width module)))
         (height (slothbar-pixel-height (slothbar-module--bar module))))
    (slothbar-smoke--drawable-data pixmap width height)))

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
  (when (equal (getenv "SLOTHBAR_EXPECT_NO_ELWL") "1")
    (slothbar-smoke--check
     "elwl is unavailable for this X run"
     (and (not (featurep 'elwl))
          (not (locate-library "elwl")))))
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
         (bar (slothbar-create :display-backend backend
                               :pixel-width slothbar-width
                               :pixel-height slothbar-height))
         (module (slothbar-module-create
                  :name "smoke" :format "hello" :lpad 2 :rpad 2)))
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
          (slothbar-smoke--check "created a background pixmap"
                                 (slothbar-backend-xcb-pixmap backend))
          (let ((names (slothbar-backend-output-names backend)))
            (slothbar-smoke--record (format "X outputs: %S\n" names))
            (slothbar-smoke--check
             "Xwayland exposes multiple outputs"
             (>= (length names) 2))
            (slothbar-smoke--check
             "headless output names are visible"
             (and (member "HEADLESS-1" names)
                  (member "HEADLESS-2" names))))
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
          (slothbar-smoke--check "xcb supports focused-window"
                                 (slothbar-backend-supports-p
                                  backend 'focused-window))
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
          ;; Partial damage must preserve pixels outside the damaged span.
          (let* ((module-a
                  (slothbar-module-create
                   :name "damage-a" :format "hello" :lpad 2 :rpad 2)))
            (slothbar-module-init module-a)
            (let* ((layout (list (list (list 0 0) module-a)))
                   (window (slothbar-xcb-window)))
              (slothbar-backend-show backend bar)
              (slothbar-backend-present backend bar layout nil)
              (let* ((before (slothbar-smoke--drawable-data
                              window slothbar-width slothbar-height))
                     (bytes-per-pixel
                      (/ (length before)
                         (* slothbar-width slothbar-height)))
                     (cut (* 100 bytes-per-pixel)))
                (aset module-a
                      (cl-struct-slot-offset 'slothbar-module 'format)
                      "i")
                (slothbar-module-layout module-a)
                (slothbar-backend-draw-module backend module-a)
                (slothbar-backend-present backend bar layout '((0 100)))
                (let ((after (slothbar-smoke--drawable-data
                              window slothbar-width slothbar-height)))
                  (slothbar-smoke--check
                   "X partial damage leaves untouched pixels unchanged"
                   (cl-loop for y below slothbar-height
                            for start = (+ (* y slothbar-width
                                               bytes-per-pixel)
                                           cut)
                            always
                            (equal
                             (cl-subseq before start
                                        (+ start (- (* slothbar-width
                                                       bytes-per-pixel)
                                                    cut)))
                             (cl-subseq after start
                                        (+ start (- (* slothbar-width
                                                       bytes-per-pixel)
                                                    cut))))))
                  (slothbar-smoke--check
                   "X partial damage changes the damaged region"
                   (cl-loop for y below slothbar-height
                            for start = (* y slothbar-width
                                           bytes-per-pixel)
                            thereis
                            (not
                             (equal
                              (cl-subseq before start (+ start cut))
                              (cl-subseq after start (+ start cut)))))))))
            (slothbar-module-exit module-a))
          ;; Two independent X windows must keep independent damage.
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
            (aset module-a
                  (cl-struct-slot-offset 'slothbar-module 'backend) backend)
            (aset module-a
                  (cl-struct-slot-offset 'slothbar-module 'bar) bar)
            (aset module-b
                  (cl-struct-slot-offset 'slothbar-module 'backend) backend)
            (aset module-b
                  (cl-struct-slot-offset 'slothbar-module 'bar) bar-b)
            (slothbar--with-bar bar-b
              (slothbar-backend-create-bar backend bar-b))
            (slothbar--with-bar bar
              (slothbar-module-init module-a))
            (slothbar--with-bar bar-b
              (slothbar-module-init module-b))
            (let* ((layout-a (list (list (list 0 0) module-a)))
                   (layout-b (list (list (list 0 0) module-b)))
                   (window-a
                    (slothbar-backend-xcb-surface-window
                     (slothbar-surface bar)))
                   (window-b
                    (slothbar-backend-xcb-surface-window
                     (slothbar-surface bar-b))))
              (slothbar--with-bar bar
                (slothbar-backend-show backend bar)
                (slothbar-backend-present backend bar layout-a nil))
              (slothbar--with-bar bar-b
                (slothbar-backend-show backend bar-b)
                (slothbar-backend-present backend bar-b layout-b nil))
              (let ((before-a
                     (slothbar-smoke--drawable-data
                      window-a slothbar-width slothbar-height))
                    (before-b
                     (slothbar-smoke--drawable-data
                      window-b slothbar-width slothbar-height)))
                (slothbar--with-bar bar
                  (aset module-a
                        (cl-struct-slot-offset 'slothbar-module 'format)
                        "i")
                  (slothbar-module-layout module-a)
                  (slothbar-backend-draw-module backend module-a)
                  (slothbar-backend-present
                   backend bar layout-a '((0 100))))
                (let ((after-a
                       (slothbar-smoke--drawable-data
                        window-a slothbar-width slothbar-height))
                      (after-b
                       (slothbar-smoke--drawable-data
                        window-b slothbar-width slothbar-height)))
                  (slothbar-smoke--check
                   "X two-bar damage changes the receiving bar"
                   (not (equal before-a after-a)))
                  (slothbar-smoke--check
                   "X two-bar damage leaves the other window unchanged"
                   (equal before-b after-b)))))
            ;; Synthetic X pointer events must map each window to its bar.
            (let ((button-events nil)
                  (motion-events nil)
                  (window-a
                   (slothbar-backend-xcb-surface-window
                    (slothbar-surface bar)))
                  (window-b
                   (slothbar-backend-xcb-surface-window
                    (slothbar-surface bar-b)))
                  (connection (slothbar-backend-xcb-connection backend))
                  (root (slothbar-backend-xcb--root-window-id backend)))
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
                      (slothbar-backend-xcb--on-ButtonPress
                       backend
                       (xcb:marshal
                        (make-instance 'xcb:ButtonPress
                                       :detail 1 :time 0 :root root
                                       :event window-a :child 0
                                       :root-x 12 :root-y 5
                                       :event-x 12 :event-y 5
                                       :state 0 :same-screen 1)
                        connection)
                       nil)
                      (slothbar-backend-xcb--on-ButtonPress
                       backend
                       (xcb:marshal
                        (make-instance 'xcb:ButtonPress
                                       :detail 3 :time 0 :root root
                                       :event window-b :child 0
                                       :root-x 21 :root-y 9
                                       :event-x 21 :event-y 9
                                       :state 0 :same-screen 1)
                        connection)
                       nil)
                      (slothbar-backend-xcb--on-MotionNotify
                       backend
                       (xcb:marshal
                        (make-instance 'xcb:MotionNotify
                                       :detail 0 :time 0 :root root
                                       :event window-b :child 0
                                       :root-x 23 :root-y 10
                                       :event-x 23 :event-y 10
                                       :state 0 :same-screen 1)
                        connection)
                       nil)
                      (slothbar-backend-xcb--on-MotionNotify
                       backend
                       (xcb:marshal
                        (make-instance 'xcb:MotionNotify
                                       :detail 0 :time 0 :root root
                                       :event window-a :child 0
                                       :root-x 13 :root-y 8
                                       :event-x 13 :event-y 8
                                       :state 0 :same-screen 1)
                        connection)
                       nil)
                      (slothbar-smoke--check
                       "X multi-bar pointer buttons route to the receiving bar"
                       (equal (list (nth 1 button-events)
                                    (nth 0 button-events))
                              (list (list bar 12 5 :left)
                                    (list bar-b 21 9 :right))))
                      (slothbar-smoke--check
                       "X multi-bar pointer motion routes to the receiving bar"
                       (equal motion-events
                              (list (list bar 13 8)
                                    (list bar-b 23 10)))))
                  (remove-hook 'slothbar-backend-pointer-button-hook
                               button-hook)
                  (remove-hook 'slothbar-backend-pointer-motion-hook
                               motion-hook))))
            (slothbar-module-exit module-a)
            (slothbar-module-exit module-b)
            (slothbar--with-bar bar-b
              (slothbar-backend-destroy-bar backend bar-b)))
          (slothbar-backend-present
           backend bar
           (list (list (list 0 0) module))
           '((0 100)))
          (slothbar-backend-show backend bar)
          (slothbar-backend-hide backend bar)
          (slothbar-module-exit module)
          (slothbar-smoke--check
           "module exit removed its surface"
           (null (gethash module
                          (slothbar-backend-xcb-module-surfaces backend)))))
      (ignore-errors (slothbar-backend-destroy-bar backend bar))
      (slothbar-backend-close backend)
      (setq slothbar--backend nil
            slothbar--bar nil))
    (slothbar-smoke--check "closed the connection"
                           (null (slothbar-backend-xcb-connection backend))))

  ;; Multi-output core lifecycle: one bar per named X output.
  (setq slothbar-bars
        (list (list :id 'left
                    :output "HEADLESS-1"
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "left" :format "left" :lpad 2 :rpad 2))))
              (list :id 'right
                    :output "HEADLESS-2"
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "right" :format "right" :lpad 2 :rpad 2))))))
  (slothbar--start)
  (slothbar-smoke--check "core start created one bar per output"
                         (= 2 (length slothbar--bars)))
  (slothbar-smoke--check "each X bar has a window surface"
                         (cl-every (lambda (bar) (slothbar-surface bar))
                                   slothbar--bars))
  (let ((by-output
         (cl-loop for bar in slothbar--bars
                  collect (cons (slothbar-output bar)
                                (slothbar-x-offset bar)))))
    (slothbar-smoke--record (format "bar offsets: %S\n" by-output))
    (slothbar-smoke--check
     "bars use distinct monitor offsets"
     (= 2 (length (delete-dups (mapcar #'cdr by-output))))))
  ;; Physical output removal/re-add through sway's RandR backing.
  (require 'slothbar-randr)
  (slothbar-randr-mode 1)
  (slothbar-smoke--check "randr mode enabled for hotplug test"
                         slothbar-randr-mode)
  (slothbar-smoke--swaymsg "output" "HEADLESS-2" "disable")
  (slothbar-smoke--check
   "disabling an output removes its bar"
   (slothbar-smoke--wait-for
    (lambda () (= 1 (length slothbar--bars)))))
  (slothbar-smoke--swaymsg "output" "HEADLESS-2" "enable")
  (slothbar-smoke--check
   "re-enabling an output restores its bar"
   (slothbar-smoke--wait-for
    (lambda () (= 2 (length slothbar--bars)))))
  (slothbar-randr-mode -1)
  (setq slothbar-bars (list (car slothbar-bars)))
  (slothbar--reconcile-bars)
  (slothbar-smoke--check "reconciliation drops removed bar definitions"
                         (= 1 (length slothbar--bars)))
  (slothbar-exit)
  (slothbar-smoke--check "multi-output exit cleared the backend"
                         (null slothbar--backend))
  (setq slothbar-bars nil)

  ;; Runtime reconfiguration must update one X bar in place, not its neighbor.
  (setq slothbar-bars
        (list (list :id "runtime-a"
                    :output "HEADLESS-1"
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
    (slothbar-smoke--check
     "X runtime reconfiguration applies through the bar watcher"
     (slothbar-smoke--wait-for
      (lambda ()
        (and (= 30 (slothbar-pixel-height bar-a))
             (equal (mapcar #'slothbar-module-name
                            (slothbar-module-list bar-a))
                    '("runtime-a-new"))))
      5))
    (slothbar-smoke--check
     "X runtime reconfiguration keeps both bar identities"
     (and (= 2 (length slothbar--bars))
          (eq bar-a (car slothbar--bars))
          (eq bar-b (cadr slothbar--bars))))
    (slothbar-smoke--check
     "X runtime reconfiguration recreates only the changed surface"
     (and (not (eq surface-a (slothbar-surface bar-a)))
          (eq surface-b (slothbar-surface bar-b))))
    (slothbar-smoke--check
     "X runtime reconfiguration updates the changed bar height"
     (= 30 (slothbar-pixel-height bar-a)))
    (slothbar-smoke--check
     "X runtime reconfiguration leaves the other bar height alone"
     (= 24 (slothbar-pixel-height bar-b)))
    (slothbar-smoke--check
     "X runtime reconfiguration replaces the changed bar modules"
     (equal (mapcar #'slothbar-module-name
                    (slothbar-module-list bar-a))
            '("runtime-a-new")))
    (slothbar-smoke--check
     "X runtime reconfiguration leaves the other bar modules alone"
     (equal (mapcar #'slothbar-module-name
                    (slothbar-module-list bar-b))
            '("runtime-b"))))
  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; Geometry refresh must resize the live X window.
  (setq slothbar-bars
        (list (list :id "geometry"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 10
                    :offset-y 10
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "geometry" :format "geometry"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (let* ((bar (car slothbar--bars))
         (window (slothbar-backend-xcb-surface-window
                  (slothbar-surface bar))))
    (slothbar-smoke--check
     "X geometry refresh starts at the configured geometry"
     (equal (slothbar-smoke--window-geometry window) '(10 10 300 20)))
    (let ((config (copy-sequence (slothbar-config bar))))
      (aset bar (cl-struct-slot-offset 'slothbar 'config)
            (plist-put (plist-put config :width 320) :height 30)))
    (slothbar-refresh)
    (slothbar-refresh-modules)
    (slothbar-smoke--check
     "X geometry refresh updates the bar model"
     (slothbar-smoke--wait-for
      (lambda ()
        (and (= 320 (slothbar-pixel-width bar))
             (= 30 (slothbar-pixel-height bar))))
      3))
    (slothbar-smoke--check
     "X geometry refresh preserves configured offsets"
     (and (= 10 (slothbar-x-offset bar))
          (= 10 (slothbar-y-offset bar))))
    (slothbar-smoke--check
     "X geometry refresh resizes the X window"
     (slothbar-smoke--wait-for
      (lambda ()
        (let ((geometry (slothbar-smoke--window-geometry window)))
          (equal (list (nth 2 geometry) (nth 3 geometry))
                 '(320 30))))
      3)))
  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; Two-bar geometry refresh: both bars resize independently.
  (setq slothbar-bars
        (list (list :id "geometry-a"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 10
                    :offset-y 10
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "geometry-a" :format "geometry-a"
                             :lpad 2 :rpad 2))))
              (list :id "geometry-b"
                    :output "HEADLESS-1"
                    :width 320
                    :height 24
                    :offset-x 20
                    :offset-y 34
                    :modules
                    (list (lambda ()
                            (slothbar-module-create
                             :name "geometry-b" :format "geometry-b"
                             :lpad 2 :rpad 2))))))
  (slothbar--start)
  (sit-for 0.2)
  (let* ((bar-a (car slothbar--bars))
         (bar-b (cadr slothbar--bars)))
    (let ((config-a (copy-sequence (slothbar-config bar-a)))
          (config-b (copy-sequence (slothbar-config bar-b))))
      (slothbar-smoke--set-bar-config
       bar-a
       (plist-put (plist-put (plist-put (plist-put config-a
                                                   :width 340)
                                      :height 26)
                             :offset-x 30)
                  :offset-y 40))
      (slothbar-smoke--set-bar-config
       bar-b
       (plist-put (plist-put (plist-put (plist-put config-b
                                                   :width 360)
                                      :height 28)
                             :offset-x 50)
                  :offset-y 70)))
    (slothbar-refresh)
    (slothbar-refresh-modules)
    (slothbar-smoke--check
     "X two-bar geometry updates both bar models"
     (slothbar-smoke--wait-for
      (lambda ()
        (and (= 340 (slothbar-pixel-width bar-a))
             (= 26 (slothbar-pixel-height bar-a))
             (= 30 (slothbar-x-offset bar-a))
             (= 40 (slothbar-y-offset bar-a))
             (= 360 (slothbar-pixel-width bar-b))
             (= 28 (slothbar-pixel-height bar-b))
             (= 50 (slothbar-x-offset bar-b))
             (= 70 (slothbar-y-offset bar-b))))
      3))
    (let ((resized
           (slothbar-smoke--wait-for
            (lambda ()
              (let* ((current-a (car slothbar--bars))
                     (current-b (cadr slothbar--bars))
                     (window-a (and current-a
                                    (slothbar-backend-xcb-surface-window
                                     (slothbar-surface current-a))))
                     (window-b (and current-b
                                    (slothbar-backend-xcb-surface-window
                                     (slothbar-surface current-b))))
                     (geometry-a (and window-a
                                      (slothbar-smoke--window-geometry window-a)))
                     (geometry-b (and window-b
                                      (slothbar-smoke--window-geometry window-b))))
                (and window-a window-b
                     (equal (list (nth 2 geometry-a) (nth 3 geometry-a))
                            '(340 26))
                     (equal (list (nth 2 geometry-b) (nth 3 geometry-b))
                            '(360 28)))))
            3)))
      (slothbar-smoke--check
       "X two-bar geometry resizes both X windows"
       resized)))
  (slothbar-exit)
  (setq slothbar-bars nil)

  ;; `slothbar-modules' fallback must reload only inherited bars.
  (let ((slothbar-modules
         (list (lambda ()
                 (slothbar-module-create
                  :name "fallback-old" :format "old"
                  :lpad 2 :rpad 2)))))
    (setq slothbar-bars
          (list (list :id "fallback"
                      :output "HEADLESS-1"
                      :width 300
                      :height 20
                      :offset-x 0
                      :offset-y 0)
                (list :id "explicit"
                      :output "HEADLESS-1"
                      :width 300
                      :height 20
                      :offset-x 0
                      :offset-y 20
                      :modules
                      (list (lambda ()
                              (slothbar-module-create
                               :name "explicit" :format "explicit"
                               :lpad 2 :rpad 2))))))
    (slothbar--start)
    (let* ((bar-fallback (car slothbar--bars))
           (bar-explicit (cadr slothbar--bars))
           (explicit-module (car (slothbar-module-list bar-explicit))))
      (slothbar-smoke--check
       "slothbar-modules fallback fills only the inherited bar"
       (and (equal (mapcar #'slothbar-module-name
                           (slothbar-module-list bar-fallback))
                   '("fallback-old"))
            (equal (mapcar #'slothbar-module-name
                           (slothbar-module-list bar-explicit))
                   '("explicit"))))
      (setq slothbar-modules
            (list (lambda ()
                    (slothbar-module-create
                     :name "fallback-new" :format "new"
                     :lpad 2 :rpad 2))))
      (slothbar-smoke--check
       "changing slothbar-modules reloads the inherited bar"
       (slothbar-smoke--wait-for
        (lambda ()
          (equal (mapcar #'slothbar-module-name
                         (slothbar-module-list bar-fallback))
                 '("fallback-new")))
        3))
      (slothbar-smoke--check
       "changing slothbar-modules leaves explicit modules alone"
       (and (eq explicit-module
                (car (slothbar-module-list bar-explicit)))
            (equal (mapcar #'slothbar-module-name
                           (slothbar-module-list bar-explicit))
                   '("explicit")))))
    (slothbar-exit)
    (setq slothbar-bars nil))

  ;; Shared providers must fan out to modules in both bars.
  (require 'slothbar-focus)
  (require 'slothbar-sysinfo)
  (setq slothbar-bars
        (list (list :id "shared-a"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules
                    (list (lambda () (slothbar-focus-create))
                          (lambda () (slothbar-sysinfo-create))))
              (list :id "shared-b"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 20
                    :modules
                    (list (lambda () (slothbar-focus-create))
                          (lambda () (slothbar-sysinfo-create))))))
  (slothbar--start)
  (let* ((focus-a (cl-find-if (lambda (m) (slothbar-focus-p m))
                              (slothbar-module-list (car slothbar--bars))))
         (focus-b (cl-find-if (lambda (m) (slothbar-focus-p m))
                              (slothbar-module-list (cadr slothbar--bars))))
         (sys-a (cl-find-if (lambda (m) (slothbar-sysinfo-p m))
                            (slothbar-module-list (car slothbar--bars))))
         (sys-b (cl-find-if (lambda (m) (slothbar-sysinfo-p m))
                            (slothbar-module-list (cadr slothbar--bars)))))
    (slothbar-smoke--check
     "two focus modules share one backend hook"
     (and (= 2 slothbar-focus--subscribers)
          (= 1 (seq-count (lambda (fn)
                            (eq fn #'slothbar-focus--on-change))
                          slothbar-backend-focused-window-change-hook))))
    (slothbar-smoke--check
     "two sysinfo modules share one provider timer"
     (and (= 2 (length slothbar-sysinfo--subscribers))
          slothbar-sysinfo--provider-timer
          (eq (slothbar-sysinfo-state sys-a)
              (slothbar-sysinfo-state sys-b))))
    (cl-letf (((symbol-function 'slothbar-backend-focused-window)
               (lambda (_backend)
                 '(:title "shared focus" :app-id "shared"))))
      (run-hook-with-args 'slothbar-backend-focused-window-change-hook
                          slothbar--backend)
      (slothbar-smoke--check
       "one focus hook update reaches both bars"
       (slothbar-smoke--wait-for
        (lambda ()
          (and (equal "shared focus" (slothbar-module-text focus-a))
               (equal "shared focus" (slothbar-module-text focus-b))))
        3)))
    (let ((state (slothbar-sysinfo-state sys-a)))
      (cl-letf (((symbol-function 'slothbar-sysinfo-update)
                 (lambda (state &optional _now _force)
                   (setf (alist-get :cpu-usage
                                    (slothbar-sysinfo-state-values state))
                         7.5)
                   state)))
        (slothbar-sysinfo--provider-tick)
        (slothbar-smoke--check
         "one sysinfo provider update reaches both bars"
         (and (equal (slothbar-module-text sys-a)
                     (slothbar-module-text sys-b))
              (not (string-empty-p (slothbar-module-text sys-a)))))))
    (slothbar-exit)
    (slothbar-smoke--check
     "shared providers unsubscribe after exit"
     (and (zerop slothbar-focus--subscribers)
          (null slothbar-sysinfo--subscribers)
          (null slothbar-sysinfo--provider-timer))))
  (setq slothbar-bars nil)

  ;; Live theme and font-scale fan-out to two X bars.
  (setq slothbar-bars
        (list (list :id "theme-a"
                    :output "HEADLESS-1"
                    :width 1280
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
                    :width 1280
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
         (backend (slothbar-display-backend bar-a))
         (module-a (car (slothbar-module-list bar-a)))
         (module-b (car (slothbar-module-list bar-b)))
         (before-a (slothbar-smoke--module-pixmap-data backend module-a))
         (before-b (slothbar-smoke--module-pixmap-data backend module-b))
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
            (slothbar-refresh-modules)
            (slothbar-smoke--check
             "X theme change updates both bars' module colors"
             (and (eq (slothbar-module-rgb-background-color
                       (slothbar-module-colors module-a))
                      new-bg)
                  (eq (slothbar-module-rgb-background-color
                       (slothbar-module-colors module-b))
                      new-bg)))
            (slothbar-smoke--check
             "X theme change redraws both bars"
             (slothbar-smoke--wait-for
              (lambda ()
                (and (not (equal
                           before-a
                           (slothbar-smoke--module-pixmap-data
                            backend module-a)))
                     (not (equal
                           before-b
                           (slothbar-smoke--module-pixmap-data
                            backend module-b)))))
              3)))
          (let ((theme-a (slothbar-smoke--module-pixmap-data backend module-a))
                (theme-b (slothbar-smoke--module-pixmap-data backend module-b)))
            (setq slothbar-font-px-scale 0.75)
            (slothbar-smoke--check
             "X font scale changes both bars' computed sizes"
             (slothbar-smoke--wait-for
              (lambda ()
                (and (not (equal font-a (slothbar-font-px-sizes bar-a)))
                     (not (equal font-b (slothbar-font-px-sizes bar-b)))))
              3))
            (slothbar-smoke--check
             "X font scale redraws both bars"
             (slothbar-smoke--wait-for
              (lambda ()
                (and (not (equal
                           theme-a
                           (slothbar-smoke--module-pixmap-data
                            backend module-a)))
                     (not (equal
                           theme-b
                           (slothbar-smoke--module-pixmap-data
                            backend module-b)))))
              3))))
      (slothbar-exit)
      (setq slothbar-bars nil)
      (setq slothbar-font-px-scale old-scale)
      (sit-for 0.1)))

  ;; XEmbed tray is a global singleton across bars.
  (require 'slothbar-tray)
  (setq slothbar-bars
        (list (list :id "tray-a"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 0
                    :modules (list #'slothbar-tray-create))
              (list :id "tray-b"
                    :output "HEADLESS-1"
                    :width 300
                    :height 20
                    :offset-x 0
                    :offset-y 20
                    :modules (list #'slothbar-tray-create))))
  (slothbar--start)
  (let* ((bar-a (car slothbar--bars))
         (bar-b (cadr slothbar--bars)))
    (slothbar-smoke--check
     "only the first bar constructs an XEmbed tray"
     (and (equal (mapcar #'slothbar-module-name
                         (slothbar-module-list bar-a))
                 '("tray"))
          (null (slothbar-module-list bar-b))))
    (slothbar-smoke--check
     "the XEmbed tray opened its manager connection"
     (and slothbar-tray--connection
          slothbar-tray--selection-owner-window))
    (slothbar-smoke--check
     "the tray module is owned by the first bar"
     (eq slothbar-tray--module
         (car (slothbar-module-list bar-a)))))
  (slothbar-exit)
  (slothbar-smoke--check
   "exiting closes the XEmbed tray connection"
   (and (null slothbar-tray--connection)
        (null slothbar-tray--module)))
  (setq slothbar-bars nil)

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
  ;; An X Expose must force a redraw of the exposed bar, otherwise the
  ;; first map can leave the initial paint blank until another update.
  (let* ((bar (car slothbar--bars))
         (module (cl-find-if #'slothbar-module-p
                             (slothbar-module-list bar)))
         (window (slothbar-backend-xcb-surface-window
                  (slothbar-surface bar)))
         (data (xcb:marshal
                (make-instance 'xcb:Expose
                               :window window
                               :x 0 :y 0 :width 1 :height 1 :count 0)
                (slothbar-xcb-connection))))
    (aset module
          (cl-struct-slot-offset 'slothbar-module 'needs-refresh?)
          nil)
    (slothbar-backend-xcb--on-Expose data nil)
    (slothbar-smoke--check
     "Expose marks the exposed bar's module for redraw"
     (slothbar-module-needs-refresh? module)))
  ;; Let Expose and other X events reach the handlers.
  (sit-for 0.3)
  (slothbar-exit)
  (slothbar-smoke--check "core exit disabled slothbar"
                         (not (slothbar-enabled-p)))
  (slothbar-smoke--check "core exit cleared the backend"
                         (null slothbar--backend))
  (when (equal (getenv "SLOTHBAR_EXPECT_NO_ELWL") "1")
    (slothbar-smoke--check
     "the X path did not load elwl"
     (and (not (featurep 'elwl))
          (not (locate-library "elwl"))))))

(condition-case err
    (progn
      (slothbar-smoke--run)
      (slothbar-smoke--record "all xwayland smoke checks passed\n")
      (kill-emacs 0))
  (error
   (slothbar-smoke--record
    (format "ERROR: %S\nBACKTRACE:\n%s\n" err (backtrace)))
   (kill-emacs 1)))

;;; slothbar-xwayland-smoke.el ends here
