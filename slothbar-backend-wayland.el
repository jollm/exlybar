;;; slothbar-backend-wayland.el --- Wayland display backend for slothbar  -*- lexical-binding: t -*-

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

;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Part of slothbar.
;;
;; A wlr-layer-shell bar rendered into wl_shm buffers.  Glyphs come from
;; fontsloth's A8 rasterizer and are blended into an ARGB8888 frame by
;; `slothbar-text-draw-ops'.  X-specific painting lives in
;; slothbar-render-xcb.el; this backend never needs an X connection.

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'elwl-connection)
(require 'elwl-layer-shell)
(require 'elwl-output)
(require 'elwl-protocol)
(require 'elwl-registry)
(require 'elwl-shm)
(require 'elwl-surface)
(require 'elwl-workspace)
(require 'fontsloth)
(require 'fontsloth-layout)

(require 'slothbar)
(require 'slothbar-backend)
(require 'slothbar-color)
(require 'slothbar-draw-op)
(require 'slothbar-font)
(require 'slothbar-log)
(require 'slothbar-module)

(cl-defstruct
    (slothbar-backend-wayland
     (:include slothbar-backend)
     (:constructor slothbar-backend-wayland--make)
     (:copier nil))
  "A Wayland layer-shell display backend."
  connection
  registry
  compositor
  shm
  layer-shell
  surface
  layer-surface
  output
  ext-workspace-manager
  (scale 1 :type 'integer)
  (logical-width 0 :type 'integer)
  (logical-height 0 :type 'integer)
  (frame-width 0 :type 'integer)
  (frame-height 0 :type 'integer)
  (frame-stride 0 :type 'integer)
  (configured-serial nil)
  (rendered nil)
  (presented nil)
  (pending nil)
  pool
  buffers
  retired
  frame
  (module-ops (make-hash-table :test 'eq) :type 'hash-table)
  (glyph-cache (make-hash-table :test 'equal) :type 'hash-table))

(defun slothbar-backend-wayland-create ()
  "Create an unopened Wayland backend."
  (slothbar-backend-wayland--make :kind 'wayland))

(cl-defmethod slothbar-backend-supports-p
  ((backend slothbar-backend-wayland) feature)
  "The Wayland backend has no X-only facilities."
  (or (memq feature '(surface exclusive-zone layer-shell buffer-scale))
      (and (eq feature 'ext-workspace)
           (slothbar-backend-ext-workspace-manager backend))))

(cl-defmethod slothbar-backend-ext-workspace-manager
  ((backend slothbar-backend-wayland))
  "Return the live ext-workspace manager for BACKEND, if any."
  (let ((manager (slothbar-backend-wayland-ext-workspace-manager backend)))
    (and manager
         (not (elwl-workspace-manager-dead manager))
         manager)))

;;; Colors and pixels

(defun slothbar-backend-wayland--color-vector (color)
  "Return an 8-bit (B G R A) vector for COLOR."
  (let ((color (or color
                   (slothbar-color-from-plist '(:red 0 :green 0 :blue 0))))
        (vector (make-vector 4 255)))
    (aset vector 0 (logand #xff (ash (slothbar-color-rgb-blue color) -8)))
    (aset vector 1 (logand #xff (ash (slothbar-color-rgb-green color) -8)))
    (aset vector 2 (logand #xff (ash (slothbar-color-rgb-red color) -8)))
    (aset vector 3 (logand #xff (ash (slothbar-color-rgb-alpha color) -8)))
    vector))

(defun slothbar-backend-wayland--pixel (color)
  "Return one opaque little-endian BGRA pixel for COLOR."
  (let ((v (slothbar-backend-wayland--color-vector color)))
    (unibyte-string (aref v 0) (aref v 1) (aref v 2) 255)))

(defun slothbar-backend-wayland--row (width color)
  "Return WIDTH pixels of COLOR as a unibyte BGRA row."
  (let ((pixel (slothbar-backend-wayland--pixel color)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dotimes (_ width) (insert pixel))
      (buffer-string))))

(defun slothbar-backend-wayland--blank-frame (backend)
  "Return a background-filled frame for BACKEND."
  (let* ((width (slothbar-backend-wayland-frame-width backend))
         (height (slothbar-backend-wayland-frame-height backend))
         (row (slothbar-backend-wayland--row
               width (slothbar--background-color))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dotimes (_ height) (insert row))
      (buffer-string))))

(defun slothbar-backend-wayland--fill-rect
    (frame stride x y width height color)
  "Fill a rectangle in FRAME with COLOR.
FRAME a unibyte BGRA frame
STRIDE bytes per row
X, Y, WIDTH, HEIGHT the rectangle in buffer pixels
COLOR a `slothbar-color-rgb'"
  (when (and (> width 0) (> height 0))
    (let* ((frame-width (/ stride 4))
           (frame-height (/ (length frame) stride))
           (left (max 0 x))
           (right (min frame-width (+ x width)))
           (visible-width (- right left)))
      (when (> visible-width 0)
        (let* ((row (slothbar-backend-wayland--row width color))
               (start (* 4 (- left x)))
               (row (substring row start (+ start (* 4 visible-width)))))
          (dotimes (row-index height)
            (let ((draw-y (+ y row-index)))
              (when (and (>= draw-y 0) (< draw-y frame-height))
                (store-substring frame
                                 (+ (* draw-y stride) (* left 4))
                                 row)))))))))

(defun slothbar-backend-wayland--blend-glyph
    (frame stride frame-width frame-height fg bg left top pixmap width height)
  "Blend an A8 glyph PIXMAP into FRAME.
FG and BG are 8-bit (B G R A) vectors.  LEFT and TOP are buffer pixels."
  (dotimes (row height)
    (let ((y (+ top row)))
      (when (and (>= y 0) (< y frame-height))
        (dotimes (col width)
          (let ((x (+ left col))
                (alpha (aref pixmap (+ col (* row width)))))
            (when (and (> alpha 0) (>= x 0) (< x frame-width))
              (let* ((index (+ (* y stride) (* x 4)))
                     (source-alpha (/ (* alpha (aref fg 3)) 255))
                     (inverse (- 255 source-alpha)))
                (aset frame index
                      (/ (+ (* (aref fg 0) source-alpha)
                            (* (aref bg 0) inverse))
                         255))
                (aset frame (1+ index)
                      (/ (+ (* (aref fg 1) source-alpha)
                            (* (aref bg 1) inverse))
                         255))
                (aset frame (+ index 2)
                      (/ (+ (* (aref fg 2) source-alpha)
                            (* (aref bg 2) inverse))
                         255))
                (aset frame (+ index 3) 255)))))))))

;;; Glyph rasterization

(defun slothbar-backend-wayland--glyph (backend font-index glyph-id px scale)
  "Return a cached `fontsloth-metrics+pixmap' for a glyph.
FONT-INDEX indexes `slothbar-font--color-code-map'; PX is the logical pixel
size and SCALE the output buffer scale."
  (let ((key (list font-index glyph-id px scale)))
    (or (gethash key (slothbar-backend-wayland-glyph-cache backend))
        (let* ((path (slothbar-font-find font-index))
               (font (and path (fontsloth-load-font path))))
          (when font
            (let ((entry (fontsloth-font-rasterize
                          font glyph-id (* px scale))))
              (when entry
                (puthash key entry
                         (slothbar-backend-wayland-glyph-cache backend)))
              entry))))))

(defun slothbar-backend-wayland--glyph-top
    (glyph-y raster-height scale module-y)
  "Return the buffer-space top of a glyph.
GLYPH-Y is fontsloth's negative baseline offset, RASTER-HEIGHT the scaled
glyph bitmap height, SCALE the output scale, and MODULE-Y the module's
placement."
  (- (* scale module-y)
     (round (* scale glyph-y))
     raster-height))

(defun slothbar-backend-wayland--draw-ops (backend module ops module-x module-y)
  "Draw MODULE's OPS into BACKEND's frame at MODULE-X, MODULE-Y."
  (let* ((scale (slothbar-backend-wayland-scale backend))
         (frame (slothbar-backend-wayland-frame backend))
         (stride (slothbar-backend-wayland-frame-stride backend))
         (frame-width (slothbar-backend-wayland-frame-width backend))
         (frame-height (slothbar-backend-wayland-frame-height backend))
         (colors (slothbar-module-colors module))
         (bg (slothbar-backend-wayland--color-vector
              (or (slothbar-module-rgb-background-color colors)
                  (slothbar--background-color)))))
    (dolist (op ops)
      (let ((fg (slothbar-backend-wayland--color-vector
                 (slothbar-draw-op-color op))))
        (dolist (pos (slothbar-draw-op-glyph-positions op))
          (when (fontsloth-layout-glyph-position-p pos)
            (pcase-let* (((cl-struct
                           fontsloth-layout-glyph-position
                           key x y (user-data font-index)) pos)
                         ((cl-struct
                           fontsloth-layout-glyph-raster-config
                           glyph-id px) key)
                         (entry (slothbar-backend-wayland--glyph
                                 backend font-index glyph-id px scale)))
              (when entry
                (pcase-let* (((cl-struct
                               fontsloth-metrics+pixmap metrics pixmap) entry)
                             ((cl-struct fontsloth-metrics
                                         width height) metrics)
                             (left (truncate (* scale (+ module-x x))))
                             (top (slothbar-backend-wayland--glyph-top
                                   y height scale module-y)))
                  (slothbar-backend-wayland--blend-glyph
                   frame stride frame-width frame-height fg bg
                   left top pixmap width height))))))))))

(defun slothbar-backend-wayland--draw-module
    (backend module placement-x placement-y)
  "Draw MODULE into BACKEND's frame at PLACEMENT-X, PLACEMENT-Y."
  (let* ((scale (slothbar-backend-wayland-scale backend))
         (width (* scale (slothbar-module-width module)))
         (height (* scale (slothbar-backend-wayland-logical-height backend)))
         (colors (slothbar-module-colors module))
         (background (or (slothbar-module-rgb-background-color colors)
                         (slothbar--background-color))))
    (slothbar-backend-wayland--fill-rect
     (slothbar-backend-wayland-frame backend)
     (slothbar-backend-wayland-frame-stride backend)
     (* scale placement-x) (* scale placement-y)
     width height background)
    (slothbar-backend-wayland--draw-ops
     backend module
     (gethash module (slothbar-backend-wayland-module-ops backend))
     placement-x placement-y)))

(defun slothbar-backend-wayland--compose-frame (backend layout)
  "Return a new frame for BACKEND with LAYOUT drawn."
  (let ((frame (slothbar-backend-wayland--blank-frame backend)))
    (setf (slothbar-backend-wayland-frame backend) frame)
    (dolist (placement layout)
      (pcase-let ((`((,x ,y) ,module) placement))
        (when (slothbar-module-p module)
          (slothbar-backend-wayland--draw-module backend module x y))))
    (slothbar-backend-wayland-frame backend)))

;;; Shm buffers

(defun slothbar-backend-wayland--release-listener (backend)
  "Return a buffer-release listener for BACKEND."
  (lambda (_object _arguments)
    (when (and (eq slothbar--backend backend)
               (slothbar-backend-wayland-pending backend))
      (setf (slothbar-backend-wayland-pending backend) nil)
      (run-at-time
       0 nil
       (lambda ()
         (when (eq slothbar--backend backend)
           (slothbar-refresh-modules)))))))

(defun slothbar-backend-wayland--rebuild-frame (backend)
  "Create a pool, two buffers, and a blank frame for BACKEND."
  (let* ((scale (slothbar-backend-wayland-scale backend))
         (logical-width (slothbar-backend-wayland-logical-width backend))
         (logical-height (slothbar-backend-wayland-logical-height backend))
         (width (* logical-width scale))
         (height (* logical-height scale))
         (stride (* width 4))
         (bytes (* stride height))
         (old-pool (slothbar-backend-wayland-pool backend))
         (old-buffers (slothbar-backend-wayland-buffers backend))
         (shm (slothbar-backend-wayland-shm backend)))
    (when old-pool
      (push (cons old-pool old-buffers)
            (slothbar-backend-wayland-retired backend)))
    (let* ((pool (elwl-shm-create-pool shm (* bytes 2)))
           (buffers (list (elwl-shm-pool-create-buffer
                           pool width height 'argb8888 0)
                          (elwl-shm-pool-create-buffer
                           pool width height 'argb8888 bytes))))
      (dolist (buffer buffers)
        (elwl-object-add-listener
         (elwl-shm-buffer-object buffer) 'release
         (slothbar-backend-wayland--release-listener backend)))
      (setf (slothbar-backend-wayland-pool backend) pool
            (slothbar-backend-wayland-buffers backend) buffers
            (slothbar-backend-wayland-frame-width backend) width
            (slothbar-backend-wayland-frame-height backend) height
            (slothbar-backend-wayland-frame-stride backend) stride
            (slothbar-backend-wayland-frame backend)
            (slothbar-backend-wayland--blank-frame backend)
            (slothbar-backend-wayland-rendered backend) nil
            (slothbar-backend-wayland-presented backend) nil
            (slothbar-backend-wayland-pending backend) nil))
    backend))

(defun slothbar-backend-wayland--destroy-pool (pool buffers)
  "Destroy BUFFERS and POOL."
  (dolist (buffer buffers)
    (when (elwl-shm-buffer-p buffer)
      (elwl-shm-buffer-destroy buffer)))
  (when (elwl-shm-pool-p pool)
    (elwl-shm-pool-destroy pool)))

(defun slothbar-backend-wayland--cleanup-retired (backend)
  "Destroy BACKEND's retired pools after their buffers are no longer needed."
  (let ((retired (slothbar-backend-wayland-retired backend)))
    (while retired
      (let ((pair (pop retired)))
        (slothbar-backend-wayland--destroy-pool (car pair) (cdr pair))))
    (setf (slothbar-backend-wayland-retired backend) nil)))

(defun slothbar-backend-wayland--present-frame (backend frame)
  "Attach and commit FRAME on BACKEND's surface.
Return non-nil if a buffer was presented."
  (let ((buffer (cl-find-if #'elwl-shm-buffer-free-p
                            (slothbar-backend-wayland-buffers backend))))
    (if (not buffer)
        (progn
          (setf (slothbar-backend-wayland-pending backend) t)
          nil)
      (elwl-shm-buffer-acquire buffer)
      (condition-case err
          (progn
            (elwl-shm-buffer-write buffer frame)
            (elwl-surface-attach (slothbar-backend-wayland-surface backend)
                                 (elwl-shm-buffer-object buffer))
            (elwl-surface-damage
             (slothbar-backend-wayland-surface backend)
             0 0
             (slothbar-backend-wayland-logical-width backend)
             (slothbar-backend-wayland-logical-height backend))
            (elwl-surface-commit (slothbar-backend-wayland-surface backend))
            (elwl-shm-buffer-submit buffer)
            (slothbar-backend-wayland--cleanup-retired backend)
            (setf (slothbar-backend-wayland-pending backend) nil
                  (slothbar-backend-wayland-presented backend) t)
            t)
        (error
         (when (elwl-shm-buffer-leased-p buffer)
           (ignore-errors (elwl-shm-buffer-unlease buffer)))
         (signal (car err) (cdr err)))))))

;;; Layer-shell configuration

(defun slothbar-backend-wayland--exclusive-zone (backend)
  "Return BACKEND's exclusive zone."
  (+ (slothbar-backend-wayland-logical-height backend)
     slothbar-margin-y))

(defun slothbar-backend-wayland--apply-layer-state (backend visible)
  "Apply size, anchors, margins, and zone to BACKEND's layer surface.
VISIBLE non-nil reserves the bar's exclusive zone."
  (let ((layer (slothbar-backend-wayland-layer-surface backend)))
    (elwl-layer-surface-set-size
     layer 0 (slothbar-backend-wayland-logical-height backend))
    (elwl-layer-surface-set-anchor
     layer (if slothbar-is-bottom '(bottom left right) '(top left right)))
    (elwl-layer-surface-set-margin
     layer
     (if slothbar-is-bottom 0 slothbar-margin-y)
     0
     (if slothbar-is-bottom slothbar-margin-y 0)
     0)
    (elwl-layer-surface-set-keyboard-interactivity layer 'none)
    (elwl-layer-surface-set-exclusive-zone
     layer (if visible (slothbar-backend-wayland--exclusive-zone backend) 0))
    layer))

(defun slothbar-backend-wayland--on-configure (backend arguments)
  "Handle a layer-surface configure for BACKEND."
  (let* ((serial (alist-get 'serial arguments))
         (width (alist-get 'width arguments))
         (height (alist-get 'height arguments))
         (initial (null (slothbar-backend-wayland-configured-serial backend)))
         (new-width (if (and (integerp width) (> width 0))
                        width
                      (slothbar-backend-wayland-logical-width backend))))
    (elwl-layer-surface-ack-configure
     (slothbar-backend-wayland-layer-surface backend) serial)
    (setf (slothbar-backend-wayland-configured-serial backend) serial)
    (when (and height (> height 0)
               (/= height (slothbar-backend-wayland-logical-height backend)))
      (slothbar--log-debug*
       "wayland configure height %s differs from slothbar-height %s"
       height (slothbar-backend-wayland-logical-height backend)))
    (when (or initial
              (/= new-width (slothbar-backend-wayland-logical-width backend)))
      (setf (slothbar-backend-wayland-logical-width backend) new-width)
      (when (and slothbar--backend
                 (eq slothbar--backend backend))
        (setq slothbar-width new-width))
      (slothbar-backend-wayland--rebuild-frame backend)
      (unless initial
        (run-at-time 0 nil #'slothbar-refresh-modules)))))

(defun slothbar-backend-wayland--select-output (outputs)
  "Choose an output from OUTPUTS using `slothbar-preferred-display'."
  (or (and slothbar-preferred-display
           (cl-find slothbar-preferred-display outputs
                    :key #'elwl-output-name :test #'equal))
      (car outputs)))

(defun slothbar-backend-wayland--wait-for (backend predicate seconds)
  "Dispatch BACKEND until PREDICATE is true or SECONDS elapse."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (elwl-dispatch (slothbar-backend-wayland-connection backend) 0.1))
    (funcall predicate)))

;;; Backend protocol

(cl-defmethod slothbar-backend-open ((backend slothbar-backend-wayland))
  "Open BACKEND's Wayland connection and layer surface."
  (condition-case err
      (let* ((protocols (list (elwl-protocol-load-wayland)
                              (elwl-protocol-load-layer-shell)
                              (elwl-protocol-load-ext-workspace)))
             (connection (elwl-connect protocols)))
        (setf (slothbar-backend-wayland-connection backend) connection)
        (let ((registry (elwl-get-registry connection)))
          (setf (slothbar-backend-wayland-registry backend) registry)
          (elwl-roundtrip connection)
          (setf (slothbar-backend-wayland-compositor backend)
                (elwl-registry-bind registry 'wl-compositor 4)
                (slothbar-backend-wayland-shm backend)
                (elwl-registry-bind registry 'wl-shm 1)
                (slothbar-backend-wayland-layer-shell backend)
                (elwl-layer-shell-bind registry))
          (let ((outputs (elwl-output-sync registry)))
            ;; Names and scale arrive as events.
            (elwl-roundtrip connection)
            (let* ((output (slothbar-backend-wayland--select-output outputs))
                   (scale (or (and output (elwl-output-scale output)) 1)))
              (setf (slothbar-backend-wayland-output backend) output
                    (slothbar-backend-wayland-scale backend) scale)))
          ;; ext-workspace is optional: the compositor may not advertise it.
          (when (elwl-registry-find registry 'ext-workspace-manager-v1)
            (let ((manager
                   (elwl-workspace-manager-bind
                    registry nil
                    (lambda (_manager)
                      (condition-case hook-err
                          (run-hook-with-args
                           'slothbar-backend-workspace-change-hook backend)
                        (error
                         (slothbar--log-debug*
                          "workspace change hook error: %S" hook-err)))))))
              (setf (slothbar-backend-wayland-ext-workspace-manager backend)
                    manager)
              (unless (slothbar-backend-wayland--wait-for
                       backend
                       (lambda () (elwl-workspace-manager-done manager))
                       2)
                (slothbar--log-debug*
                 "ext-workspace initial sync timed out"))))
          (let* ((surface (elwl-compositor-create-surface
                           (slothbar-backend-wayland-compositor backend)))
                 (output (slothbar-backend-wayland-output backend))
                 (layer (elwl-layer-shell-create
                         (slothbar-backend-wayland-layer-shell backend)
                         surface
                         :layer 'top
                         :namespace "slothbar"
                         :output (and output (elwl-output-object output)))))
            (setf (slothbar-backend-wayland-surface backend) surface
                  (slothbar-backend-wayland-layer-surface backend) layer
                  (slothbar-backend-wayland-logical-width backend)
                  (max 1 (or slothbar-width 1))
                  (slothbar-backend-wayland-logical-height backend)
                  (max 1 slothbar-height))
            (when (>= (elwl-object-version surface) 3)
              (elwl-surface-set-buffer-scale
               surface (slothbar-backend-wayland-scale backend)))
            (elwl-object-add-listener
             layer 'configure
             (lambda (_object arguments)
               (slothbar-backend-wayland--on-configure backend arguments)))
            (slothbar-backend-wayland--rebuild-frame backend)
            (slothbar-backend-wayland--apply-layer-state backend nil)
            (elwl-surface-commit surface)
            (unless (slothbar-backend-wayland--wait-for
                     backend
                     (lambda ()
                       (slothbar-backend-wayland-configured-serial backend))
                     5)
              (error "Timed out waiting for a layer-surface configure"))))
        backend)
    (error
     (ignore-errors (slothbar-backend-close backend))
     (signal (car err) (cdr err)))))

(cl-defmethod slothbar-backend-close ((backend slothbar-backend-wayland))
  "Tear down BACKEND's Wayland objects and connection."
  (when (slothbar-backend-wayland-ext-workspace-manager backend)
    (ignore-errors
      (elwl-workspace-manager-stop
       (slothbar-backend-wayland-ext-workspace-manager backend))))
  (when (slothbar-backend-wayland-layer-surface backend)
    (ignore-errors
      (elwl-layer-surface-destroy
       (slothbar-backend-wayland-layer-surface backend))))
  (when (slothbar-backend-wayland-surface backend)
    (ignore-errors
      (elwl-surface-destroy (slothbar-backend-wayland-surface backend))))
  (when (slothbar-backend-wayland-pool backend)
    (ignore-errors
      (slothbar-backend-wayland--destroy-pool
       (slothbar-backend-wayland-pool backend)
       (slothbar-backend-wayland-buffers backend))))
  (let ((retired (slothbar-backend-wayland-retired backend)))
    (while retired
      (let ((pair (pop retired)))
        (ignore-errors
          (slothbar-backend-wayland--destroy-pool (car pair) (cdr pair))))))
  (when (slothbar-backend-wayland-output backend)
    (ignore-errors
      (elwl-output-release (slothbar-backend-wayland-output backend))))
  (when (slothbar-backend-wayland-connection backend)
    (ignore-errors
      (elwl-disconnect (slothbar-backend-wayland-connection backend))))
  (setf (slothbar-backend-wayland-connection backend) nil
        (slothbar-backend-wayland-registry backend) nil
        (slothbar-backend-wayland-compositor backend) nil
        (slothbar-backend-wayland-shm backend) nil
        (slothbar-backend-wayland-layer-shell backend) nil
        (slothbar-backend-wayland-surface backend) nil
        (slothbar-backend-wayland-layer-surface backend) nil
        (slothbar-backend-wayland-output backend) nil
        (slothbar-backend-wayland-ext-workspace-manager backend) nil
        (slothbar-backend-wayland-pool backend) nil
        (slothbar-backend-wayland-buffers backend) nil
        (slothbar-backend-wayland-retired backend) nil
        (slothbar-backend-wayland-frame backend) nil)
  nil)

(cl-defmethod slothbar-backend-show ((backend slothbar-backend-wayland))
  "Show BACKEND's bar and reserve its exclusive zone."
  (slothbar-backend-wayland--apply-layer-state backend t)
  (if (and (slothbar-backend-wayland-rendered backend)
           (slothbar-backend-wayland-frame backend))
      (slothbar-backend-wayland--present-frame
       backend (slothbar-backend-wayland-frame backend))
    (elwl-surface-commit (slothbar-backend-wayland-surface backend))))

(cl-defmethod slothbar-backend-hide ((backend slothbar-backend-wayland))
  "Hide BACKEND's bar and release its exclusive zone."
  (setf (slothbar-backend-wayland-pending backend) nil
        (slothbar-backend-wayland-presented backend) nil)
  (elwl-layer-surface-set-exclusive-zone
   (slothbar-backend-wayland-layer-surface backend) 0)
  (elwl-surface-attach (slothbar-backend-wayland-surface backend) nil)
  (elwl-surface-commit (slothbar-backend-wayland-surface backend)))

(cl-defmethod slothbar-backend-add-module
    ((backend slothbar-backend-wayland) (module slothbar-module))
  "Register MODULE with BACKEND."
  (puthash module nil (slothbar-backend-wayland-module-ops backend))
  module)

(cl-defmethod slothbar-backend-remove-module
    ((backend slothbar-backend-wayland) (module slothbar-module))
  "Forget MODULE in BACKEND."
  (remhash module (slothbar-backend-wayland-module-ops backend)))

(cl-defmethod slothbar-backend-draw-module
    ((backend slothbar-backend-wayland) (module slothbar-module))
  "Compute MODULE's draw operations for BACKEND."
  (puthash module
           (slothbar-text-draw-ops (slothbar-module-text-layout module))
           (slothbar-backend-wayland-module-ops backend)))

(cl-defmethod slothbar-backend-reset-module-cache
    ((backend slothbar-backend-wayland) (module slothbar-module))
  "Forget MODULE's cached draw operations in BACKEND."
  (remhash module (slothbar-backend-wayland-module-ops backend)))

(cl-defmethod slothbar-backend-present
    ((backend slothbar-backend-wayland) layout damage)
  "Compose LAYOUT into BACKEND's frame and present it when visible."
  (ignore damage)
  (let ((frame (slothbar-backend-wayland--compose-frame backend layout)))
    (setf (slothbar-backend-wayland-rendered backend) t)
    (when slothbar--visible
      (slothbar-backend-wayland--present-frame backend frame))))

(provide 'slothbar-backend-wayland)
;;; slothbar-backend-wayland.el ends here
