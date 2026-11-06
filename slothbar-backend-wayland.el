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
(require 'elwl-foreign-toplevel)
(require 'elwl-seat)
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
  outputs
  ext-workspace-manager
  foreign-toplevel-manager
  seat
  pointer
  pointer-surface
  pointer-x
  pointer-y
  (glyph-cache (make-hash-table :test 'equal) :type 'hash-table))

(cl-defstruct
    (slothbar-backend-wayland-surface
     (:constructor slothbar-backend-wayland-surface--make)
     (:copier nil))
  "Per-bar Wayland layer surface and frame state."
  backend
  bar
  output
  surface
  layer-surface
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
  (module-ops (make-hash-table :test 'eq) :type 'hash-table))

(defun slothbar-backend-wayland-create ()
  "Create an unopened Wayland backend."
  (slothbar-backend-wayland--make :kind 'wayland))

(cl-defmethod slothbar-backend-supports-p
  ((backend slothbar-backend-wayland) feature)
  "The Wayland backend has no X-only facilities."
  (or (memq feature '(surface exclusive-zone layer-shell buffer-scale))
      (and (eq feature 'pointer-input)
           (slothbar-backend-wayland-pointer backend))
      (and (eq feature 'ext-workspace)
           (slothbar-backend-ext-workspace-manager backend))
      (and (eq feature 'focused-window)
           (let ((manager
                  (slothbar-backend-wayland-foreign-toplevel-manager backend)))
             (and manager
                  (not (elwl-foreign-toplevel-manager-dead-p manager)))))))

(defun slothbar-backend-wayland--bar-for-pointer (backend)
  "Return BACKEND's bar under the pointer, if any."
  (cl-find-if
   (lambda (bar)
     (let ((state (slothbar-surface bar)))
       (and state
            (eq (slothbar-backend-wayland-pointer-surface backend)
                (slothbar-backend-wayland-surface-surface state)))))
   (slothbar-backend-bars backend)))

(defun slothbar-backend-wayland--pointer-motion (backend x y)
  "Dispatch pointer motion at X,Y on BACKEND when over a bar."
  (when-let ((bar (slothbar-backend-wayland--bar-for-pointer backend)))
    (condition-case err
        (run-hook-with-args
         'slothbar-backend-pointer-motion-hook
         backend bar
         (and x (round x))
         (and y (round y)))
      (error
       (slothbar--log-debug* "pointer motion hook error: %S" err)))))

(defun slothbar-backend-wayland--ensure-pointer (backend)
  "Create BACKEND's pointer when the seat advertises one."
  (let ((seat (slothbar-backend-wayland-seat backend)))
    (when (and seat
               (memq 'pointer (elwl-seat-capabilities seat))
               (null (slothbar-backend-wayland-pointer backend)))
      (let ((pointer (elwl-seat-get-pointer seat)))
        (setf (slothbar-backend-wayland-pointer backend) pointer)
        (elwl-object-add-listener
         (elwl-pointer-object pointer) 'enter
         (lambda (_object arguments)
           (setf (slothbar-backend-wayland-pointer-surface backend)
                 (alist-get 'surface arguments)
                 (slothbar-backend-wayland-pointer-x backend)
                 (alist-get 'surface-x arguments)
                 (slothbar-backend-wayland-pointer-y backend)
                 (alist-get 'surface-y arguments))
           (slothbar-backend-wayland--pointer-motion
            backend
            (slothbar-backend-wayland-pointer-x backend)
            (slothbar-backend-wayland-pointer-y backend))))
        (elwl-object-add-listener
         (elwl-pointer-object pointer) 'motion
         (lambda (_object arguments)
           (setf (slothbar-backend-wayland-pointer-x backend)
                 (alist-get 'surface-x arguments)
                 (slothbar-backend-wayland-pointer-y backend)
                 (alist-get 'surface-y arguments))
           (slothbar-backend-wayland--pointer-motion
            backend
            (slothbar-backend-wayland-pointer-x backend)
            (slothbar-backend-wayland-pointer-y backend))))
        (elwl-object-add-listener
         (elwl-pointer-object pointer) 'leave
         (lambda (_object _arguments)
           (slothbar-backend-wayland--pointer-motion backend nil nil)
           (setf (slothbar-backend-wayland-pointer-surface backend) nil)))
        (elwl-object-add-listener
         (elwl-pointer-object pointer) 'button
         (lambda (_object arguments)
           (slothbar-backend-wayland--pointer-button
            backend arguments)))))))

(defun slothbar-backend-wayland--pointer-button (backend arguments)
  "Dispatch a pointer button event from ARGUMENTS on BACKEND."
  (let ((bar (slothbar-backend-wayland--bar-for-pointer backend)))
    (when (and bar
               (or (eq (alist-get 'state arguments) 'pressed)
                   (eql (alist-get 'state arguments) 1)))
    (let ((button (pcase (alist-get 'button arguments)
                    (272 :left)
                    (273 :right)
                    (274 :middle))))
      (when button
        (condition-case err
            (run-hook-with-args
             'slothbar-backend-pointer-button-hook
             backend bar
             (round (or (slothbar-backend-wayland-pointer-x backend) 0))
             (round (or (slothbar-backend-wayland-pointer-y backend) 0))
             button)
          (error
           (slothbar--log-debug*
            "pointer button hook error: %S" err))))))))

(cl-defmethod slothbar-backend-focused-window
  ((backend slothbar-backend-wayland))
  "Return the focused Wayland toplevel's title and app-id."
  (let* ((manager
          (slothbar-backend-wayland-foreign-toplevel-manager backend))
         (toplevel (and manager
                        (elwl-foreign-toplevel-manager-focused manager))))
    (when toplevel
      (list :title (elwl-foreign-toplevel-title toplevel)
            :app-id (elwl-foreign-toplevel-app-id toplevel)
            :icon nil))))

(cl-defmethod slothbar-backend-ext-workspace-manager
  ((backend slothbar-backend-wayland))
  "Return the live ext-workspace manager for BACKEND, if any."
  (let ((manager (slothbar-backend-wayland-ext-workspace-manager backend)))
    (and manager
         (not (elwl-workspace-manager-dead manager))
         manager)))

(cl-defmethod slothbar-backend-output ((_backend slothbar-backend-wayland))
  "Return BACKEND's selected Wayland output object."
  (let ((output (and slothbar--bar (slothbar-output slothbar--bar))))
    (and output (elwl-output-object output))))

(cl-defmethod slothbar-backend-output-names ((backend slothbar-backend-wayland))
  "Return the names of BACKEND's connected outputs."
  (delq nil
        (mapcar #'elwl-output-name
                (slothbar-backend-wayland-outputs backend))))

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

(defun slothbar-backend-wayland--blank-frame (surface)
  "Return a background-filled frame for SURFACE."
  (let* ((width (slothbar-backend-wayland-surface-frame-width surface))
         (height (slothbar-backend-wayland-surface-frame-height surface))
         (row (slothbar-backend-wayland--row
               width (slothbar--background-color))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dotimes (_ height) (insert row))
      (buffer-string))))

(defun slothbar-backend-wayland--clear-frame (surface frame)
  "Clear FRAME in place with SURFACE's background color."
  (let* ((width (slothbar-backend-wayland-surface-frame-width surface))
         (height (slothbar-backend-wayland-surface-frame-height surface))
         (stride (slothbar-backend-wayland-surface-frame-stride surface))
         (row (slothbar-backend-wayland--row
               width (slothbar--background-color))))
    (dotimes (y height)
      (store-substring frame (* y stride) row))
    frame))

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

(defun slothbar-backend-wayland--draw-ops
    (surface module ops module-x module-y)
  "Draw MODULE's OPS into SURFACE at MODULE-X, MODULE-Y."
  (let* ((backend (slothbar-backend-wayland-surface-backend surface))
         (scale (slothbar-backend-wayland-surface-scale surface))
         (frame (slothbar-backend-wayland-surface-frame surface))
         (stride (slothbar-backend-wayland-surface-frame-stride surface))
         (frame-width (slothbar-backend-wayland-surface-frame-width surface))
         (frame-height (slothbar-backend-wayland-surface-frame-height surface))
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
    (surface module placement-x placement-y)
  "Draw MODULE into SURFACE at PLACEMENT-X, PLACEMENT-Y."
  (let* ((scale (slothbar-backend-wayland-surface-scale surface))
         (width (* scale (slothbar-module-width module)))
         (height (* scale (slothbar-backend-wayland-surface-logical-height surface)))
         (colors (slothbar-module-colors module))
         (background (or (slothbar-module-rgb-background-color colors)
                         (slothbar--background-color))))
    (slothbar-backend-wayland--fill-rect
     (slothbar-backend-wayland-surface-frame surface)
     (slothbar-backend-wayland-surface-frame-stride surface)
     (* scale placement-x) (* scale placement-y)
     width height background)
    (slothbar-backend-wayland--draw-ops
     surface module
     (gethash module (slothbar-backend-wayland-surface-module-ops surface))
     placement-x placement-y)))

(defun slothbar-backend-wayland--compose-frame (surface layout)
  "Clear and redraw SURFACE's frame with LAYOUT."
  (let ((frame (or (slothbar-backend-wayland-surface-frame surface)
                   (slothbar-backend-wayland--blank-frame surface))))
    (slothbar-backend-wayland--clear-frame surface frame)
    (dolist (placement layout)
      (pcase-let ((`((,x ,y) ,module) placement))
        (when (slothbar-module-p module)
          (slothbar-backend-wayland--draw-module surface module x y))))
    (setf (slothbar-backend-wayland-surface-frame surface) frame)
    frame))

;;; Shm buffers

(defun slothbar-backend-wayland--release-listener (surface)
  "Return a buffer-release listener for SURFACE."
  (lambda (_object _arguments)
    (when (slothbar-backend-wayland-surface-pending surface)
      (setf (slothbar-backend-wayland-surface-pending surface) nil)
      (run-at-time
       0 nil
       (lambda ()
         (when (slothbar-enabled-p)
           (slothbar-refresh-modules)))))))

(defun slothbar-backend-wayland--rebuild-frame (surface)
  "Create a pool, two buffers, and a blank frame for SURFACE."
  (let* ((backend (slothbar-backend-wayland-surface-backend surface))
         (scale (slothbar-backend-wayland-surface-scale surface))
         (logical-width (slothbar-backend-wayland-surface-logical-width surface))
         (logical-height (slothbar-backend-wayland-surface-logical-height surface))
         (width (* logical-width scale))
         (height (* logical-height scale))
         (stride (* width 4))
         (bytes (* stride height))
         (old-pool (slothbar-backend-wayland-surface-pool surface))
         (old-buffers (slothbar-backend-wayland-surface-buffers surface))
         (shm (slothbar-backend-wayland-shm backend)))
    (when old-pool
      (push (cons old-pool old-buffers)
            (slothbar-backend-wayland-surface-retired surface)))
    (let* ((pool (elwl-shm-create-pool shm (* bytes 2)))
           (buffers (list (elwl-shm-pool-create-buffer
                           pool width height 'argb8888 0)
                          (elwl-shm-pool-create-buffer
                           pool width height 'argb8888 bytes))))
      (dolist (buffer buffers)
        (elwl-object-add-listener
         (elwl-shm-buffer-object buffer) 'release
         (slothbar-backend-wayland--release-listener surface)))
      (setf (slothbar-backend-wayland-surface-pool surface) pool
            (slothbar-backend-wayland-surface-buffers surface) buffers
            (slothbar-backend-wayland-surface-frame-width surface) width
            (slothbar-backend-wayland-surface-frame-height surface) height
            (slothbar-backend-wayland-surface-frame-stride surface) stride
            (slothbar-backend-wayland-surface-frame surface)
            (slothbar-backend-wayland--blank-frame surface)
            (slothbar-backend-wayland-surface-rendered surface) nil
            (slothbar-backend-wayland-surface-presented surface) nil
            (slothbar-backend-wayland-surface-pending surface) nil))
    surface))

(defun slothbar-backend-wayland--destroy-pool (pool buffers)
  "Destroy BUFFERS and POOL."
  (dolist (buffer buffers)
    (when (elwl-shm-buffer-p buffer)
      (elwl-shm-buffer-destroy buffer)))
  (when (elwl-shm-pool-p pool)
    (elwl-shm-pool-destroy pool)))

(defun slothbar-backend-wayland--cleanup-retired (surface)
  "Destroy SURFACE's retired pools after their buffers are no longer needed."
  (let ((retired (slothbar-backend-wayland-surface-retired surface)))
    (while retired
      (let ((pair (pop retired)))
        (slothbar-backend-wayland--destroy-pool (car pair) (cdr pair))))
    (setf (slothbar-backend-wayland-surface-retired surface) nil)))

(defun slothbar-backend-wayland--present-frame (surface frame)
  "Attach and commit FRAME on SURFACE.
Return non-nil if a buffer was presented."
  (let ((buffer (cl-find-if #'elwl-shm-buffer-free-p
                            (slothbar-backend-wayland-surface-buffers surface))))
    (if (not buffer)
        (progn
          (setf (slothbar-backend-wayland-surface-pending surface) t)
          nil)
      (elwl-shm-buffer-acquire buffer)
      (condition-case err
          (progn
            (elwl-shm-buffer-write buffer frame)
            (elwl-surface-attach (slothbar-backend-wayland-surface-surface surface)
                                 (elwl-shm-buffer-object buffer))
            (elwl-surface-damage
             (slothbar-backend-wayland-surface-surface surface)
             0 0
             (slothbar-backend-wayland-surface-logical-width surface)
             (slothbar-backend-wayland-surface-logical-height surface))
            (elwl-surface-commit (slothbar-backend-wayland-surface-surface surface))
            (elwl-shm-buffer-submit buffer)
            (slothbar-backend-wayland--cleanup-retired surface)
            (setf (slothbar-backend-wayland-surface-pending surface) nil
                  (slothbar-backend-wayland-surface-presented surface) t)
            t)
        (error
         (when (elwl-shm-buffer-leased-p buffer)
           (ignore-errors (elwl-shm-buffer-unlease buffer)))
         (signal (car err) (cdr err)))))))

;;; Layer-shell configuration

(defun slothbar-backend-wayland--exclusive-zone (surface)
  "Return SURFACE's exclusive zone."
  (+ (slothbar-backend-wayland-surface-logical-height surface)
     slothbar-margin-y))

(defun slothbar-backend-wayland--apply-layer-state (surface visible)
  "Apply size, anchors, margins, and zone to SURFACE's layer surface.
VISIBLE non-nil reserves the bar's exclusive zone."
  (let ((layer (slothbar-backend-wayland-surface-layer-surface surface)))
    (elwl-layer-surface-set-size
     layer 0 (slothbar-backend-wayland-surface-logical-height surface))
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
     layer (if visible (slothbar-backend-wayland--exclusive-zone surface) 0))
    layer))

(defun slothbar-backend-wayland--on-configure (surface arguments)
  "Handle a layer-surface configure for SURFACE."
  (let* ((bar (slothbar-backend-wayland-surface-bar surface))
         (serial (alist-get 'serial arguments))
         (width (alist-get 'width arguments))
         (height (alist-get 'height arguments))
         (initial (null (slothbar-backend-wayland-surface-configured-serial surface)))
         (new-width (if (and (integerp width) (> width 0))
                        width
                      (slothbar-backend-wayland-surface-logical-width surface))))
    (elwl-layer-surface-ack-configure
     (slothbar-backend-wayland-surface-layer-surface surface) serial)
    (setf (slothbar-backend-wayland-surface-configured-serial surface) serial)
    (when (and height (> height 0)
               (/= height (slothbar-backend-wayland-surface-logical-height surface)))
      (slothbar--log-debug*
       "wayland configure height %s differs from slothbar-height %s"
       height (slothbar-backend-wayland-surface-logical-height surface)))
    (when (or initial
              (/= new-width (slothbar-backend-wayland-surface-logical-width surface)))
      (setf (slothbar-backend-wayland-surface-logical-width surface) new-width)
      (when bar
        (setf (slothbar-pixel-width bar) new-width))
      (when (eq bar slothbar--bar)
        (setq slothbar-width new-width))
      (slothbar-backend-wayland--rebuild-frame surface)
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
  "Open BACKEND's shared Wayland connection and protocol managers."
  (condition-case err
      (let* ((protocols (list (elwl-protocol-load-wayland)
                              (elwl-protocol-load-layer-shell)
                              (elwl-protocol-load-ext-workspace)
                              (elwl-protocol-load-wlr-foreign-toplevel)))
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
          (when (elwl-registry-find registry 'wl-seat)
            (let ((seat (elwl-seat-bind registry)))
              (setf (slothbar-backend-wayland-seat backend) seat)
              ;; The initial capability list arrives on the next roundtrip.
              (elwl-object-add-listener
               (elwl-seat-object seat) 'capabilities
               (lambda (_object _arguments)
                 (slothbar-backend-wayland--ensure-pointer backend)))))
          (let ((outputs (elwl-output-sync registry)))
            ;; Names and scale arrive as events.
            (elwl-roundtrip connection)
            (slothbar-backend-wayland--ensure-pointer backend)
            (setf (slothbar-backend-wayland-outputs backend) outputs)
            (elwl-output-watch
             registry
             (lambda (_action _output)
               ;; Never dispatch recursively from this callback.  Let the
               ;; registry finish delivering output events, then reconcile.
               (run-at-time
                0.1 nil
                (lambda ()
                  (when (slothbar-backend-wayland-connection backend)
                    (setf (slothbar-backend-wayland-outputs backend)
                          (elwl-output-list registry))
                    (run-hook-with-args
                     'slothbar-backend-output-change-hook backend)))))))
          (when (elwl-registry-find registry 'zwlr-foreign-toplevel-manager-v1)
            (let ((manager
                   (elwl-foreign-toplevel-manager-bind
                    registry nil
                    (lambda (_manager)
                      (condition-case hook-err
                          (run-hook-with-args
                           'slothbar-backend-focused-window-change-hook
                           backend)
                        (error
                         (slothbar--log-debug*
                          "focused-window change hook error: %S"
                          hook-err)))))))
              (setf (slothbar-backend-wayland-foreign-toplevel-manager backend)
                    manager)
              (elwl-roundtrip connection)))
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
          backend))
    (error
     (ignore-errors (slothbar-backend-close backend))
     (signal (car err) (cdr err)))))

(cl-defmethod slothbar-backend-create-bar
    ((backend slothbar-backend-wayland) (bar slothbar))
  "Create BAR's layer surface on BACKEND."
  (let* ((outputs (slothbar-backend-wayland-outputs backend))
         (requested (slothbar-output bar))
         (output (cond
                  ((and (stringp requested)
                        (cl-find requested outputs
                                 :key #'elwl-output-name :test #'equal))
                   (cl-find requested outputs
                            :key #'elwl-output-name :test #'equal))
                  ((elwl-output-p requested) requested)
                  (t (slothbar-backend-wayland--select-output outputs))))
         (scale (or (and output (elwl-output-scale output)) 1))
         (surface (elwl-compositor-create-surface
                   (slothbar-backend-wayland-compositor backend)))
         (layer (elwl-layer-shell-create
                 (slothbar-backend-wayland-layer-shell backend)
                 surface
                 :layer 'top
                 :namespace "slothbar"
                 :output (and output (elwl-output-object output))))
         (state (slothbar-backend-wayland-surface--make
                 :backend backend
                 :bar bar
                 :output output
                 :surface surface
                 :layer-surface layer
                 :scale scale
                 :logical-width (max 1 (or (slothbar-pixel-width bar)
                                           slothbar-width 1))
                 :logical-height (max 1 (or (slothbar-pixel-height bar)
                                            slothbar-height)))))
    (setf (slothbar-output bar) output
          (slothbar-surface bar) state)
    (push bar (slothbar-backend-bars backend))
    (when (>= (elwl-object-version surface) 3)
      (elwl-surface-set-buffer-scale surface scale))
    (elwl-object-add-listener
     layer 'configure
     (lambda (_object arguments)
       (slothbar-backend-wayland--on-configure state arguments)))
    (slothbar-backend-wayland--rebuild-frame state)
    (slothbar-backend-wayland--apply-layer-state state nil)
    (elwl-surface-commit surface)
    (unless (slothbar-backend-wayland--wait-for
             backend
             (lambda ()
               (slothbar-backend-wayland-surface-configured-serial state))
             5)
      (error "Timed out waiting for a layer-surface configure"))
    state))

(cl-defmethod slothbar-backend-destroy-bar
    ((backend slothbar-backend-wayland) (bar slothbar))
  "Destroy BAR's layer surface and buffers on BACKEND."
  (let ((surface (slothbar-surface bar)))
    (when surface
      (when (slothbar-backend-wayland-surface-layer-surface surface)
        (ignore-errors
          (elwl-layer-surface-destroy
           (slothbar-backend-wayland-surface-layer-surface surface))))
      (when (slothbar-backend-wayland-surface-surface surface)
        (ignore-errors
          (elwl-surface-destroy
           (slothbar-backend-wayland-surface-surface surface))))
      (ignore-errors
        (slothbar-backend-wayland--destroy-pool
         (slothbar-backend-wayland-surface-pool surface)
         (slothbar-backend-wayland-surface-buffers surface)))
      (let ((retired (slothbar-backend-wayland-surface-retired surface)))
        (while retired
          (let ((pair (pop retired)))
            (ignore-errors
              (slothbar-backend-wayland--destroy-pool (car pair) (cdr pair)))))))
    (setf (slothbar-surface bar) nil
          (slothbar-backend-bars backend)
          (delq bar (slothbar-backend-bars backend)))))

(cl-defmethod slothbar-backend-close ((backend slothbar-backend-wayland))
  "Tear down BACKEND's shared Wayland objects and connection."
  (when (slothbar-backend-wayland-ext-workspace-manager backend)
    (ignore-errors
      (elwl-workspace-manager-stop
       (slothbar-backend-wayland-ext-workspace-manager backend))))
  (when (slothbar-backend-wayland-foreign-toplevel-manager backend)
    (ignore-errors
      (elwl-foreign-toplevel-manager-stop
       (slothbar-backend-wayland-foreign-toplevel-manager backend))))
  (when (slothbar-backend-wayland-pointer backend)
    (ignore-errors
      (elwl-pointer-release (slothbar-backend-wayland-pointer backend))))
  (when (slothbar-backend-wayland-seat backend)
    (ignore-errors
      (elwl-seat-release (slothbar-backend-wayland-seat backend))))
  (when (slothbar-backend-wayland-connection backend)
    (ignore-errors
      (elwl-disconnect (slothbar-backend-wayland-connection backend))))
  (setf (slothbar-backend-wayland-connection backend) nil
        (slothbar-backend-wayland-registry backend) nil
        (slothbar-backend-wayland-compositor backend) nil
        (slothbar-backend-wayland-shm backend) nil
        (slothbar-backend-wayland-layer-shell backend) nil
        (slothbar-backend-wayland-outputs backend) nil
        (slothbar-backend-wayland-ext-workspace-manager backend) nil
        (slothbar-backend-wayland-foreign-toplevel-manager backend) nil
        (slothbar-backend-wayland-seat backend) nil
        (slothbar-backend-wayland-pointer backend) nil
        (slothbar-backend-wayland-pointer-surface backend) nil
        (slothbar-backend-wayland-pointer-x backend) nil
        (slothbar-backend-wayland-pointer-y backend) nil)
  nil)

(cl-defmethod slothbar-backend-show
    ((_backend slothbar-backend-wayland) (bar slothbar))
  "Show BAR and reserve its exclusive zone."
  (when-let ((surface (slothbar-surface bar)))
    (slothbar-backend-wayland--apply-layer-state surface t)
    (if (and (slothbar-backend-wayland-surface-rendered surface)
             (slothbar-backend-wayland-surface-frame surface))
        (slothbar-backend-wayland--present-frame
         surface (slothbar-backend-wayland-surface-frame surface))
      (elwl-surface-commit
       (slothbar-backend-wayland-surface-surface surface)))))

(cl-defmethod slothbar-backend-hide
    ((_backend slothbar-backend-wayland) (bar slothbar))
  "Hide BAR and release its exclusive zone."
  (when-let ((surface (slothbar-surface bar)))
    (setf (slothbar-backend-wayland-surface-pending surface) nil
          (slothbar-backend-wayland-surface-presented surface) nil)
    (elwl-layer-surface-set-exclusive-zone
     (slothbar-backend-wayland-surface-layer-surface surface) 0)
    (elwl-surface-attach
     (slothbar-backend-wayland-surface-surface surface) nil)
    (elwl-surface-commit
     (slothbar-backend-wayland-surface-surface surface))))

(defun slothbar-backend-wayland--module-surface (module)
  "Return the Wayland surface state owning MODULE."
  (let ((bar (slothbar-module--bar module)))
    (and bar (slothbar-surface bar))))

(cl-defmethod slothbar-backend-add-module
    ((_backend slothbar-backend-wayland) (module slothbar-module))
  "Register MODULE with BACKEND."
  (when-let ((surface (slothbar-backend-wayland--module-surface module)))
    (puthash module nil
             (slothbar-backend-wayland-surface-module-ops surface)))
  module)

(cl-defmethod slothbar-backend-remove-module
    ((_backend slothbar-backend-wayland) (module slothbar-module))
  "Forget MODULE in BACKEND."
  (when-let ((surface (slothbar-backend-wayland--module-surface module)))
    (remhash module
             (slothbar-backend-wayland-surface-module-ops surface))))

(cl-defmethod slothbar-backend-draw-module
    ((_backend slothbar-backend-wayland) (module slothbar-module))
  "Compute MODULE's draw operations for BACKEND."
  (when-let ((surface (slothbar-backend-wayland--module-surface module)))
    (puthash module
             (slothbar-text-draw-ops (slothbar-module-text-layout module))
             (slothbar-backend-wayland-surface-module-ops surface))))

(cl-defmethod slothbar-backend-reset-module-cache
    ((_backend slothbar-backend-wayland) (module slothbar-module))
  "Forget MODULE's cached draw operations in BACKEND."
  (when-let ((surface (slothbar-backend-wayland--module-surface module)))
    (remhash module
             (slothbar-backend-wayland-surface-module-ops surface))))

(cl-defmethod slothbar-backend-present
    ((_backend slothbar-backend-wayland) (bar slothbar) layout damage)
  "Compose LAYOUT into BAR's frame and present it when visible."
  (ignore damage)
  (when-let ((surface (slothbar-surface bar)))
    (let ((frame (slothbar-backend-wayland--compose-frame surface layout)))
      (setf (slothbar-backend-wayland-surface-rendered surface) t)
      (when (slothbar-visible bar)
        (slothbar-backend-wayland--present-frame surface frame)))))

(provide 'slothbar-backend-wayland)
;;; slothbar-backend-wayland.el ends here
