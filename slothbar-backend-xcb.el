;;; slothbar-backend-xcb.el --- X11 display backend for slothbar  -*- lexical-binding: t -*-

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
;; The xcb display backend: owns the X connection, the dock window, the
;; background pixmap, and each module's pixmap/GC/glyphset.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'pcase)

(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)
(require 'xcb-keysyms)
(require 'fontsloth-layout)

(require 'slothbar)
(require 'slothbar-backend)
(require 'slothbar-color)
(require 'slothbar-font)
(require 'slothbar-log)
(require 'slothbar-module)
(require 'slothbar-draw-op)
(require 'slothbar-render-xcb)

(cl-defstruct
    (slothbar-backend-xcb
     (:include slothbar-backend)
     (:constructor slothbar-backend-xcb--make)
     (:copier nil))
  "An xcb display backend."
  connection)

(cl-defstruct
    (slothbar-backend-xcb-surface
     (:constructor slothbar-backend-xcb-surface--make)
     (:copier nil))
  "Per-bar X window and pixel resources."
  backend
  bar
  window
  gc
  pixmap
  (module-surfaces (make-hash-table :test 'eq) :type 'hash-table))

(cl-defstruct
    (slothbar-backend-xcb--module-surface
     (:constructor slothbar-backend-xcb--module-surface-create)
     (:copier nil))
  "Pixel resources for one module on the xcb backend."
  pixmap
  gc
  gs
  cache
  width)

(defun slothbar-backend-xcb--current-surface ()
  "Return the current bar's xcb surface, if any."
  (let ((bar slothbar--bar))
    (and bar (slothbar-surface bar))))

(defun slothbar-backend-xcb-window (_backend)
  "Return BACKEND's current bar window id."
  (let ((surface (slothbar-backend-xcb--current-surface)))
    (and surface (slothbar-backend-xcb-surface-window surface))))

(defun slothbar-backend-xcb-gc (_backend)
  "Return BACKEND's current bar GC id."
  (let ((surface (slothbar-backend-xcb--current-surface)))
    (and surface (slothbar-backend-xcb-surface-gc surface))))

(defun slothbar-backend-xcb-pixmap (_backend)
  "Return BACKEND's current bar background pixmap id."
  (let ((surface (slothbar-backend-xcb--current-surface)))
    (and surface (slothbar-backend-xcb-surface-pixmap surface))))

(defun slothbar-backend-xcb-module-surfaces (_backend)
  "Return BACKEND's current bar module-surface table."
  (let ((surface (slothbar-backend-xcb--current-surface)))
    (and surface (slothbar-backend-xcb-surface-module-surfaces surface))))

(defun slothbar-backend-xcb-create ()
  "Create an unopened xcb backend."
  (slothbar-backend-xcb--make :kind 'xcb))

(cl-defmethod slothbar-backend-supports-p
  ((_backend slothbar-backend-xcb) feature)
  "The xcb backend supports X-specific features."
  (memq feature '(system-tray ewmh randr pointer-input focused-window)))

(cl-defmethod slothbar-backend-ext-workspace-manager
  ((_backend slothbar-backend-xcb))
  "The xcb backend has no ext-workspace manager."
  nil)

(cl-defmethod slothbar-backend-output ((_backend slothbar-backend-xcb))
  "The xcb backend has no Wayland output."
  nil)

(cl-defmethod slothbar-backend-output-names ((_backend slothbar-backend-xcb))
  "Return the X monitor names reported by Emacs."
  (delq nil
        (mapcar (lambda (monitor) (alist-get 'name monitor))
                (display-monitor-attributes-list))))

(cl-defmethod slothbar-backend-focused-window ((backend slothbar-backend-xcb))
  "Return the active EWMH window's title and class."
  (let ((connection (slothbar-backend-xcb-connection backend))
        (root (slothbar-backend-xcb--root-window-id backend)))
    (when (and connection root)
      (let ((active (ignore-errors
                      (slot-value
                       (xcb:+request-unchecked+reply
                           connection
                         (make-instance 'xcb:ewmh:get-_NET_ACTIVE_WINDOW
                                        :window root))
                       'value))))
        (when (and active (/= active 0))
          (let* ((title
                  (or (ignore-errors
                        (slot-value
                         (xcb:+request-unchecked+reply
                             connection
                           (make-instance 'xcb:ewmh:get-_NET_WM_NAME
                                          :window active))
                         'value))
                      (ignore-errors
                        (slot-value
                         (xcb:+request-unchecked+reply
                             connection
                           (make-instance 'xcb:icccm:get-WM_NAME
                                          :window active))
                         'value))))
                 (class-reply
                  (ignore-errors
                    (xcb:+request-unchecked+reply
                        connection
                      (make-instance 'xcb:icccm:get-WM_CLASS
                                     :window active))))
                 (app-id (or (and class-reply
                                  (slot-value class-reply 'class-name))
                             (and class-reply
                                  (slot-value class-reply 'instance-name)))))
            (list :title title :app-id app-id :icon nil)))))))

;;; Accessors for X-only modules

(defun slothbar-xcb-connection ()
  "Return the active xcb connection, or nil."
  (when (slothbar-backend-xcb-p slothbar--backend)
    (slothbar-backend-xcb-connection slothbar--backend)))

(defun slothbar-xcb-window ()
  "Return the active slothbar X window, or nil."
  (let ((surface (and slothbar--bar (slothbar-surface slothbar--bar))))
    (and surface (slothbar-backend-xcb-surface-window surface))))

(defun slothbar-xcb-root-window-id ()
  "Return the root window id of the active xcb connection, or nil."
  (let ((backend slothbar--backend))
    (when (slothbar-backend-xcb-p backend)
      (slothbar-backend-xcb--root-window-id backend))))

(defun slothbar-backend-xcb--root-window-id (backend)
  "Return BACKEND's root window id."
  (slot-value (car (slot-value
                    (xcb:get-setup (slothbar-backend-xcb-connection backend))
                    'roots))
              'root))

;;; Colors

(defun slothbar-backend-xcb--background-pixel ()
  "Return the current background color as an X pixel."
  (slothbar-render-color->pixel (slothbar--background-color)))

(defun slothbar-backend-xcb--foreground-pixel ()
  "Return the current foreground color as an X pixel."
  (slothbar-render-color->pixel (slothbar--foreground-color)))

;;; Struts and window geometry

(defun slothbar-backend-xcb--surface (bar)
  "Return BAR's xcb surface."
  (and bar (slothbar-surface bar)))

(defun slothbar-backend-xcb--do-struts
    (backend bar top bottom left top-start-x top-end-x
             bottom-start-x bottom-end-x)
  "Do requests for struts on BAR's window.
TOP, BOTTOM, LEFT are the strut sizes. TOP-START-X and TOP-END-X delimit
the top strut; BOTTOM-START-X and BOTTOM-END-X delimit the bottom strut."
  (let ((connection (slothbar-backend-xcb-connection backend))
        (surface (slothbar-backend-xcb--surface bar)))
    (when surface
      (let ((window (slothbar-backend-xcb-surface-window surface)))
        (xcb:+request connection
            (make-instance 'xcb:ewmh:set-_NET_WM_STRUT
                           :window window
                           :left left
                           :right 0
                           :top top
                           :bottom bottom))
        (xcb:+request connection
            (make-instance 'xcb:ewmh:set-_NET_WM_STRUT_PARTIAL
                           :window window
                           :left left
                           :right 0
                           :top top
                           :bottom bottom
                           :left-start-y 0
                           :left-end-y 0
                           :right-start-y 0
                           :right-end-y 0
                           :top-start-x top-start-x
                           :top-end-x top-end-x
                           :bottom-start-x bottom-start-x
                           :bottom-end-x bottom-end-x))))))

(defun slothbar-backend-xcb--configure-struts (backend bar &optional hide?)
  "Configure BAR's struts on BACKEND.
If HIDE? is non-nil, configure struts to make the bar invisible."
  (if hide?
      (slothbar-backend-xcb--do-struts backend bar 0 0 0 0 0 0 0)
    (pcase-let* (((eieio (height root-window-height))
                  (xcb:+request-unchecked+reply
                      (slothbar-backend-xcb-connection backend)
                      (make-instance 'xcb:GetGeometry
                                     :drawable
                                     (slothbar-backend-xcb--root-window-id
                                      backend))))
                 ((map ('y-offset mon-y-offset) ('height mon-height))
                  (slothbar--find-display-geometry
                   (or (slothbar-output bar) slothbar-preferred-display)))
                 (bottom-strut (if slothbar-is-bottom
                                   (+ slothbar-height slothbar-margin-y
                                      (- root-window-height
                                         (+ mon-y-offset mon-height)))
                                 0)))
      (slothbar-backend-xcb--do-struts
       backend bar
       (if slothbar-is-bottom 0
         (+ slothbar-height slothbar-offset-y slothbar-margin-y))
       bottom-strut
       slothbar-offset-x
       (if slothbar-is-bottom 0
         slothbar-offset-x)
       (if slothbar-is-bottom 0
         (1- (+ slothbar-offset-x slothbar-width)))
       (if slothbar-is-bottom slothbar-offset-x
         0)
       (if slothbar-is-bottom (1- (+ slothbar-offset-x slothbar-width))
         0)))))

(defun slothbar-backend-xcb--configure-geom (backend bar)
  "Configure BAR's window geometry on BACKEND."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (let ((ecw (xcb:+request-checked+request-check
                   (slothbar-backend-xcb-connection backend)
                 (make-instance 'xcb:ConfigureWindow
                                :window
                                (slothbar-backend-xcb-surface-window surface)
                                :value-mask (logior xcb:ConfigWindow:X
                                                    xcb:ConfigWindow:Y
                                                    xcb:ConfigWindow:Width
                                                    xcb:ConfigWindow:Height)
                                :x slothbar-offset-x
                                :y slothbar-offset-y
                                :width slothbar-width
                                :height slothbar-height))))
      (slothbar--log-debug*
       "slothbar-refresh: configure window errors: %s" ecw))))

(defun slothbar-backend-xcb--configure-pos (backend bar)
  "Configure BAR's window position on BACKEND."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (let ((ecw (xcb:+request-checked+request-check
                   (slothbar-backend-xcb-connection backend)
                 (make-instance 'xcb:ConfigureWindow
                                :window
                                (slothbar-backend-xcb-surface-window surface)
                                :value-mask (logior xcb:ConfigWindow:X
                                                    xcb:ConfigWindow:Y)
                                :x slothbar-offset-x
                                :y slothbar-offset-y))))
      (slothbar--log-debug*
       "slothbar-refresh: configure window errors: %s" ecw))))

(defun slothbar-backend-xcb--change-background (backend bar &optional color)
  "Change BAR's background pixel attribute with optional COLOR."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (let ((connection (slothbar-backend-xcb-connection backend))
          (window (slothbar-backend-xcb-surface-window surface))
          (gc (slothbar-backend-xcb-surface-gc surface))
          (pixmap (slothbar-backend-xcb-surface-pixmap surface)))
      (xcb:+request connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window window
                         :value-mask (logior xcb:CW:BackPixmap
                                             xcb:CW:BackPixel)
                         :background-pixmap xcb:BackPixmap:ParentRelative
                         :background-pixel
                         (or color (slothbar-backend-xcb--background-pixel))))
      (xcb:+request-checked+request-check connection
          (make-instance 'xcb:ChangeGC
                         :gc gc
                         :value-mask (logior xcb:GC:Background
                                             xcb:GC:Foreground)
                         :background (slothbar-backend-xcb--background-pixel)
                         :foreground (slothbar-backend-xcb--background-pixel)))
      (slothbar-render-fill-rectangle
       connection gc pixmap slothbar-width slothbar-height))))

(defun slothbar-backend-xcb--configure-wm-hints (backend bar)
  "Configure BAR's icccm and ewmh hints on BACKEND."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (let ((connection (slothbar-backend-xcb-connection backend))
          (window (slothbar-backend-xcb-surface-window surface)))
      (xcb:+request-checked connection
          (make-instance 'xcb:icccm:set-WM_SIZE_HINTS
                         :window window
                         :data (xcb:icccm:-WM_SIZE_HINTS
                                :x slothbar-offset-x
                                :y slothbar-offset-y
                                :width slothbar-width
                                :height slothbar-height)))
      (xcb:+request-checked connection
          (make-instance 'xcb:ewmh:set-_NET_WM_DESKTOP
                         :window window
                         :data #xFFFFFFFF)))))

;;; Showing and hiding

(cl-defmethod slothbar-backend-show
    ((backend slothbar-backend-xcb) (bar slothbar))
  "Map BAR and reserve its space."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (slothbar-backend-xcb--configure-geom backend bar)
    (slothbar-backend-xcb--change-background backend bar)
    (slothbar-backend-xcb--configure-struts backend bar)
    (slothbar-backend-xcb--configure-wm-hints backend bar)
    (let ((connection (slothbar-backend-xcb-connection backend)))
      (xcb:+request connection
          (make-instance 'xcb:MapWindow
                         :window
                         (slothbar-backend-xcb-surface-window surface)))
      (slothbar-backend-xcb--configure-pos backend bar)
      (xcb:flush connection))))

(cl-defmethod slothbar-backend-hide
    ((backend slothbar-backend-xcb) (bar slothbar))
  "Unmap BAR and release its reserved space."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (slothbar-backend-xcb--configure-struts backend bar t)
    (let ((connection (slothbar-backend-xcb-connection backend)))
      (xcb:+request connection
          (make-instance 'xcb:UnmapWindow
                         :window
                         (slothbar-backend-xcb-surface-window surface)))
      (xcb:flush connection))))

;;; Presentation

(cl-defmethod slothbar-backend-present
    ((backend slothbar-backend-xcb) (bar slothbar) layout damage)
  "Draw LAYOUT into BAR and update its X window."
  (when-let ((surface (slothbar-backend-xcb--surface bar)))
    (let ((connection (slothbar-backend-xcb-connection backend))
          (window (slothbar-backend-xcb-surface-window surface))
          (gc (slothbar-backend-xcb-surface-gc surface))
          (pixmap (slothbar-backend-xcb-surface-pixmap surface))
          (module-surfaces
           (slothbar-backend-xcb-surface-module-surfaces surface)))
      (dolist (segment damage)
        (pcase-let ((`(,left ,right) segment))
          (xcb:+request connection
              (make-instance 'xcb:ClearArea
                             :exposures 0
                             :window window
                             :x left :y 0
                             :width (- right left)
                             :height slothbar-height))))
      (xcb:+request connection
          (make-instance 'xcb:CopyArea
                         :src-drawable pixmap
                         :dst-drawable window
                         :gc gc
                         :src-x 0 :src-y 0 :dst-x 0 :dst-y 0
                         :width slothbar-width :height slothbar-height))
      (dolist (placement layout)
        (pcase-let* ((`((,x ,y) ,m) placement)
                     (module-surface (gethash m module-surfaces)))
          (when (and module-surface
                     (slothbar-backend-xcb--module-surface-pixmap
                      module-surface))
            (xcb:+request connection
                (make-instance 'xcb:CopyArea
                               :src-drawable
                               (slothbar-backend-xcb--module-surface-pixmap
                                module-surface)
                               :dst-drawable window
                               :gc (slothbar-backend-xcb--module-surface-gc
                                    module-surface)
                               :src-x 0 :src-y 0 :dst-x x :dst-y y
                               :width (slothbar-module-width m)
                               :height slothbar-height)))
          (slothbar-module-reposition m x y)))
      (xcb:flush connection))))

;;; Module pixel resources

(defun slothbar-backend-xcb--module-surface-table (module)
  "Return MODULE's owning bar surface table, if any."
  (let ((bar (slothbar-module--bar module)))
    (when-let ((surface (and bar (slothbar-surface bar))))
      (slothbar-backend-xcb-surface-module-surfaces surface))))

(cl-defmethod slothbar-backend-add-module
    ((backend slothbar-backend-xcb) (module slothbar-module))
  "Give BACKEND the pixel resources MODULE needs to draw."
  (when-let ((table (slothbar-backend-xcb--module-surface-table module)))
    (pcase-let* ((bar (slothbar-module--bar module))
           (bar-surface (slothbar-surface bar))
           (connection (slothbar-backend-xcb-connection backend))
           (pixmap (xcb:generate-id connection))
           (gc (xcb:generate-id connection))
           ((cl-struct slothbar-module width colors) module)
           (background-pixel
            (slothbar-render-color->pixel
             (slothbar-module-rgb-background-color colors))))
      (slothbar-render-create-pixmap connection pixmap width slothbar-height)
      (let ((egc
             (xcb:+request-checked+request-check connection
                 (make-instance 'xcb:CreateGC
                                :cid gc
                                :drawable
                                (slothbar-backend-xcb-surface-window
                                 bar-surface)
                                :value-mask (logior xcb:GC:Background
                                                    xcb:GC:Foreground
                                                    xcb:GC:GraphicsExposures)
                                :background background-pixel
                                :foreground background-pixel
                                :graphics-exposures 0))))
        (slothbar--log-debug* "create %s gc, errors %s"
                              (slothbar-module-name module) egc))
      (slothbar-render-fill-rectangle connection gc pixmap
                                      width slothbar-height)
      (puthash module
               (slothbar-backend-xcb--module-surface-create
                :pixmap pixmap
                :gc gc
                :gs (slothbar-render-create-glyphset connection)
                :cache (make-hash-table :test 'equal)
                :width width)
               table))))

(cl-defmethod slothbar-backend-draw-module
    ((backend slothbar-backend-xcb) (module slothbar-module))
  "Draw MODULE's text layout with BACKEND."
  (let* ((table (slothbar-backend-xcb--module-surface-table module))
         (surface (and table (gethash module table))))
    (when surface
      (slothbar-backend-xcb--prepare-module backend surface module)
      (slothbar-backend-xcb--load-glyphs backend module surface)
      (slothbar-backend-xcb--draw-text backend module surface))))

(cl-defmethod slothbar-backend-reset-module-cache
    ((_backend slothbar-backend-xcb) (module slothbar-module))
  "Discard BACKEND's render cache for MODULE."
  (let* ((table (slothbar-backend-xcb--module-surface-table module))
         (surface (and table (gethash module table))))
    (when surface
      (setf (slothbar-backend-xcb--module-surface-cache surface)
            (make-hash-table :test 'equal)))))

(cl-defmethod slothbar-backend-remove-module
    ((backend slothbar-backend-xcb) (module slothbar-module))
  "Release BACKEND's pixel resources for MODULE."
  (let* ((connection (slothbar-backend-xcb-connection backend))
         (surfaces (slothbar-backend-xcb--module-surface-table module))
         (surface (and surfaces (gethash module surfaces))))
    (when surface
      (when (slothbar-backend-xcb--module-surface-pixmap surface)
        (xcb:+request connection
            (make-instance 'xcb:FreePixmap
                           :pixmap
                           (slothbar-backend-xcb--module-surface-pixmap
                            surface))))
      (when (slothbar-backend-xcb--module-surface-gc surface)
        (xcb:+request connection
            (make-instance 'xcb:FreeGC
                           :gc (slothbar-backend-xcb--module-surface-gc
                                surface))))
      (when (slothbar-backend-xcb--module-surface-gs surface)
        (let ((egs
               (xcb:+request-checked+request-check connection
                   (make-instance 'xcb:render:FreeGlyphSet
                                  :glyphset
                                  (slothbar-backend-xcb--module-surface-gs
                                   surface)))))
          (slothbar--log-debug* "trying to free glyphset, errors %s" egs)))
      (remhash module surfaces))))

(defun slothbar-backend-xcb--prepare-module (backend surface module)
  "Ensure SURFACE's pixmap matches MODULE and clear it before drawing.
A refreshed module may keep the same width while its text shrinks, so the
pixmap is always refilled with the module background."
  (let* ((width (slothbar-module-width module))
         (connection (slothbar-backend-xcb-connection backend))
         (colors (slothbar-module-colors module))
         (background-pixel
          (slothbar-render-color->pixel
           (slothbar-module-rgb-background-color colors)))
         (gc (slothbar-backend-xcb--module-surface-gc surface)))
    (unless (= width (slothbar-backend-xcb--module-surface-width surface))
      (let ((pixmap (slothbar-backend-xcb--module-surface-pixmap surface)))
        (when pixmap
          (xcb:+request connection
              (make-instance 'xcb:FreePixmap :pixmap pixmap)))
        (let ((new-pixmap (xcb:generate-id connection)))
          (slothbar-render-create-pixmap connection new-pixmap
                                         width slothbar-height)
          (setf (slothbar-backend-xcb--module-surface-pixmap surface)
                new-pixmap)
          (setf (slothbar-backend-xcb--module-surface-width surface)
                width))))
    (xcb:+request-checked+request-check connection
        (make-instance 'xcb:ChangeGC
                       :gc gc
                       :value-mask (logior xcb:GC:Background
                                           xcb:GC:Foreground)
                       :background background-pixel
                       :foreground background-pixel))
    (slothbar-render-fill-rectangle
     connection gc
     (slothbar-backend-xcb--module-surface-pixmap surface)
     width slothbar-height)
    surface))

(defun slothbar-backend-xcb--load-glyphs (backend module surface)
  "Load MODULE's glyphs into SURFACE for BACKEND."
  (dolist (pos (slothbar-module-text-layout module))
    (when (fontsloth-layout-glyph-position-p pos)
      (pcase-let* (((cl-struct
                     fontsloth-layout-glyph-position
                     key (parent char-code) (user-data font-idx)) pos)
                   (cache (slothbar-backend-xcb--module-surface-cache surface))
                   (cache-key (gethash char-code cache)))
        (unless (equal cache-key key)
          (let ((font-path (slothbar-font-find font-idx)))
            (when font-path
              (slothbar-render-load-glyph
               (slothbar-backend-xcb-connection backend)
               (slothbar-backend-xcb--module-surface-gs surface)
               (fontsloth-load-font font-path)
               pos)
              (puthash char-code key cache))))))))

(defun slothbar-backend-xcb--draw-text (backend module surface)
  "Draw MODULE's text into SURFACE for BACKEND."
  (let ((pixmap (slothbar-backend-xcb--module-surface-pixmap surface))
        (gs (slothbar-backend-xcb--module-surface-gs surface)))
    (when (and pixmap gs)
      (dolist (op (slothbar-text-draw-ops
                   (slothbar-module-text-layout module)))
        (slothbar-render-draw-text
         (slothbar-backend-xcb-connection backend)
         pixmap gs
         (slothbar-draw-op-glyph-positions op)
         (slothbar-draw-op-color op))))))

;;; Event handling

(defun slothbar-backend-xcb--ensure-event-mask (backend win event)
  "Ensure WIN's event mask has EVENT on BACKEND."
  (let ((connection (slothbar-backend-xcb-connection backend)))
    (with-slots (your-event-mask)
        (xcb:+request-unchecked+reply connection
            (make-instance 'xcb:GetWindowAttributes :window win))
      (xcb:+request-checked+request-check connection
          (make-instance 'xcb:ChangeWindowAttributes
                         :window win
                         :value-mask xcb:CW:EventMask
                         :event-mask (logior event your-event-mask))))))

(defun slothbar-backend-xcb--clear-event-mask (win)
  "Clear the event mask from WIN."
  (xcb:+request-checked+request-check (slothbar-xcb-connection)
      (make-instance 'xcb:ChangeWindowAttributes
                     :window win
                     :value-mask xcb:CW:EventMask
                     :event-mask xcb:EventMask:NoEvent)))

(defun slothbar-backend-xcb--on-DestroyNotify (data _synthetic)
  "DestroyNotify.
DATA the event data"
  (slothbar--log-trace* "received destroynotify %s" data))

(defun slothbar-backend-xcb--on-ReparentNotify (data _synthetic)
  "ReparentNotify.
DATA the event data"
  (slothbar--log-trace* "received reparentnotify %s" data))

(defun slothbar-backend-xcb--on-ResizeRequest (data _synthetic)
  "ResizeRequest.
DATA the event data"
  (slothbar--log-trace* "received resizerequest %s" data))

(defun slothbar-backend-xcb--on-PropertyNotify (data _synthetic)
  "PropertyNotify.
DATA the event data"
  (slothbar--log-trace* "received propertynotify %s" data))

(defun slothbar-backend-xcb--on-ClientMessage (data _synthetic)
  "Handle client messages.
DATA the event data"
  (slothbar--log-trace* "received clientmessage %s" data))

(defun slothbar-backend-xcb--on-KeyPress (data _synthetic)
  "TODO: do something useful with keypress DATA."
  (let ((keypress (make-instance 'xcb:KeyPress)))
    (xcb:unmarshal keypress data)
    (with-slots (detail state) keypress
      (let ((keysym (xcb:keysyms:keycode->keysym
                     (slothbar-xcb-connection) detail state)))
        (slothbar--log-debug* "keypress: %s" keysym)))))

(defun slothbar-backend-xcb--bar-for-window (backend window)
  "Return BACKEND's bar whose X window is WINDOW."
  (cl-find-if
   (lambda (bar)
     (let ((surface (slothbar-surface bar)))
       (and surface
            (eql window (slothbar-backend-xcb-surface-window surface)))))
   (slothbar-backend-bars backend)))

(defun slothbar-backend-xcb--on-MotionNotify (backend data _synthetic)
  "Dispatch pointer motion on BACKEND."
  (let ((event (make-instance 'xcb:MotionNotify)))
    (xcb:unmarshal event data)
    (let ((bar (slothbar-backend-xcb--bar-for-window
                backend (slot-value event 'event))))
      (when bar
        (with-slots (event-x event-y) event
          (condition-case err
              (run-hook-with-args 'slothbar-backend-pointer-motion-hook
                                  backend bar event-x event-y)
            (error
             (slothbar--log-debug*
              "pointer motion hook error: %S" err))))))))

(defun slothbar-backend-xcb--on-LeaveNotify (backend _data _synthetic)
  "Clear pointer hover state on BACKEND."
  (dolist (bar (slothbar-backend-bars backend))
    (condition-case err
        (run-hook-with-args 'slothbar-backend-pointer-motion-hook
                            backend bar nil nil)
      (error
       (slothbar--log-debug*
        "pointer motion hook error: %S" err)))))

(defun slothbar-backend-xcb--on-ButtonPress (backend data _synthetic)
  "Dispatch a pointer button press on BACKEND."
  (let ((event (make-instance 'xcb:ButtonPress)))
    (xcb:unmarshal event data)
    (let ((bar (slothbar-backend-xcb--bar-for-window
                backend (slot-value event 'event))))
      (when bar
        (with-slots (event-x event-y detail) event
          (let ((button (pcase detail
                          (1 :left)
                          (2 :middle)
                          (3 :right))))
            (when button
              (condition-case err
                  (run-hook-with-args
                   'slothbar-backend-pointer-button-hook
                   backend bar event-x event-y button)
                (error
                 (slothbar--log-debug*
                  "pointer button hook error: %S" err))))))))))

(defun slothbar-backend-xcb--on-Expose (data _synthetic)
  "Can draw things after Expose.
DATA the event data"
  (slothbar--log-debug* "slothbar received expose %s" data)
  (ignore data)
  (when slothbar--enabled
    (when slothbar--module-refresh-timer
      (slothbar--log-debug* "slothbar restarting module refresh timer")
      (cancel-timer slothbar--module-refresh-timer)
      (setq slothbar--module-refresh-timer nil))
    (slothbar--start-module-refresh-timer))
  (when (and slothbar--enabled slothbar--geometry-changed?)
    (setq slothbar--geometry-changed? nil)
    (run-at-time 0 nil #'slothbar-refresh-modules)))

;;; Backend lifecycle

(cl-defmethod slothbar-backend-open ((backend slothbar-backend-xcb))
  "Open BACKEND's X connection.  Bar windows are created separately."
  (let ((connection (xcb:connect)))
    (setf (slothbar-backend-xcb-connection backend) connection)
    ;; apparently ewmh initializes icccm automatically
    (xcb:ewmh:init connection)
    (slothbar-backend-xcb--ensure-event-mask
     backend (slothbar-backend-xcb--root-window-id backend)
     xcb:EventMask:PropertyChange)
    (set-process-query-on-exit-flag (slot-value connection 'process) nil)
    ;; Attach event listeners.
    (xcb:+event connection 'xcb:DestroyNotify
                #'slothbar-backend-xcb--on-DestroyNotify)
    (xcb:+event connection 'xcb:ReparentNotify
                #'slothbar-backend-xcb--on-ReparentNotify)
    (xcb:+event connection 'xcb:ResizeRequest
                #'slothbar-backend-xcb--on-ResizeRequest)
    (xcb:+event connection 'xcb:PropertyNotify
                #'slothbar-backend-xcb--on-PropertyNotify)
    (xcb:+event connection 'xcb:ClientMessage
                #'slothbar-backend-xcb--on-ClientMessage)
    (xcb:+event connection 'xcb:KeyPress
                #'slothbar-backend-xcb--on-KeyPress)
    (xcb:+event connection 'xcb:ButtonPress
                (lambda (data synthetic)
                  (slothbar-backend-xcb--on-ButtonPress
                   backend data synthetic)))
    (xcb:+event connection 'xcb:MotionNotify
                (lambda (data synthetic)
                  (slothbar-backend-xcb--on-MotionNotify
                   backend data synthetic)))
    (xcb:+event connection 'xcb:LeaveNotify
                (lambda (data synthetic)
                  (slothbar-backend-xcb--on-LeaveNotify
                   backend data synthetic)))
    (xcb:+event connection 'xcb:Expose
                #'slothbar-backend-xcb--on-Expose)
    backend))

(cl-defmethod slothbar-backend-create-bar
    ((backend slothbar-backend-xcb) (bar slothbar))
  "Create BAR's dock window and pixel resources on BACKEND."
  (let* ((connection (slothbar-backend-xcb-connection backend))
         (background-pixel (slothbar-backend-xcb--background-pixel))
         (id (xcb:generate-id connection))
         (surface (slothbar-backend-xcb-surface--make
                   :backend backend
                   :bar bar
                   :window id
                   :gc (xcb:generate-id connection)
                   :pixmap (xcb:generate-id connection)))
         (parent (slothbar-backend-xcb--root-window-id backend))
         (depth (slot-value (xcb:+request-unchecked+reply
                                connection
                                (make-instance 'xcb:GetGeometry
                                               :drawable parent))
                            'depth)))
    (setf (slothbar-surface bar) surface)
    (push bar (slothbar-backend-bars backend))
    (slothbar--log-debug* "Slothbar window id: %s" id)
    (xcb:+request connection
        (make-instance 'xcb:CreateWindow
                       :depth depth
                       :wid id
                       :parent parent
                       :override-redirect 1
                       :x slothbar-offset-x
                       :y slothbar-offset-y
                       :width 1
                       :height slothbar-height
                       :border-width 1
                       :class xcb:WindowClass:InputOutput
                       :visual 0
                       :value-mask (logior xcb:CW:BackPixmap
                                           (if background-pixel
                                               xcb:CW:BackPixel 0)
                                           xcb:CW:EventMask)
                       :background-pixmap xcb:BackPixmap:ParentRelative
                       :background-pixel background-pixel
                       :event-mask (logior xcb:EventMask:Exposure
                                           xcb:EventMask:KeyPress
                                           xcb:EventMask:ButtonPress
                                           xcb:EventMask:PointerMotion
                                           xcb:EventMask:LeaveWindow
                                           xcb:EventMask:PropertyChange
                                           xcb:EventMask:SubstructureNotify)))
    (xcb:+request connection
        (make-instance 'xcb:icccm:set-WM_NAME
                       :window id
                       :data "slothbar"))
    (xcb:+request connection
        (make-instance 'xcb:icccm:set-WM_CLASS
                       :window id
                       :instance-name "slothbar"
                       :class-name "Slothbar"))
    (xcb:+request connection
        (make-instance 'xcb:ewmh:set-_NET_WM_WINDOW_TYPE
                       :window id
                       :data `(,xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK)))
    (xcb:+request connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window id
                       :data `(,xcb:Atom:_NET_WM_STATE_STICKY
                               ,xcb:Atom:_NET_WM_STATE_ABOVE)))
    (slothbar-render-create-pixmap connection
                                   (slothbar-backend-xcb-surface-pixmap surface)
                                   slothbar-width slothbar-height)
    (let ((egc
           (xcb:+request-checked+request-check connection
               (make-instance 'xcb:CreateGC
                              :cid (slothbar-backend-xcb-surface-gc surface)
                              :drawable id
                              :value-mask (logior xcb:GC:Background
                                                  xcb:GC:Foreground)
                              :background
                              (slothbar-backend-xcb--background-pixel)
                              :foreground
                              (slothbar-backend-xcb--foreground-pixel)))))
      (slothbar--log-debug* "slothbar init create gc errors: %s" egc))
    (slothbar-render-fill-rectangle
     connection
     (slothbar-backend-xcb-surface-gc surface)
     (slothbar-backend-xcb-surface-pixmap surface)
     slothbar-width slothbar-height)
    (xcb:flush connection)
    surface))

(cl-defmethod slothbar-backend-destroy-bar
    ((backend slothbar-backend-xcb) (bar slothbar))
  "Destroy BAR's window and pixel resources on BACKEND."
  (let* ((connection (slothbar-backend-xcb-connection backend))
         (surface (slothbar-surface bar)))
    (when (and connection surface)
      (let ((window (slothbar-backend-xcb-surface-window surface))
            (gc (slothbar-backend-xcb-surface-gc surface)))
        (when window
          (xcb:+request connection
              (make-instance 'xcb:UnmapWindow :window window)))
        (when gc
          (xcb:+request connection
              (make-instance 'xcb:FreeGC :gc gc)))
        (when window
          (xcb:+request-checked+request-check connection
              (make-instance 'xcb:DestroyWindow :window window))))
      (clrhash (slothbar-backend-xcb-surface-module-surfaces surface))
      (xcb:flush connection))
    (setf (slothbar-surface bar) nil
          (slothbar-backend-bars backend)
          (delq bar (slothbar-backend-bars backend)))))

(cl-defmethod slothbar-backend-close ((backend slothbar-backend-xcb))
  "Tear down BACKEND's connection."
  (let ((connection (slothbar-backend-xcb-connection backend)))
    (when connection
      (xcb:disconnect connection)
      (setf (slothbar-backend-xcb-connection backend) nil)))
  nil)

(provide 'slothbar-backend-xcb)
;;; slothbar-backend-xcb.el ends here
