;;; exlybar-render.el --- Exlybar XCB specific render fns -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((xelb "0.18") (fontsloth "0.12.0") (emacs "27.1"))
;; Keywords: window-manager, status-bar, exwm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Part of exlybar.

;; This module provides xcb helpers for exlybar.
;; Particularly it provides useful helpers involving the render/renderutil
;; extensions.

;;; Code:

(require 'xcb)
(require 'xcb-render)
(require 'xcb-renderutil)
(require 'fontsloth-layout)

(defun exlybar-render-create-pixmap (c id width height &optional depth)
  "Sent a request to create an `xcb:Pixmap'.
C the xcb connection
ID an id generated by `xcb:generate-id'
WIDTH desired width
HEIGHT desired height
DEPTH optional depth, default is root geometry depth"
  (let* ((parent (exlybar--find-root-window-id))
         (depth (or depth (slot-value (xcb:+request-unchecked+reply c
                                          (make-instance 'xcb:GetGeometry
                                                         :drawable parent))
                                      'depth))))
    (xcb:+request-checked+request-check c
        (make-instance 'xcb:CreatePixmap
                       :pid id
                       :depth depth
                       :drawable parent
                       :width width
                       :height height))))

(defun exlybar-render-fill-rectangle (c gc drawable width height)
  "Send a request to fill a rectangle area given a graphics context.
C the xcb connection
GC a graphics context id
DRAWABLE id of the drawable in which to fill the rectangle
WIDTH desired width
HEIGHT desired height"
  (xcb:+request-checked+request-check c
      (make-instance
       'xcb:PolyFillRectangle
       :gc gc
       :drawable drawable
       :rectangles
       `(,(make-instance 'xcb:RECTANGLE :x 0 :y 0
                          :width width :height height)))))

(cl-defun exlybar-render-create-color (&key red green blue alpha)
  "Create an xcb rgba color given its components as 16 bit values.
RED 16 bit red value
GREEN 16 bit green value
BLUE 16 bit blue value
ALPHA 16 bit alpha value"
  (make-instance 'xcb:render:COLOR
                  :red red :green green :blue blue :alpha alpha))

(defun exlybar-render--create-pen (c color)
  "Create a pen for sketching glyphs.
C the connection
COLOR the pen color, an `xcb:render:COLOR'"
  (let* ((fmts (xcb:renderutil:query-formats c))
         (fmt (xcb:renderutil:find-standard
               fmts xcb:renderutil:PICT_STANDARD:ARGB_32))
         (pmap (xcb:generate-id c)))
    (exlybar-render-create-pixmap c pmap 1 1 32)
    (let ((picture (xcb:generate-id c)))
      (xcb:+request c
          (make-instance 'xcb:render:CreatePicture
                         :pid picture
                         :drawable pmap
                         :format fmt
                         :value-mask xcb:render:CP:Repeat
                         :repeat xcb:render:Repeat:Normal))
      (xcb:+request c
          (make-instance 'xcb:render:FillRectangles
                         :op xcb:render:PictOp:Over
                         :dst picture
                         :color color
                         :rects `(,(make-instance 'xcb:RECTANGLE
                                                  :x 0
                                                  :y 0
                                                  :width 1
                                                  :height 1))))
      (xcb:+request c (make-instance 'xcb:FreePixmap :pixmap pmap))
      picture)))

;; TODO: this should support a text stream rather than only a single char
;; TODO: this will eventually do a TODO in xcb-renderutil
(defun exlybar-render--stream-glyph-cmds (char-code x y)
  "Return an xcb 32 bit glyph stream given CHAR-CODE and X,Y coords.
As used by xcb CompositeGlyphs32.
This is a basis for filling in the missing glyph stream functionality in
xcb-renderutil."
  `(1 0 0 0
      ,(logand #x00ff x) ,(ash (logand #xff00 x) -8)
      ,(logand #x00ff y) ,(ash (logand #xff00 y) -8)
      ,(logand #x000000ff char-code)
      ,(ash (logand #x0000ff00 char-code) -8)
      ,(ash (logand #x00ff0000 char-code) -16)
      ,(ash (logand #xff000000 char-code) -24) 0 0 0 0))

(defun exlybar-render-draw-text (c pmap gs glyph-positions color)
  "Draw text into a pixmap.
C connection
PMAP the pixmap
GS the glyphset
GLYPH-POSITIONS the list of glyph positions
TEXT the text to draw
COLOR color of text, an `xcb:render:COLOR'"
  (pcase-let* ((fmts (xcb:renderutil:query-formats c))
               (fmt (xcb:renderutil:find-standard
                     fmts xcb:renderutil:PICT_STANDARD:RGB_24))
               (pic (xcb:generate-id c))
               (cookie (xcb:+request-checked+request-check c
                           (make-instance
                            'xcb:render:CreatePicture
                            :pid pic
                            :drawable pmap
                            :format fmt
                            :value-mask (logior xcb:render:CP:PolyMode
                                                xcb:render:CP:PolyEdge)
                            :polymode xcb:render:PolyMode:Imprecise
                            :polyedge xcb:render:PolyEdge:Smooth)))
               (pen (exlybar-render--create-pen c color)))
    (dolist (pos glyph-positions)
      (pcase-let* (((cl-struct fontsloth-layout-glyph-position
                               (parent char-code) x width height) pos)
                   (x (truncate x)))
        ;; TODO: allow request check if debugging is enabled
        (xcb:+request c
            (make-instance 'xcb:render:CompositeGlyphs32
                           :op xcb:render:PictOp:Over
                           :src pen
                           :dst pic
                           :mask-format 0
                           :glyphset gs
                           :src-x 0
                           :src-y 0
                           :glyphcmds
                           (exlybar-render--stream-glyph-cmds
                            char-code x 0)))
        ;; (message "trying to compositeglyph32 %s"
        ;;          )
        ))
    (xcb:+request c
        (make-instance 'xcb:render:FreePicture
                       :picture pen))
    (xcb:+request c
        (make-instance 'xcb:render:FreePicture
                       :picture pic))))

(defun exlybar-render-create-glyphset (c)
  "Create a glyphset and return its id.
C the xcb connection"
(let* ((fmts (xcb:renderutil:query-formats c))
       (fmt-a8 (xcb:renderutil:find-standard
                fmts xcb:renderutil:PICT_STANDARD:A_8))
       (gs (xcb:generate-id c)))
  (xcb:+request c
      (make-instance 'xcb:render:CreateGlyphSet
                     :gsid gs
                     :format fmt-a8))
  gs))

(defun exlybar-render--stride-pixmap (pm w h)
  "Format a greyscale pixmap PM for format A_8.
W the pixmap width
H the pixmap height"
  (let* ((stride (logand (lognot 3) (+ 3 w)))
         (sm (make-vector (* stride h) 0)))
    ;; (message "width %s height %s pm %s stride %s" w h (length pm) stride)
    (dotimes (y h)
      (dotimes (x w)
        (aset sm (+ x (* y stride))
              (aref pm (+ x (* y w))))))
    `(,sm ,stride ,h)))

(defun exlybar-render-load-glyph (c gs font glyph-position)
  "Load into the glyphset GS with font FONT and glyph-position GLYPH-POSITION.
C the connection
GS the glyphset
FONT the `fontsloth-font'
GLYPH-POSITION the `fontsloth-layout-glyph-position'"
  (pcase-let* (((cl-struct fontsloth-layout-glyph-position
                           key x y (parent char-code)
                           (width glyph-width) (height glyph-height))
                glyph-position)
               (x (round x)) (y (round y))
               ((cl-struct fontsloth-layout-glyph-raster-config glyph-id px)
                key)
               ((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                (fontsloth-font-rasterize font glyph-id px))
               ((cl-struct fontsloth-metrics width height) metrics)
               (pm+wh (exlybar-render--stride-pixmap pixmap width height))
               (pixmap (car pm+wh))
               (width (cadr pm+wh))
               (height (caddr pm+wh))
               (padding 1))
    (xcb:+request-checked+request-check c
        (make-instance 'xcb:render:AddGlyphs
                       :glyphset gs
                       :glyphs-len 1
                       :glyphids `(,char-code)
                       :glyphs `(,(make-instance
                                   'xcb:render:GLYPHINFO
                                   :width width :height height
                                   :x 0
                                   :y (+ height y padding)
                                   :x-off 0 :y-off 0))
                       :data pixmap))
    ;; (message "trying to add a glyph %s "
    ;;          )
    char-code))

(provide 'exlybar-render)
;;; exlybar-render.el ends here
