;;; slothbar-menu.el --- Backend-neutral menu model and renderer  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;;; Commentary:

;; A first single-level menu model.  It consumes a normalized dbusmenu tree,
;; computes row geometry, and renders a logical RGBA frame for a popup.  Input
;; and submenus are handled by the popup/menu controller.

;;; Code:

(require 'cl-lib)
(require 'fontsloth)
(require 'fontsloth-layout)

(require 'slothbar)
(require 'slothbar-color)
(require 'slothbar-dbusmenu)
(require 'slothbar-font)
(require 'slothbar-module-)
(require 'slothbar-popup)

(defcustom slothbar-menu-padding 8
  "Horizontal padding inside menu items."
  :type 'natnum
  :group 'slothbar)

(cl-defstruct (slothbar-menu-row
               (:constructor slothbar-menu-row--make)
               (:copier nil))
  "One rendered menu row."
  item
  label
  type
  y
  height)

(cl-defstruct (slothbar-menu
               (:constructor slothbar-menu--make)
               (:copier nil))
  "A single-level menu."
  root
  rows
  width
  height
  default-row-height)

(defun slothbar-menu--font ()
  "Return the primary menu font, or nil."
  (when-let* ((path (slothbar-font-find 0)))
    (fontsloth-load-font path)))

(defun slothbar-menu--px ()
  "Return the primary menu font pixel size."
  (+ (aref slothbar-font-px-size 0)
     (aref slothbar-font-px-delta 0)))

(defun slothbar-menu--text-width (text)
  "Return TEXT's rendered width in the primary menu font."
  (when-let* ((font (slothbar-menu--font)))
    (let ((layout (slothbar-module--create-layout
                   (or text "") font 0 (slothbar-menu--px)
                   slothbar-menu-padding)))
      (if layout (round (fontsloth-layout-current-pos layout)) 0))))

(defun slothbar-menu-create (menu-or-root)
  "Create a single-level `slothbar-menu' from MENU-OR-ROOT.
MENU-OR-ROOT may be a `slothbar-dbusmenu' or a normalized root item."
  (let* ((root (if (slothbar-dbusmenu-p menu-or-root)
                   (slothbar-dbusmenu-root menu-or-root)
                 menu-or-root))
         (children (and root (slothbar-dbusmenu-item-children root)))
         (row-height (max 1 slothbar-height))
         (rows nil)
         (width 0)
         (y 0))
    (dolist (item children)
      (when (slothbar-dbusmenu-item-visible item)
        (let* ((separator (equal "separator"
                                 (slothbar-dbusmenu-item-type item)))
               (label (or (slothbar-dbusmenu-item-label item) ""))
               (label-width (if separator 0
                               (round (or (slothbar-menu--text-width label) 0)))))
          (push (slothbar-menu-row--make
                 :item item :label label
                 :type (if separator "separator" "item")
                 :y y :height row-height)
                rows)
          (setq width (max width (+ label-width (* 2 slothbar-menu-padding)))
                y (+ y row-height)))))
    (slothbar-menu--make
     :root root :rows (nreverse rows)
     :width (max 1 (round width)) :height (max 1 (round y))
     :default-row-height row-height)))

(defun slothbar-menu--rgba (color)
  "Return COLOR as an RGBA byte list."
  (list (ash (slothbar-color-rgb-red color) -8)
        (ash (slothbar-color-rgb-green color) -8)
        (ash (slothbar-color-rgb-blue color) -8)
        (ash (slothbar-color-rgb-alpha color) -8)))

(defun slothbar-menu--fill (frame width height color)
  "Fill a WIDTHxHEIGHT RGBA FRAME with COLOR."
  (let ((rgba (slothbar-menu--rgba color)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((offset (* (+ x (* y width)) 4)))
          (dotimes (n 4)
            (aset frame (+ offset n) (nth n rgba))))))))

(defun slothbar-menu--fill-row (frame width x y row-width height color)
  "Fill a row region in FRAME."
  (let ((rgba (slothbar-menu--rgba color)))
    (dotimes (dy height)
      (dotimes (dx row-width)
        (let ((px (+ x dx)) (py (+ y dy)))
          (when (and (>= px 0) (< px width))
            (let ((offset (* (+ px (* py width)) 4)))
              (dotimes (n 4)
                (aset frame (+ offset n) (nth n rgba))))))))))

(defun slothbar-menu--blend-pixmap
    (frame width height left top glyph-width glyph-height pixmap color)
  "Blend an A8 PIXMAP into FRAME at LEFT, TOP using COLOR."
  (let ((rgba (slothbar-menu--rgba color)))
    (dotimes (row glyph-height)
      (let ((y (+ top row)))
        (when (and (>= y 0) (< y height))
          (dotimes (col glyph-width)
            (let ((x (+ left col)))
              (when (and (>= x 0) (< x width))
                (let* ((alpha (aref pixmap (+ col (* row glyph-width))))
                       (offset (* (+ x (* y width)) 4)))
                  (when (> alpha 0)
                    (dotimes (n 3)
                      (aset frame (+ offset n)
                            (/ (+ (* (nth n rgba) alpha)
                                  (* (aref frame (+ offset n)) (- 255 alpha)))
                               255)))
                    (aset frame (+ offset 3) 255)))))))))))

(defun slothbar-menu--render-label
    (frame width height row-x row-y text color)
  "Render TEXT into FRAME at ROW-X,ROW-Y using COLOR."
  (when-let* ((font (slothbar-menu--font))
              (layout (slothbar-module--create-layout
                       text font 0 (slothbar-menu--px) row-x)))
    (dolist (pos (fontsloth-layout-output layout))
      (when (fontsloth-layout-glyph-position-p pos)
        (pcase-let* (((cl-struct fontsloth-layout-glyph-position
                                 key (x glyph-x) (y glyph-y)) pos)
                     ((cl-struct fontsloth-layout-glyph-raster-config
                                 glyph-id px) key)
                     (entry (fontsloth-font-rasterize font glyph-id px)))
          (when entry
            (pcase-let* (((cl-struct fontsloth-metrics+pixmap metrics pixmap) entry)
                         ((cl-struct fontsloth-metrics
                                     (width glyph-width)
                                     (height glyph-height))
                          metrics)
                         (glyph-width (round glyph-width))
                         (glyph-height (round glyph-height))
                         (left (+ row-x (round glyph-x)))
                         (top (+ row-y (- (round glyph-y)) (- glyph-height))))
              (slothbar-menu--blend-pixmap
               frame width height left top glyph-width glyph-height
               pixmap color))))))))

(defun slothbar-menu-render-frame (menu)
  "Render MENU to a logical RGBA frame string."
  (let* ((width (max 1 (round (slothbar-menu-width menu))))
         (height (max 1 (round (slothbar-menu-height menu))))
         (frame (make-string (* width height 4) 0))
         (background (or (slothbar--background-color)
                         (slothbar-color-from-name "#000000")))
         (foreground (or (slothbar--foreground-color)
                         (slothbar-color-from-name "#ffffff"))))
    (slothbar-menu--fill frame width height background)
    (dolist (row (slothbar-menu-rows menu))
      (let ((row-y (slothbar-menu-row-y row))
            (row-height (slothbar-menu-row-height row)))
        (if (equal "separator" (slothbar-menu-row-type row))
            (slothbar-menu--fill-row
             frame width slothbar-menu-padding
             (+ row-y (max 0 (1- (/ row-height 2))))
             (max 1 (- width (* 2 slothbar-menu-padding))) 1 foreground)
          (slothbar-menu--render-label
           frame width height slothbar-menu-padding row-y
           (slothbar-menu-row-label row)
           (if (eq t (slothbar-dbusmenu-item-enabled
                      (slothbar-menu-row-item row)))
               foreground
             (slothbar-color-brighten background 0.5))))))
    frame))

(defun slothbar-menu-show (menu bar x y)
  "Show MENU as a popup on BAR at X,Y.
Return the `slothbar-popup' or nil."
  (let* ((frame (slothbar-menu-render-frame menu))
         (popup (slothbar-popup-create
                 bar x y (slothbar-menu-width menu)
                 (slothbar-menu-height menu))))
    (when popup
      (slothbar-popup-present popup frame)
      popup)))

(provide 'slothbar-menu)
;;; slothbar-menu.el ends here
