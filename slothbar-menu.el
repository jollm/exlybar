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

(defvar slothbar-menu--active nil
  "The menu popup currently open, if any.")

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
  default-row-height
  dbusmenu
  popup
  selected)

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
     :default-row-height row-height
     :dbusmenu (and (slothbar-dbusmenu-p menu-or-root) menu-or-root))))

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
    (let ((index 0))
      (dolist (row (slothbar-menu-rows menu))
        (let ((row-y (slothbar-menu-row-y row))
              (row-height (slothbar-menu-row-height row)))
          (when (and (slothbar-menu-selected menu)
                     (= index (slothbar-menu-selected menu)))
            (slothbar-menu--fill-row
             frame width 0 row-y width row-height
             (slothbar-color-brighten background 1.4)))
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
               (slothbar-color-brighten background 0.5)))))
        (setq index (1+ index))))
    frame))

(defun slothbar-menu--row-index-at-y (menu y)
  "Return MENU's row index at Y, or nil."
  (cl-loop for row in (slothbar-menu-rows menu)
           for index from 0
           when (and (>= y (slothbar-menu-row-y row))
                     (< y (+ (slothbar-menu-row-y row)
                             (slothbar-menu-row-height row))))
           return index))

(defun slothbar-menu-select (menu index)
  "Select row INDEX in MENU and redraw it."
  (setf (slothbar-menu-selected menu) index)
  (when-let* ((popup (slothbar-menu-popup menu)))
    (slothbar-popup-present popup (slothbar-menu-render-frame menu)))
  index)

(defun slothbar-menu-motion (menu x y)
  "Update MENU's selected row for pointer position X,Y."
  (ignore x)
  (if (numberp y)
      (slothbar-menu-select menu (slothbar-menu--row-index-at-y menu y))
    (slothbar-menu-select menu nil)))

(defun slothbar-menu-close (menu)
  "Close MENU's popup and tell its dbusmenu service it closed."
  (when-let* ((popup (slothbar-menu-popup menu)))
    (when-let* ((dbusmenu (slothbar-menu-dbusmenu menu))
                (root (slothbar-menu-root menu)))
      (slothbar--log-debug* "menu close dbusmenu=%s%s id=%s"
                            (slothbar-dbusmenu-service dbusmenu)
                            (slothbar-dbusmenu-path dbusmenu)
                            (slothbar-dbusmenu-item-id root))
      (ignore-errors
        (slothbar-dbusmenu-close
         dbusmenu (slothbar-dbusmenu-item-id root))))
    (slothbar-popup-destroy popup))
  (when (eq menu slothbar-menu--active)
    (setq slothbar-menu--active nil))
  (setf (slothbar-menu-popup menu) nil
        (slothbar-menu-selected menu) nil))

(defun slothbar-menu-active-p ()
  "Return non-nil when a menu popup is active."
  (and slothbar-menu--active
       (slothbar-menu-popup slothbar-menu--active)))

(defun slothbar-menu-close-active ()
  "Close the currently active menu, if any."
  (when (slothbar-menu-active-p)
    (slothbar-menu-close slothbar-menu--active)))

(defun slothbar-menu-click (menu x y button)
  "Handle a BUTTON press at X,Y on MENU."
  (let* ((index (slothbar-menu--row-index-at-y menu y))
         (row (and index (nth index (slothbar-menu-rows menu))))
         (item (and row (slothbar-menu-row-item row)))
         (dbusmenu (slothbar-menu-dbusmenu menu)))
    (cond
     ((and (eq button :left) row item dbusmenu
           (slothbar-dbusmenu-item-enabled item)
           (not (equal "separator" (slothbar-dbusmenu-item-type item))))
      (slothbar-dbusmenu-click dbusmenu
                               (slothbar-dbusmenu-item-id item))
      (slothbar-menu-close menu)
      t)
     ((eq button :right)
      (slothbar-menu-close menu)
      t)
     (t
      (ignore x y)
      nil))))

(defun slothbar-menu-show (menu bar x y)
  "Show MENU as a popup on BAR at X,Y.
Return the `slothbar-popup' or nil."
  (let* ((frame (slothbar-menu-render-frame menu))
         (popup (slothbar-popup-create
                 bar x y (slothbar-menu-width menu)
                 (slothbar-menu-height menu))))
    (when popup
      (when (and slothbar-menu--active
                 (not (eq slothbar-menu--active menu)))
        (slothbar-menu-close slothbar-menu--active))
      (setq slothbar-menu--active menu)
      (slothbar--log-debug* "menu popup rows=%d size=%dx%d at %d,%d configured=%S"
                            (length (slothbar-menu-rows menu))
                            (slothbar-menu-width menu)
                            (slothbar-menu-height menu)
                            (round x) (round y)
                            (slothbar-popup-configured-p popup))
      (setf (slothbar-menu-popup menu) popup
            (slothbar-popup-motion-function popup)
            (lambda (px py) (slothbar-menu-motion menu px py))
            (slothbar-popup-button-function popup)
            (lambda (px py button)
              (slothbar-menu-click menu px py button))
            (slothbar-popup-done-function popup)
            (lambda () (slothbar-menu-close menu)))
      (slothbar-popup-present popup frame)
      popup)))

(provide 'slothbar-menu)
;;; slothbar-menu.el ends here
