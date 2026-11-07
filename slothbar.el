;;; slothbar.el --- Emacs X window manager status bar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;;         Agnes Li <agnes.li@mailfence.com>
;; Version: 0.30.2
;; Homepage: https://codeberg.org/agnes-li/slothbar
;; Package-Requires: ((all-the-icons "5.0.0") (backlight "1.4") (compat "29.1") (dash "2.1.0") (f "0.20.0") (fontsloth "0.19.1") (log4e "0.3.3") (nerd-icons "0.1.0") (s "1.12.0") (volume "1.0") (xelb "0.18") (emacs "28.1"))
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

;; This package uses xelb to build a dock bar on X11 and elwl/wlr-layer-shell
;; to build a bar on Wayland.

;; An example configuration with use-package:

;; (use-package slothbar
;;   :config
;;   (require 'slothbar-module-requires)
;;   (setq slothbar-modules '(:left
;;                            slothbar-tray-create slothbar-date-create
;;                            slothbar-workspaces-create
;;                            :right
;;                            slothbar-wifi-create slothbar-volume-create
;;                            slothbar-backlight-create
;;                            slothbar-battery-create))
;;   ;; to enable multi-screen support
;;   (slothbar-randr-mode))

;; then M-x: slothbar-mode
;; To exit:
;; M-x: slothbar-mode

;; On first run, expect a delay of around twenty to thirty seconds
;; (more or less depending on which fonts are chosen).  During this
;; time, available fonts configured in slothbar-font-candidates are
;; loaded and cached in a background process for future reference.

;; Check the default value of slothbar-font-candidates in
;; slothbar-font.el to determine a minimal set of fonts.

;; E.g. if slothbar-font-candidates is:
;;   '(("Aporetic Sans" "IBM Plex Serif" "Deja Vu Serif" "Cantarell")
;;     ("Font Awesome")
;;     ("Aporetic Sans Mono" "IBM Plex Mono" "DejaVu Sans Mono:style=Book")
;;     ("all-the-icons")
;;     ("Symbols Nerd Font Mono"))
;; then for module format strings using:
;; + ^f0 ensure one of Aporetic Sans, IBM Plex Serif, Deja Vu Serif, or
;;   Cantarell is installed
;; + ^f1 install the free version of font awesome
;; + ^f2 ensure one of Aporetic Sans Mono, IBM Plex Mono, or Deja Vu Sans
;;   Mono is installed
;; + ^f3 run M-x all-the-icons-install-fonts (or install another way)
;; + ^f4 run M-x nerd-icons-install-fonts (or install another way)

;;; Code:

(require 'cl-lib)
(require 'fontsloth)
(require 'pcase)
(require 'seq)

(require 'slothbar-backend)
(require 'slothbar-color)
(require 'slothbar-module)
(require 'slothbar-util)
(require 'slothbar-log)

(defgroup slothbar nil
  "Slothbar is a status bar that displays as a dock or layer-shell window."
  :group 'display)

(defvar slothbar--enabled nil "Non-nil if slothbar is enabled.")

(cl-defstruct
    (slothbar
     (:constructor slothbar-create)
     (:copier nil))
  "A rendered status bar instance."
  id
  config
  display-backend
  output
  surface
  module-list
  placements
  hovered-module
  (pixel-width 0 :type 'integer)
  (pixel-height 0 :type 'integer)
  font-px-sizes
  (x-offset 0 :type 'integer)
  (y-offset 0 :type 'integer)
  (scale 1 :type 'integer)
  (bottom nil :type 'boolean)
  visible)

(defvar slothbar--bars nil
  "The active list of `slothbar' instances.")

(defvar slothbar--bar nil
  "The current `slothbar' instance during a scoped operation.")

(defcustom slothbar-width (display-pixel-width)
  "Slothbar width.

Defaults to the width obtained from `display-pixel-width'.  The Wayland
backend replaces this with the width supplied by a layer-surface configure
event."
  :type 'integer
  :group 'slothbar)

(require 'slothbar-layout)

(defcustom slothbar-height 20
  "Slothbar height."
  :type 'integer
  :group 'slothbar)

(require 'slothbar-font)

(defcustom slothbar-offset-x 0
  "Bar display x offset in pixels."
  :type 'integer
  :group 'slothbar)

(defcustom slothbar-offset-y 0
  "Bar display y offset in pixels."
  :type 'integer
  :group 'slothbar)

(defcustom slothbar-margin-y 2
  "Bar vertical margin in pixels."
  :type 'integer
  :group 'slothbar)

(defcustom slothbar-preferred-display "eDP-1"
  "If multiple displays are connected:

- nil indicates to automatically choose one.  If the slothbar-randr
  extension is enabled, this will be the primary display
- string should be the display name as reported by
  `display-monitor-attributes-list' on X or by `wl_output.name' on Wayland"
  :type '(choice (const :tag "auto" nil)
		 string)
  :group 'slothbar)

(defcustom slothbar-is-bottom nil
  "Non-nil to position slothbar at the bottom of the display and nil for top."
  :type 'boolean
  :group 'slothbar)

(defcustom slothbar-modules nil
  "List of slothbar module constructor names with optional layout instructions.

E.g.: (:left
       `slothbar-tray-create' `slothbar-date-create'
       :right
       `slothbar-wifi-create' `slothbar-volume-create'
       `slothbar-backlight-create' `slothbar-battery-create')"
  :type '(repeat (choice (radio :tag "Layout instruction keyword" :value :left
                                (const :left) (const :right) (const :center))
                         (function :tag "Module constructor or lambda"
                                   :value slothbar-date-create)))
  :group 'slothbar
  :require 'slothbar-module-requires)

(defcustom slothbar-backend 'auto
  "Display backend to use.

`auto' prefers Wayland when WAYLAND_DISPLAY names an existing socket and the
elwl client is loadable; otherwise it uses xcb.  Set `xcb' or `wayland' to
choose explicitly."
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "X11 (xcb)" xcb)
                 (const :tag "Wayland" wayland))
  :group 'slothbar)

(defcustom slothbar-bars nil
  "Bar definitions.

When non-nil, this list is authoritative: one bar is created for each
entry and each matching output, and no legacy default bar is added.
When nil, a non-nil `slothbar-modules' creates one legacy bar.  When
both are nil, no bars are created.

Each entry is a plist with the following keys:

- `:id' distinguishes bars that would otherwise have the same output.
- `:output' selects the output or outputs:
  - nil or omitted: one bar on `slothbar-preferred-display', or the
    first connected output.
  - a string: one bar while that output exists.
  - a list of strings: one bar for each listed output that exists.
  - `:each': one bar for every connected output.
  Multiple entries may use `:each' to create multiple bars per output.
- `:modules' is the module list or layout instructions for this bar.
  Omitted means use `slothbar-modules'; nil means an empty bar.
- `:height', `:width', `:offset-x', `:offset-y', and `:bottom' override
  the corresponding global geometry settings.

Changing this variable while slothbar is running reconciles the bar list.

The XEmbed tray is single-instance: if it appears in more than one bar,
the first configured bar keeps it and later instances are skipped."
  :type '(repeat plist)
  :group 'slothbar)

(defcustom slothbar-before-init-hook nil
  "Functions to run when before slothbar is initialized."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-before-module-init-hook nil
  "Functions to run before modules are initialized."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-after-init-hook nil
  "Functions to run when after slothbar is initialized."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-before-exit-hook nil
  "Functions to run when before slothbar exits."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-after-exit-hook nil
  "Functions to run when after slothbar exits."
  :type 'hook
  :group 'slothbar)

(defvar slothbar--modules nil
  "List of slothbar modules with optional layout instructions.")

(defvar slothbar--visible nil
  "Non-nil if the bar is visible.")

(defvar slothbar--last-layout nil
  "The most recent module layout, used to route pointer clicks.")

(defvar slothbar--hovered-module nil
  "The module currently under the pointer, or nil.")

(defmacro slothbar--global-minor-mode-body (name &optional init exit)
  "Global minor mode body for mode with NAME.

The INIT and EXIT functions are added to
`slothbar-before-module-init-hook' and `slothbar-before-exit-hook'
respectively.  If a display backend exists, the mode is immediately
enabled or disabled."
  (declare (indent 1) (debug t))
  (let* ((mode (intern (format "slothbar-%s-mode" name)))
         (init (or init (intern (format "slothbar-%s--init" name))))
         (exit (or exit (intern (format "slothbar-%s--exit" name)))))
    `(progn
       (cond
        (,mode
         (add-hook 'slothbar-before-module-init-hook #',init)
         (add-hook 'slothbar-before-exit-hook #',exit)
         (when slothbar--backend (,init)))
        (t
         (remove-hook 'slothbar-before-module-init-hook #',init)
         (remove-hook 'slothbar-before-exit-hook #',exit)
         (when slothbar--backend (,exit)))))))

(defsubst slothbar-enabled-p ()
  "Return t if slothbar is enabled."
  slothbar--enabled)

(defun slothbar--background-color ()
  "Return the current background color as a `slothbar-color-rgb'."
  (slothbar-color-from-name (slothbar-util--find-background-color)))

(defun slothbar--foreground-color ()
  "Return the current foreground color as a `slothbar-color-rgb'."
  (slothbar-color-from-name (slothbar-util--find-foreground-color)))

(defun slothbar--find-display-geometry (&optional display)
  "Find DISPLAY geometry as an alist of x-offset, y-offset, width, and height.

If DISPLAY is not found, the value chosen is the first found in
`display-monitor-attributes-list'."
  (let* ((all-attrs (display-monitor-attributes-list))
	 (display-attrs
	  (or (seq-some
	       (lambda (l) (when (equal (alist-get 'name l) display) l)) all-attrs)
	      (car all-attrs)))
	 (geom (alist-get 'geometry display-attrs)))
    `((x-offset . ,(car geom))
      (y-offset . ,(cadr geom))
      (width . ,(caddr geom))
      (height . ,(cadddr geom)))))

(defmacro slothbar--with-bar (bar &rest body)
  "Evaluate BODY with BAR as the current bar and geometry."
  (declare (indent 1) (debug t))
  `(let* ((,bar ,bar)
          (slothbar--bar ,bar)
          (slothbar-width (slothbar-pixel-width ,bar))
          (slothbar-height (slothbar-pixel-height ,bar))
          (slothbar-font-px-size (or (slothbar-font-px-sizes ,bar)
                                     slothbar-font-px-size))
          (slothbar-offset-x (slothbar-x-offset ,bar))
          (slothbar-offset-y (slothbar-y-offset ,bar))
          (slothbar-is-bottom (slothbar-bottom ,bar)))
     ,@body))

(defun slothbar-show ()
  "Show every active bar."
  (dolist (bar (or slothbar--bars (list slothbar--bar)))
    (when bar
      (slothbar--with-bar bar
        (let ((backend (slothbar-display-backend bar)))
          (when backend
            (slothbar-backend-show backend bar)
            (setf (slothbar-visible bar) t))))))
  (setq slothbar--visible t))

(defun slothbar-hide ()
  "Hide every active bar."
  (dolist (bar (or slothbar--bars (list slothbar--bar)))
    (when bar
      (slothbar--with-bar bar
        (let ((backend (slothbar-display-backend bar)))
          (when backend
            (slothbar-backend-hide backend bar)
            (setf (slothbar-visible bar) nil))))))
  (setq slothbar--visible nil))

(defun slothbar-hide-show ()
  "Hide slothbar if visible and show if hidden."
  (interactive)
  (if slothbar--visible
      (slothbar-hide)
    (slothbar-show)))

(defun slothbar--refresh (&optional unmap?)
  "Refresh the bar.

If UNMAP? is non-nil, first unmap the window."
  (when slothbar--visible
    (when unmap?
      (slothbar-hide))
    (slothbar-show)))

(defun slothbar--on-theme-change (_)
  "A function to run when the Emacs theme changes.

Remap colors in every bar and refresh the display."
  (setq slothbar-color-map-fg (slothbar-color--gen-color-map))
  (when (slothbar-enabled-p)
    (let ((background (slothbar--background-color))
          (foreground (slothbar--foreground-color))
          (bars (or slothbar--bars (list slothbar--bar))))
      (dolist (bar bars)
        (when bar
          (let ((modules (or (slothbar-module-list bar)
                             slothbar--modules)))
            (dolist (m modules)
              (when (slothbar-module-p m)
                (let ((colors (slothbar-module-colors m)))
                  (setf (slothbar-module-rgb-background-color colors)
                        background
                        (slothbar-module-rgb-foreground-color colors)
                        foreground
                        (slothbar-module-needs-refresh? m) t))))))))
    (run-with-idle-timer
     0 nil
     (lambda ()
       (slothbar--refresh t)
       (slothbar-refresh-modules)))))

(defvar slothbar--geometry-changed? nil
  "Non-nil when the bar geometry changed.
Cleared by `slothbar-refresh-modules' after the change is processed.")

(cl-defun slothbar-refresh-modules (&optional modules bar)
  "Refresh bars whose modules or geometry changed, and present them.
MODULES, when non-nil, is used instead of each bar's module list.
When BAR is non-nil, only that bar is refreshed."
  (let ((geometry-changed slothbar--geometry-changed?)
        (bars (if bar (list bar)
                (or slothbar--bars (list slothbar--bar)))))
    (when geometry-changed
      (setq slothbar--geometry-changed? nil)
      (dolist (bar bars)
        (slothbar--with-bar bar
          (slothbar--refresh-bar-font-px-sizes bar)
          (dolist (m (slothbar-module-list bar))
            (when (slothbar-module-p m)
              (setf (slothbar-module-needs-refresh? m) t
                    (slothbar-module-cache m) (make-hash-table :test 'equal))
              (when (slothbar-display-backend bar)
                (slothbar-backend-reset-module-cache
                 (slothbar-display-backend bar) m))))))
      (slothbar--refresh))
    (dolist (bar bars)
      (when bar
        (slothbar--with-bar bar
          (let* ((modules (or modules
                              (slothbar-module-list bar)
                              slothbar--modules))
                 (width (slothbar-pixel-width bar))
                 (backend (slothbar-display-backend bar))
                 (dirty
                  (or geometry-changed
                      (cl-some (lambda (m)
                                 (and (slothbar-module-p m)
                                      (slothbar-module-needs-refresh? m)))
                               modules)))
                 (first (null (slothbar-placements bar))))
            (when (or dirty first)
              (let* ((dirty-modules
                      (cl-remove-if-not
                       (lambda (m)
                         (and (slothbar-module-p m)
                              (slothbar-module-needs-refresh? m)))
                       modules))
                     (old-layout
                      (or (slothbar-placements bar)
                          (slothbar-layout-coordinate
                           (slothbar-layout modules) 0 0 width)))
                     (old-widths
                      (cl-loop for placement in old-layout
                               collect (cons (cadr placement)
                                             (slothbar-module-width
                                              (cadr placement)))))
                     (prev-extents (slothbar-layout-extents old-layout)))
                (dolist (m modules)
                  (when (slothbar-module-p m)
                    (slothbar-module-refresh m)))
                (let* ((new-layout
                        (slothbar-layout-coordinate
                         (slothbar-layout modules) 0 0 width))
                       (new-extents (slothbar-layout-extents new-layout))
                       (extent-damage
                        (unless (equal prev-extents new-extents)
                          (slothbar-layout-subtract-extents
                           new-extents prev-extents)))
                       (damage
                        (unless first
                          (slothbar--merge-damage
                           (append
                            extent-damage
                            (slothbar--content-damage
                             old-layout old-widths new-layout
                             dirty-modules))))))
                  (setq slothbar--last-layout new-layout)
                  (setf (slothbar-placements bar) new-layout)
                  (when backend
                    (slothbar-backend-present
                     backend bar new-layout damage)))))))))))

(defun slothbar--merge-damage (spans)
  "Sort and merge horizontal damage SPANS."
  (let ((spans (sort (cl-remove-if-not
                     (lambda (span)
                       (and (consp span)
                            (numberp (car span))
                            (numberp (cadr span))
                            (< (car span) (cadr span))))
                     (copy-sequence spans))
                    (lambda (a b) (< (car a) (car b)))))
        (merged nil))
    (dolist (span spans)
      (if (and merged (<= (car span) (cadar merged)))
          (setf (cadar merged) (max (cadar merged) (cadr span)))
        (push (list (car span) (cadr span)) merged)))
    (nreverse merged)))

(defun slothbar--content-damage (old-layout old-widths new-layout modules)
  "Return damage spans for changed MODULES between layouts.
OLD-WIDTHS maps modules to their widths before refresh."
  (slothbar--merge-damage
   (cl-loop for module in modules
            when (slothbar-module-p module)
            append
            (let* ((old-placement
                    (cl-find module old-layout :key #'cadr :test #'eq))
                   (new-placement
                    (cl-find module new-layout :key #'cadr :test #'eq))
                   (old-width (cdr (assq module old-widths)))
                   (new-width (slothbar-module-width module))
                   (left (cond ((and old-placement new-placement)
                                (min (caar old-placement)
                                     (caar new-placement)))
                               (old-placement (caar old-placement))
                               (new-placement (caar new-placement))))
                   (right (cond ((and old-placement new-placement)
                                 (max (+ (caar old-placement) (or old-width 0))
                                      (+ (caar new-placement) new-width)))
                                (old-placement
                                 (+ (caar old-placement) (or old-width 0)))
                                (new-placement
                                 (+ (caar new-placement) new-width)))))
              (when (and left right) (list (list left right)))))))

(defun slothbar--module-placement-at (x y &optional bar)
  "Return BAR's layout placement containing X and Y, or nil."
  (let ((bar (or bar slothbar--bar)))
    (cl-find-if
     (lambda (placement)
       (pcase-let ((`((,px ,py) ,m) placement))
         (and (>= x px)
              (< x (+ px (slothbar-module-width m)))
              (>= y py)
              (< y (+ py (if bar
                              (slothbar-pixel-height bar)
                            slothbar-height))))))
     (or (and bar (slothbar-placements bar))
         slothbar--last-layout))))

(defun slothbar--click (x y button &optional bar)
  "Dispatch a click at X,Y with BUTTON to BAR's module under it.
X and Y are bar-local logical coordinates."
  (let ((placement (slothbar--module-placement-at x y bar)))
    (when placement
      (pcase-let ((`((,px ,py) ,m) placement))
        (slothbar-module-click m (- x px) (- y py) button)))))

(defun slothbar--clear-hover (&optional bar)
  "Tell BAR's hovered module that the pointer has left it."
  (let ((bar (or bar slothbar--bar)))
    (when bar
      (let ((hovered (slothbar-hovered-module bar)))
        (when hovered
          (slothbar-module-hover hovered nil nil)
          (setf (slothbar-hovered-module bar) nil))))))

(defun slothbar--on-pointer-motion (backend bar x y)
  "Route pointer motion on BAR's BACKEND at X,Y to the module under it."
  (when (and slothbar--enabled bar
             (eq backend (slothbar-display-backend bar)))
    (slothbar--with-bar bar
      (let ((placement (and x y (slothbar--module-placement-at x y bar))))
        (if (null placement)
            (slothbar--clear-hover bar)
          (pcase-let ((`((,px ,py) ,m) placement))
            (unless (eq m (slothbar-hovered-module bar))
              (slothbar--clear-hover bar)
              (setf (slothbar-hovered-module bar) m))
            (slothbar-module-hover m (- x px) (- y py))))))))

(defun slothbar--on-pointer-button (backend bar x y button)
  "Route a pointer BUTTON press on BAR's BACKEND at X,Y to a module."
  (when (and slothbar--enabled bar
             (eq backend (slothbar-display-backend bar)))
    (slothbar--with-bar bar
      (slothbar--click x y button bar))))

(defun slothbar--watch-modules (_ nval oper where)
  "With OPER eq \\='set and nil WHERE, refresh `slothbar--modules' with NVAL."
  (when (and slothbar--enabled (not where) (eq 'set oper))
    ;; exit modules that have been removed
    (dolist (m slothbar--modules)
      (when (slothbar-module-p m)
        (unless (seq-contains-p nval m #'eq)
          (slothbar-module-exit m))))
    ;; check for uninitialized modules
    (dolist (m nval)
      (when (slothbar-module-p m)
        (unless (slothbar-module-initialized m)
          (slothbar-module-init m))))
    (when slothbar--bar
      (setf (slothbar-module-list slothbar--bar) nval))
    (slothbar-refresh-modules nval)))

(add-variable-watcher 'slothbar--modules #'slothbar--watch-modules)

(defvar slothbar--module-refresh-timer nil)
(defun slothbar--start-module-refresh-timer ()
  "Start a timer to periodically refresh the modules."
  (setq slothbar--module-refresh-timer
        (run-at-time nil 10 #'slothbar-refresh-modules)))

(defun slothbar--watch-height (_ nval oper where)
  "Refresh modules when changing `slothbar-height'.

Refresh only when OPER eq \\='set and WHERE is nil.
NVAL is check against the existing value for any changes."
  (when (and slothbar--enabled (not where) (eq 'set oper)
             (not (eql nval slothbar-height)))
    (setq slothbar--geometry-changed? t)
    (run-at-time 0 nil #'slothbar-refresh-modules)))

(add-variable-watcher 'slothbar-height #'slothbar--watch-height)

(defun slothbar--construct-modules (&optional bar modules)
  "Construct modules for BAR, or the current environment when BAR is nil."
  (let* ((config (and bar (slothbar-config bar)))
         (constructors
          (cond
           (modules modules)
           ((and config (plist-member config :modules))
            (plist-get config :modules))
           (t slothbar-modules)))
         (backend (if bar (slothbar-display-backend bar) slothbar--backend))
         (built
          (seq-filter
           (lambda (m)
             (or (not (slothbar-module-p m))
                 (null backend)
                 (if (slothbar-module-supported-p m backend)
                     t
                   (message
                    "Slothbar: skipping %s; not supported on the %s backend"
                    (slothbar-module-name m)
                    (slothbar-backend-kind backend))
                   nil)))
           (mapcar (lambda (val)
                     (cond
                      ((or (keywordp val) (slothbar-module-p val))
                       val)
                      ((functionp val)
                       (funcall val))
                      ((and (listp val) (functionp (car val)))
                       (apply (car val) (cdr val)))
                      (t (error "Unsupported type in slothbar-bars: %s"
                                val))))
                   constructors))))
    (let ((claimed
           (cl-loop for other in slothbar--bars
                    unless (eq other bar)
                    append (cl-loop for m in (slothbar-module-list other)
                                    when (slothbar-module-p m)
                                    collect (slothbar-module-singleton-id m))))
          (kept nil))
      (dolist (m built)
        (let ((id (and (slothbar-module-p m)
                       (slothbar-module-singleton-id m))))
          (cond
           ((and id (member id claimed))
            (message "Slothbar: skipping %s; %s is already provided"
                     (slothbar-module-name m) id))
           (t
            (when id (push id claimed))
            (push m kept)))))
      (setq built (nreverse kept)))
    (dolist (m built)
      (when (slothbar-module-p m)
        (setf (slothbar-module-backend m) backend
              (slothbar-module-bar m) bar)))
    (if bar
        (setf (slothbar-module-list bar) built)
      (setq slothbar--modules built))
    built))

(defun slothbar--bar-configs ()
  "Return the configured bar definitions.

A non-nil `slothbar-bars' is authoritative.  Otherwise, a non-nil
`slothbar-modules' creates the legacy single default bar.  With neither
configured, no bars are created."
  (cond
   (slothbar-bars slothbar-bars)
   (slothbar-modules (list nil))
   (t nil)))

(defun slothbar--refresh-bar-font-px-sizes (bar)
  "Recompute BAR's per-bar font pixel sizes."
  (setf (slothbar-font-px-sizes bar)
        (slothbar-font--precompute-px-sizes
         (slothbar-pixel-height bar)
         slothbar-font--color-code-map)))

(defun slothbar--make-bar (backend config index geometry &optional output)
  "Create a bar from CONFIG for BACKEND on OUTPUT.
INDEX identifies CONFIG among the bar definitions.  GEOMETRY supplies
fallback offsets."
  (let* ((height (or (plist-get config :height) slothbar-height))
         (bottom (if (plist-member config :bottom)
                     (plist-get config :bottom)
                   slothbar-is-bottom))
         (x-offset (or (plist-get config :offset-x)
                       (alist-get 'x-offset geometry)))
         (y-offset (or (plist-get config :offset-y)
                       (if bottom
                           (- (+ (alist-get 'y-offset geometry)
                                 (alist-get 'height geometry))
                              height)
                         (alist-get 'y-offset geometry)))))
    (slothbar-create
     :id (list (or (plist-get config :id) index) output)
     :config config
     :display-backend backend
     :output output
     :pixel-width (or (plist-get config :width)
                      (alist-get 'width geometry))
     :pixel-height height
     :font-px-sizes (slothbar-font--precompute-px-sizes
                     height slothbar-font--color-code-map)
     :x-offset x-offset
     :y-offset y-offset
     :bottom bottom)))

(defvar slothbar--output-change-timer nil
  "Debounce timer for output changes.")

(defvar slothbar--reconciling nil
  "Non-nil while `slothbar--reconcile-bars' is running.")

(defvar slothbar--reconcile-pending nil
  "Non-nil when another reconciliation is needed.")

(defun slothbar--desired-bar-specs (backend)
  "Return desired bar specs for BACKEND from `slothbar-bars'."
  (let ((outputs (slothbar-backend-output-names backend)))
    (cl-loop
     for config in (slothbar--bar-configs)
     for index from 0
     for output = (plist-get config :output)
     append
     (cond
      ((null output)
       (list (list :config config :index index :output nil)))
      ((eq output :each)
       (mapcar (lambda (name) (list :config config :index index :output name))
               outputs))
      ((listp output)
       (cl-loop for name in output
                when (member name outputs)
                collect (list :config config :index index :output name)))
      ((stringp output)
       (when (member output outputs)
         (list (list :config config :index index :output output))))
      (t nil)))))

(defun slothbar--bar-by-id (id bars)
  "Return the bar in BARS whose id is ID."
  (cl-find id bars :key #'slothbar-id :test #'equal))

(defun slothbar--reconcile-bars ()
  "Create and destroy bars to match the current outputs and configuration."
  (when slothbar--reconciling
    (setq slothbar--reconcile-pending t))
  (unless slothbar--reconciling
    (setq slothbar--reconciling t)
    (unwind-protect
        (when slothbar--backend
          (let* ((backend slothbar--backend)
           (specs (slothbar--desired-bar-specs backend))
           (desired-ids (mapcar (lambda (spec)
                                  (list (or (plist-get (plist-get spec :config)
                                                       :id)
                                            (plist-get spec :index))
                                        (plist-get spec :output)))
                                specs))
           (existing slothbar--bars)
           (kept nil))
      ;; Remove bars whose output or configuration disappeared.
      (dolist (bar existing)
        (if (member (slothbar-id bar) desired-ids)
            (push bar kept)
          (slothbar--with-bar bar
            (dolist (m (slothbar-module-list bar))
              (when (slothbar-module-p m)
                (slothbar-module-exit m)))
            (ignore-errors (slothbar-backend-destroy-bar backend bar)))))
      (setq kept (nreverse kept))
      ;; Keep the global bar list current while creating new bars so module
      ;; construction can see singleton modules already claimed by earlier bars.
      (setq slothbar--bars kept)
      ;; Add bars for new specs.
      (dolist (spec specs)
        (let* ((config (plist-get spec :config))
               (index (plist-get spec :index))
               (output (plist-get spec :output))
               (id (list (or (plist-get config :id) index) output)))
          (unless (slothbar--bar-by-id id kept)
            (let* ((geom (slothbar--find-display-geometry
                          (or output slothbar-preferred-display)))
                   (bar (slothbar--make-bar backend config index geom output)))
              (setq kept (append kept (list bar)))
              (setq slothbar--bars kept)
              (slothbar--with-bar bar
                (slothbar-backend-create-bar backend bar)
                (slothbar--construct-modules bar)
                (dolist (m (slothbar-module-list bar))
                  (when (slothbar-module-p m)
                    (slothbar-module-init m)))
                (when slothbar--enabled
                  (slothbar-backend-show backend bar)
                  (setf (slothbar-visible bar) t)))))))
      (let ((changed (not (equal (mapcar #'slothbar-id existing)
                                 (mapcar #'slothbar-id kept)))))
        (setq slothbar--bars kept
              slothbar--bar (car kept)
              slothbar--modules (and kept (slothbar-module-list (car kept))))
        (when (and changed slothbar--enabled)
          (run-at-time 0 nil #'slothbar-refresh-modules)))))
      (setq slothbar--reconciling nil)
      (when slothbar--reconcile-pending
        (setq slothbar--reconcile-pending nil)
        (run-at-time 0 nil #'slothbar--reconcile-bars)))))

(defun slothbar--on-output-change (backend)
  "Reconcile bars after BACKEND reports an output change."
  (when (and (eq backend slothbar--backend) slothbar--enabled)
    (when slothbar--output-change-timer
      (cancel-timer slothbar--output-change-timer))
    (setq slothbar--output-change-timer
          (run-at-time 0.1 nil
                       (lambda ()
                         (setq slothbar--output-change-timer nil)
                         (slothbar--reconcile-bars))))))

(defun slothbar--watch-slothbar-modules (_ nval oper where)
  "(Re)construct modules when `slothbar-modules' is modified.

Do this only when OPER eq \\='set and WHERE is nil.
NVAL is check against the existing value for any changes."
  (when (and (not where) (eq 'set oper)
             (not (equal nval slothbar-modules)))
    (run-at-time
     0 nil
     (lambda ()
       (if slothbar--bars
           (dolist (bar slothbar--bars)
             (slothbar--construct-modules bar))
         (slothbar--construct-modules))))))

(defun slothbar--watch-slothbar-bars (_ nval oper where)
  "Reconcile bars when `slothbar-bars' is modified.

Do this only when OPER eq \='set, WHERE is nil, and slothbar is running."
  (when (and slothbar--enabled
             (not where)
             (eq 'set oper)
             (not (equal nval slothbar-bars)))
    (run-at-time 0 nil #'slothbar--reconcile-bars)))

(defun slothbar--start ()
  "Start slothbar: open the backend, create bars, and initialize modules."
  (run-hook-with-args 'slothbar-before-init-hook)
  (cl-assert (not slothbar--backend))
  (slothbar--log-enable-logging)
  (let ((geom (slothbar--find-display-geometry slothbar-preferred-display)))
    (setq slothbar-offset-x (alist-get 'x-offset geom)
	  slothbar-offset-y (if slothbar-is-bottom
                               (- (+ (alist-get 'y-offset geom)
                                     (alist-get 'height geom))
                                  slothbar-height)
                             (alist-get 'y-offset geom))
	  slothbar-width (alist-get 'width geom))
    (setq slothbar-font-px-size
          (slothbar-font--precompute-px-sizes
           slothbar-height slothbar-font--color-code-map))
    (setq slothbar--backend (slothbar-backend-create slothbar-backend))
    (slothbar-backend-open slothbar--backend)
    (run-hooks 'slothbar-before-module-init-hook)
    (slothbar--reconcile-bars)
    (add-hook 'slothbar-backend-output-change-hook
              #'slothbar--on-output-change)
    (add-hook 'slothbar-backend-pointer-button-hook
              #'slothbar--on-pointer-button)
    (add-hook 'slothbar-backend-pointer-motion-hook
              #'slothbar--on-pointer-motion)
    (add-variable-watcher 'slothbar-modules #'slothbar--watch-slothbar-modules)
    (add-variable-watcher 'slothbar-bars #'slothbar--watch-slothbar-bars)
    (slothbar-show)
    (slothbar-refresh-modules)
    (setq slothbar--enabled t)
    (slothbar--start-module-refresh-timer)
    (run-hook-with-args 'slothbar-after-init-hook)))

(defun slothbar-refresh ()
  "Refresh bar geometry and modules."
  (interactive)
  (dolist (bar (or slothbar--bars (list slothbar--bar)))
    (when bar
      (let* ((config (slothbar-config bar))
             (display (or (slothbar-output bar)
                          (let ((configured (plist-get config :output)))
                            (and (stringp configured) configured))
                          slothbar-preferred-display))
             (geom (slothbar--find-display-geometry display))
             (height (slothbar-pixel-height bar))
             (width (or (plist-get config :width)
                        (alist-get 'width geom)))
             (x-offset (or (plist-get config :offset-x)
                           (alist-get 'x-offset geom)))
             (y-offset (or (plist-get config :offset-y)
                           (if (slothbar-bottom bar)
                               (- (+ (alist-get 'y-offset geom)
                                     (alist-get 'height geom))
                                  height)
                             (alist-get 'y-offset geom)))))
        (setf (slothbar-pixel-width bar) width
              (slothbar-x-offset bar) x-offset
              (slothbar-y-offset bar) y-offset))))
  (setq slothbar--geometry-changed? t)
  (run-at-time 0 nil #'slothbar-refresh-modules))

;;;###autoload
(defun slothbar ()
  "Start slothbar."
  (if (display-graphic-p)
      (fontsloth-async-load-and-cache-fonts
       (slothbar-font-map-candidates)
       :finish-func (lambda (_) (slothbar--start)))
    (message "Slothbar requires a graphical display to run")))

;;;###autoload
(defun slothbar-exit ()
  "Exit slothbar."
  (run-hook-with-args 'slothbar-before-exit-hook)
  (when slothbar--module-refresh-timer
    (cancel-timer slothbar--module-refresh-timer)
    (setq slothbar--module-refresh-timer nil))
  (when slothbar--output-change-timer
    (cancel-timer slothbar--output-change-timer)
    (setq slothbar--output-change-timer nil))
  (remove-hook 'slothbar-backend-output-change-hook
               #'slothbar--on-output-change)
  (remove-hook 'slothbar-backend-pointer-button-hook
               #'slothbar--on-pointer-button)
  (remove-hook 'slothbar-backend-pointer-motion-hook
               #'slothbar--on-pointer-motion)
  (dolist (bar slothbar--bars)
    (slothbar--clear-hover bar)
    (dolist (m (slothbar-module-list bar))
      (when (slothbar-module-p m)
        (slothbar-module-exit m))))
  (setq slothbar--last-layout nil)
  (setq slothbar--enabled nil)
  (remove-variable-watcher 'slothbar-modules #'slothbar--watch-slothbar-modules)
  (remove-variable-watcher 'slothbar-bars #'slothbar--watch-slothbar-bars)
  (dolist (bar slothbar--bars)
    (when slothbar--backend
      (ignore-errors
        (slothbar-backend-destroy-bar slothbar--backend bar))))
  (when slothbar--backend
    (slothbar-backend-close slothbar--backend)
    (run-hook-with-args 'slothbar-after-exit-hook))
  (setq slothbar--backend nil
        slothbar--bars nil
        slothbar--bar nil
        slothbar--modules nil
        slothbar--visible nil))

;;;###autoload
(define-minor-mode slothbar-mode
  "Slothbar-mode provides a window manager status bar."
  :global t
  :lighter " 🦥"
  (when (and slothbar-mode (not (slothbar-enabled-p)))
    (add-variable-watcher 'slothbar-height #'slothbar-font--watch-px-size)
    (add-hook 'enable-theme-functions #'slothbar--on-theme-change)
    (add-hook 'disable-theme-functions #'slothbar--on-theme-change)
    (setq  slothbar-color-map-fg (slothbar-color--gen-color-map)
           slothbar-font--color-code-map (slothbar-font-map-candidates))
    (add-variable-watcher 'slothbar-font-candidates
                          #'slothbar-font--watch-font-candidates)
    (add-variable-watcher 'slothbar-font--color-code-map
                          #'slothbar-font--watch-px-size)
    (slothbar))
  (when (and (not slothbar-mode) (slothbar-enabled-p))
    (remove-hook 'enable-theme-functions #'slothbar--on-theme-change)
    (remove-hook 'disable-theme-functions #'slothbar--on-theme-change)
    (slothbar-exit)
    (remove-variable-watcher 'slothbar-height #'slothbar-font--watch-px-size)
    (remove-variable-watcher 'slothbar-font-candidates
                             #'slothbar-font--watch-font-candidates)
    (remove-variable-watcher 'slothbar-font--color-code-map
                             #'slothbar-font--watch-px-size)))

(provide 'slothbar)

;;; slothbar.el ends here
