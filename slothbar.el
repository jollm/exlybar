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
  display-backend
  output
  surface
  module-list
  placements
  (pixel-width 0 :type 'integer)
  (pixel-height 0 :type 'integer)
  (x-offset 0 :type 'integer)
  (y-offset 0 :type 'integer)
  (scale 1 :type 'integer)
  visible)

(defvar slothbar--bar nil
  "The current `slothbar' instance.")

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

(defun slothbar-show ()
  "Show slothbar."
  (let ((backend (if slothbar--bar
                     (slothbar-display-backend slothbar--bar)
                   slothbar--backend)))
    (when backend
      (slothbar-backend-show backend slothbar--bar)
      (setq slothbar--visible t)
      (when slothbar--bar
        (setf (slothbar-visible slothbar--bar) t)))))

(defun slothbar-hide ()
  "Hide slothbar."
  (let ((backend (if slothbar--bar
                     (slothbar-display-backend slothbar--bar)
                   slothbar--backend)))
    (when backend
      (slothbar-backend-hide backend slothbar--bar)
      (setq slothbar--visible nil)
      (when slothbar--bar
        (setf (slothbar-visible slothbar--bar) nil)))))

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

It will remap colors and refresh the display."
  (setq slothbar-color-map-fg (slothbar-color--gen-color-map))
  (when (slothbar-enabled-p)
    (dolist (m slothbar--modules)
      (when (slothbar-module-p m)
        (setf (slothbar-module-rgb-background-color
               (slothbar-module-colors m))
              (slothbar--background-color))
        (setf (slothbar-module-needs-refresh? m) t)))
    (run-with-idle-timer 0 nil (lambda () (slothbar--refresh t)))))

(defvar slothbar--geometry-changed? nil
  "Non-nil when the bar geometry changed.
Cleared by `slothbar-refresh-modules' after the change is processed.")

(cl-defun slothbar-refresh-modules (&optional modules)
  "Ask the modules to refresh and see whether the layout has changed.
MODULES optional modules to refresh and compare with prev-extents."
  (let* ((bar slothbar--bar)
         (modules (or modules
                      (and bar (slothbar-module-list bar))
                      slothbar--modules))
         (width (if bar (slothbar-pixel-width bar) slothbar-width))
         (backend (if bar (slothbar-display-backend bar) slothbar--backend)))
    (when slothbar--geometry-changed?
      (setq slothbar--geometry-changed? nil)
      (dolist (m modules)
        (when (slothbar-module-p m)
          (setf (slothbar-module-needs-refresh? m) t
                (slothbar-module-cache m) (make-hash-table :test 'equal))
          (when backend
            (slothbar-backend-reset-module-cache backend m))))
      (slothbar--refresh))
    ;; refresh modules to update to latest dimensions
    (let ((prev-extents
           (slothbar-layout-extents
            (slothbar-layout-coordinate
             (slothbar-layout modules) 0 0 width))))
      (dolist (m modules)
        (when (slothbar-module-p m)
          (slothbar-module-refresh m)))
      (let* ((new-layout
              (slothbar-layout-coordinate
               (slothbar-layout modules) 0 0 width))
             (new-extents (slothbar-layout-extents new-layout))
             (damage (unless (equal prev-extents new-extents)
                       (slothbar-layout-subtract-extents
                        new-extents prev-extents))))
        (setq slothbar--last-layout new-layout)
        (when bar
          (setf (slothbar-placements bar) new-layout))
        (when backend
          (slothbar-backend-present backend bar new-layout damage))))))

(defun slothbar--module-placement-at (x y)
  "Return the layout placement containing X and Y, or nil."
  (cl-find-if
   (lambda (placement)
     (pcase-let ((`((,px ,py) ,m) placement))
       (and (>= x px)
            (< x (+ px (slothbar-module-width m)))
            (>= y py)
            (< y (+ py (if slothbar--bar
                            (slothbar-pixel-height slothbar--bar)
                          slothbar-height))))))
   (or (and slothbar--bar (slothbar-placements slothbar--bar))
       slothbar--last-layout)))

(defun slothbar--click (x y button)
  "Dispatch a click at X,Y with BUTTON to the module under it.
X and Y are bar-local logical coordinates."
  (let ((placement (slothbar--module-placement-at x y)))
    (when placement
      (pcase-let ((`((,px ,py) ,m) placement))
        (slothbar-module-click m (- x px) (- y py) button)))))

(defun slothbar--clear-hover ()
  "Tell the hovered module that the pointer has left it."
  (when slothbar--hovered-module
    (slothbar-module-hover slothbar--hovered-module nil nil)
    (setq slothbar--hovered-module nil)))

(defun slothbar--on-pointer-motion (backend x y)
  "Route pointer motion on BACKEND at X,Y to the module under it."
  (when (and slothbar--enabled
             (eq backend slothbar--backend))
    (let ((placement (and x y (slothbar--module-placement-at x y))))
      (if (null placement)
          (slothbar--clear-hover)
        (pcase-let ((`((,px ,py) ,m) placement))
          (unless (eq m slothbar--hovered-module)
            (slothbar--clear-hover)
            (setq slothbar--hovered-module m))
          (slothbar-module-hover m (- x px) (- y py)))))))

(defun slothbar--on-pointer-button (backend x y button)
  "Route a pointer BUTTON press on BACKEND at X,Y to a module."
  (when (and slothbar--enabled
             (eq backend slothbar--backend))
    (slothbar--click x y button)))

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

(defun slothbar--construct-modules ()
  "Construct modules from layout given in `slothbar-modules'."
    (setq slothbar--modules
          (seq-filter
           (lambda (m)
             (or (not (slothbar-module-p m))
                 (null slothbar--backend)
                 (if (slothbar-module-supported-p m slothbar--backend)
                     t
                   (message
                    "Slothbar: skipping %s; not supported on the %s backend"
                    (slothbar-module-name m)
                    (slothbar-backend-kind slothbar--backend))
                   nil)))
           (mapcar (lambda (val)
                     (cond
                      ((or (keywordp val) (slothbar-module-p val))
                       val)
                      ((functionp val)
                       (funcall val))
                      ((and (listp val) (functionp (car val)))
                       (apply (car val) (cdr val)))
                      (t (error "Unsupported type in slothbar-modules: %s"
                                val))))
                   slothbar-modules)))
    (when slothbar--bar
      (dolist (m slothbar--modules)
        (when (slothbar-module-p m)
          (setf (slothbar-module-backend m)
                (slothbar-display-backend slothbar--bar)
                (slothbar-module-bar m) slothbar--bar))))
    (when slothbar--bar
      (setf (slothbar-module-list slothbar--bar) slothbar--modules)))

(defun slothbar--watch-slothbar-modules (_ nval oper where)
  "(Re)construct modules when `slothbar-modules' is modified.

Do this only when OPER eq \\='set and WHERE is nil.
NVAL is check against the existing value for any changes."
  (when (and (not where) (eq 'set oper)
             (not (equal nval slothbar-modules)))
    (run-at-time 0 nil #'slothbar--construct-modules)))

(defun slothbar--start ()
  "Start slothbar.
Initialize the display backend, window, and modules."
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
	  slothbar-width (alist-get 'width geom)))
  (setq slothbar-font-px-size
        (slothbar-font--precompute-px-sizes
         slothbar-height slothbar-font--color-code-map))
  (setq slothbar--backend (slothbar-backend-create slothbar-backend))
  (setq slothbar--bar
        (slothbar-create :display-backend slothbar--backend
                         :pixel-width slothbar-width
                         :pixel-height slothbar-height
                         :x-offset slothbar-offset-x
                         :y-offset slothbar-offset-y))
  (slothbar-backend-open slothbar--backend)
  (slothbar-backend-create-bar slothbar--backend slothbar--bar)
  (add-hook 'slothbar-backend-pointer-button-hook
            #'slothbar--on-pointer-button)
  (add-hook 'slothbar-backend-pointer-motion-hook
            #'slothbar--on-pointer-motion)
  ;; initialize modules
  (slothbar--construct-modules)
  (add-variable-watcher 'slothbar-modules #'slothbar--watch-slothbar-modules)
  (run-hooks 'slothbar-before-module-init-hook)
  (dolist (m slothbar--modules)
    (when (slothbar-module-p m)
      (slothbar-module-init m)))
  (slothbar-show)
  (slothbar-refresh-modules)
  (setq slothbar--enabled t)
  (slothbar--start-module-refresh-timer)
  (run-hook-with-args 'slothbar-after-init-hook))

(defun slothbar-refresh ()
  "Refresh slothbar according to geometry and settings."
  (interactive)
  (let ((geom (slothbar--find-display-geometry slothbar-preferred-display)))
    (setq slothbar-offset-x (alist-get 'x-offset geom)
	  slothbar-offset-y (if slothbar-is-bottom
                                (- (+ (alist-get 'y-offset geom)
                                      (alist-get 'height geom))
                                   slothbar-height)
                              (alist-get 'y-offset geom))
	  slothbar-width (alist-get 'width geom)
          slothbar--geometry-changed? t)
    (when slothbar--bar
      (setf (slothbar-pixel-width slothbar--bar) slothbar-width
            (slothbar-pixel-height slothbar--bar) slothbar-height
            (slothbar-x-offset slothbar--bar) slothbar-offset-x
            (slothbar-y-offset slothbar--bar) slothbar-offset-y))
    (run-at-time 0 nil #'slothbar-refresh-modules)))

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
  ;; exit modules
  (when slothbar--module-refresh-timer
    (cancel-timer slothbar--module-refresh-timer)
    (setq slothbar--module-refresh-timer nil))
  (remove-hook 'slothbar-backend-pointer-button-hook
               #'slothbar--on-pointer-button)
  (remove-hook 'slothbar-backend-pointer-motion-hook
               #'slothbar--on-pointer-motion)
  (slothbar--clear-hover)
  (setq slothbar--last-layout nil)
  (setq slothbar--enabled nil)
  (dolist (m slothbar--modules)
    (when (slothbar-module-p m)
      (slothbar-module-exit m)))
  (remove-variable-watcher 'slothbar-modules #'slothbar--watch-slothbar-modules)
  (when (and slothbar--backend slothbar--bar)
    (ignore-errors
      (slothbar-backend-destroy-bar slothbar--backend slothbar--bar)))
  (when slothbar--backend
    (slothbar-backend-close slothbar--backend)
    (setq slothbar--backend nil
          slothbar--bar nil
          slothbar--visible nil)
    (run-hook-with-args 'slothbar-after-exit-hook))
  (setq slothbar--bar nil))

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
