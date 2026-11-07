;;; slothbar-workspaces.el --- An slothbar workspaces module  -*- lexical-binding: t -*-

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

;; This is an implementation of `slothbar-module' for active/current
;; workspaces information.

;; To use this module, add it to `slothbar-modules' with any desired
;; layout insructions.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'mule-util)
(require 'rx)
(require 'xcb)
(require 'xcb-ewmh)

(require 'slothbar-ewmh)
(require 'slothbar-log)
(require 'slothbar-module-)
(require 'slothbar-backend)

(defgroup slothbar-workspaces nil
  "An Slothbar workspaces module."
  :group 'slothbar)

(defcustom slothbar-workspaces-generate-list-fn (lambda () '())
  "A function generating a workspaces status list.

E.g.:
\(lambda ()
  \\='((\"0\" (:window))
    (\"1\" (:current))
    (\"2\" (:blankish))
    (\"3\" (:current :blankish))
    (\"4\" (:current :window))
    (\"5\" (:urgent))))
would indicate:
- that a workspace named 0 is not current, but has a window
- workspace 1 is current and may or may not have a window
- workspace 2 is not current and is blank(ish)
- workspace 3 is current and is blank(ish)
- workspace 4 is current and has a window
- workspace 5 is not current and has an urgent window

The possible status keywords are :window, :current, :blankish, :inactive,
and :urgent.  If :current appears in the status list for a workspace, it is
expected at the head of the list.  :inactive means the workspace is not
current, but the source does not know whether it contains a window; this is
what the ext-workspace protocol provides.  :window and :blankish are only
used by sources that know whether a workspace is occupied."
  :type 'function
  :group 'slothbar-workspaces)

(cl-defstruct (slothbar-workspaces
               (:include slothbar-module (name "workspaces") (icon ?󰯉)
                         (format "^2^[^5^f4%i^]%w")
                         (format-fn #'slothbar-workspaces-format-format))
               (:constructor slothbar-workspaces-create)
               (:copier nil))
  "A workspaces module."
  hover-index)

(cl-defmethod slothbar-module-supported-p
  ((_m slothbar-workspaces) (backend slothbar-backend))
  "Workspaces read EWMH root-window properties or ext-workspace."
  (or (slothbar-backend-supports-p backend 'ewmh)
      (slothbar-backend-supports-p backend 'ext-workspace)))

(defcustom slothbar-workspaces-ensure-ewmh nil
  "Non-nil to ensure ewmh mode if it is not already enabled."
  :type 'boolean
  :group 'slothbar-workspaces)

(defcustom slothbar-workspaces-truncate-to-width 16
  "A positive integer indicating a limit at which to truncate workspace names."
  :type 'natnum
  :group 'slothbar-workspaces)

(defconst slothbar-workspaces--extent-tag-characters
  "0123456789abcdefghijklmnopqrstuvwxyz"
  "Characters used as click extent tags in workspace text.")

(defun slothbar-workspaces--extent-tag (index)
  "Return the extent tag character for workspace INDEX, or nil."
  (when (and (integerp index)
             (>= index 0)
             (< index (length slothbar-workspaces--extent-tag-characters)))
    (aref slothbar-workspaces--extent-tag-characters index)))

(defun slothbar-workspaces--extent-tag-index (tag)
  "Return the workspace index for extent TAG, or nil."
  (cl-position tag slothbar-workspaces--extent-tag-characters))

(defun slothbar-workspaces--format-fn-spec (ws-list &optional hover-index)
  "Generate a spec suitable for `format-spec' from data in WS-LIST.
HOVER-INDEX is the workspace index currently under the pointer."
  (cl-flet ((sanitize (s)
              (truncate-string-to-width
               (string-replace " " "-"
                               (replace-regexp-in-string "[\\^]" "^^" (string-trim s)))
               slothbar-workspaces-truncate-to-width
               nil nil t)))
    `((?w . ,(cl-loop for (ws status) in ws-list
                      for index from 0
                      for ws-safe = (sanitize ws)
                      for base-label =
                      (pcase status
                        ((or '(:current) '(:current :window))
                         (concat "^[^1 [" ws-safe "]^]"))
                        ('(:urgent) (concat "^[^3 " ws-safe "^]"))
                        ((or '(:window) `(:window ,_))
                         (concat "^[^1 " ws-safe "^]"))
                        ('(:current :urgent)
                         (concat "^[^3 [" ws-safe "]^]"))
                        (`(:current ,_) (concat "^[^0 [" ws-safe "]^]"))
                        ('(:blankish) (concat "^[^2 " ws-safe "^]"))
                        ('(:inactive) (concat "^[^2 " ws-safe "^]")))
                      for tag = (slothbar-workspaces--extent-tag index)
                      for label = (if (eql index hover-index)
                                      (concat "^B" base-label "^b")
                                    base-label)
                      concat
                      (if tag
                          (concat (format "^M%c" tag) label "^M;")
                        label))))))

(defun slothbar-workspaces--click-regions (m)
  "Return clickable (TAG START . END) extents from M's text layout."
  (let ((tag nil)
        (start nil)
        (end nil)
        (regions nil))
    (dolist (item (slothbar-module-text-layout m))
      (cond
       ((and (consp item) (eq (car item) :mark-extent))
        (setq tag (cadr item)
              start nil
              end nil))
       ((and (consp item) (eq (car item) :end-extent))
        (when (and tag start end)
          (push (list tag start end) regions))
        (setq tag nil))
       ((fontsloth-layout-glyph-position-p item)
        (when tag
          (let ((left (fontsloth-layout-glyph-position-x item))
                (right (+ (fontsloth-layout-glyph-position-x item)
                          (fontsloth-layout-glyph-position-width item))))
            (setq start (if start (min start left) left)
                  end (if end (max end right) right)))))))
    (nreverse regions)))

(defmacro slothbar-workspaces--with-bar (m &rest body)
  "Evaluate BODY with M's bar and backend as the current context."
  (declare (indent 1) (debug t))
  `(let ((slothbar--bar (or (slothbar-module--bar ,m) slothbar--bar))
         (slothbar--backend (or (slothbar-module--backend ,m)
                                slothbar--backend)))
     ,@body))

(defun slothbar-workspaces--activate-ext-workspace (index)
  "Activate workspace INDEX through ext-workspace.
Return non-nil when an activation request was sent."
  (let* ((manager (and slothbar--backend
                       (slothbar-backend-ext-workspace-manager
                        slothbar--backend)))
         (workspace (and manager
                         (nth index
                              (slothbar-workspaces--ext-workspace-visible
                               manager)))))
    (when (and workspace
               (memq 'activate (elwl-workspace-capabilities workspace)))
      (elwl-workspace-activate workspace)
      (elwl-workspace-manager-commit manager)
      t)))

(defun slothbar-workspaces--activate-exwm (index)
  "Switch EXWM to workspace INDEX when EXWM is active."
  (when (and (boundp 'exwm--connection)
             exwm--connection
             (fboundp 'exwm-workspace-switch-create))
    (exwm-workspace-switch-create index)
    t))

(defun slothbar-workspaces--activate-ewmh (index)
  "Ask the EWMH window manager to switch to desktop INDEX."
  (when (and (slothbar-ewmh-mode)
             (slothbar-xcb-connection))
    (let ((connection (slothbar-xcb-connection))
          (root (slothbar-xcb-root-window-id)))
      (when root
        (xcb:+request connection
            (make-instance 'xcb:SendEvent
                           :propagate 0
                           :destination root
                           :event-mask xcb:EventMask:SubstructureRedirect
                           :event (xcb:marshal
                                   (make-instance 'xcb:ewmh:_NET_CURRENT_DESKTOP
                                                  :window root
                                                  :new-index index
                                                  :timestamp xcb:Time:CurrentTime)
                                   connection)))
        (xcb:flush connection)
        t))))

(defun slothbar-workspaces--activate (index)
  "Activate workspace INDEX with the active workspace source."
  (or (slothbar-workspaces--activate-ext-workspace index)
      (slothbar-workspaces--activate-exwm index)
      (slothbar-workspaces--activate-ewmh index)))

(cl-defmethod slothbar-module-click
  ((m slothbar-workspaces) x _y button)
  "Activate the workspace extent at X in M when BUTTON is :left."
  (when (eq button :left)
    (slothbar-workspaces--with-bar m
      (let* ((region
              (cl-find-if (lambda (region)
                            (and (>= x (nth 1 region))
                                 (< x (nth 2 region))))
                          (slothbar-workspaces--click-regions m)))
             (index (and region
                         (slothbar-workspaces--extent-tag-index (car region)))))
        (when index
          (slothbar-workspaces--activate index)
          t)))))

(cl-defmethod slothbar-module-hover
  ((m slothbar-workspaces) x y)
  "Track the workspace extent under the pointer in M."
  (let ((index
         (when (and x y)
           (let ((region
                  (cl-find-if (lambda (region)
                                (and (>= x (nth 1 region))
                                     (< x (nth 2 region))))
                              (slothbar-workspaces--click-regions m))))
             (and region
                  (slothbar-workspaces--extent-tag-index (car region)))))))
    (unless (eql index (slothbar-workspaces-hover-index m))
      (setf (slothbar-workspaces-hover-index m) index
            (slothbar-module-needs-refresh? m) t)
      (when (and slothbar--backend (slothbar-enabled-p))
        (slothbar-refresh-modules nil (slothbar-module--bar m))))))

(defun slothbar-workspaces-format-format (m)
  "Format M's format string."
  (slothbar-workspaces--with-bar m
    (let ((ws-list (or (map-elt (slothbar-module-cache m) 'ws-list)
                       (funcall slothbar-workspaces-generate-list-fn))))
      (format-spec (slothbar-module-format m)
                   (slothbar-workspaces--format-fn-spec
                    ws-list (slothbar-workspaces-hover-index m))
                   'ignore))))

(defun slothbar-workspaces--format-spec (icon)
  "Build the `format-spec' spec used to generate module text given ICON."
  `((?i . ,(string icon))))

(cl-defmethod slothbar-module-update-status ((m slothbar-workspaces))
  "Update M's text and format spec."
  (slothbar-workspaces--with-bar m
    (let* ((format-spec (slothbar-workspaces--format-spec
                         (slothbar-module-icon m)))
           (ws-list (funcall slothbar-workspaces-generate-list-fn))
           (txt (format-spec (slothbar-module-format m) format-spec t))
           (cache (slothbar-module-cache m)))
      (unless (equal ws-list (map-elt cache 'ws-list))
        (when cache
          (map-put! cache 'ws-list ws-list))
        (setf (slothbar-module-format-spec m) format-spec
              (slothbar-module-text m) txt
              (slothbar-module-needs-refresh? m) t)))))

(defun slothbar-workspaces--refresh-hook-fn (&rest _)
  "Refresh all workspaces modules, suitable for hooks."
  (slothbar-module--refresh-all-by-name "workspaces"))

(defvar slothbar-workspaces--generate-list-fn-prev (lambda () '())
  "To hold a previous value of `slothbar-workspaces--generate-list-fn'.")

;;; Begin slothbar-workspaces-generate-list-fn implementations

(declare-function shorten-strings "shorten" (lst))

;;; ext-workspace (Wayland)

(declare-function elwl-workspace-manager-groups "elwl-workspace" (manager))
(declare-function elwl-workspace-manager-workspaces "elwl-workspace" (manager))
(declare-function elwl-workspace-group-workspaces "elwl-workspace" (group))
(declare-function elwl-workspace-group-outputs "elwl-workspace" (group))
(declare-function elwl-workspace-hidden-p "elwl-workspace" (workspace))
(declare-function elwl-workspace-active-p "elwl-workspace" (workspace))
(declare-function elwl-workspace-urgent-p "elwl-workspace" (workspace))
(declare-function elwl-workspace-name "elwl-workspace" (workspace))
(declare-function elwl-workspace-id "elwl-workspace" (workspace))
(declare-function elwl-workspace-coordinates "elwl-workspace" (workspace))
(declare-function elwl-workspace-capabilities "elwl-workspace" (workspace))
(declare-function elwl-workspace-activate "elwl-workspace" (workspace))
(declare-function elwl-workspace-manager-commit "elwl-workspace" (manager))
(declare-function exwm-workspace-switch-create "exwm-workspace" (index))

(defun slothbar-workspaces--ext-workspace-status (active urgent)
  "Return a workspaces status list for ext-workspace flags.
ACTIVE and URGENT are booleans.  ext-workspace cannot report whether a
workspace is occupied, so inactive workspaces use :inactive."
  (cond ((and active urgent) '(:current :urgent))
        (active '(:current))
        (urgent '(:urgent))
        (t '(:inactive))))

(defun slothbar-workspaces--ext-workspace-coordinate-lessp (a b)
  "Return non-nil when coordinate list A sorts before coordinate list B."
  (while (and a b (= (car a) (car b)))
    (setq a (cdr a)
          b (cdr b)))
  (cond ((null a) (and b t))
        ((null b) nil)
        (t (< (car a) (car b)))))

(defun slothbar-workspaces--ext-workspace-sort (workspaces)
  "Return WORKSPACES ordered by their ext-workspace coordinates.
Workspaces without coordinates keep their relative order after those with
coordinates."
  (let ((indexed (cl-loop for workspace in workspaces
                          for index from 0
                          collect (cons workspace index))))
    (mapcar
     #'car
     (sort
      indexed
      (lambda (a b)
        (let ((ca (elwl-workspace-coordinates (car a)))
              (cb (elwl-workspace-coordinates (car b))))
          (cond ((and ca cb)
                 (if (equal ca cb)
                     (< (cdr a) (cdr b))
                   (slothbar-workspaces--ext-workspace-coordinate-lessp ca cb)))
                (ca t)
                (cb nil)
                (t (< (cdr a) (cdr b))))))))))

(defun slothbar-workspaces--ext-workspace-group (manager)
  "Return the workspace group for the bar's output in MANAGER.
Fall back to the first group."
  (let* ((groups (elwl-workspace-manager-groups manager))
         (output (slothbar-backend-output slothbar--backend)))
    (or (and output
             (cl-find-if
              (lambda (group)
                (memq output (elwl-workspace-group-outputs group)))
              groups))
        (car groups))))

(defun slothbar-workspaces--ext-workspace-visible (manager)
  "Return MANAGER's visible workspaces in display order."
  (let* ((group (slothbar-workspaces--ext-workspace-group manager))
         (workspaces (if group
                         (elwl-workspace-group-workspaces group)
                       (elwl-workspace-manager-workspaces manager))))
    (slothbar-workspaces--ext-workspace-sort
     (cl-remove-if #'elwl-workspace-hidden-p workspaces))))

(defun slothbar-workspaces-generate-list-fn-ext-workspace ()
  "Implement `slothbar-workspaces-generate-list-fn' for ext-workspace.
Use the workspace group assigned to the bar's output, falling back to the
first group.  Hidden workspaces are omitted."
  (let ((manager (and slothbar--backend
                      (slothbar-backend-ext-workspace-manager
                       slothbar--backend))))
    (when manager
      (cl-loop for workspace
               in (slothbar-workspaces--ext-workspace-visible manager)
               for index from 0
               collect
               (list (or (elwl-workspace-name workspace)
                         (elwl-workspace-id workspace)
                         (number-to-string index))
                     (slothbar-workspaces--ext-workspace-status
                      (elwl-workspace-active-p workspace)
                      (elwl-workspace-urgent-p workspace)))))))

(defun slothbar-workspaces--ext-workspace-change (backend)
  "Refresh workspaces modules when BACKEND's workspace state changes."
  (when (eq backend slothbar--backend)
    (slothbar-module--refresh-all-by-name "workspaces")))

(defun slothbar-workspaces--setup-defaults-ext-workspace ()
  "Configure the module to use ext-workspace when BACKEND supports it.
Return non-nil when ext-workspace was selected."
  (when (and slothbar--backend
             (slothbar-backend-ext-workspace-manager slothbar--backend))
    (require 'elwl-workspace nil t)
    (setq slothbar-workspaces-generate-list-fn
          #'slothbar-workspaces-generate-list-fn-ext-workspace)
    (add-hook 'slothbar-backend-workspace-change-hook
              #'slothbar-workspaces--ext-workspace-change)
    t))

(defun slothbar-workspaces--teardown-ext-workspace ()
  "Stop using ext-workspace for workspace updates."
  (remove-hook 'slothbar-backend-workspace-change-hook
               #'slothbar-workspaces--ext-workspace-change))

;;; ewmh (extended window manager hints)

(defun slothbar-workspaces-generate-list-fn-ewmh ()
  "Implement `slothbar-workspaces-generate-list-fn' for ewmh.

Extended window manager hints are intended as a standard mechanism for
window manager to communicate about workspaces, viewports, etc."
  (cl-loop for i from 0
           for name in slothbar-ewmh--desktop-names
           collect
           `(,name
             (,@(cond ((= i slothbar-ewmh--current-desktop)
                       '(:current))
                      ((cl-find i slothbar-ewmh--urgent-desktops)
                       '(:urgent))
                      ((cl-find i (map-values slothbar-ewmh--client-desktop-map))
                       '(:window))
                      (t '(:blankish)))))))

(defun slothbar-workspaces--ewmh-on-change (sym _)
  "Hook function for `slothbar-ewmh--on-change-hook'.

SYM is a symbol for a variable defined in slothbar-ewmh.el.

This will refresh any instances of the module when a relevant variable changes."
  (when (memq sym '(slothbar-ewmh--num-desktops
                    slothbar-ewmh--desktop-names
                    slothbar-ewmh--current-desktop
                    slothbar-ewmh--client-desktop-map
                    slothbar-ewmh--urgent-desktops))
    (slothbar-module--refresh-all-by-name "workspaces")))

(defun slothbar-workspaces--ewmh-toggle ()
  "Toggle workspaces status update using ewmh (extended window manager hints)."
  (if slothbar-ewmh-mode
      (progn
        (unless (eql slothbar-workspaces--generate-list-fn-prev
                     #'slothbar-workspaces-generate-list-fn-ewmh)
          (setq slothbar-workspaces--generate-list-fn-prev
                slothbar-workspaces-generate-list-fn))
        (setq slothbar-workspaces-generate-list-fn
              #'slothbar-workspaces-generate-list-fn-ewmh)
        (add-hook 'slothbar-ewmh--on-change-hook
                  #'slothbar-workspaces--ewmh-on-change))
    (remove-hook 'slothbar-ewmh--on-change-hook
                 #'slothbar-workspaces--ewmh-on-change)
    (setq slothbar-workspaces-generate-list-fn
          slothbar-workspaces--generate-list-fn-prev)))

(defun slothbar-workspaces--setup-defaults-ewmh ()
  "Configure slothbar-workspaces to display workspaces using ewmh data."
  (when (and slothbar-workspaces-ensure-ewmh (not slothbar-ewmh-mode))
    (slothbar-ewmh-mode))
  (when slothbar-ewmh-mode
    (slothbar-workspaces--ewmh-toggle)))

;;; EXWM

(defvar exwm-workspace--list)

(defun slothbar-workspaces--shorten-frame-buffer-names-exwm ()
  "Return an alist mapping EXWM frame buffer names to shortened names.

If shorten is not available, return nil."
  (when (featurep 'shorten)
    (shorten-strings
     (mapcar (lambda (frame)
               (buffer-name (car (buffer-list frame))))
             exwm-workspace--list))))

(declare-function exwm-workspace--count "exwm-workspace")
(declare-function exwm-workspace--position "exwm-workspace" (frame))

(defcustom slothbar-workspaces-exwm-shorten-names t
  "Non-nil to display shortened buffer names or nil for numbers.

This only happens if the shorten library is available and the particular
window manager integration supports it."
  :type 'boolean
  :group 'slothbar-workspaces)

(defcustom slothbar-workspaces-exwm-add-change-functions nil
  "Non-nil to update when changing the window selection or buffer.

Nil to update lazily."
  :type 'boolean
  :group 'slothbar-workspaces)

(defun slothbar-workspaces--watch-exwm-add-change-functions (_ new oper where)
  "Watch variable `slothbar-workspaces-exwm-add-change-functions'.

If ever OPER is equal to \\='set and the NEW value is not a buffer local
value indicated by WHERE, call
`slothbar-workspaces--exwm-modify-change-functions' with the value of
NEW."
  (when (and (not where) (eq 'set oper))
    (slothbar-workspaces--exwm-modify-change-functions new)))

(add-variable-watcher 'slothbar-workspaces-exwm-add-change-functions
                      'slothbar-workspaces--watch-exwm-add-change-functions)

(defvar exwm--id-buffer-alist)
(defvar exwm--frame)
(defvar exwm-workspace--current)

(defun slothbar-workspaces-generate-list-fn-exwm ()
  "Implement `slothbar-workspaces-generate-list-fn' for EXWM.

If the shorten library is available (from circe), the displayed names
are shortened buffer names.  Otherwise, names are the workspace numbers."
  (let ((not-empty (make-vector (exwm-workspace--count) nil))
        (shorts (slothbar-workspaces--shorten-frame-buffer-names-exwm)))
    (dolist (i exwm--id-buffer-alist)
      (with-current-buffer (cdr i)
        (when exwm--frame
          (setf (aref not-empty
                      (exwm-workspace--position exwm--frame))
                t))))
    (cl-loop for frame in exwm-workspace--list
             for pos = (exwm-workspace--position frame)
             for sn = (map-elt shorts (buffer-name (car (buffer-list frame))))
             collect
             `(,(or (and slothbar-workspaces-exwm-shorten-names sn)
                    (number-to-string pos))
               (,@(when (eq exwm-workspace--current frame)
                    '(:current))
                ,@(when (aref not-empty pos)
                    '(:window))
                ,@(unless (aref not-empty pos)
                    '(:blankish)))))))

(defun slothbar-workspaces--exwm-modify-change-functions (add?)
  "Add window buffer and selection change functions when ADD? is non-nil.
Remove chnage functions when ADD? is non-nil."
  (if add?
      (progn (add-hook 'window-selection-change-functions
                       #'slothbar-workspaces--refresh-hook-fn)
             (add-hook 'window-buffer-change-functions
                       #'slothbar-workspaces--refresh-hook-fn))
    (remove-hook 'window-selection-change-functions
                 #'slothbar-workspaces--refresh-hook-fn)
    (remove-hook 'window-buffer-change-functions
                 #'slothbar-workspaces--refresh-hook-fn)))

(defvar exwm--connection)

(defun slothbar-workspaces-setup-defaults-exwm ()
  "Configure slothbar-workspaces to display EXWM workspaces."
  (when (and (not slothbar-ewmh-mode)
             (boundp 'exwm--connection)
             exwm--connection)
    (when (and slothbar-workspaces-exwm-shorten-names
               (locate-library "shorten"))
      (require 'shorten))
    (setq slothbar-workspaces-generate-list-fn
          #'slothbar-workspaces-generate-list-fn-exwm)
    (add-hook 'exwm-workspace-switch-hook
              #'slothbar-workspaces--refresh-hook-fn)
    (slothbar-workspaces--exwm-modify-change-functions
     slothbar-workspaces-exwm-add-change-functions)))

(defun slothbar-workspaces-remove-hooks-exwm ()
  "Remove exwm related hooks for slothbar-workspaces."
  (remove-hook 'exwm-workspace-switch-hook
               #'slothbar-workspaces--refresh-hook-fn)
  (slothbar-workspaces--exwm-modify-change-functions nil))

;;; herbstluftwm

(defun slothbar-workspaces-generate-list-fn-herbstluftwm ()
  "Implement `slothbar-workspaces-generate-list-fn' for herbstluftwm."
  (when (executable-find "herbstclient")
    (when-let ((tags (with-temp-buffer
                       (when (eq 0 (call-process "herbstclient" nil t nil "tag_status"))
                         (buffer-string)))))
      (cl-loop for tag in (string-split tags)
               for name = (substring tag 1)
               collect
               `(,name
                 (,@(pcase tag
                    ((rx (seq ?# (+ anychar))) '(:current :window))
                    ((rx (seq ?: (+ anychar))) '(:window))
                    ((rx (seq ?. (+ anychar))) '(:blankish)))))))))

(defun slothbar-workspaces--make-herbstluft-events-filter ()
  "Return a process filter for herbstclient -i to monitor tags."
  (let ((processed-lines 0))
    (lambda (proc string)
      (when (buffer-live-p (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              ;; Insert the text, advancing the process marker.
              (goto-char (process-mark proc))
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc))))
          (save-excursion
            (goto-char (point-max))
            (beginning-of-line (and (looking-at-p "^$") 0))
            (when (> (line-number-at-pos) processed-lines)
              (setq processed-lines (line-number-at-pos))
              (when (string-prefix-p
                     "tag_changed"
                     (buffer-substring-no-properties (point) (line-end-position)))
                (slothbar-module--refresh-all-by-name "workspaces")))))))))

(defvar slothbar-workspaces--herbstluft-evt-listener-proc nil)

(defun slothbar-workspaces--start-herbstluft-event-listener ()
  "Start a process to listen for herbstluftwm events."
  (let* ((stdout (generate-new-buffer " *herbstluftwm events*"))
         (stderr (generate-new-buffer " *herbstluftwm errors*"))
         (process (make-process :name "herbstclient"
                      :command '("herbstclient" "-i")
                      :buffer stdout
                      :filter (slothbar-workspaces--make-herbstluft-events-filter)
                      :stderr stderr)))
    (unless process
      (error "Process unexpectedly nil"))
    (setq slothbar-workspaces--herbstluft-evt-listener-proc process)))

(defun slothbar-workspaces--stop-herbstluft-event-listener ()
  "Stop process to listen for herbstluftwm events."
  (when (and slothbar-workspaces--herbstluft-evt-listener-proc
             (process-live-p slothbar-workspaces--herbstluft-evt-listener-proc))
    (kill-process slothbar-workspaces--herbstluft-evt-listener-proc)))

(defun slothbar-workspaces-setup-defaults-herbstluftwm ()
  "Configure slothbar-workspaces to display herbstluftwm tags."
  (when (and (not slothbar-ewmh-mode)
             (executable-find "herbstclient")
             (equal "herbstluftwm" (getenv "DESKTOP_SESSION")))
    (setq slothbar-workspaces-generate-list-fn
          #'slothbar-workspaces-generate-list-fn-herbstluftwm)
    (slothbar-workspaces--start-herbstluft-event-listener)
    (add-hook 'slothbar-after-exit-hook #'slothbar-workspaces--stop-herbstluft-event-listener)))

;;; xmonad

(defvar slothbar-workspaces--xmonad-dbus-last-val nil
  "Holds the most recent dbus message from xmonad.")

(defun slothbar-workspaces-generate-list-fn-xmonad ()
  "Implement `slothbar-workspaces-generate-list-fn' for xmonad.

In order for this to work, a section in xmonad.hs like the following:

slothbarHook :: D.Client -> PP
slothbarHook dbus =
  let wrapper c s | s /= \"NSP\" = wrap (\":\" <> c <> \" \") \" . \" s
                  | otherwise  = mempty
      cur   = \"current\"
      vis   = \"visible\"
      urg   = \"urgent\"
      hid   = \"window\"
      bla   = \"blankish\"
  in  def { ppOutput          = dbusOutput dbus
          , ppVisible         = wrapper vis
          , ppHidden          = wrapper hid
          , ppHiddenNoWindows = wrapper bla
          , ppUrgent          = wrapper urg
          , ppCurrent         = wrapper cur
          , ppTitle           = wrapper \"title\" . shorten 90
          }

mySlothbarLogHook dbus = myLogHook <+> dynamicLogWithPP (slothbarHook dbus)"
  (when (stringp slothbar-workspaces--xmonad-dbus-last-val)
    (cl-loop for (status name)
             in (mapcar #'string-split
                        (butlast (string-split
                                  slothbar-workspaces--xmonad-dbus-last-val
                                  " \\. "
                                  t)))
             collect `(,name
                       (,(intern status))))))

(defun slothbar-workspaces--watch-xmonad-dbus-last-val (_ _ oper where)
  "Refresh if the dbus message variable is modified.

Only do this when WHERE is nil and OPER eq \\='set."
  (when (and (not where) (eq 'set oper))
    (run-with-timer 0 nil (lambda () (slothbar-module--refresh-all-by-name "workspaces")))))

(add-variable-watcher 'slothbar-workspaces--xmonad-dbus-last-val
                      #'slothbar-workspaces--watch-xmonad-dbus-last-val)

(defun slothbar-workspaces--xmonad-dbus-monitor (&optional workspaces)
  "Set `slothbar-workspaces--xmonad-dbus-last-val' to the value of WORKSPACES.

This is intended as a dbus monitor to set a value for
`slothbar-workspaces-generate-list-fn-xmonad' to parse."
  (when workspaces
    (setq slothbar-workspaces--xmonad-dbus-last-val workspaces)))

(defvar slothbar-workspaces--xmonad-dbus-object nil
  "The state object for the xmonad dbus monitor.")

(declare-function dbus-unregister-object "dbus")

(defun slothbar-workspaces--unregister-xmonad-dbus-monitor ()
  "Unregister the dbus object for the xmonad monitor."
  (dbus-unregister-object slothbar-workspaces--xmonad-dbus-object))

(declare-function dbus-register-monitor "dbus")

(defun slothbar-workspaces-setup-defaults-xmonad ()
  "Try to start a dbus monitor and set `slothbar-workspaces-generate-list-fn'.

This checks DESKTOP_SESSION to determine if xmonad is the current session.

Note that the xmonad config must send dbus events.  See the
`slothbar-workspaces-generate-list-fn-xmonad' docstring for an example."
  (when (and (not slothbar-ewmh-mode)
             (locate-library "dbus")
             (equal "xmonad" (getenv "DESKTOP_SESSION")))
    (setq
     slothbar-workspaces--xmonad-dbus-object
     (dbus-register-monitor :session
                            #'slothbar-workspaces--xmonad-dbus-monitor
                            :path "/org/xmonad/Log"
                            :interface "org.xmonad.Log"
                            :member "Update")
     slothbar-workspaces-generate-list-fn
     #'slothbar-workspaces-generate-list-fn-xmonad)
    (add-hook 'slothbar-after-exit-hook #'slothbar-workspaces--unregister-xmonad-dbus-monitor)))

(cl-defmethod slothbar-module-init :before ((m slothbar-workspaces))
  "Configure M and its initial icon and text."
  (unless (slothbar-workspaces--setup-defaults-ext-workspace)
    (slothbar-workspaces--setup-defaults-ewmh)
    (add-hook 'slothbar-ewmh-mode-hook #'slothbar-workspaces--ewmh-toggle)
    (when (locate-library "exwm")
      (slothbar-workspaces-setup-defaults-exwm)
      (add-hook 'slothbar-ewmh-mode-hook
                (lambda ()
                  (when slothbar-ewmh-mode
                    (slothbar-workspaces-remove-hooks-exwm)))))
    (slothbar-workspaces-setup-defaults-herbstluftwm)
    (slothbar-workspaces-setup-defaults-xmonad))
  (slothbar-module-update-status m))

(cl-defmethod slothbar-module-exit :before ((_m slothbar-workspaces))
  "Stop ext-workspace updates when the module exits."
  (slothbar-workspaces--teardown-ext-workspace))

(provide 'slothbar-workspaces)
;;; slothbar-workspaces.el ends here
