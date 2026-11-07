;;; slothbar-unit-test.el --- Pure unit tests for slothbar  -*- lexical-binding: t -*-

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

;; Unit tests for the pure parts of slothbar: layout, damage, and text draw
;; operations.  These need no X server or Wayland compositor.

;;; Code:

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(require 'ert)
(require 'fontsloth-layout)

(require 'slothbar)
(require 'slothbar-backend)
(require 'slothbar-format)
(require 'slothbar-sysinfo)
(require 'slothbar-layout)
(require 'slothbar-module)
(require 'slothbar-draw-op)
(require 'slothbar-focus)

;; `slothbar-layout' only declares this variable; `slothbar.el' gives it a
;; defcustom value.  Define it here so tests can bind it dynamically.
(defvar slothbar-width 0)

(defun slothbar-unit--module (name width)
  "Create a module called NAME with WIDTH."
  (slothbar-module-create :name name :width width))

(defun slothbar-unit--group-names (layout)
  "Return LAYOUT's module names grouped by alignment."
  (mapcar (lambda (group)
            (mapcar #'slothbar-module-name group))
          layout))

(ert-deftest slothbar-unit-layout-alignment ()
  "Modules are grouped by :left, :center, :right."
  (let* ((left (slothbar-unit--module "left" 10))
         (center (slothbar-unit--module "center" 10))
         (right (slothbar-unit--module "right" 10))
         (grouped (slothbar-layout
                   (list :left left :center center :right right))))
    (should (equal (slothbar-unit--group-names grouped)
                   '(("left") ("center") ("right")))))
  (let* ((first (slothbar-unit--module "first" 10))
         (second (slothbar-unit--module "second" 10)))
    ;; Without an instruction modules go to the right.
    (should (equal (slothbar-unit--group-names
                    (slothbar-layout (list first second)))
                   '(nil nil ("first" "second"))))))

(ert-deftest slothbar-unit-layout-coordinate ()
  "Coordinates respect left, centered, and right alignment."
  (let* ((m1 (slothbar-unit--module "a" 10))
         (m2 (slothbar-unit--module "b" 10))
         (m3 (slothbar-unit--module "c" 10))
         (layout (list (list m1) (list m2) (list m3)))
         (coords (slothbar-layout-coordinate layout 0 0 100)))
    (should (equal (mapcar #'car coords)
                   '((0 0) (45 0) (90 0)))))
  ;; The width argument is required for correctness; the defcustom is only a
  ;; compatibility default.
  (let* ((m1 (slothbar-unit--module "a" 10))
         (layout (list (list m1) nil nil))
         (slothbar-width 200))
    (should (equal (mapcar #'car
                           (slothbar-layout-coordinate layout 0 0))
                   '((0 0))))
    (should (equal (mapcar #'car
                           (slothbar-layout-coordinate layout 0 0 50))
                   '((0 0))))))

(ert-deftest slothbar-unit-layout-extents ()
  "Contiguous modules merge into one extent."
  (let* ((m1 (slothbar-unit--module "a" 30))
         (m2 (slothbar-unit--module "b" 25))
         (m3 (slothbar-unit--module "c" 100))
         (layout (list (list '(795 0) m1)
                       (list '(825 0) m2)
                       (list '(1400 0) m3))))
    (should (equal (slothbar-layout-extents layout)
                   '((795 850) (1400 1500))))))

(ert-deftest slothbar-unit-layout-subtract-extents ()
  "Damage is the part of the old extents not covered by the new ones."
  (should (equal (slothbar-layout-subtract-extents
                  '((0 180) (500 600))
                  '((0 168) (168 816)))
                 '((180 500) (600 816)))))

(defun slothbar-unit--glyph (char x)
  "Create a glyph position for CHAR at X."
  (fontsloth-layout-glyph-position-create
   :parent char :x x :y 0 :width 5 :height 10))

(ert-deftest slothbar-unit-text-draw-ops ()
  "Color commands split glyph runs and switch their colors."
  (let* ((a (slothbar-unit--glyph ?a 0))
         (b (slothbar-unit--glyph ?b 6))
         (c (slothbar-unit--glyph ?c 12))
         (d (slothbar-unit--glyph ?d 18))
         (ops (slothbar-text-draw-ops
               (list a '(:push) '(:fg 1) b '(:pop) c))))
    (should (= (length ops) 3))
    (should (equal (mapcar #'fontsloth-layout-glyph-position-parent
                           (slothbar-draw-op-glyph-positions (nth 0 ops)))
                   '(?a)))
    (should (eq (slothbar-draw-op-color (nth 0 ops))
                (slothbar-color-find 0 t)))
    (should (equal (mapcar #'fontsloth-layout-glyph-position-parent
                           (slothbar-draw-op-glyph-positions (nth 1 ops)))
                   '(?b)))
    (should (eq (slothbar-draw-op-color (nth 1 ops))
                (slothbar-color-find 1 t)))
    (should (equal (mapcar #'fontsloth-layout-glyph-position-parent
                           (slothbar-draw-op-glyph-positions (nth 2 ops)))
                   '(?c)))
    (should (eq (slothbar-draw-op-color (nth 2 ops))
                (slothbar-color-find 0 t)))))

(ert-deftest slothbar-unit-text-draw-ops-empty-runs ()
  "A color change before any glyph does not produce an empty run."
  (let* ((a (slothbar-unit--glyph ?a 0))
         (ops (slothbar-text-draw-ops (list '(:fg 2) a))))
    (should (= (length ops) 1))
    (should (eq (slothbar-draw-op-color (car ops))
                (slothbar-color-find 2 t)))))

(cl-defstruct (slothbar-unit-backend (:include slothbar-backend))
  "A test backend."
  features)

(cl-defstruct (slothbar-unit-unsupported
               (:include slothbar-module)
               (:constructor slothbar-unit-unsupported-create))
  "A module that is unsupported on the test backend.")

(cl-defstruct (slothbar-unit-clickable
               (:include slothbar-module)
               (:constructor slothbar-unit-clickable-create))
  "A module that records clicks."
  clicks)

(cl-defmethod slothbar-module-click
  ((m slothbar-unit-clickable) x y button)
  "Record a click on M."
  (push (list x y button) (slothbar-unit-clickable-clicks m))
  t)

(cl-defmethod slothbar-module-supported-p
  ((_m slothbar-unit-unsupported) (_backend slothbar-unit-backend))
  "Pretend this module needs X-only facilities."
  nil)

(ert-deftest slothbar-unit-make-bar-geometry ()
  "Bar construction honors per-bar geometry overrides and fallbacks."
  (let* ((slothbar-height 20)
         (geometry '((x-offset . 10) (y-offset . 20)
                     (width . 800) (height . 600)))
         (bar (slothbar--make-bar
               nil '(:id "main" :width 400 :height 30
                     :offset-x 5 :offset-y 60 :bottom t)
               0 geometry "HDMI-1"))
         (fallback (slothbar--make-bar
                    nil '(:id "other") 1 geometry "HDMI-2")))
    (should (equal (slothbar-id bar) '("main" "HDMI-1")))
    (should (equal (slothbar-output bar) "HDMI-1"))
    (should (= 400 (slothbar-pixel-width bar)))
    (should (= 30 (slothbar-pixel-height bar)))
    (should (= 5 (slothbar-x-offset bar)))
    (should (= 60 (slothbar-y-offset bar)))
    (should (slothbar-bottom bar))
    (should (equal (slothbar-id fallback) '("other" "HDMI-2")))
    (should (= 800 (slothbar-pixel-width fallback)))
    (should (= 20 (slothbar-pixel-height fallback)))
    (should (= 10 (slothbar-x-offset fallback)))
    (should (= 20 (slothbar-y-offset fallback)))
    (should-not (slothbar-bottom fallback))))

(ert-deftest slothbar-unit-reconcile-updates-id-bar ()
  "Reconciliation keeps an :id bar and updates its output and geometry."
  (let* ((backend (make-slothbar-unit-output-backend
                  :kind 'test :outputs '("A" "B")))
         (slothbar--backend backend)
         (slothbar--bars nil)
         (slothbar--enabled nil)
         (slothbar-modules nil)
         (slothbar-bars
          (list (list :id "main" :output "A" :modules nil :width 100))))
    (cl-letf (((symbol-function 'slothbar--find-display-geometry)
               (lambda (_display)
                 '((x-offset . 0) (y-offset . 0)
                   (width . 1000) (height . 700)))))
      (slothbar--reconcile-bars)
      (should (= 1 (length slothbar--bars)))
      (let ((bar (car slothbar--bars)))
        (setq slothbar-bars
              (list (list :id "main" :output "B"
                          :modules nil :width 400)))
        (slothbar--reconcile-bars)
        (should (= 1 (length slothbar--bars)))
        (should (eq bar (car slothbar--bars)))
        (should (equal (slothbar-output bar) "B"))
        (should (= 400 (slothbar-pixel-width bar)))
        (should (equal (slothbar-id bar) '("main" "B")))))))

(ert-deftest slothbar-unit-update-bar-by-id ()
  "An :id-tagged bar is updated in place, even when its output changes."
  (let* ((slothbar-modules nil)
         (backend (make-slothbar-unit-backend :kind 'test))
         (bar (slothbar-create
               :id '("main" "A")
               :config '(:id "main" :width 100 :height 20)
               :output "A" :pixel-width 100 :pixel-height 20
               :x-offset 1 :y-offset 2 :bottom nil))
         (spec (list
                :config '(:id "main" :width 400 :height 30
                          :offset-x 5 :offset-y 6)
                :index 0 :output "B")))
    (cl-letf (((symbol-function 'slothbar--find-display-geometry)
               (lambda (_display)
                 '((x-offset . 10) (y-offset . 20)
                   (width . 1000) (height . 700)))))
      (should (slothbar--update-bar backend bar spec)))
    (should (equal (slothbar-id bar) '("main" "B")))
    (should (equal (slothbar-output bar) "B"))
    (should (= 400 (slothbar-pixel-width bar)))
    (should (= 30 (slothbar-pixel-height bar)))
    (should (= 5 (slothbar-x-offset bar)))
    (should (= 6 (slothbar-y-offset bar)))
    (should (equal (slothbar--bar-key bar) '("main")))
    (should (equal (slothbar--spec-key spec) '("main")))))

(ert-deftest slothbar-unit-refresh-bottom-offset ()
  "Refresh computes the bottom offset from the bar height."
  (let* ((bar (slothbar-create
               :config '(:output :each :bottom t)
               :bottom t
               :output "A" :pixel-height 20 :pixel-width 100))
         (slothbar--bars (list bar))
         (slothbar--geometry-changed? nil))
    (cl-letf (((symbol-function 'slothbar--find-display-geometry)
               (lambda (_display)
                 '((x-offset . 0) (y-offset . 100)
                   (width . 800) (height . 200))))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) nil)))
      (slothbar-refresh))
    (should (= 280 (slothbar-y-offset bar)))))

(ert-deftest slothbar-unit-refresh-uses-bar-output ()
  "Refresh uses each bar's resolved output and honors :width."
  (let* ((bar-a (slothbar-create
                 :id 'a :config '(:output :each :width 300
                                  :offset-x 10 :offset-y 5)
                 :output "A" :pixel-height 20))
         (bar-b (slothbar-create
                 :id 'b :config '(:output :each
                                  :offset-x 20 :offset-y 30)
                 :output "B" :pixel-height 20))
         (slothbar--bars (list bar-a bar-b))
         (slothbar--geometry-changed? nil)
         (seen nil))
    (cl-letf (((symbol-function 'slothbar--find-display-geometry)
               (lambda (display)
                 (push display seen)
                 (pcase display
                   ("A" '((x-offset . 0) (y-offset . 0)
                          (width . 1000) (height . 100)))
                   ("B" '((x-offset . 1000) (y-offset . 0)
                          (width . 1000) (height . 100)))
                   (_ (error "Unexpected output %S" display)))))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) nil)))
      (slothbar-refresh))
    (should (equal (nreverse seen) '("A" "B")))
    (should (= 300 (slothbar-pixel-width bar-a)))
    (should (= 1000 (slothbar-pixel-width bar-b)))
    (should (= 10 (slothbar-x-offset bar-a)))
    (should (= 5 (slothbar-y-offset bar-a)))
    (should (= 20 (slothbar-x-offset bar-b)))
    (should (= 30 (slothbar-y-offset bar-b)))))

(ert-deftest slothbar-unit-per-bar-font-px-sizes ()
  "Each bar binds its own precomputed font pixel sizes."
  (let* ((bar-a (slothbar-create :font-px-sizes [20]))
         (bar-b (slothbar-create :font-px-sizes [30]))
         (seen nil))
    (slothbar--with-bar bar-a
      (push slothbar-font-px-size seen))
    (slothbar--with-bar bar-b
      (push slothbar-font-px-size seen))
    (should (equal seen '([30] [20])))))

(ert-deftest slothbar-unit-explicit-empty-bar-modules ()
  "A bar can explicitly opt out of the global module list."
  (let ((slothbar-modules (list (slothbar-module-create :name "global"))))
    (let ((bar (slothbar-create :config '(:modules nil))))
      (slothbar--construct-modules bar)
      (should-not (slothbar-module-list bar)))
    (let ((bar (slothbar-create :config '(:id "inherits"))))
      (slothbar--construct-modules bar)
      (should (= 1 (length (slothbar-module-list bar)))))))

(ert-deftest slothbar-unit-click-dispatch ()
  "Clicks are routed to the module under the pointer."
  (let* ((left (slothbar-unit-clickable-create :name "left" :width 10))
         (right (slothbar-unit-clickable-create :name "right" :width 10))
         (slothbar--last-layout
          (list (list (list 0 0) left)
                (list (list 20 0) right)))
         (slothbar-height 30))
    (should (slothbar--click 5 10 :left))
    (should (equal (slothbar-unit-clickable-clicks left)
                   '((5 10 :left))))
    (should (null (slothbar-unit-clickable-clicks right)))
    (should (slothbar--click 25 3 :right))
    (should (equal (slothbar-unit-clickable-clicks right)
                   '((5 3 :right))))
    (should-not (slothbar--click 100 3 :left))))

(cl-defstruct (slothbar-unit-output-backend
               (:include slothbar-unit-backend))
  "A test backend with named outputs."
  outputs)

(cl-defmethod slothbar-backend-output-names
  ((backend slothbar-unit-output-backend))
  "Return the fake output names for BACKEND."
  (slothbar-unit-output-backend-outputs backend))

(cl-defstruct (slothbar-unit-singleton
               (:include slothbar-module))
  "A module that may only appear once across all bars.")

(cl-defmethod slothbar-module-singleton-id
  ((_m slothbar-unit-singleton))
  "A test singleton identifier."
  'unit-singleton)

(ert-deftest slothbar-unit-watch-slothbar-bars ()
  "Changing slothbar-bars while running schedules reconciliation."
  (let ((slothbar-bars '((:id old)))
        (slothbar--enabled t)
        (called nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function) (funcall function)))
              ((symbol-function 'slothbar--reconcile-bars)
               (lambda () (setq called t))))
      (slothbar--watch-slothbar-bars nil '((:id new)) 'set nil))
    (should called)))

(ert-deftest slothbar-unit-singleton-bar-modules ()
  "Singleton modules are only constructed on the first bar that claims them."
  (let* ((bar-a (slothbar-create :config '(:modules nil)))
         (bar-b (slothbar-create :config '(:modules nil)))
         (one (make-slothbar-unit-singleton :name "one"))
         (two (make-slothbar-unit-singleton :name "two")))
    (setf (slothbar-module-list bar-a) (list one)
          (slothbar-config bar-b) (list :modules (list two)))
    (let ((slothbar--bars (list bar-a bar-b)))
      (slothbar--construct-modules bar-b)
      (should-not (slothbar-module-list bar-b)))
    (let ((bar-c (slothbar-create
                  :config (list :modules (list one two))))
          (slothbar--bars nil))
      (slothbar--construct-modules bar-c)
      (should (equal (slothbar-module-list bar-c) (list one))))))

(ert-deftest slothbar-unit-default-bar-config ()
  "An explicit bar list replaces the legacy default bar."
  (let ((slothbar-bars nil)
        (slothbar-modules nil))
    (should-not (slothbar--bar-configs)))
  (let ((slothbar-bars nil)
        (slothbar-modules '(slothbar-date-create)))
    (should (equal (slothbar--bar-configs) '(nil))))
  (let ((slothbar-bars '((:id one)))
        (slothbar-modules '(slothbar-date-create)))
    (should (equal (slothbar--bar-configs) '((:id one))))))

(ert-deftest slothbar-unit-bar-output-specs ()
  "Bar definitions expand per output, named output, or default output."
  (let ((backend (make-slothbar-unit-output-backend
                  :kind 'test :outputs '("A" "B")))
        (slothbar-bars
         (list (list :id 'each :output :each)
               (list :id 'listed :output '("B" "C"))
               (list :id 'named :output "A")
               (list :id 'default))))
    (should
     (equal
      (mapcar (lambda (spec)
                (list (plist-get (plist-get spec :config) :id)
                      (plist-get spec :output)))
              (slothbar--desired-bar-specs backend))
      '((each "A") (each "B")
        (listed "B")
        (named "A")
        (default nil))))))

(ert-deftest slothbar-unit-content-damage ()
  "Content damage covers old and new module rectangles."
  (let* ((module (slothbar-module-create :name "damage" :width 10))
         (old-layout (list (list (list 0 0) module)))
         (new-layout (list (list (list 5 0) module))))
    (should (equal (slothbar--content-damage
                    old-layout (list (cons module 10))
                    new-layout (list module))
                   '((0 15))))
    (should (equal (slothbar--merge-damage
                    '((0 10) (5 15) (20 30)))
                   '((0 15) (20 30))))))

(ert-deftest slothbar-unit-bar-local-refresh ()
  "Refreshing with a bar refreshes only that bar."
  (let* ((backend (make-slothbar-unit-backend :kind 'test))
         (module-a (slothbar-module-create :name "a" :width 10))
         (module-b (slothbar-module-create :name "b" :width 10))
         (bar-a (slothbar-create :id "a" :display-backend backend
                                 :module-list (list module-a)
                                 :pixel-width 100 :pixel-height 20))
         (bar-b (slothbar-create :id "b" :display-backend backend
                                 :module-list (list module-b)
                                 :pixel-width 100 :pixel-height 20))
         (slothbar--bars (list bar-a bar-b))
         (slothbar--bar bar-a)
         (presented nil))
    (setf (slothbar-module-needs-refresh? module-a) t
          (slothbar-module-needs-refresh? module-b) t)
    (cl-letf (((symbol-function 'slothbar-backend-present)
               (lambda (_backend bar &rest _)
                 (push (slothbar-id bar) presented)))
              ((symbol-function 'slothbar-module-refresh)
               (lambda (_module) nil)))
      (slothbar-refresh-modules nil bar-a))
    (should (equal presented '("a")))))

(ert-deftest slothbar-unit-theme-change-updates-every-bar ()
  "Theme changes update module colors in every bar."
  (let* ((backend (make-slothbar-unit-backend :kind 'test))
         (m-a (slothbar-module-create :name "a"))
         (m-b (slothbar-module-create :name "b"))
         (bar-a (slothbar-create :display-backend backend
                                 :module-list (list m-a)))
         (bar-b (slothbar-create :display-backend backend
                                 :module-list (list m-b)))
         (slothbar--bars (list bar-a bar-b))
         (slothbar--enabled t)
         (refreshed nil))
    (cl-letf (((symbol-function 'slothbar--background-color)
               (lambda () 'background))
              ((symbol-function 'slothbar--foreground-color)
               (lambda () 'foreground))
              ((symbol-function 'run-with-idle-timer)
               (lambda (_seconds _repeat function) (funcall function)))
              ((symbol-function 'slothbar--refresh)
               (lambda (&optional _) (setq refreshed t)))
              ((symbol-function 'slothbar-refresh-modules)
               (lambda (&optional _) nil)))
      (slothbar--on-theme-change nil))
    (dolist (module (list m-a m-b))
      (should (eq (slothbar-module-rgb-background-color
                   (slothbar-module-colors module))
                  'background))
      (should (eq (slothbar-module-rgb-foreground-color
                   (slothbar-module-colors module))
                  'foreground))
      (should (slothbar-module-needs-refresh? module)))
    (should refreshed)))

(ert-deftest slothbar-unit-multi-bar-pointer-routing ()
  "Pointer clicks are routed to the bar that received the event."
  (let* ((backend (make-slothbar-unit-backend :kind 'test))
         (bar-a (slothbar-create :id "a" :display-backend backend
                                 :pixel-width 100 :pixel-height 30))
         (bar-b (slothbar-create :id "b" :display-backend backend
                                 :pixel-width 100 :pixel-height 30))
         (module-a (slothbar-unit-clickable-create :name "a" :width 10))
         (module-b (slothbar-unit-clickable-create :name "b" :width 10))
         (slothbar--bars (list bar-a bar-b))
         (slothbar--bar bar-a)
         (slothbar--enabled t))
    (setf (slothbar-placements bar-a)
          (list (list (list 0 0) module-a))
          (slothbar-placements bar-b)
          (list (list (list 0 0) module-b)))
    (should (slothbar--on-pointer-button backend bar-b 4 5 :left))
    (should (equal (slothbar-unit-clickable-clicks module-b)
                   '((4 5 :left))))
    (should (null (slothbar-unit-clickable-clicks module-a)))))

(ert-deftest slothbar-unit-module-gating ()
  "Unsupported modules are dropped during module construction."
  (let ((slothbar--backend (make-slothbar-unit-backend :kind 'test))
        (slothbar-modules
         (list (slothbar-module-create :name "plain")
               (slothbar-unit-unsupported-create :name "x-only"))))
    (slothbar--construct-modules)
    (should (equal (mapcar #'slothbar-module-name slothbar--modules)
                   '("plain")))))

(require 'slothbar-workspaces)

(ert-deftest slothbar-unit-ext-workspace-status ()
  "Ext-workspace state flags map to the documented status lists."
  (should (equal (slothbar-workspaces--ext-workspace-status nil nil)
                 '(:inactive)))
  (should (equal (slothbar-workspaces--ext-workspace-status t nil)
                 '(:current)))
  (should (equal (slothbar-workspaces--ext-workspace-status nil t)
                 '(:urgent)))
  (should (equal (slothbar-workspaces--ext-workspace-status t t)
                 '(:current :urgent))))

(ert-deftest slothbar-unit-ext-workspace-coordinate-order ()
  "Ext-workspace coordinates sort lexicographically."
  (should (slothbar-workspaces--ext-workspace-coordinate-lessp
           '(1) '(2)))
  (should (slothbar-workspaces--ext-workspace-coordinate-lessp
           '(1) '(1 0)))
  (should-not (slothbar-workspaces--ext-workspace-coordinate-lessp
               '(1 0) '(1)))
  (should-not (slothbar-workspaces--ext-workspace-coordinate-lessp
               '(1) '(1)))
  (cl-letf (((symbol-function 'elwl-workspace-coordinates)
             (lambda (workspace) (plist-get workspace :coordinates))))
    (should
     (equal (mapcar (lambda (workspace)
                      (plist-get workspace :name))
                    (slothbar-workspaces--ext-workspace-sort
                     (list '(:name "unset" :coordinates nil)
                           '(:name "second" :coordinates (2))
                           '(:name "first" :coordinates (1))
                           '(:name "first-b" :coordinates (1 0)))))
            '("first" "first-b" "second" "unset")))))

(ert-deftest slothbar-unit-color-parse-embedded-codes ()
  "Color codes parse correctly after literal text."
  (should (equal (slothbar-color-parse-string "a^1b")
                 '("a" (:bg :reset) (:fg 1) (:reverse nil) "b")))
  (should (equal (slothbar-color-parse-string "a^f1b")
                 '("a" (:font 1) "b")))
  (should (equal (slothbar-color-parse-string "a^1b^0c")
                 '("a" (:bg :reset) (:fg 1) (:reverse nil)
                   "b" (:bg :reset) (:fg 0) (:reverse nil) "c"))))

(ert-deftest slothbar-unit-color-parse-extent-markers ()
  "Extent markers survive parsing as layout-only forms."
  (should (equal (slothbar-color-parse-string "^M0a^M;b")
                 '((:mark-extent ?0) "a" (:end-extent) "b")))
  (should (equal (slothbar-color-parse-string "^MZ")
                 '((:mark-extent ?Z))))
  (should (equal (slothbar-color-parse-string "^M;")
                 '((:end-extent))))
  (should (equal (slothbar-color-parse-string "^M abc")
                 '("^M abc")))
  (should (equal (slothbar-color-parse-string "")
                 '(""))))

(ert-deftest slothbar-unit-text-draw-ops-ignores-extent-markers ()
  "Extent markers do not create draw operations."
  (let* ((a (slothbar-unit--glyph ?a 0))
         (ops (slothbar-text-draw-ops
               (list '(:mark-extent ?0) a '(:end-extent)))))
    (should (= (length ops) 1))
    (should (equal (mapcar #'fontsloth-layout-glyph-position-parent
                           (slothbar-draw-op-glyph-positions (car ops)))
                   '(?a)))))

(ert-deftest slothbar-unit-workspaces-click-regions ()
  "Workspace extent markers produce clickable x ranges."
  (let* ((m (slothbar-workspaces-create))
         (a (slothbar-unit--glyph ?1 10))
         (b (slothbar-unit--glyph ?\] 15))
         (c (slothbar-unit--glyph ?2 30)))
    (setf (slothbar-module-text-layout m)
          (list '(:mark-extent ?0) a b '(:end-extent)
                '(:mark-extent ?1) c '(:end-extent)))
    (should (equal (slothbar-workspaces--click-regions m)
                   '((?0 10 20) (?1 30 35)))))
  (let ((spec (slothbar-workspaces--format-fn-spec
               '(("1" (:current)) ("2" (:inactive))))))
    (should (string-match-p "\\^M0" (alist-get ?w spec)))
    (should (string-match-p "\\^M;" (alist-get ?w spec)))))

(ert-deftest slothbar-unit-format-compile ()
  "Format tokens compile to the expected `format-spec' forms."
  (let ((fields '((:cpu . ?c) (:name . ?n) (:string . "s"))))
    (should (equal (slothbar-format-compile '("CPU " :cpu) fields)
                   "CPU %c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :width 4 :pad :zero :suffix "%"))
                    fields)
                   "%04c%%"))
    (should (equal (slothbar-format-compile
                    (list (cons :cpu 4))
                    fields)
                   "%4c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :pad :left :width 4))
                    fields)
                   "%-4c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :truncate :left :width 5))
                    fields)
                   "%<5c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :truncate :right :width 5))
                    fields)
                   "%>5c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :case :upper))
                    fields)
                   "%^c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :case :lower))
                    fields)
                   "%_c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :width 5 :precision 2))
                    fields)
                   "%5.2c"))
    (should (equal (slothbar-format-compile
                    (list (list :cpu :prefix "[" :suffix "%]"))
                    fields)
                   "[%c%%]"))
    (should (equal (slothbar-format-compile
                    (list (list :anything :char ?z))
                    nil)
                   "%z"))
    (should (equal (slothbar-format-compile '(:string) fields)
                   "%s"))
    (should (equal (slothbar-format-compile '("100%" :cpu) fields)
                   "100%%%c"))
    (should (equal (slothbar-format-compile '("^1CPU^0 " :cpu) fields)
                   "^1CPU^0 %c"))))

(ert-deftest slothbar-unit-format-compile-format-spec ()
  "Compiled format strings work with `format-spec'."
  (let ((format
         (slothbar-format-compile
          (list (list :cpu :width 4 :pad :zero)
                " "
                (list :name :width 5 :truncate :right))
          '((:cpu . ?c) (:name . ?n)))))
    (should (equal format "%04c %>5n"))
    (should (equal (format-spec format '((?c . "12") (?n . "abcdef")))
                   "0012 abcde"))))

(ert-deftest slothbar-unit-format-compile-errors ()
  "The compiler rejects unknown fields and malformed options."
  (should-error (slothbar-format-compile '(:missing) '((:cpu . ?c))))
  (should-error (slothbar-format-compile
                 (list (list :cpu :bogus 1))
                 '((:cpu . ?c))))
  (should-error (slothbar-format-compile
                 (list (list :cpu :pad :middle))
                 '((:cpu . ?c))))
  (should-error (slothbar-format-compile
                 (list (list :cpu :width -1))
                 '((:cpu . ?c))))
  (should-error (slothbar-format-compile '(:cpu) '((:cpu . ?%)))))

(ert-deftest slothbar-unit-sysinfo-registry ()
  "Sysinfo fields have unique keys and resolve through the compiler."
  (let ((characters (mapcar #'slothbar-sysinfo-field-char
                            slothbar-sysinfo-fields))
        (keywords (mapcar #'slothbar-sysinfo-field-keyword
                          slothbar-sysinfo-fields)))
    (should (= (length characters)
               (length (delete-dups (copy-sequence characters)))))
    (should (= (length keywords)
               (length (delete-dups (copy-sequence keywords))))))
  (let ((field (slothbar-sysinfo-find-field :cpu-usage)))
    (should field)
    (should (= (slothbar-sysinfo-field-char field) ?c))
    (should (eq (slothbar-sysinfo-field-formatter field) 'percent))
    (should (eq (slothbar-sysinfo-find-field-by-char ?c) field)))
  (dolist (field slothbar-sysinfo-fields)
    (should (assq (slothbar-sysinfo-field-formatter field)
                  slothbar-sysinfo--formatters)))
  (should (equal (slothbar-sysinfo-compile-format
                  (list (list :cpu-usage :width 4 :suffix "%")))
                 "%4c%%")))

(ert-deftest slothbar-unit-sysinfo-formatters ()
  "Sysinfo formatters render values in their display units."
  (should (equal (slothbar-sysinfo-format-percent 42.4 0) "42"))
  (should (equal (slothbar-sysinfo-format-percent 42.4 1) "42.4"))
  (should (equal (slothbar-sysinfo-format-byte-count 0) "0B"))
  (should (equal (slothbar-sysinfo-format-byte-count 1024) "1.0KiB"))
  (should (equal (slothbar-sysinfo-format-byte-count 1536 0) "2KiB"))
  (should (equal (slothbar-sysinfo-format-rate 1024) "1.0KiB/s"))
  (should (equal (slothbar-sysinfo-format-duration 59) "59s"))
  (should (equal (slothbar-sysinfo-format-duration 3661) "1h 1m"))
  (should (equal (slothbar-sysinfo-format-duration 90061) "1d 1h"))
  (should (equal (slothbar-sysinfo-format-load 1.234) "1.23"))
  (should (equal (slothbar-sysinfo-format-frequency 3200000000) "3.2GHz"))
  (should (equal (slothbar-sysinfo-format-frequency 800000000) "800MHz"))
  (should (equal (slothbar-sysinfo-format-temperature 42.24 1) "42.2°C"))
  (let ((field (slothbar-sysinfo-find-field :cpu-usage)))
    (should (equal (slothbar-sysinfo-format-value field 42) "42"))
    (should (equal (slothbar-sysinfo-format-value field nil)
                   slothbar-sysinfo-missing))))

(ert-deftest slothbar-unit-sysinfo-parsers ()
  "Pure sysinfo parsers handle representative Linux data."
  (let ((meminfo
         (slothbar-sysinfo-parse-meminfo
          (concat "MemTotal:       16384000 kB\n"
                  "MemFree:         8000000 kB\n"
                  "MemAvailable:   12000000 kB\n"
                  "Buffers:          100000 kB\n"
                  "Cached:          4000000 kB\n"
                  "SwapTotal:       2000000 kB\n"
                  "SwapFree:        1500000 kB\n"))))
    (should (= (alist-get :memory-total meminfo)
               (* 16384000 1024)))
    (should (= (alist-get :memory-free meminfo)
               (* 8000000 1024)))
    (should (= (alist-get :memory-available meminfo)
               (* 12000000 1024)))
    (should (= (alist-get :swap-total meminfo)
               (* 2000000 1024))))
  (let* ((previous
          (slothbar-sysinfo-parse-cpu-stat
           "cpu  100 0 100 800 0 0 0 0 0 0\n"))
         (current
          (slothbar-sysinfo-parse-cpu-stat
           "cpu  200 0 150 900 0 0 0 0 0 0\n")))
    (should (= (alist-get :total previous) 1000))
    (should (= (alist-get :idle previous) 800))
    (should (= (slothbar-sysinfo-cpu-usage previous current) 60.0)))
  (let ((load (slothbar-sysinfo-parse-loadavg
               "0.50 0.25 0.10 3/123 4567\n")))
    (should (= (alist-get :load-1 load) 0.5))
    (should (= (alist-get :load-5 load) 0.25))
    (should (= (alist-get :running load) 3))
    (should (= (alist-get :total load) 123)))
  (should (= (slothbar-sysinfo-parse-uptime "12345.67 999.0\n")
             12345.67))
  (let* ((net (slothbar-sysinfo-parse-net-dev
               (concat "Inter-|   Receive                          |  Transmit\n"
                       " face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed\n"
                       "  eth0: 1000 10 0 0 0 0 0 0 2000 20 0 0 0 0 0 0\n")))
         (stats (cdr (assoc "eth0" net))))
    (should (= (alist-get :rx-bytes stats) 1000))
    (should (= (alist-get :rx-packets stats) 10))
    (should (= (alist-get :tx-bytes stats) 2000))
    (should (= (alist-get :tx-packets stats) 20)))
  (let ((os (slothbar-sysinfo-parse-os-release
             "NAME=\"Guix System\"\nID=guix\n")))
    (should (equal (cdr (assoc "NAME" os)) "Guix System"))
    (should (equal (cdr (assoc "ID" os)) "guix")))
  (let ((cpu (slothbar-sysinfo-parse-cpuinfo
              "model name\t: Example CPU\ncpu MHz\t\t: 3200.000\n")))
    (should (equal (alist-get :cpu-brand cpu) "Example CPU"))
    (should (= (alist-get :cpu-frequency cpu) 3200000000.0))))


(ert-deftest slothbar-unit-sysinfo-update-intervals ()
  "Sysinfo providers run only when their interval has elapsed."
  (let* ((state (slothbar-sysinfo-state-create))
         (calls nil)
         (slothbar-sysinfo--providers
          (list (cons 'once (lambda (_state _now)
                              (push 'once calls) '((:a . 1))))
                (cons 'fast (lambda (_state _now)
                              (push 'fast calls) '((:b . 2))))
                (cons 'slow (lambda (_state _now)
                              (push 'slow calls) '((:c . 3))))))
         (slothbar-sysinfo-intervals
          '((once . nil) (fast . 1) (slow . 10))))
    (slothbar-sysinfo-update state 100)
    (should (equal (reverse calls) '(once fast slow)))
    (should (= (alist-get :a (slothbar-sysinfo-state-values state)) 1))
    (should (= (alist-get :b (slothbar-sysinfo-state-values state)) 2))
    (should (= (alist-get :c (slothbar-sysinfo-state-values state)) 3))
    (setq calls nil)
    (slothbar-sysinfo-update state 100.5)
    (should (null calls))
    (slothbar-sysinfo-update state 101.1)
    (should (equal calls '(fast)))
    (setq calls nil)
    (slothbar-sysinfo-update state 111.1)
    (should (equal (reverse calls) '(fast slow)))
    (setq calls nil)
    (slothbar-sysinfo-update state 120 t)
    (should (equal (reverse calls) '(once fast slow)))))

(ert-deftest slothbar-unit-sysinfo-provider-cpu ()
  "The CPU provider tracks counters and exposes cpuinfo fields."
  (let ((stat "cpu  100 0 100 800 0 0 0 0 0 0\n")
        (cpuinfo (concat "processor\t: 0\n"
                         "model name\t: Example CPU\n"
                         "cpu MHz\t\t: 1000.000\n"
                         "processor\t: 1\n")))
    (cl-letf (((symbol-function 'slothbar-sysinfo--read-file)
               (lambda (path)
                 (cond ((equal path "/proc/stat") stat)
                       ((equal path "/proc/cpuinfo") cpuinfo)))))
      (let* ((state (slothbar-sysinfo-state-create))
             (first (slothbar-sysinfo--read-cpu state 0)))
        (should-not (alist-get :cpu-usage first))
        (should (equal (alist-get :cpu-brand first) "Example CPU"))
        (should (= (alist-get :cpu-frequency first) 1000000000.0))
        (should (= (alist-get :cpu-count first) 2))
        (setq stat "cpu  200 0 150 900 0 0 0 0 0 0\n")
        (let ((second (slothbar-sysinfo--read-cpu state 1)))
          (should (= (alist-get :cpu-usage second) 60.0)))))))

(ert-deftest slothbar-unit-sysinfo-provider-memory-network-disk ()
  "Memory, network, and disk providers derive their values from samples."
  (let ((meminfo (concat "MemTotal:       1000 kB\n"
                         "MemFree:         500 kB\n"
                         "MemAvailable:    750 kB\n"
                         "SwapTotal:       200 kB\n"
                         "SwapFree:         50 kB\n")))
    (cl-letf (((symbol-function 'slothbar-sysinfo--read-file)
               (lambda (path)
                 (when (equal path "/proc/meminfo") meminfo))))
      (let ((values (slothbar-sysinfo--read-memory
                     (slothbar-sysinfo-state-create) 0)))
        (should (= (alist-get :memory-total values) (* 1000 1024)))
        (should (= (alist-get :memory-used values) (* 250 1024)))
        (should (= (alist-get :memory-usage values) 25.0))
        (should (= (alist-get :swap-usage values) 75.0)))))
  (let ((netdev (concat "  eth0: 100 1 0 0 0 0 0 0 200 2 0 0 0 0 0 0\n")))
    (cl-letf (((symbol-function 'slothbar-sysinfo--read-file)
               (lambda (path)
                 (when (equal path "/proc/net/dev") netdev))))
      (let ((state (slothbar-sysinfo-state-create)))
        (let ((first (slothbar-sysinfo--read-network state 100)))
          (should (equal (alist-get :net-interface first) "eth0"))
          (should (= (alist-get :net-down first) 0.0))
          (should (= (alist-get :net-up first) 0.0)))
        (setq netdev "  eth0: 1100 1 0 0 0 0 0 0 2200 2 0 0 0 0 0 0\n")
        (let ((second (slothbar-sysinfo--read-network state 110)))
          (should (= (alist-get :net-down second) 100.0))
          (should (= (alist-get :net-up second) 200.0))))))
  (cl-letf (((symbol-function 'file-system-info)
             (lambda (_path) '(1000 250 0))))
    (let ((values (slothbar-sysinfo--read-disk
                   (slothbar-sysinfo-state-create) 0)))
      (should (= (alist-get :disk-total values) 1000))
      (should (= (alist-get :disk-used values) 750))
      (should (= (alist-get :disk-usage values) 75.0)))))

(ert-deftest slothbar-unit-sysinfo-module-render ()
  "Sysinfo module rendering applies field formatting and missing values."
  (let* ((values '((:cpu-usage . 42) (:memory-usage . 17)))
         (format (slothbar-sysinfo-compile-format
                  '((:cpu-usage :width 3 :suffix "%")
                    " "
                    (:memory-usage :width 3 :suffix "%")))))
    (should (equal (slothbar-sysinfo--render format values)
                   " 42%  17%"))
    (should (equal (slothbar-sysinfo--render "%c" nil) "--"))))

(ert-deftest slothbar-unit-sysinfo-module-update ()
  "The sysinfo module updates its format spec and text."
  (let ((m (slothbar-sysinfo-create))
        (slothbar-sysinfo-format
         '((:cpu-usage :width 3 :suffix "%")
           " "
           (:memory-usage :width 3 :suffix "%"))))
    (setf (slothbar-sysinfo-state m) (slothbar-sysinfo-state-create))
    (cl-letf (((symbol-function 'slothbar-sysinfo-collect)
               (lambda (&optional _state _now _force)
                 '((:cpu-usage . 5) (:memory-usage . 10)))))
      (slothbar-module-update-status m))
    (should (equal (slothbar-module-text m) "  5%  10%"))
    (should (equal (alist-get ?c (slothbar-module-format-spec m)) "5"))
    (should (slothbar-module-needs-refresh? m))))

(ert-deftest slothbar-unit-sysinfo-shared-provider ()
  "One provider tick updates all subscribed sysinfo modules."
  (let* ((m-a (slothbar-sysinfo-create))
         (m-b (slothbar-sysinfo-create))
         (slothbar-sysinfo--subscribers (list m-a m-b))
         (slothbar-sysinfo--shared-state (slothbar-sysinfo-state-create))
         (slothbar-sysinfo-format '((:cpu-usage :width 3)))
         (updates 0))
    (cl-letf (((symbol-function 'slothbar-sysinfo-update)
               (lambda (state &optional _now _force)
                 (cl-incf updates)
                 (setf (slothbar-sysinfo-state-values state)
                       '((:cpu-usage . 5)))
                 state)))
      (slothbar-sysinfo--provider-tick))
    (should (= updates 1))
    (should (equal (slothbar-module-text m-a) "  5"))
    (should (equal (slothbar-module-text m-b) "  5"))))

(ert-deftest slothbar-unit-sysinfo-percent-suffix ()
  "A literal percent in a colored suffix renders once."
  (let ((format (slothbar-sysinfo-compile-format
                 '((:cpu-usage :width 3 :suffix "^[^6%^]")))))
    (should (equal (slothbar-sysinfo--render format '((:cpu-usage . 3)))
                   "  3^[^6%^]"))))

(ert-deftest slothbar-unit-format-compile-plan-decimals ()
  "Format plans keep repeated fields separate for per-token decimals."
  (let* ((fields '((:cpu . ?c)))
         (plan (slothbar-format-compile-plan
                '((:cpu :decimals 0)
                  " "
                  (:cpu :decimals 1))
                fields))
         (slots (slothbar-format-plan-slots plan)))
    (should (= (length slots) 2))
    (should (equal (mapcar #'slothbar-format-slot-keyword slots)
                   '(:cpu :cpu)))
    (should (equal (mapcar (lambda (slot)
                             (plist-get (slothbar-format-slot-options slot)
                                        :decimals))
                           slots)
                   '(0 1)))
    (should-not (equal (slothbar-format-slot-char (nth 0 slots))
                       (slothbar-format-slot-char (nth 1 slots))))
    (should (equal (slothbar-format-compile
                    '((:cpu :decimals 1))
                    fields)
                   "%c"))
    (should-error (slothbar-format-compile-plan
                   '((:cpu :decimals -1))
                   fields))))

(ert-deftest slothbar-unit-sysinfo-token-decimals ()
  "Sysinfo honors per-token decimal overrides."
  (let* ((plan (slothbar-format-compile-plan
                '((:cpu-usage :decimals 1)
                  " "
                  (:cpu-usage :decimals 0))
                (slothbar-sysinfo-field-characters))))
    (should (equal (slothbar-sysinfo--render-plan
                    plan '((:cpu-usage . 12.34)))
                   "12.3 12")))
  (let ((m (slothbar-sysinfo-create))
        (slothbar-sysinfo-format
         '((:cpu-usage :decimals 1)
           " "
           (:cpu-usage :decimals 0))))
    (setf (slothbar-sysinfo-state m) (slothbar-sysinfo-state-create))
    (cl-letf (((symbol-function 'slothbar-sysinfo-collect)
               (lambda (&optional _state _now _force)
                 '((:cpu-usage . 12.34)))))
      (slothbar-module-update-status m))
    (should (equal (slothbar-module-text m) "12.3 12"))))

(ert-deftest slothbar-unit-font-vertical-offset ()
  "Vertical offset centers a line box in the bar."
  (cl-letf (((symbol-function 'fontsloth-scale-horizontal-line-metrics)
             (lambda (_font _px)
               (fontsloth-line-metrics-create 10 2 0))))
    (let ((slothbar-height 30))
      (should (= (slothbar-font--vertical-offset 'font 12) -11.0)))
    (let ((slothbar-height 8))
      (should (= (slothbar-font--vertical-offset 'font 12) 0.0)))))

(ert-deftest slothbar-unit-font-px-scale-watcher ()
  "Changing the font scale recomputes pixel sizes."
  (let ((slothbar-height 30)
        (slothbar-font-px-scale 1.0)
        (slothbar-font-px-size nil)
        (heights nil))
    (cl-letf (((symbol-function 'slothbar-font--precompute-px-sizes)
               (lambda (height)
                 (push height heights)
                 :scaled))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) nil)))
      (slothbar-font--watch-px-scale
       'slothbar-font-px-scale 0.5 'set nil)
      (should (equal heights '(30)))
      (should (eq slothbar-font-px-size :scaled)))))

(ert-deftest slothbar-unit-color-brighten ()
  "Brightening clamps channels and preserves alpha."
  (let* ((color (slothbar-color-rgb-create
                 :red 100 :green 200 :blue 300 :alpha 5000))
         (bright (slothbar-color-brighten color 2)))
    (should (= (slothbar-color-rgb-red bright) 200))
    (should (= (slothbar-color-rgb-green bright) 400))
    (should (= (slothbar-color-rgb-blue bright) 600))
    (should (= (slothbar-color-rgb-alpha bright) 5000)))
  (let ((bright (slothbar-color-brighten
                 (slothbar-color-rgb-create
                  :red #xff00 :green #xffff :blue 1)
                 10)))
    (should (= (slothbar-color-rgb-red bright) #xffff))
    (should (= (slothbar-color-rgb-green bright) #xffff))
    (should (= (slothbar-color-rgb-blue bright) 10))))

(ert-deftest slothbar-unit-text-draw-ops-bright ()
  "The ^B state brightens only its glyph run."
  (let* ((a (slothbar-unit--glyph ?a 0))
         (b (slothbar-unit--glyph ?b 6))
         (ops (slothbar-text-draw-ops
               (list '(:bright t) a '(:bright nil) b)))
         (normal (slothbar-color-find 0 t)))
    (should (= (length ops) 2))
    (should (equal (slothbar-draw-op-color (nth 0 ops))
                   (slothbar-color-brighten normal)))
    (should (equal (slothbar-draw-op-color (nth 1 ops)) normal))))

(ert-deftest slothbar-unit-workspaces-hover-markers ()
  "Only the hovered workspace label gets a bright range."
  (let ((hovered (alist-get
                  ?w (slothbar-workspaces--format-fn-spec
                      '(("1" (:current)) ("2" (:inactive))) 0)))
        (plain (alist-get
                ?w (slothbar-workspaces--format-fn-spec
                    '(("1" (:current)) ("2" (:inactive))) 1))))
    (should (string-match-p "\\^B" hovered))
    (should (string-match-p "\\^b" hovered))
    (should-not (string-match-p "\\^B"
                                (substring plain 0
                                           (string-match "\\^M1" plain))))
    (should (string-match-p "\\^B" plain))))

(cl-defstruct (slothbar-unit-focus-backend
               (:include slothbar-unit-backend))
  "A test backend that reports a focused window."
  focused)

(cl-defmethod slothbar-backend-focused-window
  ((backend slothbar-unit-focus-backend))
  "Return the fake focused window for BACKEND."
  (slothbar-unit-focus-backend-focused backend))

(ert-deftest slothbar-unit-focus-hook-refcount ()
  "Focus hook remains installed while any focus module is active."
  (let ((slothbar-focus--subscribers 0)
        (slothbar-focus--refresh-timer nil))
    (slothbar-focus--subscribe)
    (slothbar-focus--subscribe)
    (should (= slothbar-focus--subscribers 2))
    (should (memq #'slothbar-focus--on-change
                  slothbar-backend-focused-window-change-hook))
    (slothbar-focus--unsubscribe)
    (should (= slothbar-focus--subscribers 1))
    (should (memq #'slothbar-focus--on-change
                  slothbar-backend-focused-window-change-hook))
    (slothbar-focus--unsubscribe)
    (should (= slothbar-focus--subscribers 0))
    (should-not (memq #'slothbar-focus--on-change
                      slothbar-backend-focused-window-change-hook))))

(ert-deftest slothbar-unit-focus-render ()
  "Focus formatting uses title and app-id with empty fallbacks."
  (should (equal (slothbar-focus--render
                  "%a: %t" '(:title "Hello" :app-id "app.org"))
                 "app.org: Hello"))
  (should (equal (slothbar-focus--render "%t" nil) ""))
  (let ((slothbar-focus-truncate-to-width 5))
    (should (<= (length (slothbar-focus--truncate "abcdefgh")) 5)))
  (let ((m (slothbar-focus-create :format "%a: %t")))
    (setf (slothbar-module-text m) "")
    (should (equal (slothbar-focus-format-format m) ""))
    (setf (slothbar-module-text m) "Hello")
    (should (equal (slothbar-focus-format-format m) "%a: %t"))))

(ert-deftest slothbar-unit-focus-module-update ()
  "The focus module reads its backend and updates format-spec and text."
  (let* ((backend (make-slothbar-unit-focus-backend
                   :kind 'test
                   :focused '(:title "Hello" :app-id "app.org")))
         (m (slothbar-focus-create :format "%a: %t")))
    (let ((slothbar--backend backend))
      (slothbar-module-update-status m))
    (should (equal (slothbar-module-text m) "app.org: Hello"))
    (should (equal (alist-get ?t (slothbar-module-format-spec m)) "Hello"))
    (should (equal (alist-get ?a (slothbar-module-format-spec m)) "app.org"))
    (should (slothbar-module-needs-refresh? m))))

(provide 'slothbar-unit-test)
;;; slothbar-unit-test.el ends here
