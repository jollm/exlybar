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
(require 'slothbar-layout)
(require 'slothbar-module)
(require 'slothbar-draw-op)

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

(provide 'slothbar-unit-test)
;;; slothbar-unit-test.el ends here
