;;; exlybar-layout.el --- Exlybar layout helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.0
;; Package-Requires: ((cl-lib "0.5") (emacs "27.1"))
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
;;
;; This module handles module layout for exlybar.
;;
;; Currently this produces horizontal layouts supporting combination of left,
;; right, and center alignments of modules. It supports x and y coordinate
;; offsets.
;;
;; The extents fns exlybar-layout-extents and exlybar-layout-subtract-extents
;; should allow for selective display update when the layout changes.

;;; Code:

(require 'cl-lib)

(require 'exlybar-module-types)

(defun exlybar-layout (module-ins)
  "Layout modules horizontally given modules and optional layout instructions.

Valid instructions are `:left' `:right' `:center'. Default is `:right' given no
instruction. `module-ins' is a flat list.
e.g. (mfoo :left mbar) -> ((mbar) nil (mfoo))

MODULE-INS the list of modules to layout with any instructions"
  (cl-macrolet ((slide (mn p ms)
                  (let ((m (cl-gensym 'm)))
                    `(progn (pop ,ms)
                            (cl-loop for ,m in ,mn while
                                     (not (keywordp ,m)) do
                                     (push (pop ,ms) ,p))))))
    (push :right module-ins)
    (let ((left) (right) (center-left) (center-right) (from-left?))
      (cl-loop for next = (cdr module-ins)
               while module-ins do
        (pcase (car module-ins)
          (:left (slide next left module-ins)
                 (setf from-left? t))
          (:right (slide next right module-ins)
                  (setf from-left? nil))
          (:center (if from-left?
                       (slide next center-left module-ins)
                     (slide next center-right module-ins)))))
      `(,(nreverse left) ,(append (nreverse center-left)
                                  (nreverse center-right))
        ,(nreverse right)))))

(defun exlybar-layout-coordinate (layout start-x start-y)
  "Given starting coordinates, attach coordinates to each module in the layout.
LAYOUT a layout as generated by `exlybar-layout'
START-X the starting x coord
START-Y the starting y coord"
  ;; TODO: check for overlap
  (cl-flet ((from-left (ms x)
              (let ((prev-m))
                (cl-loop for m in ms
                         for offset = x then (+ offset
                                                (exlybar-module-width prev-m))
                         collect `((,offset ,start-y) ,m)
                         do (setq prev-m m))))
            (total-width (ms)
              (seq-reduce (lambda (acc m)
                            (+ acc (exlybar-module-width m)))
                          ms 0)))
    `(,@(from-left (car layout) start-x)
      ,@(let ((mid-offset (- (/ exlybar-width 2)
                             (/ (total-width (cadr layout)) 2))))
          (from-left (cadr layout) (+ start-x mid-offset)))
      ,@(let ((right-offset (- (+ start-x exlybar-width)
                               (total-width (caddr layout)))))
          (from-left (caddr layout) right-offset)))))

(defun exlybar-layout-extents (layout)
  "Given a LAYOUT compute a list of contiguous segments.

E.g.: (exlybar-layout-extents '(((795 0) m1) ((825 0) m2) ((1400 0) m3)))
      > ((795 850) (1400 1500))
assuming m1 width is 30, m2 is 25, and m3 is 100.

LAYOUT as per per `exlybar-layout-coordinate'"
  (let ((extents))
    (dolist (m layout)
      (pcase-let ((`((,x ,_) ,m) m))
        (let* ((width (exlybar-module-width m))
               (extent (+ x width)))
          (if (and extents (= x (cadar extents)))
              (setf (cadar extents) extent)
            (push `(,x ,extent) extents)))))
    (nreverse extents)))

(defun exlybar-layout-subtract-extents (new old)
  "Given NEW and OLD extents, give the list of segments in old not in new.

E.g.: (let ((new '((0 180) (500 600)))
            (old '((0 168) (168 816))))
        (exlybar-layout-subtract-extents new old))
      > ((180 500) (600 816))

NEW as per `exlybar-layout-extents'
OLD as per `exlybar-layout-extents'"
  (cl-labels ((subtract-extents (new old res)
              (if (or (not new) (not old))
                  (progn (when old
                           (dolist (e old)
                             (push `(,(car e) ,(cadr e)) res)))
                         res)
                (let ((lold (caar old)) (lnew (caar new))
                      (rold (cadar old)) (rnew (cadar new)))
                  (when (> lnew lold)
                    (push `(,lold ,(min rold lnew)) res))
                  (if (and (<= lnew lold) (>= rnew rold))
                      (subtract-extents new (cdr old) res)
                    (if (< rnew lold)
                        (subtract-extents (cdr new) old res)
                      (if (< rnew rold)
                          (progn (setf (caar old) rnew)
                                 (subtract-extents (cdr new) old res))
                        (subtract-extents (cdr new) (cdr old) res))))))))
    (nreverse (subtract-extents new old nil))))

(provide 'exlybar-layout)

;;; exlybar-layout.el ends here
