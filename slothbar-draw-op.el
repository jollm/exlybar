;;; slothbar-draw-op.el --- Backend-neutral draw operations  -*- lexical-binding: t -*-

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
;; A module's text layout is a sequence of glyph positions interleaved with
;; color commands.  This module turns that sequence into a list of
;; `slothbar-draw-op', each carrying the glyphs of one color run.  The result
;; is display-backend neutral; X-specific painting lives in
;; slothbar-render-xcb.el.

;;; Code:

(require 'cl-lib)
(require 'fontsloth-layout)

(require 'slothbar-color)

(cl-defstruct
    (slothbar-draw-op
     (:constructor slothbar-draw-op-create)
     (:copier nil))
  "A run of glyphs sharing one foreground color."
  (glyph-positions nil :type 'list)
  (color nil :type 'slothbar-color-rgb))

(defun slothbar-text-draw-ops (text-layout)
  "Return a list of `slothbar-draw-op' for TEXT-LAYOUT.

TEXT-LAYOUT is a module's text layout: a list of
`fontsloth-layout-glyph-position' objects interspersed with the color
commands :push, :pop, and :fg.  Empty runs are omitted."
  (let ((ops nil)
        (next nil)
        (fgidx 0)
        (bright nil)
        (fg-stack nil))
    (cl-labels ((current-color ()
                  (let ((color (slothbar-color-find fgidx t)))
                    (if bright
                        (slothbar-color-brighten color)
                      color)))
                (flush ()
                  (when next
                    (push (slothbar-draw-op-create
                           :glyph-positions (nreverse next)
                           :color (current-color))
                          ops)
                    (setq next nil))))
      (dolist (pos text-layout)
        (if (fontsloth-layout-glyph-position-p pos)
            (push pos next)
          (pcase (car pos)
            (:push (push (cons fgidx bright) fg-stack))
            (:pop (flush)
                  (let ((state (pop fg-stack)))
                    (setq fgidx (car state)
                          bright (cdr state))))
            (:fg (flush)
                 (setq fgidx (cadr pos)))
            (:bright (flush)
                     (setq bright (cadr pos))))))
      (flush))
    (nreverse ops)))

(provide 'slothbar-draw-op)
;;; slothbar-draw-op.el ends here
