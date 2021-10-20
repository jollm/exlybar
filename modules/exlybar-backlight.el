;;; exlybar-backlight.el --- An exlybar backlight module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((backlight "20210513.129") (emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for backlight status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'backlight)
(require 'cl-lib)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-backlight nil
  "An Exlybar backlight module."
  :group 'exlybar)

(defcustom exlybar-backlight-progress-increment 10
  "The percent step increment for the backlight module progress bar."
  :group 'exlybar-backlight)

(defcustom exlybar-backlight-text-font
  "/usr/share/fonts/TTF/IBMPlexMono-Regular.ttf"
  "The text font for the backlight module progress bar."
  :group 'exlybar-backlight)

(cl-defstruct (exlybar-backlight
               (:include exlybar-module (name "backlight") (icon ?)
                         (fonts `(,exlybar-icon-font
                                  ,exlybar-backlight-text-font)))
               (:constructor exlybar-backlight-create)
               (:copier nil)))

(defvar exlybar-backlight--update-timer nil
  "A variable to hold the update timer.")

(defun exlybar-backlight-current-progress ()
  (exlybar-progress-bar (backlight--current-percentage)
                        exlybar-backlight-progress-increment))

(defun exlybar-backlight--do-update (m)
  "Poll the battery status and check whether to update M's text."
  (let* ((txt+fonts `((,(string (exlybar-backlight-icon m))
                       . ,exlybar-icon-font)
                      (,(concat " " (exlybar-backlight-current-progress))
                       . ,exlybar-backlight-text-font))))
    (unless (equal txt+fonts (exlybar-backlight-text m))
      (setf (exlybar-module-text m)
            txt+fonts
            (exlybar-module-needs-refresh? m)
            t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-backlight))
  "Set the M's icon and update the text."
  (exlybar-backlight--do-update m))

(cl-defmethod exlybar-module-init :after ((m exlybar-backlight))
  "Run the update timer."
  (unless exlybar-backlight--update-timer
    (setq exlybar-backlight--update-timer
          (run-at-time nil 10 #'exlybar-backlight--do-update m))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-backlight))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-backlight--update-timer
    (cancel-timer exlybar-backlight--update-timer))
  (setq exlybar-backlight--update-timer nil))

(defadvice backlight--set-brightness
    (after exlybar-backlight-after-set-brightness activate)
  "Refresh the module if the brightness is adjusted in Emacs."
  (when (exlybar-enabled-p)
    (let ((m (seq-find (lambda (m)
                         (equal "backlight"
                                (when (exlybar-module-p m)
                                  (exlybar-module-name m))))
                       exlybar-modules)))
      (when m
        (exlybar-backlight--do-update m)
        (exlybar-refresh-modules)))))

(provide 'exlybar-backlight)
;;; exlybar-backlight.el ends here
