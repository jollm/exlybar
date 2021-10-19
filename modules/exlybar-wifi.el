;;; exlybar-wifi.el --- An exlybar wifi module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((xelb "0.18") (dash "2.1.0") (f "0.20.0") (emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for wifi status.
;; information

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(require 'exlybar)
(require 'exlybar-module)
(require 'exlybar-render)

(defgroup exlybar-wifi nil
  "An Exlybar wifi module."
  :group 'exlybar)

(defcustom exlybar-wifi-text-separator "|"
  "The text separator."
  :type 'string
  :group 'exlybar-wifi)

(defun exlybar-wifi-guess-device ()
  "Try to guess the wireless device."
  (seq-some (lambda (p)
              (when (f-exists? (f-join p "wireless"))
                (f-filename p)))
            (f-entries "/sys/class/net/")))

(defun exlybar-wifi-iw-essid ()
  "For now scrape iw <dev> info output.
Doing this even though iw says 'Do NOT screenscrape this tool, we don't
consider its output stable.' :(
This should be deprecated in favor of something better."
  (when-let ((dev (exlybar-wifi-guess-device)))
    (->> (concat "iw " dev " info")
        (shell-command-to-string)
        (s-lines)
        (s-join ";")
        (s-match ".*?[[:blank:]]*ssid[[:blank:]]*\\(.*?\\);")
        (cadr))))

(defun exlybar-wifi-iw-quality ()
  "For now scrape iw <dev> link output.
Doing this even though iw says 'Do NOT screenscrape this tool, we don't
consider its output stable.' :(
This should be deprecated in favor of something better."
  (when-let* ((dev (exlybar-wifi-guess-device))
              (qual (->> (concat "iw " dev " link")
                         (shell-command-to-string)
                         (s-lines)
                         (s-join ";")
                         (s-match (concat ".*?[[:blank:]]*signal:[[:blank:]]*"
                                          "\\(-?[[:digit:]]+?\\) dBm;"))
                         (cadr))))
    qual))

;;; let's just try a simple display of link quality and ssid
(cl-defstruct (exlybar-wifi
               (:include exlybar-module (name "wifi"))
               (:constructor exlybar-wifi-create)
               (:copier nil)))

(defsubst exlybar-wifi--icon-string ()
  "Return the icon as a string."
  (string (exlybar-wifi-icon exlybar-wifi--module)))

(defvar exlybar-wifi--update-timer nil "A variable to hold the update timer.")

(defun exlybar-wifi--do-update ()
  "Poll the wifi status and check whether to update the text."
  (let ((txt+fonts `((,(exlybar-wifi--icon-string) . ,exlybar-icon-font)
                     (,(concat exlybar-wifi-text-separator
                               (exlybar-wifi-iw-essid)
                               exlybar-wifi-text-separator
                               (exlybar-wifi-iw-quality))
                      . ,exlybar-text-font))))
    (unless (equal txt+fonts (exlybar-wifi-text exlybar-wifi--module))
      (setf (exlybar-module-text exlybar-wifi--module)
            txt+fonts
            (exlybar-module-needs-refresh? exlybar-wifi--module)
            t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-wifi))
  "Set the M's icon and update the text."
  (setf (exlybar-wifi-icon m) ?)
  (exlybar-wifi--do-update))

(cl-defmethod exlybar-module-init :after ((m exlybar-wifi))
  "Run the update timer."
  (ignore m)
  (unless exlybar-wifi--update-timer
    (setq exlybar-wifi--update-timer
          (run-at-time nil 10 #'exlybar-wifi--do-update))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-wifi))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-wifi--update-timer
    (cancel-timer exlybar-wifi--update-timer))
  (setq exlybar-wifi--update-timer nil))

(provide 'exlybar-wifi)
;;; exlybar-wifi.el ends here
