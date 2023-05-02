;;; exlybar-wifi.el --- An exlybar wifi module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.2
;; Package-Requires: ((cl-lib "0.5") (xelb "0.18") (dash "2.1.0") (f "0.20.0") (s "1.12.0") (emacs "27.1"))
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

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 's)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-wifi nil
  "An Exlybar wifi module."
  :group 'exlybar)

(defcustom exlybar-wifi-qual-color-zones '(-40 -64 -70 t)
  "Wifi signal qualities indicating color changes.
See `exlybar-zone-color'"
  :type 'list
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
  (let ((default-directory (f-full "~")) ; ensure status checks don't remote
        )
    (when-let ((dev (exlybar-wifi-guess-device)))
      (->> (concat "iw " dev " info")
          (shell-command-to-string)
          (s-lines)
          (s-join ";")
          (s-match ".*?[[:blank:]]*ssid[[:blank:]]*\\(.*?\\);")
          (cadr)))))

(defun exlybar-wifi-iw-quality ()
  "For now scrape iw <dev> link output.
Doing this even though iw says 'Do NOT screenscrape this tool, we don't
consider its output stable.' :(
This should be deprecated in favor of something better."
  (let ((default-directory (f-full "~")) ; ensure status checks don't remote
        )
    (when-let* ((dev (exlybar-wifi-guess-device))
                (qual (->> (concat "iw " dev " link")
                           (shell-command-to-string)
                           (s-lines)
                           (s-join ";")
                           (s-match (concat ".*?[[:blank:]]*signal:[[:blank:]]*"
                                            "\\(-?[[:digit:]]+?\\) dBm;"))
                           (cadr))))
      qual)))

;;; let's just try a simple display of link quality and ssid
(cl-defstruct (exlybar-wifi
               (:include exlybar-module (name "wifi") (icon ?)
                         (format "^6^[^f1%i^]^[^2|^]%e^[^2|^]%p")
                         (format-fn 'exlybar-wifi-format-format))
               (:constructor exlybar-wifi-create)
               (:copier nil)))

(defun exlybar-wifi--format-fn-spec (zone-color)
  "Build the `format-spec' spec used by the format-fn."
  `((?p . ,(format "^[%s%%p^]" zone-color))))

(defun exlybar-wifi-format-format (m)
  "This is the default format-fn that is applied to M's format.
It applies zone colors to %p quality format specifier."
  (let* ((qual (or (map-elt (exlybar-module-cache m) 'qual)
                  (exlybar-wifi-iw-quality)))
         (zone-color
          (if qual (apply #'exlybar-zone-color (string-to-number qual)
                          exlybar-wifi-qual-color-zones)
            "")))
    (format-spec (exlybar-module-format m)
                 (exlybar-wifi--format-fn-spec zone-color) t)))

(defun exlybar-wifi--format-spec (icon qual)
  "Build the `format-spec' spec used to generate module text given ICON.
QUAL is the wifi signal quality as a string"
  `((?i . ,(string icon))
    (?e . ,(exlybar-wifi-iw-essid))
    (?p . ,qual)))

(defvar exlybar-wifi--update-timer nil "A variable to hold the update timer.")

(defun exlybar-wifi--do-update (m)
  "Poll the wifi status and check whether to update the M's text."
  (let* ((qual (exlybar-wifi-iw-quality))
         (status (exlybar-wifi--format-spec (exlybar-module-icon m) qual))
         (txt (format-spec (exlybar-module-format m) status t)))
    (when (exlybar-module-cache m)
      (map-put! (exlybar-module-cache m) 'qual qual))
    (unless (equal txt (exlybar-module-text m))
      (setf (exlybar-module-format-spec m) status
            (exlybar-module-text m) txt
            (exlybar-module-needs-refresh? m) t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-wifi))
  "Set the M's icon and update the text."
  (exlybar-wifi--do-update m))

(cl-defmethod exlybar-module-init :after ((m exlybar-wifi))
  "Run the update timer."
  (ignore m)
  (unless exlybar-wifi--update-timer
    (setq exlybar-wifi--update-timer
          (run-at-time nil 10 #'exlybar-wifi--do-update m))))

(cl-defmethod exlybar-module-exit :before ((m exlybar-wifi))
  "Cancel the update timer."
  (ignore m)
  (when exlybar-wifi--update-timer
    (cancel-timer exlybar-wifi--update-timer))
  (setq exlybar-wifi--update-timer nil))

(provide 'exlybar-wifi)
;;; exlybar-wifi.el ends here
