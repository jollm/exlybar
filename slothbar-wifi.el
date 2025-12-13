;;; slothbar-wifi.el --- An slothbar wifi module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

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

;; This is an implementation of `slothbar-module' for wifi status.
;; information

;; To use this module, add it to `slothbar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'map)
(require 's)
(require 'seq)

(require 'slothbar-color)
(require 'slothbar-module-)

(defgroup slothbar-wifi nil
  "An Slothbar wifi module."
  :group 'slothbar)

(defcustom slothbar-wifi-qual-color-zones '(-40 -64 -70 t nil)
  "Wifi signal qualities indicating color changes.
See `slothbar-color-zone'"
  :type '(list (integer :tag "Med") (integer :tag "Hi") (integer :tag "Crit")
               (boolean :tag "Reverse?") (boolean :tag "Local?"))
  :group 'slothbar-wifi)

(defcustom slothbar-wifi-preferred-interface nil
  "Preferred wifi interface name as listed by command `iw dev'.

Default nil means to choose automatically"
  :type '(choice (const :tag "Auto" nil)
		 (string))
  :group 'slothbar-wifi)

(defun slothbar-wifi-guess-device ()
  "Try to guess the wireless device."
  (or slothbar-wifi-preferred-interface
      (seq-some (lambda (p)
		  (when (f-exists? (f-join p "wireless"))
                    (f-filename p)))
		(f-entries "/sys/class/net/"))))

(defun slothbar-wifi-iw-essid ()
  "For now scrape iw <dev> info output.
Doing this even though iw says \\='Do NOT screenscrape this tool, we don't
consider its output stable.\\=' :(
This should be deprecated in favor of something better."
  (let ((default-directory (f-full "~")) ; ensure status checks don't remote
        )
    (when-let ((dev (slothbar-wifi-guess-device)))
      (->> (concat "iw " dev " info")
          (shell-command-to-string)
          (s-lines)
          (s-join ";")
          (s-match ".*?[[:blank:]]*ssid[[:blank:]]*\\(.*?\\);")
          (cadr)))))

(defun slothbar-wifi-iw-quality ()
  "For now scrape iw <dev> link output.
Doing this even though iw says \\='Do NOT screenscrape this tool, we don't
consider its output stable.\\=' :(
This should be deprecated in favor of something better."
  (let ((default-directory (f-full "~")) ; ensure status checks don't remote
        )
    (when-let* ((dev (slothbar-wifi-guess-device))
                (qual (->> (concat "iw " dev " link")
                           (shell-command-to-string)
                           (s-lines)
                           (s-join ";")
                           (s-match (concat ".*?[[:blank:]]*signal:[[:blank:]]*"
                                            "\\(-?[[:digit:]]+?\\) dBm;"))
                           (cadr))))
      qual)))

;;; let's just try a simple display of link quality and ssid
(cl-defstruct (slothbar-wifi
               (:include slothbar-module (name "wifi") (icon ?)
                         (format "^6^[^f1%i^]^[^2|^]%e^[^2|^]%p")
                         (format-fn #'slothbar-wifi-format-format))
               (:constructor slothbar-wifi-create)
               (:copier nil)))

(defun slothbar-wifi--format-fn-spec (zone-color)
  "Build the `format-spec' spec used by the format-fn.

ZONE-COLOR the color code as determined by `slothbar-color-zone'"
  `((?p . ,(format "^[%s%%p^]" zone-color))))

(defun slothbar-wifi-format-format (m)
  "This is the default format-fn that is applied to M's format.
It applies zone colors to %p quality format specifier."
  (let* ((qual (or (map-elt (slothbar-module-cache m) 'qual)
                  (slothbar-wifi-iw-quality)))
         (zone-color
          (if qual (apply #'slothbar-color-zone (string-to-number qual)
                          slothbar-wifi-qual-color-zones)
            "")))
    (format-spec (slothbar-module-format m)
                 (slothbar-wifi--format-fn-spec zone-color) t)))

(defun slothbar-wifi--format-spec (icon qual)
  "Build the `format-spec' spec used to generate module text given ICON.
QUAL is the wifi signal quality as a string"
  `((?i . ,(string icon))
    (?e . ,(or (slothbar-wifi-iw-essid) "nil"))
    (?p . ,(or qual "nil"))))

(cl-defmethod slothbar-module-update-status ((m slothbar-wifi))
  "Poll the wifi status and check whether to update the M's text."
  (let* ((qual (slothbar-wifi-iw-quality))
         (status (slothbar-wifi--format-spec (slothbar-module-icon m) qual))
         (txt (format-spec (slothbar-module-format m) status t)))
    (when (slothbar-module-cache m)
      (map-put! (slothbar-module-cache m) 'qual qual))
    (unless (equal txt (slothbar-module-text m))
      (setf (slothbar-module-format-spec m) status
            (slothbar-module-text m) txt
            (slothbar-module-needs-refresh? m) t))))

(cl-defmethod slothbar-module-init :before ((m slothbar-wifi))
  "Set the M's icon and update the text."
  (slothbar-module-update-status m))

(provide 'slothbar-wifi)
;;; slothbar-wifi.el ends here
