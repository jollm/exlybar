;;; slothbar-sysinfo.el --- System information fields and parsers  -*- lexical-binding: t; -*-

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

;; Field definitions, formatters, and pure parsers for the sysinfo module.
;; This file deliberately does not read /proc or /sys; providers do that in a
;; later stage so the parsing logic can be tested without a particular host.

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'slothbar-format)

(defgroup slothbar-sysinfo nil
  "System information module."
  :group 'slothbar)

(defcustom slothbar-sysinfo-missing "--"
  "String displayed when a sysinfo value is unavailable."
  :type 'string
  :group 'slothbar-sysinfo)

(cl-defstruct
    (slothbar-sysinfo-field
     (:constructor slothbar-sysinfo-field-create)
     (:copier nil))
  "A field exposed by the sysinfo module."
  keyword
  char
  category
  formatter
  (decimals 0)
  doc)

(defconst slothbar-sysinfo-fields
  (list
   ;; System
   (slothbar-sysinfo-field-create
    :keyword :hostname :char ?h :category 'system :formatter 'string
    :doc "Host name")
   (slothbar-sysinfo-field-create
    :keyword :username :char ?U :category 'system :formatter 'string
    :doc "Login name")
   (slothbar-sysinfo-field-create
    :keyword :os :char ?o :category 'system :formatter 'string
    :doc "Operating system name")
   (slothbar-sysinfo-field-create
    :keyword :os-version :char ?v :category 'system :formatter 'string
    :doc "Operating system version")
   (slothbar-sysinfo-field-create
    :keyword :kernel :char ?k :category 'system :formatter 'string
    :doc "Kernel release")
   (slothbar-sysinfo-field-create
    :keyword :architecture :char ?A :category 'system :formatter 'string
    :doc "Machine architecture")
   (slothbar-sysinfo-field-create
    :keyword :desktop :char ?w :category 'system :formatter 'string
    :doc "Desktop environment")
   (slothbar-sysinfo-field-create
    :keyword :wm :char ?W :category 'system :formatter 'string
    :doc "Window manager")
   ;; CPU
   (slothbar-sysinfo-field-create
    :keyword :cpu-usage :char ?c :category 'cpu :formatter 'percent
    :doc "Total CPU usage")
   (slothbar-sysinfo-field-create
    :keyword :cpu-frequency :char ?f :category 'cpu :formatter 'frequency
    :doc "CPU frequency")
   (slothbar-sysinfo-field-create
    :keyword :cpu-brand :char ?C :category 'cpu :formatter 'string
    :doc "CPU model name")
   (slothbar-sysinfo-field-create
    :keyword :cpu-count :char ?P :category 'cpu :formatter 'integer
    :doc "Logical CPU count")
   ;; Memory
   (slothbar-sysinfo-field-create
    :keyword :memory-usage :char ?m :category 'memory :formatter 'percent
    :doc "Memory usage")
   (slothbar-sysinfo-field-create
    :keyword :memory-used :char ?u :category 'memory :formatter 'bytes
    :decimals 1 :doc "Used memory")
   (slothbar-sysinfo-field-create
    :keyword :memory-total :char ?M :category 'memory :formatter 'bytes
    :decimals 1 :doc "Total memory")
   (slothbar-sysinfo-field-create
    :keyword :memory-free :char ?F :category 'memory :formatter 'bytes
    :decimals 1 :doc "Free memory")
   (slothbar-sysinfo-field-create
    :keyword :memory-available :char ?V :category 'memory :formatter 'bytes
    :decimals 1 :doc "Available memory")
   (slothbar-sysinfo-field-create
    :keyword :swap-usage :char ?s :category 'swap :formatter 'percent
    :doc "Swap usage")
   (slothbar-sysinfo-field-create
    :keyword :swap-used :char ?S :category 'swap :formatter 'bytes
    :decimals 1 :doc "Used swap")
   (slothbar-sysinfo-field-create
    :keyword :swap-total :char ?X :category 'swap :formatter 'bytes
    :decimals 1 :doc "Total swap")
   ;; Disk
   (slothbar-sysinfo-field-create
    :keyword :disk-usage :char ?d :category 'disk :formatter 'percent
    :doc "Disk usage")
   (slothbar-sysinfo-field-create
    :keyword :disk-used :char ?D :category 'disk :formatter 'bytes
    :decimals 1 :doc "Used disk space")
   (slothbar-sysinfo-field-create
    :keyword :disk-total :char ?E :category 'disk :formatter 'bytes
    :decimals 1 :doc "Total disk space")
   (slothbar-sysinfo-field-create
    :keyword :disk-free :char ?R :category 'disk :formatter 'bytes
    :decimals 1 :doc "Free disk space")
   ;; Network
   (slothbar-sysinfo-field-create
    :keyword :net-down :char ?n :category 'network :formatter 'rate
    :decimals 1 :doc "Download rate")
   (slothbar-sysinfo-field-create
    :keyword :net-up :char ?N :category 'network :formatter 'rate
    :decimals 1 :doc "Upload rate")
   (slothbar-sysinfo-field-create
    :keyword :net-ip :char ?i :category 'network :formatter 'string
    :doc "Network address")
   (slothbar-sysinfo-field-create
    :keyword :net-interface :char ?I :category 'network :formatter 'string
    :doc "Network interface")
   ;; Load, uptime, and processes
   (slothbar-sysinfo-field-create
    :keyword :load-1 :char ?l :category 'load :formatter 'load
    :decimals 2 :doc "One-minute load average")
   (slothbar-sysinfo-field-create
    :keyword :load-5 :char ?L :category 'load :formatter 'load
    :decimals 2 :doc "Five-minute load average")
   (slothbar-sysinfo-field-create
    :keyword :load-15 :char ?O :category 'load :formatter 'load
    :decimals 2 :doc "Fifteen-minute load average")
   (slothbar-sysinfo-field-create
    :keyword :uptime :char ?t :category 'uptime :formatter 'duration
    :doc "System uptime")
   (slothbar-sysinfo-field-create
    :keyword :process-count :char ?p :category 'process :formatter 'integer
    :doc "Process count")
   (slothbar-sysinfo-field-create
    :keyword :thread-count :char ?q :category 'process :formatter 'integer
    :doc "Thread count")
   ;; Thermal, power, and GPU
   (slothbar-sysinfo-field-create
    :keyword :temperature :char ?e :category 'thermal :formatter 'temperature
    :decimals 1 :doc "Temperature")
   (slothbar-sysinfo-field-create
    :keyword :fan-speed :char ?a :category 'thermal :formatter 'integer
    :doc "Fan speed")
   (slothbar-sysinfo-field-create
    :keyword :battery-percent :char ?b :category 'power :formatter 'percent
    :doc "Battery charge")
   (slothbar-sysinfo-field-create
    :keyword :battery-state :char ?B :category 'power :formatter 'string
    :doc "Battery state")
   (slothbar-sysinfo-field-create
    :keyword :gpu-usage :char ?g :category 'gpu :formatter 'percent
    :doc "GPU usage")
   (slothbar-sysinfo-field-create
    :keyword :gpu-memory :char ?G :category 'gpu :formatter 'bytes
    :decimals 1 :doc "GPU memory usage")
   (slothbar-sysinfo-field-create
    :keyword :gpu-temperature :char ?j :category 'gpu :formatter 'temperature
    :decimals 1 :doc "GPU temperature"))
  "All fields known to the sysinfo module.")

(defun slothbar-sysinfo-find-field (keyword)
  "Return the sysinfo field for KEYWORD, or nil."
  (seq-find (lambda (field)
              (eq keyword (slothbar-sysinfo-field-keyword field)))
            slothbar-sysinfo-fields))

(defun slothbar-sysinfo-find-field-by-char (char)
  "Return the sysinfo field using CHAR, or nil."
  (seq-find (lambda (field)
              (eq char (slothbar-sysinfo-field-char field)))
            slothbar-sysinfo-fields))

(defun slothbar-sysinfo-field-characters ()
  "Return the keyword-to-character alist used by the format compiler."
  (mapcar (lambda (field)
            (cons (slothbar-sysinfo-field-keyword field)
                  (slothbar-sysinfo-field-char field)))
          slothbar-sysinfo-fields))

(defun slothbar-sysinfo-compile-format (tokens)
  "Compile sysinfo format TOKENS with the sysinfo field registry."
  (slothbar-format-compile tokens (slothbar-sysinfo-field-characters)))

;;; Formatters

(defun slothbar-sysinfo-format-string (value &optional _decimals)
  "Format VALUE as a string."
  (format "%s" value))

(defun slothbar-sysinfo-format-integer (value &optional _decimals)
  "Format VALUE as an integer."
  (format "%d" value))

(defun slothbar-sysinfo-format-percent (value &optional decimals)
  "Format VALUE as a percentage number without a percent sign."
  (let ((decimals (or decimals 0)))
    (format (concat "%." (number-to-string decimals) "f") value)))

(defun slothbar-sysinfo-format-byte-count (value &optional decimals)
  "Format VALUE bytes with an IEC unit suffix.
DECIMALS defaults to 1."
  (let* ((decimals (or decimals 1))
         (units ["B" "KiB" "MiB" "GiB" "TiB" "PiB"])
         (index 0)
         (amount (float value)))
    (while (and (>= amount 1024) (< index (1- (length units))))
      (setq amount (/ amount 1024.0)
            index (1+ index)))
    (if (zerop index)
        (format "%.0f%s" amount (aref units index))
      (format (concat "%." (number-to-string decimals) "f%s")
              amount (aref units index)))))

(defun slothbar-sysinfo-format-rate (value &optional decimals)
  "Format VALUE bytes per second."
  (concat (slothbar-sysinfo-format-byte-count value decimals) "/s"))

(defun slothbar-sysinfo-format-duration (value &optional _decimals)
  "Format VALUE seconds as a compact duration."
  (let* ((seconds (round value))
         (days (/ seconds 86400))
         (hours (/ (% seconds 86400) 3600))
         (minutes (/ (% seconds 3600) 60)))
    (cond
     ((> days 0) (format "%dd %dh" days hours))
     ((> hours 0) (format "%dh %dm" hours minutes))
     ((> minutes 0) (format "%dm" minutes))
     (t (format "%ds" seconds)))))

(defun slothbar-sysinfo-format-load (value &optional decimals)
  "Format VALUE as a load average."
  (let ((decimals (or decimals 2)))
    (format (concat "%." (number-to-string decimals) "f") value)))

(defun slothbar-sysinfo-format-frequency (value &optional _decimals)
  "Format VALUE hertz as Hz, MHz, or GHz."
  (cond
   ((>= value 1000000000) (format "%.1fGHz" (/ value 1000000000.0)))
   ((>= value 1000000) (format "%.0fMHz" (/ value 1000000.0)))
   (t (format "%.0fHz" value))))

(defun slothbar-sysinfo-format-temperature (value &optional decimals)
  "Format VALUE degrees Celsius."
  (let ((decimals (or decimals 1)))
    (format (concat "%." (number-to-string decimals) "f°C") value)))

(defconst slothbar-sysinfo--formatters
  '((string . slothbar-sysinfo-format-string)
    (integer . slothbar-sysinfo-format-integer)
    (percent . slothbar-sysinfo-format-percent)
    (bytes . slothbar-sysinfo-format-byte-count)
    (rate . slothbar-sysinfo-format-rate)
    (duration . slothbar-sysinfo-format-duration)
    (load . slothbar-sysinfo-format-load)
    (frequency . slothbar-sysinfo-format-frequency)
    (temperature . slothbar-sysinfo-format-temperature))
  "Formatter symbols and their implementations.")

(defun slothbar-sysinfo-format-value (field value)
  "Format VALUE for FIELD, or return the missing-value string."
  (if (null value)
      slothbar-sysinfo-missing
    (let ((formatter (alist-get (slothbar-sysinfo-field-formatter field)
                                slothbar-sysinfo--formatters)))
      (if formatter
          (funcall formatter value (slothbar-sysinfo-field-decimals field))
        (format "%s" value)))))

;;; Pure Linux parsers

(defconst slothbar-sysinfo--meminfo-keys
  '(("MemTotal" . :memory-total)
    ("MemFree" . :memory-free)
    ("MemAvailable" . :memory-available)
    ("Buffers" . :memory-buffers)
    ("Cached" . :memory-cached)
    ("SwapTotal" . :swap-total)
    ("SwapFree" . :swap-free))
  "Meminfo names and their sysinfo keywords.")

(defun slothbar-sysinfo-parse-meminfo (text)
  "Parse /proc/meminfo TEXT into an alist of byte counts."
  (let (values)
    (dolist (line (split-string text "\n" t))
      (when (string-match
             "^\\([A-Za-z0-9_]+\\):\\s-+\\([0-9]+\\)\\(?:\\s-+\\([A-Za-z]+\\)\\)?"
             line)
        (let* ((name (match-string 1 line))
               (amount (string-to-number (match-string 2 line)))
               (unit (match-string 3 line))
               (multiplier (if (and unit
                                    (string-prefix-p "k" (downcase unit)))
                               1024
                             1))
               (key (cdr (assoc name slothbar-sysinfo--meminfo-keys))))
          (when key
            (push (cons key (* amount multiplier)) values)))))
    (nreverse values)))

(defun slothbar-sysinfo-parse-cpu-stat (text)
  "Parse the aggregate CPU line of /proc/stat TEXT.
Return an alist with :total and :idle counters."
  (when (string-match "^cpu\\s-+\\(.+\\)$" text)
    (let* ((fields (mapcar #'string-to-number
                           (split-string (match-string 1 text) "\\s-+" t)))
           (count (min 8 (length fields)))
           (total (cl-reduce #'+ (cl-subseq fields 0 count)
                             :initial-value 0))
           (idle (+ (or (nth 3 fields) 0)
                    (or (nth 4 fields) 0))))
      (list (cons :total total)
            (cons :idle idle)))))

(defun slothbar-sysinfo-cpu-usage (previous current)
  "Return CPU usage between PREVIOUS and CURRENT cpu-stat alists."
  (let ((total-delta (- (alist-get :total current 0)
                        (alist-get :total previous 0)))
        (idle-delta (- (alist-get :idle current 0)
                       (alist-get :idle previous 0))))
    (when (> total-delta 0)
      (* 100.0 (/ (float (- total-delta idle-delta)) total-delta)))))

(defun slothbar-sysinfo-parse-loadavg (text)
  "Parse /proc/loadavg TEXT into an alist."
  (when (string-match
         "^\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9]+\\)/\\([0-9]+\\)"
         text)
    (list (cons :load-1 (string-to-number (match-string 1 text)))
          (cons :load-5 (string-to-number (match-string 2 text)))
          (cons :load-15 (string-to-number (match-string 3 text)))
          (cons :running (string-to-number (match-string 4 text)))
          (cons :total (string-to-number (match-string 5 text))))))

(defun slothbar-sysinfo-parse-uptime (text)
  "Parse /proc/uptime TEXT and return seconds."
  (let ((seconds (car (split-string text "\\s-+" t))))
    (when seconds
      (string-to-number seconds))))

(defun slothbar-sysinfo-parse-net-dev (text)
  "Parse /proc/net/dev TEXT into an alist of interface statistics."
  (let (interfaces)
    (dolist (line (split-string text "\n" t))
      (when (string-match "^\\s-*\\([^:]+\\):\\s-+\\(.+\\)$" line)
        (let* ((interface (string-trim (match-string 1 line)))
               (fields (mapcar #'string-to-number
                               (split-string (match-string 2 line)
                                             "\\s-+" t))))
          (when (>= (length fields) 16)
            (push (cons interface
                        (list (cons :rx-bytes (nth 0 fields))
                              (cons :rx-packets (nth 1 fields))
                              (cons :tx-bytes (nth 8 fields))
                              (cons :tx-packets (nth 9 fields))))
                  interfaces)))))
    (nreverse interfaces)))

(defun slothbar-sysinfo-parse-os-release (text)
  "Parse /etc/os-release TEXT into an alist of strings."
  (let (values)
    (dolist (line (split-string text "\n" t))
      (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
        (let ((key (match-string 1 line))
              (value (string-trim (match-string 2 line))))
          (when (and (> (length value) 1)
                     (or (and (eq (aref value 0) ?\")
                              (eq (aref value (1- (length value))) ?\"))
                         (and (eq (aref value 0) ?')
                              (eq (aref value (1- (length value))) ?'))))
            (setq value (substring value 1 -1)))
          (push (cons key value) values))))
    (nreverse values)))

(defun slothbar-sysinfo-parse-cpuinfo (text)
  "Parse /proc/cpuinfo TEXT for the model name and frequency.
Return an alist with :cpu-brand and :cpu-frequency in hertz."
  (let (brand mhz)
    (dolist (line (split-string text "\n" t))
      (cond
       ((and (not brand)
             (string-match "^model name\\s-*:\\s-*\\(.+\\)$" line))
        (setq brand (string-trim (match-string 1 line))))
       ((and (not mhz)
             (string-match "^cpu MHz\\s-*:\\s-*\\([0-9.]+\\)$" line))
        (setq mhz (string-to-number (match-string 1 line))))))
    (list (cons :cpu-brand brand)
          (cons :cpu-frequency (and mhz (* mhz 1000000.0))))))

(defun slothbar-sysinfo-format-network-address (value)
  "Format a network address VALUE."
  (format "%s" value))

(provide 'slothbar-sysinfo)
;;; slothbar-sysinfo.el ends here
