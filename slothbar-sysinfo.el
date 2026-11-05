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

(require 'slothbar)
(require 'slothbar-format)
(require 'slothbar-module-)

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
  "Parse /proc/cpuinfo TEXT for brand, frequency, and logical CPU count.
Return an alist with :cpu-brand, :cpu-frequency in hertz, and :cpu-count."
  (let (brand mhz (count 0))
    (dolist (line (split-string text "\n" t))
      (cond
       ((string-match "^processor\\s-*:" line)
        (cl-incf count))
       ((and (not brand)
             (string-match "^model name\\s-*:\\s-*\\(.+\\)$" line))
        (setq brand (string-trim (match-string 1 line))))
       ((and (not mhz)
             (string-match "^cpu MHz\\s-*:\\s-*\\([0-9.]+\\)$" line))
        (setq mhz (string-to-number (match-string 1 line))))))
    (list (cons :cpu-brand brand)
          (cons :cpu-frequency (and mhz (* mhz 1000000.0)))
          (cons :cpu-count (and (> count 0) count)))))

(defun slothbar-sysinfo-format-network-address (value)
  "Format a network address VALUE."
  (format "%s" value))

;;; Live providers and update state

(defcustom slothbar-sysinfo-intervals
  '((system . nil)
    (cpu . 1)
    (memory . 2)
    (disk . 5)
    (network . 1)
    (load . 2)
    (uptime . 5)
    (process . 5)
    (thermal . 5)
    (power . 5)
    (gpu . 5))
  "Seconds between updates for each sysinfo category.
A nil interval means read once per state."
  :type '(alist :key-type symbol
                :value-type (choice (const :tag "Once" nil) natnum))
  :group 'slothbar-sysinfo)

(defcustom slothbar-sysinfo-disk-path "/"
  "Path whose filesystem is reported by the disk fields."
  :type 'string
  :group 'slothbar-sysinfo)

(defcustom slothbar-sysinfo-network-interface nil
  "Network interface to report, or nil to select one automatically."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'slothbar-sysinfo)

(defcustom slothbar-sysinfo-count-threads nil
  "Non-nil to count threads in addition to processes."
  :type 'boolean
  :group 'slothbar-sysinfo)

(cl-defstruct
    (slothbar-sysinfo-state
     (:constructor slothbar-sysinfo-state-create)
     (:copier nil))
  "Cache and delta state for live sysinfo providers."
  (values nil)
  (timestamps (make-hash-table :test 'eq))
  (cpu-previous nil)
  (net-previous (make-hash-table :test 'equal)))

(defun slothbar-sysinfo--read-file (path)
  "Return the contents of PATH, or nil if it cannot be read."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))
    (error nil)))

(defun slothbar-sysinfo--command-output (&rest args)
  "Run ARGS with `call-process' and return trimmed output, or nil."
  (condition-case nil
      (with-temp-buffer
        (apply #'call-process (car args) nil t nil (cdr args))
        (string-trim (buffer-string)))
    (error nil)))

(defun slothbar-sysinfo--percent (used total)
  "Return USED as a percentage of TOTAL, or nil."
  (when (and used total (> total 0))
    (* 100.0 (/ (float used) total))))

(defun slothbar-sysinfo--rate (current previous elapsed)
  "Return per-second rate from PREVIOUS to CURRENT over ELAPSED."
  (if (and current previous elapsed (> elapsed 0))
      (max 0.0 (/ (float (- current previous)) elapsed))
    0.0))

(defun slothbar-sysinfo--read-system (_state _now)
  "Read static system identity fields."
  (let* ((os (slothbar-sysinfo-parse-os-release
              (or (slothbar-sysinfo--read-file "/etc/os-release") "")))
         (pretty (cdr (assoc "PRETTY_NAME" os)))
         (name (cdr (assoc "NAME" os))))
    `((:hostname . ,(system-name))
      (:username . ,(user-login-name))
      (:os . ,(or pretty name))
      (:os-version . ,(or (cdr (assoc "VERSION_ID" os))
                          (cdr (assoc "VERSION" os))))
      (:kernel . ,(or (slothbar-sysinfo--command-output "uname" "-r")
                      system-configuration))
      (:architecture . ,(or (slothbar-sysinfo--command-output "uname" "-m")
                            system-configuration))
      (:desktop . ,(or (getenv "XDG_CURRENT_DESKTOP")
                       (getenv "DESKTOP_SESSION")))
      (:wm . ,(or (getenv "DESKTOP_SESSION")
                  (getenv "XDG_CURRENT_DESKTOP"))))))

(defun slothbar-sysinfo--read-cpu (state _now)
  "Read CPU usage, model, frequency, and count."
  (let* ((current (slothbar-sysinfo-parse-cpu-stat
                   (or (slothbar-sysinfo--read-file "/proc/stat") "")))
         (previous (slothbar-sysinfo-state-cpu-previous state))
         (info (slothbar-sysinfo-parse-cpuinfo
                (or (slothbar-sysinfo--read-file "/proc/cpuinfo") "")))
         (usage (and previous current
                     (slothbar-sysinfo-cpu-usage previous current))))
    (setf (slothbar-sysinfo-state-cpu-previous state) current)
    `((:cpu-usage . ,usage)
      (:cpu-brand . ,(alist-get :cpu-brand info))
      (:cpu-frequency . ,(alist-get :cpu-frequency info))
      (:cpu-count . ,(alist-get :cpu-count info)))))

(defun slothbar-sysinfo--read-memory (_state _now)
  "Read memory and swap fields."
  (let* ((mem (slothbar-sysinfo-parse-meminfo
               (or (slothbar-sysinfo--read-file "/proc/meminfo") "")))
         (total (alist-get :memory-total mem))
         (free (alist-get :memory-free mem))
         (available (or (alist-get :memory-available mem) free))
         (used (and total available (max 0 (- total available))))
         (swap-total (alist-get :swap-total mem))
         (swap-free (alist-get :swap-free mem))
         (swap-used (and swap-total swap-free
                         (max 0 (- swap-total swap-free)))))
    `((:memory-total . ,total)
      (:memory-free . ,free)
      (:memory-available . ,available)
      (:memory-used . ,used)
      (:memory-usage . ,(slothbar-sysinfo--percent used total))
      (:swap-total . ,swap-total)
      (:swap-used . ,swap-used)
      (:swap-usage . ,(slothbar-sysinfo--percent swap-used swap-total)))))

(defun slothbar-sysinfo--read-disk (_state _now)
  "Read filesystem usage for `slothbar-sysinfo-disk-path'."
  (let* ((info (ignore-errors (file-system-info slothbar-sysinfo-disk-path)))
         (total (nth 0 info))
         (free (nth 1 info))
         (used (and total free (max 0 (- total free)))))
    `((:disk-total . ,total)
      (:disk-free . ,free)
      (:disk-used . ,used)
      (:disk-usage . ,(slothbar-sysinfo--percent used total)))))

(defun slothbar-sysinfo--default-network-interface (interfaces)
  "Return a non-loopback interface from INTERFACES, or the first one."
  (or (car (seq-find (lambda (pair) (not (equal (car pair) "lo")))
                     interfaces))
      (caar interfaces)))

(defun slothbar-sysinfo--read-network (state now)
  "Read network rates and interface identity."
  (let* ((interfaces (slothbar-sysinfo-parse-net-dev
                      (or (slothbar-sysinfo--read-file "/proc/net/dev") "")))
         (interface (or slothbar-sysinfo-network-interface
                        (slothbar-sysinfo--default-network-interface interfaces)))
         (stats (cdr (assoc interface interfaces))))
    (when (and interface stats)
      (let* ((previous (gethash interface
                                (slothbar-sysinfo-state-net-previous state)))
             (elapsed (and previous (- now (car previous))))
             (old (cdr previous))
             (down (slothbar-sysinfo--rate (alist-get :rx-bytes stats)
                                          (alist-get :rx-bytes old)
                                          elapsed))
             (up (slothbar-sysinfo--rate (alist-get :tx-bytes stats)
                                        (alist-get :tx-bytes old)
                                        elapsed)))
        (puthash interface (cons now stats)
                 (slothbar-sysinfo-state-net-previous state))
        `((:net-interface . ,interface)
          (:net-ip . ,(ignore-errors
                        (nth 0 (network-interface-info interface))))
          (:net-down . ,down)
          (:net-up . ,up))))))

(defun slothbar-sysinfo--read-load (_state _now)
  "Read load averages."
  (let ((load (slothbar-sysinfo-parse-loadavg
               (or (slothbar-sysinfo--read-file "/proc/loadavg") ""))))
    `((:load-1 . ,(alist-get :load-1 load))
      (:load-5 . ,(alist-get :load-5 load))
      (:load-15 . ,(alist-get :load-15 load)))))

(defun slothbar-sysinfo--read-uptime (_state _now)
  "Read system uptime."
  `((:uptime . ,(slothbar-sysinfo-parse-uptime
                 (or (slothbar-sysinfo--read-file "/proc/uptime") "")))))

(defun slothbar-sysinfo--read-process (_state _now)
  "Read process and optionally thread counts."
  (let* ((pids (ignore-errors
                 (directory-files "/proc" nil "\\`[0-9]+\\'" t)))
         (threads (when slothbar-sysinfo-count-threads
                    (cl-loop for pid in pids
                             sum (length
                                  (ignore-errors
                                    (directory-files
                                     (expand-file-name pid "/proc")
                                     nil "\\`[0-9]+\\'" t)))))))
    `((:process-count . ,(and pids (length pids)))
      (:thread-count . ,threads))))

(defun slothbar-sysinfo--read-temperature (zone)
  "Read temperature from thermal ZONE, in degrees Celsius."
  (let ((text (slothbar-sysinfo--read-file (expand-file-name "temp" zone))))
    (when text
      (let ((value (string-to-number (string-trim text))))
        (when (> value 0)
          (/ value 1000.0))))))

(defun slothbar-sysinfo--read-thermal (_state _now)
  "Read the first available temperature and fan speed."
  (let* ((zones (ignore-errors
                  (directory-files "/sys/class/thermal" t
                                   "\\`thermal_zone[0-9]+\\'" t)))
         (temperature (car (delq nil (mapcar #'slothbar-sysinfo--read-temperature
                                              zones))))
         (fans (ignore-errors
                 (file-expand-wildcards "/sys/class/hwmon/hwmon*/fan1_input")))
         (fan (car (delq nil
                         (mapcar (lambda (path)
                                   (let ((text (slothbar-sysinfo--read-file path)))
                                     (when text
                                       (let ((value (string-to-number
                                                     (string-trim text))))
                                         (and (> value 0) value)))))
                                 fans)))))
    `((:temperature . ,temperature)
      (:fan-speed . ,fan))))

(defun slothbar-sysinfo--read-power (_state _now)
  "Read battery percentage and state."
  (let* ((batteries (ignore-errors
                      (directory-files "/sys/class/power_supply" t
                                       "\\`BAT" t)))
         (battery (car batteries))
         (capacity (and battery
                        (slothbar-sysinfo--read-file
                         (expand-file-name "capacity" battery))))
         (status (and battery
                      (slothbar-sysinfo--read-file
                       (expand-file-name "status" battery)))))
    `((:battery-percent . ,(and capacity
                                (string-to-number (string-trim capacity))))
      (:battery-state . ,(and status (string-trim status))))))

(defvar slothbar-sysinfo--providers
  '((system . slothbar-sysinfo--read-system)
    (cpu . slothbar-sysinfo--read-cpu)
    (memory . slothbar-sysinfo--read-memory)
    (disk . slothbar-sysinfo--read-disk)
    (network . slothbar-sysinfo--read-network)
    (load . slothbar-sysinfo--read-load)
    (uptime . slothbar-sysinfo--read-uptime)
    (process . slothbar-sysinfo--read-process)
    (thermal . slothbar-sysinfo--read-thermal)
    (power . slothbar-sysinfo--read-power))
  "Provider functions keyed by sysinfo category.")

(defun slothbar-sysinfo--due-p (state category now)
  "Return non-nil when CATEGORY is due at NOW."
  (let ((last (gethash category (slothbar-sysinfo-state-timestamps state)))
        (interval (alist-get category slothbar-sysinfo-intervals)))
    (cond
     ((null last) t)
     ((null interval) nil)
     (t (>= (- now last) interval)))))

(defun slothbar-sysinfo-update (state &optional now force)
  "Update STATE's cached values.
NOW defaults to the current time.  FORCE updates every provider regardless
of its interval."
  (let ((now (or now (float-time))))
    (dolist (entry slothbar-sysinfo--providers)
      (let ((category (car entry))
            (provider (cdr entry)))
        (when (or force (slothbar-sysinfo--due-p state category now))
          (let ((values
                 (condition-case err
                     (funcall provider state now)
                   (error
                    (message "Slothbar sysinfo %s provider failed: %S"
                             category err)
                    nil))))
            (when values
              (dolist (pair values)
                (setf (alist-get (car pair)
                                 (slothbar-sysinfo-state-values state))
                      (cdr pair)))
              (puthash category now
                       (slothbar-sysinfo-state-timestamps state)))))))
    state))

(defun slothbar-sysinfo-collect (&optional state now force)
  "Update and return sysinfo values.
STATE is reused across calls to retain deltas; NOW and FORCE are for tests."
  (let ((state (or state (slothbar-sysinfo-state-create))))
    (slothbar-sysinfo-update state now force)
    (copy-sequence (slothbar-sysinfo-state-values state))))

;;; Module

(defcustom slothbar-sysinfo-format
  (let ((percent "^[^2%^]"))
    `("^f2^6"
      (:cpu-usage :width 3 :pad zero :suffix ,percent)
      "^[^f1 ^2^f4^]"
      " "
      (:memory-usage :width 3 :suffix ,percent)
      "^[^f1 ^2^f4^]"))
  "Token list used to format the sysinfo module.
See `slothbar-format-compile' for the token and option syntax."
  :type 'sexp
  :group 'slothbar-sysinfo)

(defcustom slothbar-sysinfo-refresh-interval 1
  "Seconds between sysinfo status updates."
  :type 'natnum
  :group 'slothbar-sysinfo)

(cl-defstruct
    (slothbar-sysinfo
     (:include slothbar-module
               (name "sysinfo")
               (format "")
               (lpad 4)
               (rpad 8))
     (:constructor slothbar-sysinfo-create)
     (:copier nil))
  "A system information module."
  state)

(defun slothbar-sysinfo--format-values (values)
  "Build the `format-spec' alist from sysinfo VALUES."
  (cl-loop for field in slothbar-sysinfo-fields
           collect
           (cons (slothbar-sysinfo-field-char field)
                 (slothbar-sysinfo-format-value
                  field
                  (alist-get (slothbar-sysinfo-field-keyword field) values)))))

(defun slothbar-sysinfo--render (format values)
  "Render FORMAT using sysinfo VALUES."
  (format-spec format (slothbar-sysinfo--format-values values) 'ignore))

(cl-defmethod slothbar-module-update-status ((m slothbar-sysinfo))
  "Collect sysinfo values for M and update its text."
  (let* ((state (or (slothbar-sysinfo-state m)
                    (setf (slothbar-sysinfo-state m)
                          (slothbar-sysinfo-state-create))))
         (values (slothbar-sysinfo-collect state))
         (format (slothbar-sysinfo-compile-format slothbar-sysinfo-format))
         (spec (slothbar-sysinfo--format-values values))
         (text (format-spec format spec 'ignore))
         (changed (not (equal text (slothbar-module-text m)))))
    (setf (slothbar-module-format m) format
          (slothbar-module-format-spec m) spec
          (slothbar-module-text m) text)
    (when changed
      (setf (slothbar-module-needs-refresh? m) t)
      (when (and slothbar--backend (slothbar-enabled-p))
        (slothbar-refresh-modules)))
    text))

(cl-defmethod slothbar-module-init :before ((m slothbar-sysinfo))
  "Prepare M's state and initial text before layout."
  (setf (slothbar-sysinfo-state m) (slothbar-sysinfo-state-create))
  (slothbar-module-update-status m))

(cl-defmethod slothbar-module-init ((m slothbar-sysinfo))
  "Initialize M and start its high-frequency update timer."
  (cl-call-next-method)
  (when (slothbar-module-update-timer m)
    (cancel-timer (slothbar-module-update-timer m)))
  (setf (slothbar-module-update-timer m)
        (run-at-time nil slothbar-sysinfo-refresh-interval
                     #'slothbar-module-update-status m)))

(cl-defmethod slothbar-module-exit :after ((m slothbar-sysinfo))
  "Drop M's provider state after exit."
  (setf (slothbar-sysinfo-state m) nil))

(defun slothbar-sysinfo--watch-format (_ _nval oper where)
  "Refresh sysinfo modules when `slothbar-sysinfo-format' changes."
  (when (and (not where) (eq 'set oper))
    (slothbar-module--refresh-all-by-name "sysinfo")))

(add-variable-watcher 'slothbar-sysinfo-format
                      'slothbar-sysinfo--watch-format)

(provide 'slothbar-sysinfo)
;;; slothbar-sysinfo.el ends here
