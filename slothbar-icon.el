;;; slothbar-icon.el --- Shared icon lookup and rasterization  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;;; Commentary:

;; Shared icon-name lookup for tray and menu rendering.  PNG and SVG
;; files are indexed once per icon directory per target height.  SVG
;; is rasterized through an optional external command and decoded
;; through Umbra.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'slothbar-log)

(declare-function umbra-png-read "umbra-png" (path))
(declare-function umbra-pixmap-width "umbra-pixmap" (pixmap))
(declare-function umbra-pixmap-height "umbra-pixmap" (pixmap))
(declare-function umbra-pixmap-demultiplied-data "umbra-pixmap" (pixmap))

(defcustom slothbar-icon-svg-rasterizer "rsvg-convert"
  "Optional executable used to rasterize SVG icons."
  :type 'string
  :group 'slothbar)

(defcustom slothbar-icon-preferred-themes '((dark "breeze-dark"
                                                  "Papirus-Dark"
                                                  "Yaru-dark"
                                                  "hicolor")
                                            (light "Adwaita"
                                                   "breeze"
                                                   "Papirus-Light"
                                                   "Yaru"
                                                   "hicolor"))
  "Preferred icon themes by background mode.

NOTE: These are the only themes considered when looking for icons."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'slothbar)

(defvar slothbar-icon--cache (make-hash-table :test 'equal)
  "Cache of icon name and directory sets to paths or `:none'.")

(defvar slothbar-icon--dirs-cache (make-hash-table :test 'equal)
  "Cache of theme paths to resolved icon directory lists.")

(defvar slothbar-icon--theme-index-cache (make-hash-table :test 'equal)
  "Cache of icon theme directories to an icon name -> icon dirs index.")

(defun slothbar-icon-clear-cache ()
  "Clear all shared icon caches."
  (clrhash slothbar-icon--cache)
  (clrhash slothbar-icon--dirs-cache)
  (clrhash slothbar-icon--theme-index-cache))

(defun slothbar-icon--watch-preferred-themes (_ nval oper where)
  "Clear caches when the preferred themes change.
NVAL is the new `slothbar-icon-preferred-themes' value.
Do this only when OPER eq \\='set and WHERE is nil."
  (when (and (not where)
             (eq 'set oper)
             (not (equal nval slothbar-icon-preferred-themes)))
    (when (fboundp 'slothbar-icon-clear-cache)
      (run-at-time 0 nil #'slothbar-icon-clear-cache))))

(add-variable-watcher 'slothbar-icon-preferred-themes
                      #'slothbar-icon--watch-preferred-themes)

(defun slothbar-icon--read-u16be (pos)
  "Read an unsigned 16-bit big-endian integer at 1-based buffer POS."
  (if (and (>= pos (point-min))
           (<= (+ pos 2) (point-max)))
      (logior (ash (char-after pos) 8)
              (char-after (1+ pos)))
    (signal 'end-of-file
            (format "Unexpected end of file reading u16be from pos %s"
                    pos))))

(defun slothbar-icon--read-u32be (pos)
  "Read an unsigned 32-bit big-endian integer at 1-based buffer POS."
  (if (and (>= pos (point-min))
            (<= (+ pos 4) (point-max)))
      (logior (ash (char-after pos) 24)
              (ash (char-after (+ pos 1)) 16)
              (ash (char-after (+ pos 2)) 8)
              (char-after (+ pos 3)))
    (signal 'end-of-file
            (format "Unexpected end of file reading u32be from pos %s"
                    pos))))

(defun slothbar-icon--read-cstring (offset)
  "Read a null-terminated UTF-8 string at 0-based file OFFSET."
  (let* ((start (1+ offset))
         (end start))
    (while (and (<= end (point-max))
                (not (eq 0 (char-after end))))
      (setq end (1+ end)))
    (if (eq 0 (char-after end))
        (decode-coding-string (buffer-substring-no-properties start end) 'utf-8)
      (signal 'end-of-file
              (format "Unexpected end of file reading cstring from pos %s"
                      offset)))))

(defsubst slothbar-icon--parse-icon-theme-cache (file)
  "Parse GTK icon-theme.cache FILE and return an alist of (ICON-NAME . DIRS)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let* ((major (slothbar-icon--read-u16be 1))
           (minor (slothbar-icon--read-u16be 3))
           (hash-offset (slothbar-icon--read-u32be 5))
           (dir-list-offset (slothbar-icon--read-u32be 9)))
      (unless (= major 1)
        (error "Unsupported icon-theme.cache major version: %d.%d" major minor))
      ;; 1. Read directory table
      (let* ((n-dirs (slothbar-icon--read-u32be (1+ dir-list-offset)))
             (dirs (make-vector n-dirs nil)))
        (dotimes (i n-dirs)
          (let ((str-off
                 (slothbar-icon--read-u32be (+ 1 dir-list-offset 4 (* i 4)))))
            (aset dirs i (slothbar-icon--read-cstring str-off))))
        ;; 2. Walk hash buckets and collision chains
        (let* ((n-buckets (slothbar-icon--read-u32be (1+ hash-offset)))
               (results nil))
          (dotimes (b n-buckets)
            (let ((icon-off (slothbar-icon--read-u32be
                             (+ 1 hash-offset 4 (* b 4)))))
              (while (and (/= icon-off 0) (/= icon-off #xffffffff))
                (let* ((chain-off (slothbar-icon--read-u32be (1+ icon-off)))
                       (name-off (slothbar-icon--read-u32be (+ 1 icon-off 4)))
                       (img-list-off (slothbar-icon--read-u32be (+ 1 icon-off 8)))
                       (name (slothbar-icon--read-cstring name-off))
                       (n-imgs (slothbar-icon--read-u32be (1+ img-list-off)))
                       (icon-dirs nil))
                  (dotimes (j n-imgs)
                    (let* ((entry-pos (+ 1 img-list-off 4 (* j 4)))
                           (dir-idx (slothbar-icon--read-u16be entry-pos))
                           (flags (slothbar-icon--read-u16be (+ entry-pos 2))))
                      (when (> (logand flags #x7) 0) ; Has PNG (1), XPM (2), or SVG (4)
                        (push (aref dirs dir-idx) icon-dirs))))
                  (push (cons name (nreverse icon-dirs)) results)
                  (setq icon-off chain-off)))))
          (nreverse results))))))

(defun slothbar-icon--parse-icon-theme-cache-safe (file)
  "Parse GTK icon-theme.cache FILE and return an alist of (ICON-NAME . DIRS).
Returns nil if there is any error during parsing."
  (when (file-readable-p file)
    (condition-case err
        (slothbar-icon--parse-icon-theme-cache file)
      (error
       (slothbar--log-warn* "Failed to parse icon-theme.cache %s: %s" file err)
       nil))))

(defun slothbar-icon--scan-theme-dir (theme-dir)
  "Scan THEME-DIR and return an alist of (ICON-NAME . DIRS)."
  (let* ((root (file-name-as-directory (expand-file-name theme-dir)))
         (all-files (directory-files-recursively root "\\.\\(png\\|svg\\|xpm\\|icon\\)\\'" nil t t))
         (icon-alist '()))
    (dolist (file all-files)
      (let* ((rel (file-relative-name file root))
             (subdir (directory-file-name (or (file-name-directory rel) "")))
             (ext (downcase (or (file-name-extension file) "")))
             (base (file-name-sans-extension (file-name-nondirectory file)))
             (flag (pcase ext
                     ("xpm" 1)
                     ("svg" 2)
                     ("png" 4)
                     ("icon" 8)
                     (_ 0))))
        (when (and (> flag 0) (not (string-empty-p subdir)))
          (let* ((dir-alist (assoc base icon-alist)))
            (if dir-alist
                (setcdr dir-alist
                        (cons subdir (cdr dir-alist)))
              (push (cons base (list subdir)) icon-alist))))))
    icon-alist))

(defconst slothbar-icon--fixed-re
  "[[:digit:]]+x\\([[:digit:]]+\\).*"
  "A regular expression to match fixed sized icon directories.
It captures the icon height.")

(defconst slothbar-icon--scalable-re
  "^\\(scalable\\|symbolic\\).*"
  "A regular expression to match scalable icon directories.
It captures the icon type.")

(defun slothbar-icon--icon-dirs-< (x y)
  "Return t if icon directory X should come before Y in a sorted list.

A list like
 (\"23x23/apps\" \"scalable/apps\" \"1024x1024/apps\" \"foo/apps\")
sorts to
 (\"scalable/apps\" \"foo/apps\" \"1024x1024/apps\" \"23x23/apps\")
when using this function as `:less-p'."
  (cond
   ((string-match slothbar-icon--scalable-re x)
    t)
   ((string-match slothbar-icon--scalable-re y)
    nil)
   ((or (not (string-match slothbar-icon--fixed-re x))
        (not (string-match slothbar-icon--fixed-re y)))
    t)
   (t (< (and (string-match slothbar-icon--fixed-re y)
              (string-to-number (match-string 1 y)))
         (and (string-match slothbar-icon--fixed-re x)
              (string-to-number (match-string 1 x)))))))

(defun slothbar-icon-theme-index (dir)
  "Return an icon theme index for DIR or nil if none could be made."
  (if-let* ((cache
             (gethash dir slothbar-icon--theme-index-cache)))
      cache
    (let* ((theme-cache (expand-file-name "icon-theme.cache" dir))
           (index (or (slothbar-icon--parse-icon-theme-cache-safe theme-cache)
                      (slothbar-icon--scan-theme-dir dir))))
      (when index
        (dolist (entry index)
          (sort (cdr entry) :lessp #'slothbar-icon--icon-dirs-< :in-place t))
        (puthash dir index slothbar-icon--theme-index-cache)
        index))))

(defun slothbar-icon--paths-by-height (icon-dirs)
  "Given a list of ICON-DIRS, return a list of (HEIGHT . ICON-DIR).

The ICON-DIRS are expected to be in `slothbar-icon--icon-dirs-<'
order.  The resulting heights extracted from paths are numeric."
  (cl-remove-if
   #'null
   (mapcar #'(lambda (path)
               (when (string-match slothbar-icon--fixed-re path)
                 `(,(string-to-number (match-string 1 path)) . ,path)))
           icon-dirs)))

(defun slothbar-icon--best-path-for-height
    (theme-dir icon-file icon-dirs height &optional include-symbolic?)
  "Find a matching THEME-DIR icon in one of ICON-DIRS for HEIGHT.

Return only icons readable with ICON-FILE as the basename +
extension.  Unless INCLUDE-SYMBOLIC? is t, exclude symbolic and scalable
ICON-DIRS."
  (cl-flet ((readablep (at-path)
              (file-readable-p
               (expand-file-name
                icon-file (expand-file-name at-path theme-dir)))))
    (if (numberp height)
        (let* ((sym-paths (cl-remove-if-not
                           #'(lambda (p)
                               (string-match slothbar-icon--scalable-re p))
                           icon-dirs))
               (sorted-paths (slothbar-icon--paths-by-height icon-dirs)))
          (if (and include-symbolic? sym-paths (cl-delete-if-not #'readablep sym-paths))
              (car sym-paths)
            (cl-loop for (a b) on sorted-paths
                     until (and a b (numberp (car b))
                                (> height (car b)) (readablep (cdr a)))
                     finally return (and a (readablep (cdr a)) (cdr a)))))
      (and (readablep (car icon-dirs)) (car icon-dirs)))))

(defun slothbar-icon--find-in-dir (dir name extensions target-height)
  "Find NAME with EXTENSIONS in DIR without recursively indexing theme roots.

The full path to the first matching icon is returned which best fits
TARGET-HEIGHT.  A file directly in DIR, such as an application-specific
IconThemePath, takes precedence over the theme index."
  (or (cl-loop for ext in extensions
               for direct = (expand-file-name (concat name "." ext) dir)
               when (file-readable-p direct) return direct)
      (when-let* ((theme-index (slothbar-icon-theme-index dir))
                  (paths (cdr (assoc name theme-index))))
        (cl-loop for ext in extensions
                 for icon-file = (concat name "." ext)
                 for best-path = (slothbar-icon--best-path-for-height
                                  dir icon-file paths target-height (equal "svg" ext))
                 for expanded = (and best-path (expand-file-name best-path dir))
                 for icon-path = (and expanded (expand-file-name icon-file expanded))
                 until icon-path
                 finally return icon-path))))

(defun slothbar-icon--preferred-subdirs (dirs &optional themes)
  "Return a list of full paths under DIRS matching THEMES.

THEMES is optional and defaults to the `slothbar-icon-preferred-themes'
slot corresponding to the current frame background-mode, or `dark' if it
is not set."
  (let* ((background-mode (or (frame-parameter nil 'background-mode) 'dark))
         (themes (or themes (alist-get background-mode
                                       slothbar-icon-preferred-themes)))
         (dirs (cl-loop for base in dirs
                        when (file-readable-p base)
                        append
                        (cl-loop for pref in themes
                                 append
                                 (directory-files
                                  base t (concat "^" (regexp-quote pref) "$"))))))
    (sort dirs
          :key #'file-name-base
          :lessp #'(lambda (a b)
                     (< (or (cl-position a themes :test #'equal)
                            most-positive-fixnum)
                        (or (cl-position b themes :test #'equal)
                            most-positive-fixnum))))))

(defun slothbar-icon--theme-inherits (dir)
  "Return DIR's inherited icon theme names from index.theme."
  (let ((file (expand-file-name "index.theme" dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward
               "^Inherits[ \t]*=[ \t]*\\(.*\\)$" nil t)
          (split-string (match-string 1) "," t "[ \t]+"))))))

(defun slothbar-icon--theme-direct-inherits (dir roots)
  "Return direct inherited theme directories for DIR under ROOTS."
  (cl-loop for name in (slothbar-icon--theme-inherits dir)
           with base-dir = (file-name-directory (directory-file-name dir))
           for exact = (expand-file-name name base-dir)
           append
           (if (file-readable-p exact)
             (list exact)
             (delete-dups
              (append
               (list (expand-file-name
                      name (file-name-directory (directory-file-name dir))))
               (mapcar (lambda (root) (expand-file-name name root)) roots))))))

(defun slothbar-icon--theme-inherited-dirs (dir roots &optional seen)
  "Return transitive inherited theme directories for DIR under ROOTS.

Exclude members of SEEN from the result."
  (let ((seen (or seen (list (expand-file-name dir))))
        (direct (slothbar-icon--theme-direct-inherits dir roots)))
    (cl-loop for inherited in direct
             when (and (file-readable-p inherited)
                       (not (member inherited seen)))
             append
             (cons inherited
                   (slothbar-icon--theme-inherited-dirs
                    inherited roots (append direct seen))))))

(defun slothbar-icon-roots ()
  "Return root icon paths."
  (append
   (list (expand-file-name "~/.local/share/icons")
         (expand-file-name "~/.icons"))
   (cl-loop for dir in (split-string
                        (or (getenv "XDG_DATA_DIRS")
                            "/usr/local/share:/usr/share")
                        ":" t)
            collect (expand-file-name "icons" dir))))

(defun slothbar-icon-dirs (theme-paths)
  "Return icon directories for THEME-PATHS and the XDG data dirs."
  (let* ((theme-paths
          (cond ((and (stringp theme-paths)
                      (not (string-empty-p theme-paths)))
                 (list theme-paths))
                ((listp theme-paths) theme-paths)
                (t nil)))
         (key (cons (or (frame-parameter nil 'background-mode) 'dark)
                    (or theme-paths :none))))
    (or (gethash key slothbar-icon--dirs-cache)
        (let* ((roots (slothbar-icon-roots))
               (dirs (delete-dups roots))
               (preferred (slothbar-icon--preferred-subdirs dirs)))
          (setq dirs
                (append (cl-loop for p in theme-paths
                                 append
                                 (cons p (slothbar-icon--theme-inherited-dirs
                                          p roots)))
                        (cl-loop for p in preferred
                                 append
                                 (cons p (slothbar-icon--theme-inherited-dirs
                                          p roots)))))
          (setq dirs (delete-dups dirs))
          (puthash key dirs slothbar-icon--dirs-cache)
          dirs))))

(defun slothbar-icon-find (name theme-paths &optional target-height)
  "Find NAME's PNG or SVG in THEME-PATHS fitting TARGET-HEIGHT.

The `slothbar-icon-dirs' are always searched in addtion to THEME-PATHS."
  (when (and (stringp name) (not (string-empty-p name)))
    (let* ((dirs (slothbar-icon-dirs theme-paths))
           (bg-mode (or (frame-parameter nil 'background-mode) 'dark))
           (key (cons name (cons (or target-height :any) (cons bg-mode dirs))))
           (cached (gethash key slothbar-icon--cache :missing)))
      (if (not (eq cached :missing))
          (when (stringp cached)
            (slothbar--log-debug* "icon cache hit %s -> %s" name cached)
            cached)
        (let (path)
          (cl-loop for dir in dirs
                   until path
                   do
                   (setq path
                         (slothbar-icon--find-in-dir
                          dir name '("png" "svg") target-height)))
          (if path
              (slothbar--log-debug* "icon resolved %s -> %s" name path)
            (slothbar--log-debug* "icon not found %s in %s"
                                  name (mapconcat #'identity dirs ":")))
          (puthash key (or path :none) slothbar-icon--cache)
          path)))))

(defun slothbar-icon--scale-rgba (width height rgba target-height)
  "Scale straight RGBA WIDTH by HEIGHT to TARGET-HEIGHT."
  (when (and (integerp width) (integerp height)
             (> width 0) (> height 0)
             rgba
             (>= (length rgba) (* width height 4)))
    (let* ((scale (/ (float target-height) height))
           (new-width (max 1 (round (* width scale))))
           (new-height (max 1 target-height))
           (out (make-string (* new-width new-height 4) 0)))
      (dotimes (y new-height)
        (let ((sy (min (1- height) (floor (/ y scale)))))
          (dotimes (x new-width)
            (let* ((sx (min (1- width) (floor (/ x scale))))
                   (source (* (+ sx (* sy width)) 4))
                   (target (* (+ x (* y new-width)) 4)))
              (dotimes (n 4)
                (aset out (+ target n)
                      (aref rgba (+ source n))))))))
      (list new-width new-height out))))

(defun slothbar-icon--rasterize-svg (path target-height)
  "Rasterize SVG at PATH to TARGET-HEIGHT and return scaled RGBA."
  (let ((executable (executable-find slothbar-icon-svg-rasterizer)))
    (unless executable
      (slothbar--log-debug* "SVG %s skipped.  %s not found"
                            path slothbar-icon-svg-rasterizer))
    (when executable
      (let ((png (make-temp-file "slothbar-icon-" nil ".png")))
        (unwind-protect
            (when (zerop (call-process executable nil nil nil
                                       "-h" (number-to-string target-height)
                                       "-o" png path))
              (unless (require 'umbra-png nil t)
                (slothbar--log-debug*
                 "SVG %s decoded but umbra-png is unavailable" path))
              (when (require 'umbra-png nil t)
                (let* ((pixmap (umbra-png-read png))
                       (width (umbra-pixmap-width pixmap))
                       (height (umbra-pixmap-height pixmap))
                       (rgba (umbra-pixmap-demultiplied-data pixmap)))
                  (slothbar-icon--scale-rgba
                   width height rgba target-height))))
          (delete-file png))))))

(defun slothbar-icon-load (name theme-paths target-height)
  "Load and scale NAME from THEME-PATHS for TARGET-HEIGHT."
  (when-let* ((path (slothbar-icon-find name theme-paths target-height)))
    (if (string= "svg" (file-name-extension path))
        (slothbar-icon--rasterize-svg path target-height)
      (unless (require 'umbra-png nil t)
        (slothbar--log-debug*
         "icon %s found at %s but umbra-png is unavailable" name path))
      (when (require 'umbra-png nil t)
        (condition-case err
            (let* ((pixmap (umbra-png-read path))
                   (width (umbra-pixmap-width pixmap))
                   (height (umbra-pixmap-height pixmap))
                   (rgba (umbra-pixmap-demultiplied-data pixmap)))
              (slothbar--log-debug* "icon %s decoded %dx%d -> target %d"
                                    path width height target-height)
              (slothbar-icon--scale-rgba width height rgba target-height))
          (error
           (slothbar--log-debug* "icon %s failed: %S" path err)
           nil))))))

(provide 'slothbar-icon)
;;; slothbar-icon.el ends here
