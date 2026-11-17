;;; slothbar-icon.el --- Shared icon lookup and rasterization  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;;; Commentary:

;; Shared icon-name lookup for tray and menu rendering.  PNG and SVG files
;; are indexed once per icon directory.  SVG is rasterized through an optional
;; external command and decoded through Umbra.

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

(defvar slothbar-icon--cache (make-hash-table :test 'equal)
  "Cache of icon name and directory sets to paths or `:none'.")

(defvar slothbar-icon--dirs-cache (make-hash-table :test 'equal)
  "Cache of theme paths to resolved icon directory lists.")

(defvar slothbar-icon--index-cache (make-hash-table :test 'equal)
  "Cache of icon directories to name and format indexes.")

(defun slothbar-icon-clear-cache ()
  "Clear all shared icon caches."
  (clrhash slothbar-icon--cache)
  (clrhash slothbar-icon--dirs-cache)
  (clrhash slothbar-icon--index-cache))

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

(defun slothbar-icon--theme-inherited-dirs (dir roots &optional seen)
  "Return inherited theme directories for DIR under ROOTS."
  (let ((seen (or seen (list (expand-file-name dir)))))
    (cl-loop for name in (slothbar-icon--theme-inherits dir)
             for candidates =
             (delete-dups
              (append
               (list (expand-file-name
                      name (file-name-directory (directory-file-name dir))))
               (mapcar (lambda (root) (expand-file-name name root)) roots)))
             append
             (cl-loop for candidate in candidates
                      for expanded = (expand-file-name candidate)
                      when (and (file-directory-p expanded)
                                (not (member expanded seen)))
                      append (cons expanded
                                   (slothbar-icon--theme-inherited-dirs
                                    expanded roots (cons expanded seen)))))))

(defun slothbar-icon-dirs (theme-paths)
  "Return icon directories for THEME-PATHS and the XDG data dirs."
  (let* ((theme-paths
          (cond ((stringp theme-paths) (list theme-paths))
                ((listp theme-paths) theme-paths)
                (t nil)))
         (key (or theme-paths :none)))
    (or (gethash key slothbar-icon--dirs-cache)
        (let* ((roots
                (append
                 (list (expand-file-name "~/.local/share/icons")
                       (expand-file-name "~/.icons"))
                 (cl-loop for dir in (split-string
                                      (or (getenv "XDG_DATA_DIRS")
                                          "/usr/local/share:/usr/share")
                                      ":" t)
                          collect (expand-file-name "icons" dir))
                 (list "/usr/share/icons" "/usr/local/share/icons")))
               (dirs (delete-dups
                      (append (mapcar #'expand-file-name theme-paths)
                              roots))))
          (dolist (dir dirs)
            (setq dirs (append dirs
                               (slothbar-icon--theme-inherited-dirs
                                dir roots))))
          (setq dirs (delete-dups dirs))
          (puthash key dirs slothbar-icon--dirs-cache)
          dirs))))

(defun slothbar-icon--index (dir)
  "Return DIR's cached map of icon names to `:png' and `:svg' paths."
  (or (gethash dir slothbar-icon--index-cache)
      (let ((index (make-hash-table :test 'equal)))
        (when (file-directory-p dir)
          (dolist (path
                   (directory-files-recursively
                    dir "\\.\\(png\\|svg\\)\\'" nil))
            (let* ((name (file-name-base path))
                   (extension (file-name-extension path))
                   (entry (gethash name index)))
              (cond
               ((equal extension "png")
                (setf (gethash name index)
                      (plist-put (copy-sequence entry) :png path)))
               ((and (equal extension "svg")
                     (not (plist-get entry :png)))
                (setf (gethash name index)
                      (plist-put (copy-sequence entry) :svg path)))))))
        (puthash dir index slothbar-icon--index-cache)
        index)))

(defun slothbar-icon-find (name theme-paths)
  "Find NAME's PNG or SVG in THEME-PATHS or the standard icon directories."
  (when (and (stringp name) (not (string-empty-p name)))
    (let* ((dirs (slothbar-icon-dirs theme-paths))
           (key (cons name dirs))
           (cached (gethash key slothbar-icon--cache :missing)))
      (if (not (eq cached :missing))
          (when (stringp cached)
            (slothbar--log-debug* "icon cache hit %s -> %s" name cached)
            cached)
        (let ((path
               (cl-loop
                for extension in '("png" "svg")
                thereis
                (cl-loop
                 for dir in dirs
                 for entry = (gethash name (slothbar-icon--index dir))
                 for candidate = (plist-get
                                  entry (intern (concat ":" extension)))
                 thereis (and candidate
                              (file-readable-p candidate)
                              candidate)))))
          (if path
              (slothbar--log-debug* "icon resolved %s -> %s" name path)
            (slothbar--log-debug* "icon not found %s in %s"
                                  name (mapconcat #'identity dirs ":")))
          (puthash key (or path :none) slothbar-icon--cache)
          path)))))

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
  (when-let* ((path (slothbar-icon-find name theme-paths)))
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
