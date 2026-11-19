;;; slothbar-test-utils.el --- Utilities for slothbar unit tests  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.32.0
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

;; This contains utilities for use by slothbar unit tests.

;;; Code:


(require 'cl-lib)
(require 'slothbar-icon)

(defun slothbar-test--u16be-bytes (n)
  "Encode integer N as a 2-byte big-endian unibyte string."
  (unibyte-string (logand (ash n -8) #xff)
                  (logand n #xff)))

(defun slothbar-test--u32be-bytes (n)
  "Encode integer N as a 4-byte big-endian unibyte string."
  (unibyte-string (logand (ash n -24) #xff)
                  (logand (ash n -16) #xff)
                  (logand (ash n -8) #xff)
                  (logand n #xff)))

(defun slothbar-test--padded-cstring (str)
  "Encode STR as null-terminated UTF-8 padded to a 4-byte boundary."
  (let* ((raw (encode-coding-string str 'utf-8))
         (len (1+ (length raw)))
         (pad (% (- 4 (% len 4)) 4)))
    (concat raw (make-string (1+ pad) 0))))

(defun slothbar-test--g-str-hash (str)
  "Compute GLib g_str_hash (32-bit unsigned DJB2) for UTF-8 STR."
  (let ((bytes (encode-coding-string str 'utf-8))
        (h 5381))
    (dotimes (i (length bytes))
      (setq h (logand (+ (- (ash h 5) h) (aref bytes i)) #xffffffff)))
    h))

(defun slothbar-test--scan-theme-dir (theme-dir)
  "Scan THEME-DIR and return (DIRS-VECTOR . ICON-TABLE)."
  (let* ((root (file-name-as-directory (expand-file-name theme-dir)))
         (all-files (directory-files-recursively root "\\.\\(png\\|svg\\|xpm\\|icon\\)\\'"))
         (dir-set (make-hash-table :test 'equal))
         (icon-map (make-hash-table :test 'equal)))
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
          (puthash subdir t dir-set)
          (let* ((dir-alist (gethash base icon-map))
                 (cell (assoc subdir dir-alist)))
            (if cell
                (setcdr cell (logior (cdr cell) flag))
              (puthash base (cons (cons subdir flag) dir-alist) icon-map))))))
    (let* ((dirs (vconcat (sort (hash-table-keys dir-set) #'string<)))
           (dir-idx-map (make-hash-table :test 'equal)))
      (dotimes (i (length dirs))
        (puthash (aref dirs i) i dir-idx-map))
      ;; Replace subdir strings with numeric directory indices
      (maphash (lambda (name entries)
                 (puthash name
                          (sort (mapcar (lambda (e) (cons (gethash (car e) dir-idx-map) (cdr e)))
                                        entries)
                                (lambda (a b) (< (car a) (car b))))
                          icon-map))
               icon-map)
      (cons dirs icon-map))))

(defun slothbar-test-write-icon-theme-cache (theme-dir &optional output-file)
  "Generate a GTK-compatible icon-theme.cache for THEME-DIR.
Writes to OUTPUT-FILE if provided or to icon-theme.cache in theme-dir."
  (let* ((scan (slothbar-test--scan-theme-dir theme-dir))
         (dirs (car scan))
         (icon-map (cdr scan))
         (n-dirs (length dirs))
         (icon-names (sort (hash-table-keys icon-map) #'string<))
         (n-icons (length icon-names))
         (n-buckets (max 1 (logior n-icons 1))) ; Odd bucket count >= 1
         (buckets (make-vector n-buckets nil))
         (offset 0)
         (chunks nil))
    (cl-flet ((emit (bytes)
                (let ((pos offset))
                  (push bytes chunks)
                  (setq offset (+ offset (length bytes)))
                  pos)))
      ;; 1. Reserve 12-byte header (patched at the end)
      (emit (make-string 12 0))

      ;; 2. Write directory strings and directory list table
      (let ((dir-str-offsets (make-vector n-dirs 0)))
        (dotimes (i n-dirs)
          (aset dir-str-offsets i (emit (slothbar-test--padded-cstring (aref dirs i)))))
        (let ((dir-list-offset (emit (slothbar-test--u32be-bytes n-dirs))))
          (dotimes (i n-dirs)
            (emit (slothbar-test--u32be-bytes (aref dir-str-offsets i))))

          ;; 3. Group icons into hash buckets
          (dolist (name icon-names)
            (let ((b (mod (slothbar-test--g-str-hash name) n-buckets)))
              (push name (aref buckets b))))

          ;; 4. Write icon image lists, name strings, and chain nodes
          (let ((bucket-heads (make-vector n-buckets #xffffffff)))
            (dotimes (b n-buckets)
              (let ((chain-next #xffffffff))
                (dolist (name (aref buckets b))
                  (let* ((imgs (gethash name icon-map))
                         (img-list-off (emit (slothbar-test--u32be-bytes (length imgs)))))
                    (dolist (img imgs)
                      (emit (slothbar-test--u16be-bytes (car img)))
                      (emit (slothbar-test--u16be-bytes (cdr img))))
                    (let* ((name-off (emit (slothbar-test--padded-cstring name)))
                           (node-off (emit (concat (slothbar-test--u32be-bytes chain-next)
                                                   (slothbar-test--u32be-bytes name-off)
                                                   (slothbar-test--u32be-bytes img-list-off)))))
                      (setq chain-next node-off))))
                (aset bucket-heads b chain-next)))

            ;; 5. Write hash table header and bucket pointers
            (let ((hash-offset (emit (slothbar-test--u32be-bytes n-buckets))))
              (dotimes (b n-buckets)
                (emit (slothbar-test--u32be-bytes (aref bucket-heads b))))

              ;; 6. Assemble binary payload and patch 12-byte header
              (let ((blob (apply #'concat (nreverse chunks)))
                    (out (or output-file
                             (expand-file-name "icon-theme.cache" theme-dir))))
                (aset blob 0 0) (aset blob 1 1) ; Major version = 1
                (aset blob 2 0) (aset blob 3 0) ; Minor version = 0
                (let ((h-bytes (slothbar-test--u32be-bytes hash-offset))
                      (d-bytes (slothbar-test--u32be-bytes dir-list-offset)))
                  (dotimes (i 4)
                    (aset blob (+ 4 i) (aref h-bytes i))
                    (aset blob (+ 8 i) (aref d-bytes i))))
                (with-temp-file out
                  (set-buffer-multibyte nil)
                  (insert blob))
                out))))))))

(provide 'slothbar-test-utils)
;;; slothbar-test-utils.el ends here
