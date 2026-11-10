;;; slothbar-font-cache-benchmark.el --- measure cold font loading  -*- lexical-binding: t -*-

;; This is a development benchmark, not part of the slothbar test suite.
;; It uses an isolated pcache directory so it does not disturb the user's
;; normal font cache.

;;; Code:

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory load-file-name))))

(require 'cl-lib)
(require 'fontsloth)
(require 'pcache)
(require 'slothbar-font)

(defun slothbar-font-cache-benchmark--paths ()
  "Return font file paths for the default candidates.
Prefer `font-info' when a usable frame exists; otherwise resolve the
first candidate name in each group with `fc-match'."
  (let ((mapped (cl-remove-if-not
                 #'stringp
                 (append (slothbar-font-map-candidates) nil))))
    (or mapped
        (delete-dups
         (cl-loop for names in slothbar-font-candidates
                  for name = (car names)
                  for path =
                  (ignore-errors
                    (string-trim
                     (shell-command-to-string
                      (format "fc-match -f '%%{file}' %s"
                              (shell-quote-argument name)))))
                  when (and (stringp path)
                            (not (string-empty-p path))
                            (file-exists-p path))
                  collect path)))))

(let* ((paths (slothbar-font-cache-benchmark--paths)))
  (if (null paths)
      (princ "SKIP: no font paths could be resolved\n")
    (let ((fontsloth-file (symbol-file 'fontsloth-load-font 'defun)))
      (princ (format "fontsloth-file: %s\n"
                     (or fontsloth-file "unknown")))
      (unless (and fontsloth-file
                   (string-suffix-p ".elc" fontsloth-file))
        (princ "WARNING: fontsloth is interpreted; byte-compile it for comparable timings\n")))
    (let* ((dir (make-temp-file "slothbar-font-cold-" t))
           (pcache-directory (file-name-as-directory dir))
           (fontsloth-cache--initialized? nil)
           (*pcache-repositories* (make-hash-table :test 'equal))
           (fontsloth-cache-idle-save-time 0)
           (start (float-time)))
      (unwind-protect
          (progn
            (dolist (path paths)
              (fontsloth-load-font path :cache 'reload))
            (princ (format "cold-load: %.3fs for %d font(s)\n"
                           (- (float-time) start)
                           (length paths)))
            (dolist (path paths)
              (princ (format "  %s\n" path))))
        (ignore-errors (delete-directory dir t))))))

;;; slothbar-font-cache-benchmark.el ends here
