;; What follows is a "manifest" equivalent to the command line you gave.
;; You can store it in a file that you may then pass to any 'guix' command
;; that accepts a '--manifest' (or '-m') option.

(specifications->manifest
 (list "emacs"
       "emacs-all-the-icons"
       "emacs-backlight"
       "emacs-f"
       "emacs-fontsloth"
       "emacs-log4e"
       "emacs-nerd-icons"
       "emacs-package-lint"
       "emacs-s"
       "emacs-volume"
       "emacs-xelb"
       "nss-certs"
       "pkg-config"))
