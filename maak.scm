(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak))

;; Display program help screen.
(define (help)
  ($ '("maak --help")))

;; By default (no task given), list tasks.
(define (default)
  ($ '("maak --list")))

;; Run the live xcb backend smoke test under headless sway + Xwayland.
;; Override EMACS or SLOTHBAR_FONTSLOTH_DIR in the environment as needed.
(define (test-xwayland)
  "Run the xcb backend smoke test under headless sway and Xwayland."
  ($ '("bash test/slothbar-xwayland-smoke.sh")))
