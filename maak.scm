(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak))

(define emacs (or (getenv "EMACS") "emacs"))

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

;; Run the pure layout/damage/draw-op tests.
(define (test-unit)
  "Run the pure unit tests."
  ($ (list (string-append
            emacs " -Q --batch -L . -l test/slothbar-unit-test.el"
            " -f ert-run-tests-batch-and-exit"))))

;; Run the pure tests and the live X backend smoke test.
(define (test)
  "Run the unit tests and the Xwayland smoke test."
  (test-unit)
  (test-xwayland))
