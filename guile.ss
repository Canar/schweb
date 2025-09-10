(use-modules (srfi srfi-1) (ice-9 format) (ice-9 match))

(define (format-local str . args)
  (apply format #f str args))

(include "shared-chicken-guile.ss")

(include "html.ss")
