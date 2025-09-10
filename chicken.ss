(import srfi-1 srfi-13 matchable format)

(define (format-local str . args)
  (apply format str args))

(include "shared-chicken-guile.ss")
(include "html.ss")

