(import srfi-1 srfi-13 matchable format)

(define (format-local str . args)
  (apply format str args))

(include "html.ss")

