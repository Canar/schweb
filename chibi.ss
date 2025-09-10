(import (scheme base) (scheme r5rs) (srfi 1) (srfi 130))

(define (format-local str . args)
  (apply format str args))


(include "html.ss")
