(import (scheme base) (scheme r5rs) (srfi 1) (srfi 130) (chibi trace) (chibi string))

(define (format-local str . args)
  (apply format str args))

(define (string-replace-all str from to)
  (let ((from-len (string-length from)))
    (let loop ((cursor (string-cursor-start str)) (parts '()))
      (let ((found-cursor (string-contains str from cursor)))
        (if found-cursor
            (let ((before-match (substring-cursor str cursor found-cursor)))
              (loop (string-cursor-forward str found-cursor from-len)
                    (cons to (cons before-match parts))))
            (apply string-append
                   (reverse (cons (substring-cursor str cursor (string-cursor-end str)) parts))))))))

(include "html.ss")

