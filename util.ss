(define (string-from value)
  (cond
    ((number? value) (number->string value))
    ((string? value) value)
    ((boolean? value) (if value "true" "false"))
    ((symbol? value) (symbol->string value))
    ((list? value) (string-from-list value))
    ;; For any other type, return a default string indicating the type.
    (else (string-append "#<" (object-name value) ">"))))

(define (string-from-list lst)
  (cond
    ((null? lst) "")
    (else (string-append (string-from (car lst))
                         (string-from-list (cdr lst))))))

(define (string-replace-all str from to)
  (let ((from-len (string-length from)))
    (let loop ((i 0) (parts '()))
      (let ((pos (string-contains str from i)))
        (if pos
            (loop (+ pos from-len)
                  (cons to (cons (substring str i pos) parts)))
            (apply string-append 
                   (reverse (cons (substring str i) parts))))))))

