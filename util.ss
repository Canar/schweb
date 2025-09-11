(define (string-from value)
  (cond
    ((number? value) (number->string value))
    ((string? value) value)
    ((boolean? value) (if value "true" "false"))
    ((symbol? value) (symbol->string value))
    ((list? value) (string-from-list value))
    ;; For any other type, return a default string indicating the type.
    (else (string-append "#<" value ">"))))

(define (string-from-list lst)
  (cond
    ((null? lst) "")
    (else (string-append (string-from (car lst))
                         (string-from-list (cdr lst))))))

(define (unescape-cc str)
  (fold
    (lambda (pair s)
      (string-replace-all s (car pair) (cadr pair)))
    (string-from str)
    '(("\n" "\\n")
      ("\t" "\\t"))))

(define (test name l r)
  (display (if (equal? l r )
	(string-from (list "[TEST] " name " PASSED. L equals R equals " (unescape-cc l) "\n"))
	(string-from `("[TEST] " ,name " FAILED.\n\tL: " ,l "\n\tR: " ,r "\n") ))))

(define *tests* '())
(define (add-test! name l r)
  (set! *tests* (append *tests* (list (list name l r)))))

(add-test! "test procedure" 0 0)

(add-test! "unescape-cc" (unescape-cc "\n\t\n") "\\n\\t\\n")

;; Procedure to run all tests in the global list.
(define (run-tests)
  (for-each (lambda (t) (test (car t) (cadr t) (caddr t))) *tests*))


