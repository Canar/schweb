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


(define (test name l r)
  (display (if (equal? l r )
	(string-from `("[TEST] " ,name " PASSED. L equals R equals " ,l "\n"))
	(string-from `("[TEST] " ,name " FAILED.\n\tL: " ,l "\n\tR: " ,r "\n") ))))

(define *tests* '())
(define (add-test! name l r)
  (set! *tests* (append *tests* (list (list name l r)))))

(add-test! "test procedure" 0 0)

;; Procedure to run all tests in the global list.
(define (run-tests)
  (display "Running tests...\n")
  (for-each (lambda (t) (test (car t) (cadr t) (caddr t))) *tests*)
  (display "Tests finished.\n"))


(define (unescape-cc str)
  (fold
    (lambda (pair s)
      (string-replace-all s (car pair) (cadr pair)))
    str
    '(("\n" "\\n")
      ("\t" "\\t"))))

(add-test! "unescape-cc" (unescape-cc "\n\t\n") "\\n\\t\\n")
  
(define (test-all)
  (test "test procedure" "0" "0")
  (test "string-replace-all procedure" (string-replace-all "banana" "a" "X") "bXnXnX")
  (test "unescape-cc" (unescape-cc "\n\t\n") "\\n\\t\\n")
  (test "escape-html procedure" (escape-html "&<>\"") "&amp;&lt;&gt;&quot;")
  (test "web procedure" 
    (web `((title "Q")
		   (style ,(web-style-render `((body ((padding "0")))))))
		 `((h1 "Z")
		   (p "test")))
	(string-append doctype 
	  "\n<html><head><title>Q</title><style>body {\n\tpadding:0;\n}\n\n</style></head><body><h1>Z</h1><p>test</p></body></html>"))
  )
