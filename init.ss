(cond-expand
	(guile (use-modules (srfi srfi-1) (ice-9 format) (ice-9 match)))
	(chibi (import (scheme base) (scheme r5rs) (srfi 1) (chibi) (chibi string)))
	;(chibi (import (scheme base) (scheme r5rs) (srfi 1) (srfi 130) (chibi) (chibi string)))
	(chicken (import srfi-1 srfi-13 matchable format (chicken process-context)))
	(mit (display "mit"))
	(tinyscheme (display "tiny"))
)

(cond-expand
	((or guile chicken mit tinyscheme)
		(define (string-replace-all str from to)
			(let ((from-len (string-length from)))
				(let loop ((i 0) (parts '()))
					(let ((pos (string-contains str from i)))
						(if pos
								(loop (+ pos from-len)
											(cons to (cons (substring str i pos) parts)))
								(apply string-append 
											 (reverse (cons (substring str i) parts)))))))))
	(chibi
		(define (string-replace-all str from to)
			(let ((from-len (string-length from)))
				(let loop ((cursor (string-cursor-start str)) (parts '()))
					(let ((found-cursor (string-contains str from cursor)))
						(if found-cursor
								(let ((before-match (substring-cursor str cursor found-cursor)))
									(loop (string-cursor-forward str found-cursor from-len)
												(cons to (cons before-match parts))))
								(apply string-append
											 (reverse (cons (substring-cursor str cursor (string-cursor-end str)) parts))))))))))

(cond-expand
	((or guile chibi mit)
		(define (arguments) (cdr (command-line))))
	(chicken
		(define (arguments) (command-line-arguments))))

(cond-expand
	(tinyscheme 
		(define (include file) (load file))
		(define (fold proc init lst)
			(if (null? lst)
					init
					(fold proc (proc (car lst) init) (cdr lst))))
		(load "argv")
	(define (string-contains str substr . rest)
		(let ((start (if (null? rest) 0 (car rest)))
					(sub-len (string-length substr))
					(str-len (string-length str)))
			(let loop ((i start))
				(cond
				 ((> (+ i sub-len) str-len) #f)
				 (else
					(let char-loop ((j 0))
						(cond
						 ((= j sub-len) i)
						 ((char=? (string-ref str (+ i j)) (string-ref substr j))
							(char-loop (+ j 1)))
						 (else (loop (+ i 1))))))))))
		(define (string-join strings sep)
			(if (null? strings)
					sep
		      (let loop ((result (car strings)) (rest (cdr strings)))
					  (if (null? rest)
							result
							(loop (string-append result sep (car rest))
										(cdr rest))))))
		(define (arguments) ((display "symbol_arguments") (apply symbol->string (symbol_arguments)))))
	 (else (begin #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(add-test! "test procedure" "0" "0")

;(add-test! "unescape-cc" (unescape-cc "\n\t\n") "\\n\\t\\n")



(define (run-tests)
  (for-each (lambda (t) (test (car t) (cadr t) (caddr t))) *tests*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "html.ss")

(define (arguments-handle)
	(let ((args (arguments)))
			 (cond
				 ((null? args) (display "bro"))
				 ((equal? (car args) "--test") (display (car args)) (run-tests)))))

(arguments-handle)

; vim: sw=2:ts=2:sts=2:ft=scheme
