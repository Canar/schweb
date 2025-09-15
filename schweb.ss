;;;platform;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
	(guile (use-modules (srfi srfi-1) (ice-9 format) (ice-9 match)))
	(chibi (import (scheme base) (scheme r5rs) (srfi 1) (chibi) (chibi string)))
	(chicken (import srfi-1 srfi-13 matchable format (chicken process-context)))
	(else)
)


(cond-expand
	((or guile chicken mit tinyscheme sigscheme)
		(define (string-replace-all str from to)
			(let ((from-len (string-length from)))
				(let loop ((i 0) (parts '()))
					(let ((pos (string-contains str from i)))
						(if pos
								(loop (+ pos from-len)
											(cons to (cons (substring str i pos) parts)))
								(apply string-append 
											 (reverse (cons (substring str i (string-length str)) parts)))))))))
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
	((or guile chibi)
		(define arguments (cdr (command-line))))
	(mit
		(define arguments (cdddr (command-line))))
	(chicken
		(define arguments (command-line-arguments)))
	(else))

(cond-expand
	((or tinyscheme sigscheme)
		(define (include file) (load file))
		(define (fold proc init lst)
			(if (null? lst)
					init
					(fold proc (proc (car lst) init) (cdr lst))))
		(load "argv")
		;(define arguments (map symbol->string symbol_arguments))
		)
	 (else #t))

;;;string;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
	((or tinyscheme mit sigscheme) 
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
										(cdr rest)))))))
	(else #t))

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


;;;test;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test name l r)
  (display (if (equal? l r )
	(string-from (list "test " name " passed. " (unescape-cc l) "\n"))
	(string-from (list "test " name " failed.\n\tL: " l "\n\tR: " r "\n") ))))

(define *tests* '())

(define (add-test! name l r)
  (set! *tests* (append *tests* (list (list name l r)))))


(define (run-tests)
  (for-each (lambda (t) (test (car t) (cadr t) (caddr t))) *tests*))


; test depends on unescape-cc, string functions
(add-test! "test" #t #t)
(add-test! "unescape-cc" (unescape-cc "0") "0")
(add-test! "unescape-cc" (unescape-cc "\n\t\n") "\\n\\t\\n")
(add-test! "string-replace-all" (string-replace-all "banana" "a" "X") "bXnXnX")

;;;style;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (web-style-escape str)
  (string-replace-all str "\"" "\\\""))

(define (property-render prop)
  (let ((name (car prop))
        (value (web-style-escape (cadr prop))))
    (string-append "\t" (symbol->string name) ":" value ";\n")))

(define (rule-render selector props)
  (string-append
    (symbol->string selector) " {\n"
    (apply string-append (map property-render props)) "}\n\n"))

(add-test! "rule-render procedure" (rule-render 'body '((padding "0"))) "body {\n\tpadding:0;\n}\n\n")

(define (web-style-render sexp)
  (string-join
   (map (lambda (rule)
          (let ((selector (car rule))
                (props (cadr rule)))
            (rule-render selector props)))
        sexp) ""))

(add-test! "web-style-render procedure" (web-style-render '((body ((padding "0"))))) "body {\n\tpadding:0;\n}\n\n")

;;;web;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (web-entity-encode str)
  (fold
    (lambda (pair s)
      (string-replace-all s (car pair) (cadr pair)))
    str
    '(("&" "&amp;")
      ("<" "&lt;")
      (">" "&gt;") 
      ("\"" "&quot;"))))

(add-test! "web-entity-encode procedure" (web-entity-encode "&<>\"") "&amp;&lt;&gt;&quot;")

(define (web-attr-render attrs)
  (if (null? attrs)
      ""
      (apply string-append
             (map (lambda (attr)
                    (let ((k (car attr))
                          (v (cadr attr)))
                      (string-append " " (symbol->string k) "=\"" v "\"")))
                  attrs))))

(define void-tags '(br hr img input meta link base area col embed param source track wbr))

(define (render-html-node node)
  (if (list? node)
	(render-html-list node)
	(string-from node)))
  
(define (render-html-node-void tag parts)
   (let loop ((parts parts) (attrs ""))
	 (cond
	   ((null? parts) (string-append "<" (render-html-node tag) attrs " />"))
	   ((and (pair? (car parts))
			 (eq? (caar parts) 'attr))
		(loop (cdr parts) (web-attr-render (cdar (car parts)))))
	   (else (loop (cdr parts) attrs)))))

(define (render-html-node-nonvoid tag parts)
	(let loop ((parts parts) (attrs "") (body '()))
	 (cond
	   ((null? parts)
		(string-append
		  "<" (render-html-node tag) attrs ">"
		  (apply string-append (map render-html-node (reverse body)))
		  "</" (render-html-node tag) ">"))
	   ((and (pair? (car parts))
			 (eq? (caar parts) 'attr))
		(loop (cdr parts) (web-attr-render (cdar (car parts))) body))
	   (else
		(loop (cdr parts) attrs (cons (car parts) body))))))

(define (render-html-list node)
 (let ((tag (car node))
	   (parts (cdr node)))
   (if (member tag void-tags)
	   (render-html-node-void tag parts)
	   (render-html-node-nonvoid tag parts))))

(define doctype "<!DOCTYPE html>")

(define (web head body)
  (string-append doctype "\n"
    (render-html-node `(html (head ,@head) (body ,@body)))))

(add-test! "web procedure" 
	(web `((title "Q")
		   (style ,(web-style-render `((body ((padding "0")))))))
		 `((h1 "Z")
		   (p "test")))
	(string-append doctype 
	  "\n<html><head><title>Q</title><style>body {\n\tpadding:0;\n}\n\n</style></head><body><h1>Z</h1><p>test</p></body></html>"))

;;;argument handling;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (arguments-handle)
	(let ((args arguments))
			 (cond
				 ((null? args) #t)
				 ((equal? (car args) "--test") (run-tests)))))

(arguments-handle)

; vim: sw=2:ts=2:sts=2:ft=scheme
