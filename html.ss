(include "util.ss")

;platform-specific implementation, general test
(add-test! "string-replace-all procedure" (string-replace-all "banana" "a" "X") "bXnXnX")

; CSS stuff
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

(add-test! "rule-render procedure" 
	(rule-render 'body '((padding "0")))
	"body {\n\tpadding:0;\n}\n\n")

(display "a")
(define (web-style-render sexp)
  (string-join
   (map (lambda (rule)
          (let ((selector (car rule))
                (props (cadr rule)))
            (rule-render selector props)))
        sexp) ""))
(display "b")


(add-test! "web-style-render procedure" 
	(web-style-render '((body ((padding "0")))))
	"body {\n\tpadding:0;\n}\n\n")
; HTML renderer
(display "c")

(define (web-entity-encode str)
  (fold
    (lambda (pair s)
      (begin (display pair) (newline) (display s) (newline)  (string-replace-all s (car pair) (cadr pair))))
    str
    '(("&" "&amp;")
      ("<" "&lt;")
      (">" "&gt;") 
      ("\"" "&quot;"))))

(display "d")
(add-test! "web-entity-encode procedure" (web-entity-encode "&<>\"") "&amp;&lt;&gt;&quot;")
(display "e")

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
  
; (cond
;    ((string? node) node)
;    ((list? node) (render-html-list node))
;    ((symbol? node) (symbol->string node))
;    ((number? node) (number->string node))
;    ((boolean? node) (if node "true" "false"))
;    (else (render-html-node (string-from node)))))


(define (render-html-node-void tag parts)
   (let loop ((parts parts) (attrs ""))
	 (cond
	   ((null? parts) (string-append "<" (render-html-node tag) attrs " />"))
	   ((and (pair? (car parts))
			 (eq? (caar parts) '@))
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
			 (eq? (caar parts) '@))
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
; vim: sw=2:ts=2:sts=2:ft=scheme
