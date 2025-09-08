(import srfi-1 srfi-13 matchable format)

(define (string-replace-all str from to)
  (let ((from-len (string-length from)))
    (let loop ((i 0) (parts '()))
      (let ((pos (string-contains str from i)))
        (if pos
            (loop (+ pos from-len)
                  (cons to (cons (substring str i pos) parts)))
            (apply string-append 
                   (reverse (cons (substring str i) parts))))))))

; CSS stuff
(define (escape-css str)
  (string-replace-all str "\"" "\\\""))

(define (property-render prop)
  (let ((name (car prop))
        (value (escape-css (cadr prop))))
    (string-append "\t" (symbol->string name) ":" value ";\n")))

(define (rule-render selector props)
  (string-append
    (symbol->string selector) " {\n"
    (string-join (map property-render props)) "}\n\n"))

(define (css-render sexp)
  (string-join
    (map (match-lambda
           ((selector props)
            (rule-render selector props)))
         sexp)))

(define (render-html doc)
  (string-append
	"<!doctype html>\n"
	(serialize-sxml doc method: 'html)
	"\n"))

; HTML renderer
 

(define (escape-html str)
  (fold
    (lambda (pair s)
      (string-replace-all s (car pair) (cadr pair)))
    str
    '(("&" . "&amp;")
      ("<" . "<")
      (">" . ">")   ; Fixed: was &rt; — not a valid HTML entity
      ("\"" . "&quot;"))))

(define (attrs->string attrs)
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
  (cond
    ((string? node) node)
    ((list? node) (render-html-list node))
    ((symbol? node) (symbol->string node))
    ((number? node) (number->string node))
    ((boolean? node) (if node "true" "false"))
    (else (render-html-node (format "~A" node)))))

(define (render-html-node-void tag parts)
   (let loop ((parts parts) (attrs ""))
	 (cond
	   ((null? parts) (string-append "<" (render-html-node tag) attrs " />"))
	   ((and (pair? (car parts))
			 (eq? (caar parts) '@))
		(loop (cdr parts) (attrs->string (cdar (car parts)))))
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
		(loop (cdr parts) (attrs->string (cdar (car parts))) body))
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

(define (test name l r)
  (display (if (equal? l r )
	(format "[TEST] ~A PASSED. L equals R equals ~A\n" name l)
	(format "[TEST] ~A FAILED.\n\tL: ~A\n\tR: ~A\n" name l r) )))

(define (unescape-cc str)
  (fold-left
    (lambda (s pair)
      (string-replace-all s (car pair) (cadr pair)))
    str
    '(("\n" . "\\n")
      ("\t" . "\\t"))))
  
(begin 
  (test "test procedure" 0 0)
  (test "string-replace-all procedure" (string-replace-all "banana" "a" "X") "")
  (test "escape-html procedure" (escape-html "&<>\"") "")
  (test "web procedure" 
    (web `((title "Q")
		   (style ,(css-render `((body ((padding "0")))))))
		 `((h1 "Z")
		   (p "test")))
	(string-append doctype 
	  "\n<html><head><title>Q</title><style>body {\n\tpadding:0;\n}\n\n</style></head><body><h1>Z</h1><p>test</p></body></html>"))
)

#;(begin (display (html5 
				  `((title "Big T")
					(style ,(css-render `((body ((background "white")))))))
				  `((p meow)))))

