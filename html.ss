(import srfi-13 matchable)

(define (generate-html title body)
  (display #<#EOF
<!doctype html>
<html>
<head>
<title>
#{title}
</title>
</head>
<body>
#{body}
</body>
</html>

EOF
))

(define-syntax generate-html-new 
  (syntax-rules ()
	((_ (key val) ...)
	 (generate-html-impl (list (list 'key val) ... )))))

(define (generate-html-impl args)
  (let ((title (or (assoc-ref args 'title) "ERROR" ))
		(body (or (assoc-ref args 'body) "<h2>ERROR</h2>" ))
		(style (assoc-ref args 'style)))
  (display #<#EOF
<!doctype html>
<html>
<head>
<title>
#{title}
</title>
#{(if style (string-append "<style>" style "</style>") "")}
</head>
<body>
#{body}
</body>
</html>


EOF
)))

(define (assoc-ref alist key)
  (let ((pair (assoc key alist)))
    (and pair (cadr pair))))


; CSS stuff
(define (property->css prop)
  (let ((name (car prop))
        (value (cadr prop)))
    (string-append "  " (symbol->string name) ": " value ";\n")))

(define (rule->css selector props)
  (string-append
    (symbol->string selector) " {\n"
    (string-join (map property->css props)) "}\n\n"))

(define (scss->css scss)
  (string-join
    (map (match-lambda
           ((selector props)
            (rule->css selector props)))
         scss)))

(define (render-html doc)
  (string-append
	"<!doctype html>\n"
	(serialize-sxml doc method: 'html)
	"\n"))

; HTML renderer
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

(define (html5 head body)
  (string-append
    "<!DOCTYPE html>\n"
    (render-html-node `(html (head ,@head) (body ,@body)))))

(begin (display (html5 
				  `((title "Big T")
					(style ,(scss->css `((body ((background white)))))))
				  `((p meow)))))



; (begin (display (render-html `(html (head (title "Big T")) (body (p "meow"))))))

#;(begin
  (generate-html-new
	(title "meow")
	(body "whatever")))
#; (begin
  (generate-html "This is my title!" "<p>bodytext</p>" ))
#; (begin (display (scss->css '((body ((background "white")))))))

