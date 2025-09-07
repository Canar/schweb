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

#;(begin
  (generate-html-new
	(title "meow")
	(body "whatever")))
#; (begin
  (generate-html "This is my title!" "<p>bodytext</p>" ))
(begin (display (scss->css '((body ((background "white")))))))
