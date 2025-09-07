(import srfi-13 matchable sxml-serializer)

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

(define void-tags
  '(br hr img input meta link base area col embed param source track wbr))

(define (->html node)
  (cond
    ((string? node) node)
    ((symbol? node) (symbol->string node))
    ((number? node) (number->string node))

    ((list? node)
     (let ((tag (car node))
           (rest (cdr node)))
       (if (member tag void-tags)
           (if (and (pair? rest)
                    (pair? (car rest))
                    (eq? (caar rest) '@))
               (let ((attrs (attrs->string (cdar (car rest)))))
                 (string-append "<" (->html tag) attrs " />"))
               (string-append "<" (->html tag) " />"))
           (let ((with-attr (if (and (pair? rest)
                                     (pair? (car rest))
                                     (eq? (caar rest) '@))
                                (begin
                                  (set! rest (cdr rest))
                                  (attrs->string (cdar (car rest))))
                                "")))
             (string-append
               "<" (->html tag) with-attr ">"
               (apply string-append (map ->html rest))
               "</" (->html tag) ">")))))

    (else (->html (format "~A" node)))))

(define (html5 . body)
  (string-append
    "<!DOCTYPE html>\n"
    (->html `(html ,@body))))

(begin (display (html5 `(head (title "Big T")) `(body (p "meow")))))



; (begin (display (render-html `(html (head (title "Big T")) (body (p "meow"))))))

#;(begin
  (generate-html-new
	(title "meow")
	(body "whatever")))
#; (begin
  (generate-html "This is my title!" "<p>bodytext</p>" ))
#; (begin (display (scss->css '((body ((background "white")))))))

