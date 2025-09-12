(cond-expand
	(guile (use-modules (srfi srfi-1) (ice-9 format) (ice-9 match)))
	(chibi (import (scheme base) (scheme r5rs) (srfi 1) (srfi 130) (chibi) (chibi string)))
	(chicken (import srfi-1 srfi-13 matchable format (chicken process-context)))
)

(cond-expand
	(guile 
		(define (format-local str . args) (apply format #f str args)))
	((or chibi chicken)
		(define (format-local str . args) (apply format str args)))
)

(cond-expand
	((or guile chicken)
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
	((or guile chibi)
		(define (arguments) (cdr (command-line))))
	(chicken
		(define (arguments) (command-line-arguments))))

(include "html.ss")

; vim: sw=2:ts=2:sts=2:ft=scheme
