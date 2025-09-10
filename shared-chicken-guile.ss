
(define (string-replace-all str from to)
  (let ((from-len (string-length from)))
    (let loop ((i 0) (parts '()))
      (let ((pos (string-contains str from i)))
        (if pos
            (loop (+ pos from-len)
                  (cons to (cons (substring str i pos) parts)))
            (apply string-append 
                   (reverse (cons (substring str i) parts))))))))

