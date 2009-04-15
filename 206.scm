
(define (parse-single-production p)
  (let1 left (car p)
	(if (memq (cadr p) '(=> →))
		(let loop ([right (cddr p)] [ph '()] [phs '()])
		  (if (null? right)
			  (if (null? ph) phs (cons left (reverse! (cons (reverse! ph) phs))))
			  (let1 c (car right)
				(cond [(eq? c '/)
					   (loop (cdr right) '() (cons (reverse! ph) phs))]
					  [(char? c)
					   (loop (cdr right) (cons (cons 'T c) ph) phs)]
					  [(string? c)
					   (loop (cdr right) (cons (cons 'T c) ph) phs)]
					  [(symbol? c)
					   (loop (cdr right) (cons (cons 'nT c) ph) phs)]
					  [else
					   (loop (cdr right) (cons (cons '? c) ph) phs)] ))))
		'error)))

(define (parse-productions g)
  (map parse-single-production g))

#;(define (f0 g) (print (parse-single-production g)))
(define (test g)
  (let1 parsed (parse-productions g)
	(for-each (lambda (p)
				(for-each (lambda (r)
							(format #t "~a →  ~a\n" (car p) r)
							)
						  (cdr p))
				)
			  parsed)))

(test '((S => S S #\+ / S S #\* / #\a)))
(test '((S → #\0 S #\1 / #\0 #\1)))
(test '((S → #\+ S S / #\* S S / #\a)))
(test '((S → S #\( S #\) S)))

#;(test '(bexpr → bexpr "or" bterm / bterm))
#;(test '(bterm → bterm "and" bfactor / bfactor))
#;(test '(bfactor → "not" bfactor / #\( bexpr #\) / "true" / "false"))
(test '((bexpr → bexpr "or" bterm / bterm)
	  (bterm → bterm "and" bfactor / bfactor)
	  (bfactor → "not" bfactor / #\( bexpr #\) / "true" / "false")
	  ))


#;(print g)
#;(print (memq '=> g))
#;(print (cdr (memq '=> g)))
