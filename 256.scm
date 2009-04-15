(require "./lib")
#|
S -> L = R | R
L -> *R | id
R -> L
|#

#;(define G
  (list (production S (prod-or '(L #\= R) '(R)))
		(production L (prod-or '(#\* R) '(id)))
		(production R '(L))
		))

#;(define-macro (define-grammar g . productions)
  (begin
	(for-each (lambda (prod)
				`(define ,(car prod) 0)
				)
			  productions)
	`(define => eq)
	`(define ,g 'grammar-symbol)
	(for-each (lambda (prod)
				#?=prod
			  `(format #t "~a => ~a\n" ,(car prod) ,(cddr prod))
			  )
			  productions)
	 ;;(format #t "G=~d, productions=~a\n" ,g ,productions)
	#;(print "*")
	))

#;(define-grammar G
  (S => L #\= R / R)
  (L => #\* R / id)
  (R => L))
;;(test-start "256")

#| (grammar (S L R)
            (S L = R) (S R)
            (L * R) (L id)
            (R L) ) |#

(define G_4_1
  (grammar (production 'E (list 'E #\+ 'T) (list 'T))
		   (production 'T (list 'T #\* 'F) (list 'F))
		   (production 'F (list #\( 'E #\)) (list 'id))
		   ))
(grammar-print G_4_1)

(define G_4_2
  (grammar (production 'E (list 'T 'E_))
		   (production 'E_ (list #\+ 'T 'E_) (list epsilon))
		   (production 'T (list 'F 'T_))
		   (production 'T_ (list #\* 'F 'T_) (list epsilon))
		   (production 'F (list #\( 'E #\)) (list 'id))
		   ))
(grammar-print G_4_2)

(define G_4_3
  (grammar (production 'E (list 'E #\+ 'E) (list 'E #\* 'E) (list #\( 'E #\)) (list 'id))
		   ))
(grammar-print G_4_3)

(define G_fig_4_2
  (grammar (production 'expression (list 'expression #\+ 'term))
		   (production 'expression (list 'expression #\- 'term))
		   (production 'expression (list 'term))
		   (production 'term (list 'term #\* 'factor))
		   (production 'term (list 'term #\/ 'factor))
		   (production 'term (list 'factor))
		   (production 'factor (list #\( 'expression #\)))
		   (production 'factor (list 'id))
		   ))
(grammar-print G_fig_4_2)


