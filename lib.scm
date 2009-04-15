(use srfi-1)

;; uniq
(define (uniq ls)
  (let loop ((ls ls) (last #f) (rs '()))
	(if (null? ls) (reverse! rs)
		(let1 e (car ls)
		  (loop (cdr ls) e (if (eq? e last) rs (cons e rs)))
		  ))))


(define epsilon '())
(define ε epsilon)

;;§3.3.4 Regular Definitions
(define (rd-union . exprs)
  `(:// ,@exprs))
(define (rd-concatenation . exprs) exprs)
(define rd-epsilon :epsilon)
(define (rd-zero-or-more expr)
  `(:** ,expr))
(define (rd-one-or-more expr)
  `(:++ ,expr))
(define (rd-zero-or-one expr)
  `(:?? ,expr))

(define // rd-union)
(define ~~ rd-concatenation)
(define ** rd-zero-or-more)
(define ++ rd-one-or-more)
(define ?? rd-zero-or-one)

(define (rd-symbols rd) (cdadr rd))
(define (rd-alphabets rd) (cdaddr rd))
(define (rd-definitions rd) (cdddr rd))
(define (rd-print rd)
  (define (rd-body->string body)
	(string-join (map x->string body) " "))
  (for-each (lambda (nont)
			  (format #t "~d →  " nont)
			  (print (string-join (map (compose rd-body->string cdr)
									   (filter (lambda (p) (eq? (car p) nont))
											   (rd-definitions rd)))
								  " | "))
			  ;(newline)
			  )
			(rd-symbols rd))
  (newline))

(define (make-regular-definition symbols alphabets definitions)
  `(regular-definition (:symbols ,@symbols) (:alphabets ,@alphabets) ,@definitions))
(define (regular-definition . definitions) ; grammar
  (let* ([defs (apply append definitions)]
		 [symbols (uniq (map car defs))]
		 [alphabets (uniq (sort (lset-difference! eq? (append-map cdr defs) symbols) eq?))]
		 )
	(make-regular-definition symbols alphabets defs)
	))

(define (make-definition symbol definition) (cons symbol definition))
(define (rd symbol . definition)
  (if (char-set? (car definition))
	  (list (make-definition symbol definition))
	  (map (cut make-definition symbol <>) definition)
	  ))

(define (patterns-for-tokens . pats)
  (begin0 pats (newline))
  )
(define (pattern name . definition)
  (format #t ">> ~d →  ~a\n" name definition)
  )

;;
;; grammar
;;
(define (make-grammar nonterminals terminals productions)
  `(grammar (:nonterminals ,@nonterminals) (:terminals ,@terminals) ,@productions) )
(define (grammar . productions)
  (let* ([ps (apply append productions)]
		 [nonterminals (uniq (map car ps))]
		 [terminals (uniq (sort (lset-difference! eq? (append-map cdr ps) nonterminals) eq?))]
		 )
	(make-grammar nonterminals terminals ps) ))

(define (make-production head body) (cons head body))
(define (production head . body)
  (map (cut make-production head <>) body))

(define (grammar-nonterminals grammar) (cdadr grammar))
(define (grammar-start-nonterminal grammar) (car (grammar-nonterminals grammar)))
(define (grammar-terminals grammar) (cdaddr grammar))
(define (grammar-productions grammar) (cdddr grammar))
(define (grammar-print grammar)
  (define (product-body->string body)
	(string-join (map x->string body) " "))
  (for-each (lambda (nont)
			  (format #t "~d →  " nont)
			  (print (string-join (map (compose product-body->string cdr)
									   (filter (lambda (p) (eq? (car p) nont))
											   (grammar-productions grammar)))
								  " | "))
			  )
			(grammar-nonterminals grammar))
  (newline))

(define (make-augmented-grammar nonterminals terminals productions)
  `(augmented-grammar (:nonterminals ,@nonterminals) (:terminals ,@terminals) ,@productions) )

(define (augment-grammar grammar)
  (make-augmented-grammar (cons 'S+ (grammar-nonterminals grammar))
						  (grammar-terminals grammar)
						  (cons (make-production 'S+ (list (grammar-start-nonterminal grammar)))
								(grammar-productions grammar)) ))
