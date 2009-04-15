(use srfi-1)

(define (empty? sym) (and (memq sym (list :empty 'ϵ)) #t))
(define (terminal? sym)
  (cond [(char? sym) #t]
		[(number? sym) #t]
		[(string? sym) #t]
		[(empty? sym) #t]
		[(symbol? sym)
		 (let1 chs (string->list (symbol->string sym))
		   (if (eq? #\< (car chs)) #t
			   #f))]
		[else #f]))
(define (nonterminal? sym) (not (terminal? sym)))

;;
(define fig_3_11 '((digit -> #[0-9])
				   (digits -> digit+)
				   (number -> digits (#\. digits)? (E #[+-] ? digits)? )
				   (letter -> #[A-Za-z])
				   (id -> letter (letter / digit)* )
				   (if -> "if")
				   (then -> "then")
				   (else -> "else")
				   (relop -> "<" / ">" / "<=" / ">=" / "=" / "<>")))

;(define fig_3_34 '((a / b)* a b b))

;; McNaughton-Yamada-Thompson algorithm
(define (arc i sym f) (list 'arc i sym f))
(define (empty-arc i f) (arc i :empty f))
(define (arc-start a) (second a))
(define (arc-symbol a) (third a))
(define (arc-end a) (fourth a))

(define (join-arcs . as)
  (let loop ((rest as) (joined '()))
	(cond [(null? rest) (reverse! joined)]
		  [(null? (car rest)) (loop (cdr rest) joined)]
		  [(eq? 'arc (caar rest))
		   (loop (cdr rest) (cons (car rest) joined))]
		  [else
		   (loop (cdr rest) (append (apply join-arcs (car rest)) joined))]
		  )))

(define (regex->nfa-orig r); sigma)
  (let1 state-id -1
	(define (new-state) (inc! state-id))

	(define (union? exp) (and (pair? exp) (eq? 'or (car exp))))
	(define (concatenation? exp) (pair? exp))
	(define (zero-or-more? exp) (and (pair? exp) (eq? '* (car exp))))
	(define (zero-or-one? exp) (and (pair? exp) (eq? '? (car exp))))
	(define (one-or-more? exp) (and (pair? exp) (eq? '+ (car exp))))

	(define (N r . args)
	  (let-keywords* args ((i (new-state))
						   (f (new-state)))
		(cond [(union? r)
			   (let1 arcs (map N (cdr r))
				 (join-arcs arcs
							(map (cut empty-arc i <>) (map arc-start arcs))
							(map (cut empty-arc <> f) (map arc-end arcs))
							))]
			  [(zero-or-more? r)
			   (let ([ni (new-state)] [nf (new-state)])
				 (join-arcs (empty-arc i ni)
							(append-map (cut N <> :i ni :f nf) (cdr r))
							(empty-arc nf f)
							(empty-arc nf ni) ;; zero-or-oneでは削る
							(empty-arc i f) ;; one-or-moreでは削る
							))]
			  [(one-or-more? r)
			   (let ([ni (new-state)] [nf (new-state)])
				 (join-arcs (empty-arc i ni)
							(append-map (cut N <> :i ni :f nf) (cdr r))
							(empty-arc nf f)
							(empty-arc nf ni)
							))]
			  [(zero-or-one? r)
			   (let ([ni (new-state)] [nf (new-state)])
				 (join-arcs (empty-arc i ni)
							(append-map (cut N <> :i ni :f nf) (cdr r))
							(empty-arc nf f)
							(empty-arc i f)
							))]
			  [(concatenation? r)
			   #;(format #t "(concat ~a)\n" r)
			   (let loop ((rest r) (arcs '()) (last-i i))
				 (if (null? (cdr rest))
					 (let1 a (N (car rest) :i last-i :f f)
					   (apply join-arcs (reverse! (cons a arcs))))
					 (let1 nf (new-state)
					   (let1 a (N (car rest) :i last-i :f nf)
						 (loop (cdr rest) (cons a arcs) nf)) )))]
			  [(empty? r)
			   ;;(format #t "(empty ~a)\n" r)
			   (empty-arc i f)]
			  [(terminal? r)
			   ;;(format #t "(terminal ~a)\n" r)
			   (arc i r f)]
			  [else '()] ;(format #t "(??? ~a)\n" r)
			  ))); N
	(N r)
	))

;; McNaughton-Yamada-Thompson algorithm
(define (regex->nfa r); sigma)
	;(define (new-state) (inc! state-id))

  (define (union? exp) (and (pair? exp) (eq? 'or (car exp))))
  (define (concatenation? exp) (pair? exp))
  (define (zero-or-more? exp) (and (pair? exp) (eq? '* (car exp))))
  (define (zero-or-one? exp) (and (pair? exp) (eq? '? (car exp))))
  (define (one-or-more? exp) (and (pair? exp) (eq? '+ (car exp))))

  #;(define (join-arcs . as)
	(let loop ((rest as) (joined '()))
	  (if (null? rest) joined
		  (let1 r (car rest)
			(if (eq? 'arc (car r))
				(loop (cdr rest) (append joined (list r)))
				(loop (cdr rest) (append joined (apply join-arcs r)))
				)))))
  
  (define (N r i)
	(cond [(union? r)
		   (let loop ((rest (cdr r)) (next-i (+ i 1)) (arcs '()))
			 (if (null? rest)
				 (let ([as (reverse! arcs)]
					   [f next-i])
				   (values (join-arcs (map (cut empty-arc i <>) (map arc-start as))
									  as
									  (map (cut empty-arc <> f) (map arc-end as)))
						   f))
				 (receive (as f) (N (car rest) next-i)
				   (loop (cdr rest) (+ f 1) (cons as arcs))
				   )))]
		  [(zero-or-more? r)
		   (let1 ni (+ i 1)
			 (receive (as nf) (N (cdr r) ni)
			   (let1 f (+ nf 1)
				 (values (join-arcs (empty-arc i ni)
									(empty-arc i f) ;; one-or-moreでは削る
									as
									(empty-arc nf ni) ;; zero-or-oneでは削る
									(empty-arc nf f))
						 f))))]
		  [(one-or-more? r)
		   (let1 ni (+ i 1)
			 (receive (as nf) (N (cdr r) ni)
			   (let1 f (+ nf 1)
				 (values (join-arcs (empty-arc i ni)
									#;(empty-arc i f) ;; one-or-moreでは削る
									as
									(empty-arc nf ni) ;; zero-or-oneでは削る
									(empty-arc nf f))
						 f))))]
		  [(zero-or-one? r)
		   (let1 ni (+ i 1)
			 (receive (as nf) (N (cdr r) ni)
			   (let1 f (+ nf 1)
				 (values (join-arcs (empty-arc i ni)
									(empty-arc i f) ;; one-or-moreでは削る
									as
									#;(empty-arc nf ni) ;; zero-or-oneでは削る
									(empty-arc nf f))
						 f))))]
		  [(concatenation? r)
		   #;(format #t "(concat ~a)\n" r)
		   (let loop ((rest r) (next-i i) (arcs '()))
			 (if (null? rest)
				 (values arcs next-i)
				 (receive (as f) (N (car rest) next-i)
				   (loop (cdr rest) f (join-arcs arcs as)))))]
		  [(empty? r)
		   (let1 f (+ i 1)
			 (values (empty-arc i f) f))]
		  [(terminal? r)
		   (let1 f (+ i 1)
			 (values (arc i r f) f))]
		  ;;[else '()] ;(format #t "(??? ~a)\n" r)
		  ));define N
  (N r 0)
  )

;; Algorithm 3.20: The subset construction of a DFA from an NFA
(define (nfa->dfa nfa)
  (let1 n (map cdr nfa)
	(define (move T a)
	  ;; NFA-states-set (s in T) --(a)--> [#]
	  )
	(define (e-closure s)
	  )
	(print n)
	'()))

(define ababb '((* (or #\a #\b)) #\a #\b #\b))

(define-macro (regex-to-nfa-test regex)
  `(receive (arcs f) (regex->nfa ,regex)
	 (format #t "~d --> ~d:\n" 0 f)
	 (map print (map cdr arcs))))
  
#;(receive (arcs f) (regex->nfa ababb)
  #;(format #t "~d --> ~d:\n" 0 f)
  #;(map print (map cdr arcs))
  (map print (nfa->dfa arcs))
  )

(regex-to-nfa-test ababb)
(regex-to-nfa-test '(or (#\a (* #\a)) (#\b (* #\b))))