(use srfi-1)
(use srfi-14) ;char-set

(require "./util")
(require "./debug")

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
(define (make-DFA arcs accepting-states) (list 'dfa arcs accepting-states))
(define DFA? (tagged-list?-proc 'dfa))
(define (DFA-arcs dfa) (second dfa))
(define (DFA-accepting-states dfa) (third dfa))

(define (make-NFA arcs accepting-states) (list 'nfa arcs accepting-states))
(define NFA? (tagged-list?-proc 'nfa))
(define (NFA-arcs nfa) (second nfa))
(define (NFA-accepting-states nfa) (third nfa))

;; Algorithm 3.20: The subset construction of a DFA from an NFA.
(define-macro (with-NFA nfa proc-name . body)
  `(if (NFA? ,nfa)
	   (let ([arcs (NFA-arcs nfa)]
			 [F (NFA-accepting-states nfa)])
		 (define (find-dest s c) ; s--(c)-->x なステートxを列挙
		   (if (null? s) '()
			   (if (pair? s)
				   (list->lset (append-map (cut find-dest <> c) s))
				   (let1 s->x (collect-cdr-assq s arcs)
					 (if (null? s->x) '()
						 (let1 xs (map cadr (collect-assq c s->x))
						   (if (null? xs) '()
							   xs)))))))
		 (define (move s c)
		   (if s (let1 dest (find-dest s c)
				   (if (null? dest) #f dest))
			   #f))
		 (define (e-closure s)
		   (if s (let1 ds (find-dest s :empty)
				   (list->lset (append (if (pair? s) s (list s)) ds (append-map e-closure ds))))
			   '()))

		(let1 S (e-closure 0)
		  (lambda ()
			,@body
			)))
	   (begin
		 (format #t "usage: (~a nfa)\n" ,proc-name)
		 *undefined*)))

(define (NFA->DFA nfa)
  (with-NFA nfa "NFA->DFA"
			(format #t "The start state of D is ~a.\n" S)
			'(dfa)
			))

(define (NFA-simulator nfa)
  (with-NFA nfa "NFA-simulator"
			(let loop ((S S) (c (read-char)))
			  (if (eof-object? c)
				  (not (null? (lset-intersection = S F)))
				  (loop (e-closure (move S c)) (read-char))))
			))

;; Algorithm 3.22: Simulating an NFA
#|
(define (NFA-simulator nfa)
  (if (NFA? nfa)
	  (let ([arcs (NFA-arcs nfa)]
			[F (NFA-accepting-states nfa)])
		
		(define (find-dest s c) ; s--(c)-->x なステートxを列挙
		  (if (null? s) '()
			  (if (pair? s)
				  (list->lset (append-map (cut find-dest <> c) s))
				  (let1 s->x (collect-cdr-assq s arcs)
					(if (null? s->x) '()
						(let1 xs (map cadr (collect-assq c s->x))
						  (if (null? xs) '()
							  xs)))))))
		(define (move s c)
		  (if s (let1 dest (find-dest s c)
				  (if (null? dest) #f dest))
			  #f))
		(define (e-closure s)
		  (if s (let1 ds (find-dest s :empty)
				  (list->lset (append (if (pair? s) s (list s)) ds (append-map e-closure ds))))
			  '()))

		(let1 S (e-closure 0)
		  (lambda ()
			(let loop ((S S) (c (read-char)))
			  (if (eof-object? c)
				  (not (null? (lset-intersection = S F)))
				  (loop (e-closure (move S c)) (read-char))))
			)))
	  (begin
		(format #t "usage: (NFA-simulator nfa)\n")
		*undefined*)))
|#
;; Algorithm 3.23: The McNaughton-Yamada-Thompson algorithm to convert a regular expression to an NFA.
(define (arc i sym f) (list 'arc i sym f))
(define arc? (tagged-list?-proc 'arc))
(define (arc-start a) (second a))
(define (arc-symbol a) (third a))
(define (arc-end a) (fourth a))
(define (empty-arc i f) (arc i :empty f))

(define (join-arcs . as)
  (let loop ((rest as) (joined '()))
	(cond [(null? rest) joined]
		  [(null? (car rest)) (loop (cdr rest) joined)]
		  [(arc? (car rest))
		   (loop (cdr rest) (append joined (list (car rest))))];(cons (car rest) joined))]
		  [else
		   (loop (cdr rest) (append joined (apply join-arcs (car rest))))]
		  )))

(define (regex-AST->NFA r)
  (define (concatenation? exp) (pair? exp))
  (define (union? exp) (and (pair? exp) (eq? '/ (car exp))))
  (define (zero-or-more? exp) (and (pair? exp) (eq? '* (car exp))))
  (define (zero-or-one? exp) (and (pair? exp) (eq? '? (car exp))))
  (define (one-or-more? exp) (and (pair? exp) (eq? '+ (car exp))))

  (define (N r i)
	;(display "(N ") (write r) (format #t " ~d)...\n" i)
	(cond [(string? r)
		   (N (string->list r) i)]
		  [(union? r) ;; or
		   #;(format #t "{union: ~a}\n" (cdr r))
		   (let loop ((rest (cdr r)) (next-i (+ i 1)) (arcs '()))
			 (if (null? rest)
				 (let1 f next-i
				   (values (append-map (lambda (ifas)
											(let ([ni (car ifas)]
												  [nf (cadr ifas)]
												  [as (cddr ifas)])
											  (join-arcs (empty-arc i ni)
														 as
														 (empty-arc nf f))))
										  (reverse! arcs))
							  f))
				 (receive (as nf) (N (car rest) next-i)
				   ;(loop (cdr rest) (+ f 1) (join-arcs arcs as))
				   (loop (cdr rest) (+ nf 1) (cons (list* next-i nf as) arcs))
				   )))]
		  [(zero-or-more? r)
		   #;(format #t "{0+: ~a}\n" (cadr r))
		   (let1 ni (+ i 1)
			 (receive (as nf) (N (cadr r) ni)
			   (let1 f (+ nf 1)
				 (values (join-arcs (empty-arc i ni)
									(empty-arc i f) ;; one-or-moreでは削る
									as
									(empty-arc nf ni) ;; zero-or-oneでは削る
									(empty-arc nf f))
						 f))))]
		  [(one-or-more? r)
		   #;(format #t "{1+: ~a}\n" (cadr r))
		   (let1 ni (+ i 1)
			 (receive (as nf) (N (cadr r) ni)
			   (let1 f (+ nf 1)
				 (values (join-arcs (empty-arc i ni)
									#;(empty-arc i f) ;; one-or-moreでは削る
									as
									(empty-arc nf ni) ;; zero-or-oneでは削る
									(empty-arc nf f))
						 f))))]
		  [(zero-or-one? r)
		   #;(format #t "{0|1: ~a}\n" (cadr r))
		   (let1 ni (+ i 1)
			 (receive (as nf) (N (cadr r) ni)
			   (let1 f (+ nf 1)
				 (values (join-arcs (empty-arc i ni)
									(empty-arc i f) ;; one-or-moreでは削る
									as
									#;(empty-arc nf ni) ;; zero-or-oneでは削る
									(empty-arc nf f))
						 f))))]
		  [(concatenation? r)
		   #;(format #t "{concat: ~a}\n" r)
		   (let loop ((rest r) (next-i i) (arcs '()))
			 (if (null? rest)
				 (values arcs next-i)
				 (receive (as f) (N (car rest) next-i)
				   (loop (cdr rest) f (join-arcs arcs as)))))]
		  [(empty? r)
		   (let1 f (+ i 1)
			 (values (list (empty-arc i f)) f))]
		  [(terminal? r)
		   (let1 f (+ i 1)
			 (values (list (arc i r f)) f))]
		  [(symbol? r) ;unknown symbol
		   #;(format #t "{sym: ~a}\n" r)
		   (N (symbol->string r) i)]
		  ;;[else '()] ;(format #t "(??? ~a)\n" r)
		  [(char-set? r)
		   ;;(N (cons '/ (char-set->list r)) i)]
		   (N (cons '/ (reverse! (char-set->list r))) i)] ;; char-set->list #[ab] -> (#\b #\a) なので
		  [else (values '() i)] ; ignored
		  ));define N
  (receive (arcs f) (N r 0)
	(make-NFA (map cdr arcs) (list f))))

;; Algorithm 3.20: The subset construction of a DFA from an NFA
(define (NFA->DFA nfa)
  (define (move T a)
	;; NFA-states-set (s in T) --(a)--> [#]
	)
  (define (e-closure s)
	)
  (print nfa)
  '())

#|
#;(receive (arcs f) (regex-AST->nfa ababb)
  #;(format #t "~d --> ~d:\n" 0 f)
  #;(map print (map cdr arcs))
  (map print (nfa->dfa arcs))
  )
|#
