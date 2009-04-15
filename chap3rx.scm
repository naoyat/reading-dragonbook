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
(define (make-DFA arcs accepting-states)
  (list 'dfa arcs accepting-states))
(define DFA? (tagged-list?-proc 'dfa))
(define (DFA-arcs dfa) (second dfa))
(define (DFA-accepting-states dfa) (third dfa))

(define (make-NFA arcs accepting-states) (list 'nfa arcs accepting-states))
(define NFA? (tagged-list?-proc 'nfa))
(define (NFA-arcs nfa) (second nfa))
(define (NFA-accepting-states nfa) (third nfa))

(define-macro (with-NFA nfa proc-name . body)
  `(if (NFA? ,nfa)
	   (let* ([arcs (NFA-arcs nfa)]
			  [F (NFA-accepting-states nfa)]
			  [alphabets (sort (list->lset (remove empty? (map second arcs))))]
			  [find-dest-ht (make-hash-table 'equal?)]
			  [move-ht (make-hash-table 'equal?)]
			  [e-closure-ht (make-hash-table 'equal?)]
			  )

		 (define (find-dest s c) ; s--(c)-->x なステートxを列挙
		   #;(format #t "(find-dest ~a ~a ; ~a)\n" s c val)
		   (or (hash-table-get find-dest-ht (cons s c) #f)
			   (let1 val (if (null? s) '()
							 (if (pair? s)
								 (sort (list->lset (append-map (cut find-dest <> c) s)))
								 (let1 s->x (collect-cdr-assq s arcs)
								   (if (null? s->x) '()
									   (let1 xs (map cadr (collect-assq c s->x))
										 (if (null? xs) '()
											 xs))))))
				 (hash-table-put! find-dest-ht (cons s c) val)
				 val)))

		 (define (move s c)
		   ;(format #t "(move ~a ~a)\n" s c)
		   (or (hash-table-get move-ht (cons s c) #f)
			   (let1 val (if s (let1 dest (find-dest s c)
								 (if (null? dest) #f dest))
							 #f)
				 (hash-table-put! move-ht (cons s c) val)
				 val)))

		 (define (e-closure s)
		   ;(format #t "(e-closure ~a)\n" s)
		   (or (hash-table-get e-closure-ht s #f)
			   (begin
				 (hash-table-put! e-closure-ht s '())
				 (let1 val (if s (let1 ds (find-dest s :empty)
								   (sort (list->lset (append (if (pair? s) s (list s)) ds (append-map e-closure ds)))))
							   '())
				   (hash-table-put! e-closure-ht s val)
				   val))))

		(let1 S (e-closure 0)
		  ,@body
		  ))
	   (begin
		 (format #t "usage: (~a nfa)\n" ,proc-name)
		 *undefined*)))

;; Algorithm 3.20: The subset construction of a DFA from an NFA.
(define (NFA->DFA nfa)
  (with-NFA nfa "NFA->DFA"
			;;(format #t "The start state of D is ~a.\n" S)
			;;(format #t "characters used in NFA: ~a\n" alphabets)
			(let ([Dtran (make-hash-table 'equal?)]
				  [Dstates (make-hash-table 'equal?)]
				  [Dstates-r (make-hash-table 'eq?)]
				  [DF '()]
				  [unmarkeds '(0)]
				  [state-id 0]
				  )
			  (define (add-U-as-an-unmarked-state-if-not-found U)
				(or (hash-table-get Dstates U #f)
					(let1 new-state-id (inc! state-id)
					  (hash-table-put! Dstates U new-state-id)
					  (hash-table-put! Dstates-r new-state-id U) ;;逆引き
					  (push! unmarkeds new-state-id)
					  (unless (null? (lset-intersection = F U)) (push! DF new-state-id))
					  new-state-id)))
			  #;(define (mark the-state-id)
				(set! unmarkeds (remove (cut = the-state-id <>) unmarkeds)))

			  (hash-table-put! Dstates S 0)
			  (hash-table-put! Dstates-r 0 S)
			  (unless (null? (lset-intersection = F S)) (push! DF 0))

			  (let loop ();(Dstates `((,S . #f))));(unmarked '(0)))
				#;(format #t "(loop ~a)\n" Dstates)
				;(let1 unmarkeds (filter-map (lambda (D) (if (cdr D) #f (car D))) Dstates)
				(if (null? unmarkeds)
					(let1 arcs (sort (hash-table-map Dtran (lambda (k v) (list (car k) (cdr k) v))) list<)
					  ;;(format #t "Dtran[~a,~a] => ~a\n" (car k) (cdr k) v)))
					  (values (make-DFA arcs (sort DF))
							  (sort (hash-table-map Dstates-r (lambda (k v) (list k v))) list<)
							  ))
					(let* ([T-id (pop! unmarkeds)]
						   [T (hash-table-get Dstates-r T-id #f)])
					  ;(format #t "T: ~a ~a\n" T-id T)
					  ;(let1 Tp (assoc T Dstates) (set-cdr! Tp #t))
					  (for-each (lambda (a)
								  (let* ([U (e-closure (move T a))]
										 [U-id (add-U-as-an-unmarked-state-if-not-found U)])
									#;(when (null? (collect-assoc U Dstates))
									  (push! Dstates (cons U #f)) )
									(hash-table-put! Dtran (cons T-id a) U-id)))
								alphabets)
					  (loop)))))))

;; Algorithm 3.22: Simulating an NFA
;; 3.7.3の効率化は（まだ）施していない
(define (NFA-simulator nfa)
  (with-NFA nfa "NFA-simulator"
			(lambda ()
			  (let loop ((S S) (c (read-char)))
				(if (eof-object? c)
					(not (null? (lset-intersection = S F)))
					(loop (e-closure (move S c)) (read-char))))
			  )))

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
