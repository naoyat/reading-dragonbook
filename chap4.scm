(require "./util")

;;
;; 生成規則 production の表現。ひとつの head (=nonterminal) に body が１つ
;;
(define production? (tagged-list?-proc 'production))
(define (make-production head body)
  (list* 'production head body))
(define (production-head prod) (cadr prod)) ;; left side
(define (production-body prod) (cddr prod)) ;; right side

(define (parse-production-desc pdesc)
  (let ([head (car pdesc)]
		[body (cddr pdesc)])
	(make-production head body)))

;;
;; 生成規則 production をグループ化。ひとつの head に body が複数（※0個以上）
;;
(define grouped-production? (tagged-list?-proc 'grouped-production))
(define (make-grouped-production head bodies)
  (list* 'grouped-production head bodies))

(define (grouped-production-head prod) (cadr prod)) ;; left side
(define (grouped-production-bodies prod) (cddr prod)) ;; right side
(define (grouped-production-production prod)
  (map (cut make-production (grouped-production-head prod) <>)
	   (grouped-production-bodies prod)))

(define (parse-grouped-production-desc gpdesc)
  (let ([head (car gpdesc)]
		[bodies (split-list-by-symbol (cddr gpdesc) '/)])
	(make-grouped-production head bodies)))

;;
;; 文法の表現
;;
(define (make-grammar nonterminals terminals start-symbol productions)
  (list 'grammar nonterminals terminals start-symbol productions))
(define grammar? (tagged-list?-proc 'grammar))
(define (grammar-nonterminals g) (second g))
(define (grammar-terminals g) (third g))
(define (grammar-start-symbol g) (fourth g))
(define (grammar-productions g . args)
  (let-keywords args ((separate #f))
				(if separate
					(append-map grouped-production-production (fifth g))
					(fifth g))))

(define (parse-grammar-desc gdesc)
  (let ([nonterminals-r '()]
		[terminals-r '()]
		[atomic-productions-r '()])
	(let loop ((pdesc-lines gdesc))
	  (if (null? pdesc-lines)
		  (let* ([nonterminals (reverse! nonterminals-r)]
				 [terminals (reverse! terminals-r)]
				 [atomic-productions (reverse! atomic-productions-r)])
			(make-grammar nonterminals
						  terminals
						  (car nonterminals) ; start-symbol
						  (map (lambda (head)
								 (make-grouped-production head (collect-cdr-assq head atomic-productions)))
							   nonterminals)))
		  (let* ([gprod (parse-grouped-production-desc (car pdesc-lines))]
				 [head (grouped-production-head gprod)]
				 [bodies (grouped-production-bodies gprod)])
			(unless (memq head nonterminals-r) (push! nonterminals-r head))
			(for-each (lambda (body) (push! atomic-productions-r (cons head body)))
					  bodies)
			(loop (cdr pdesc-lines)))))))

(define (describe-grammatical-object obj . args)
  (let-optionals* args ((trans identity))
	(with-output-to-string
	  (lambda ()
		(cond [(production? obj)
			   (format #t "~a →  " (trans (production-head obj)))
			   (map display (intersperse #\Space (production-body obj)))]
			  [(grouped-production? obj)
			   (format #t "~a →  " (trans (grouped-production-head obj)))
			   (let loop ((bodies (grouped-production-bodies obj)))
				 (unless (null? bodies)
				   (map display (intersperse #\Space (map trans (car bodies))))
				   (unless (null? (cdr bodies)) (format #t " | "))
				   (loop (cdr bodies)) ))]
			  [(grammar? obj)
			   #|
			   (print "nonterminals: " nonterminals-r)
			   (print "terminals: " terminals-r)
			   (print "start-symbol: " start-symbol)
			   (print "prods: " productions)
			   |#
			   (map print (map describe-grammatical-object
							   (grammar-productions obj)))
			   ]
			  [else (print (car obj))]
			  )))))

(define (trans1 elem)
  (cond [(empty? elem) 'ϵ]
		[(terminal? elem) (list 'term elem)]
		[(nonterminal? elem) (list 'nonterm elem)]
		[else (list '? elem)]))

#;(define (dump gdesc)
  (let1 g (parse-grammar-desc gdesc)
	(map print (map (cut describe-grammatical-object <> trans1)
					(grammar-productions g)))
	(newline)))

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

#;(define (leftmost-derivation gram str) )

(define (left-recursion? prod)
  (let1 head (production-head prod)
	(if (grouped-production? prod)
		(and (memq head (map car (grouped-production-bodies prod))) #t)
		(eq? head (car (production-body prod))))))

;;util
(define (check-left-recursion gdesc)
  (let1 g (parse-grammar-desc gdesc)
	(define (check prod)
	  (if (left-recursion? prod)
		  (begin
			(format #t "~a : LEFT_RECURSION\n" (describe-grammatical-object prod))
			(let1 eliminated (eliminate-immediate-left-recursion prod)
			  (for-each (lambda (prod)
						  (format #t " => ~a\n" (describe-grammatical-object prod)))
						eliminated)
			  ))
		  (format #t "~a\n" (describe-grammatical-object prod))
		  ))
	(for-each check (grammar-productions g))
	(newline)
	))

;;
;; prod の左再帰を解消し、grouped-productionのリストで返す
;;
(define (enqueue lis elm)
  (if (empty? (car lis)) (list elm)
	  (append lis (list elm))))

(define (eliminate-immediate-left-recursion prod)
  (if (left-recursion? prod)
	  (let* ([head (production-head prod)]
			 [head# (symbol-append head '|#|)])
		(if (grouped-production? prod)
			(let* ([bodies (grouped-production-bodies prod)]
				   [alpha (collect-cdr-assq head bodies)]
				   [beta (!collect-assq head bodies)])
			  (list (make-grouped-production head (map (cut enqueue <> head#) beta))
					(make-grouped-production head# (append (map (cut enqueue <> head#) alpha)
													'((:empty)))))
			  )
			(let1 body (production-body prod)
			  (list
			   (make-grouped-production head `((,@(cdr body) ,(car body))
											   (:empty)))))
			))
	  prod))

(define (replace-production prod gamma delta)
  #;(format #t "(replace-production prod:~a gamma:~a delta:~a\n" prod gamma delta)
  (let loop ([bodies (grouped-production-bodies prod)]
			 [new-bodies '()])
	#;(format #t " // RP-loop ~a || ~a\n" bodies new-bodies)
	(if (null? bodies)
		(make-grouped-production (grouped-production-head prod)
								 (reverse! new-bodies))
		(loop (cdr bodies)
			  (let1 body (car bodies)
				(if (member body gamma)
					(append (map (cut append <> (cdr body)) delta)
							new-bodies)
					(cons body new-bodies)))))))

;;
;; Algorithm 4.19: Eliminating left recursion.")
;;
(define (merge-productions prods prods#)
  ;; productionのマージ。
  ;; ((E ..) (T ..) (F ..)) + ((E# ..) (T# ..)) --> ((E ..) (E# ..) (T ..) (T# ..) (F ..))
  ;; 並び順が同じであることを仮定している
  (format #t "(mp ~a ~a)\N" prods prods#)
  (let loop ((ps prods) (ps# prods#) (merged '()))
	(if (null? ps)
		(append (reverse! merged) ps#)
		(let* ([p (car ps)]
			   [psym (production-head p)]
			   [psym# (symbol-append psym '|#|)])
		  (if (and (not (null? ps#)) (eq? psym# (production-head (car ps#))))
			  (loop (cdr ps) (cdr ps#) (list* (car ps#) (car ps) merged))
			  (loop (cdr ps) ps# (cons (car ps) merged)))))))

(define (algorithm_4_19 g)
  (let* ([prods (grammar-productions g)]
		 ;[nonterminals (list->vector (map production-head prods))]
		 [v (list->vector prods)]
		 [v# '()]
		 [n (vector-length v)])
	(let iloop ((i 0))
	  (when (< i n)
		(let* ([Pi (vector-ref v i)]
			   [Ai (grouped-production-head Pi)]
			   [Bi (grouped-production-bodies Pi)])
		  ;;(format #t "\n  i=~d(~a) : ~a\n" i Ai (describe-grammatical-object Pi))
		  (let jloop ((j 0)) ; j<i
			(when (< j i)
			  (let* ([Pj (vector-ref v j)]
					 [Aj (grouped-production-head Pj)]
					 [Bj (grouped-production-bodies Pj)]
					 [gamma (collect-assq Aj Bi)]
					 [delta Bj])
				;;(format #t "    j=~d(~a) : ~a\n" j Aj (describe-grammatical-object Pj))
				;;(format #t "      attempt to replace ~a -> {~a},  by ~a -> {δ :~a}{~a}\n" Ai gamma  Ai delta (map cdr gamma))
				(unless (null? gamma)
				  (let1 new-Pi (replace-production Pi gamma delta)
					(set! Pi new-Pi)
					(vector-set! v i Pi)))
				(jloop (+ j 1)))))
		  (let1 new-Pis (eliminate-immediate-left-recursion Pi)
			(if (grouped-production? new-Pis)
				(begin
				  (set! Pi new-Pis)
				  (vector-set! v i Pi))
				(begin
				  (set! Pi (car new-Pis))
				  (vector-set! v i Pi)
				  (when (= 2 (length new-Pis))
					(push! v# (cadr new-Pis))))))
		  (iloop (+ i 1)))))

	(let* ([prods (merge-productions (vector->list v) (reverse! v#))]
		   [nonterminals (map grouped-production-head prods)]
		   [terminals '()] ;;not supported yet
		   [start-symbol (car nonterminals)])
	  (make-grammar nonterminals terminals start-symbol prods))))

;;
;; Algorithm 4.21: Left factoring a grammar.
;;
(use gauche.sequence)
(define (find-common-sequence lis)
  (define (cmp< a b)
	(if (eq? (class-of a) (class-of b))
		(cond [(is-a? a <number>) (< a b)]
			  [else (< (eq-hash a) (eq-hash b))]
			  )
		(< (eq-hash a) (eq-hash b)) ))
  (define (longest-common lis)
	#;(format #t "(longest-common ~a)\n" lis)
	(if (null? lis) '()
		(let1 n-1 (- (length lis) 1)
		  (let loop ((common '()) (lis lis))
			(let1 c (car lis)
			  (if (null? c)
				  (reverse! common)
				  (if (= n-1 (length (filter-map (lambda (cs) (eq? (car c) (car cs))) (cdr lis))))
					  (loop (cons (car c) common) (map cdr lis))
					  (reverse! common))))))))

  (let1 targets-to-dig (filter-map (lambda (es)
									 (let1 len (length es)
									   (if (= 1 len) #f (car es))))
								   (group-sequence (sort (filter-map car lis) cmp<)))
	(let1 commons (map (lambda (c) (longest-common (collect-assq c lis))) targets-to-dig)
	  (if (null? commons) #f
		  (car (sort commons (lambda (a b) (> (length a) (length b)))))))))

(define (algorithm_4_21 g)
  (let ([nonterminals (grammar-nonterminals g)]
		[nont# '()])

	(define (left-factoring prod)
	  (let ([head (grouped-production-head prod)]
			[bodies (grouped-production-bodies prod)])
		(let1 common (find-common-sequence bodies) ;; 最長のもの
		  (if common
			  (let* ([head# (symbol-append head '|#|)] ;;（できればheadにちなんだ）新しいnonterminalシンボルを
					 [c0 (car common)]
					 [com+ (map (lambda (body)
								  (let1 rest (drop body (length common))
									(if (null? rest) (list :empty) rest)))
								(collect-assq c0 bodies))]
					 [com- (!collect-assq c0 bodies)]
					 )
				(push! nont# head#)
				(append-map left-factoring (list (make-grouped-production head
																		  (append (list (enqueue common head#))
																				  com-))
												 (make-grouped-production head# com+) )))
			  (list prod)))))

	(let1 productions (append-map left-factoring (grammar-productions g))
	  (make-grammar (map production-head productions)
					'()
					(car nonterminals)
					productions) )))

