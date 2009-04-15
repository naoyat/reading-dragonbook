(require "./chap4")

(use gauche.test)
(test-start "Chapter 4. Syntax Analysis")

;(define production? (tagged-list?-proc 'production))
;(define (make-production head body)
;(define (production-head prod) (cadr prod)) ;; left side
;(define (production-body prod) (cddr prod)) ;; right side
;(define (parse-production-desc pdesc)

(test-section "parse-production-desc")
(test* "A → A α" '(production A A α) (parse-production-desc '(A -> A α)))
(test* "A → β" '(production A β) (parse-production-desc '(A -> β)))

;(define grouped-production? (tagged-list?-proc 'grouped-production))
;(define (make-grouped-production head bodies)
;(define (grouped-production-head prod) (cadr prod)) ;; left side
;(define (grouped-production-bodies prod) (cddr prod)) ;; right side
;(define (grouped-production-production prod)

(test-section "parse-grouped-production-desc")
(test* "A → A α / β" '(grouped-production A (A α) (β)) (parse-grouped-production-desc '(A -> A α / β)))
(test* "A → B α γ / β γ" '(grouped-production A (B α γ) (β γ)) (parse-grouped-production-desc '(A -> B α γ / β γ)))
(test* "A → β" '(grouped-production A (β)) (parse-grouped-production-desc '(A -> β)))
(test* "A → β γ" '(grouped-production A (β γ)) (parse-grouped-production-desc '(A -> β γ)))

;(define (make-grammar nonterminals terminals start-symbol productions)
;  (list 'grammar nonterminals terminals start-symbol productions))
;(define grammar? (tagged-list?-proc 'grammar))
;(define (grammar-productions g . args)

; (parse-grammar-desc gdesc)

; (describe-grammatical-object obj . args)

;; trans1

(test-section "empty?")
(test* "#\a" #f (empty? #\a))
(test* "1" #f (empty? 1))
(test* "\"abc\"" #f (empty? "abc"))
(test* ":empty" #t (empty? :empty))
(test* "ϵ" #t (empty? 'ϵ))
(test* "<abc>" #f (empty? '<abc>))
(test* "'abc" #f (empty? 'abc))
(test* "'ABC" #f (empty? 'ABC))
(test* "#t" #f (empty? #t))
(test* "#f" #f (empty? #f))

(test-section "terminal?")
(test* "#\a" #t (terminal? #\a))
(test* "1" #t (terminal? 1))
(test* "\"abc\"" #t (terminal? "abc"))
(test* ":empty" #t (terminal? :empty))
(test* "ϵ" #t (terminal? 'ϵ))
(test* "<abc>" #t (terminal? '<abc>))
(test* "'abc" #f (terminal? 'abc))
(test* "'ABC" #f (terminal? 'ABC))
(test* "#t" #f (terminal? #t))
(test* "#f" #f (terminal? #f))

(test-section "nonterminal?")
(test* "#\a" #f (nonterminal? #\a))
(test* "1" #f (nonterminal? 1))
(test* "\"abc\"" #f (nonterminal? "abc"))
(test* ":empty" #f (nonterminal? :empty))
(test* "ϵ" #f (nonterminal? 'ϵ))
(test* "<abc>" #f (nonterminal? '<abc>))
(test* "'abc" #t (nonterminal? 'abc))
(test* "'ABC" #t (nonterminal? 'ABC))
(test* "#t" #t (nonterminal? #t))
(test* "#f" #t (nonterminal? #f))

(test-section "left-recursion?")
(test* "A→ Aα" #t (left-recursion? (parse-production-desc '(A -> A α))))
(test* "A→ β" #f (left-recursion? (parse-production-desc '(A -> β))))
(test* "A→ Aα|β" #t (left-recursion? (parse-grouped-production-desc '(A -> A α / β))))
(test* "A→ Bα|β" #f (left-recursion? (parse-grouped-production-desc '(A -> B α / β))))

;;(test-section "eliminate-immediate-left-recursion")
;;(test-section "replace-production")

(test-section "Algorithm 4.19: Eliminating left recursion.")
(test* "grammar (4.1)"
	   (parse-grammar-desc '((E -> T E#)
							 (E# -> #\+ T E# / :empty)
							 (T -> F T#)
							 (T# -> #\* F T# / :empty)
							 (F -> #\( E #\) / <id>)
							 ;; #つきのやつが後に来る
							 ;(E# -> #\+ T E# / :empty)
							 ;(T# -> #\* F T# / :empty)
							 ))
	   (algorithm_4_19
		(parse-grammar-desc '((E -> E #\+ T / T)
							  (T -> T #\* F / F)
							  (F -> #\( E #\) / <id>)))
		))

(test* "grammar (4.18)"
	   (parse-grammar-desc ;g_4_18_expected)
		'((S -> A #\a / #\b)
		  (A -> #\b #\d A# / A#)
		  (A# -> #\c A# / #\a #\d A# / :empty))
		)
	   (algorithm_4_19
		(parse-grammar-desc ;g_4_18)) )
		 '((S -> A #\a / #\b)
		   (A -> A #\c / S #\d / :empty))
		 )))

(test-section "Algorithm 4.21: Eliminating left recursion.")
(test* ""
	   (parse-grammar-desc
		'((S -> <if> E <then> S S# / #\a)
		  (S# -> :empty / <else> S) ;;まだソートされない
		  (E -> #\b)
		  ))
	   #;((S -> <if> E <then> S S# / #\a)
		  (S# -> <else> S / :empty)
		  (E -> #\b))
	   (algorithm_4_21
		(parse-grammar-desc ;; (4.23)
		 '((S -> <if> E <then> S / <if> E <then> S <else> S / #\a)
		   (E -> #\b)))))
(test* ""
	   (parse-grammar-desc
		'((A -> a b A# / h i)
		  (A# -> c A## / f g)
		  (A## -> d / e)
		  ))
	   (algorithm_4_21
		(parse-grammar-desc ;; (4.23)
		 '((A -> a b c d / a b c e / a b f g / h i)))))

(test-end)
