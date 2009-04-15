(use gauche.test)

(require "./chap3rx")
(require "./util")

(define (fa-equal? fa1 fa2)
  (and (lset-equal? (second fa1) (second fa2))
	   (lset-equal? (third fa1) (third fa2))))


(test-start "3.6 Finite Automata")
;;;
(test-section "regex-AST->NFA")
(define-macro (test-regex-AST->NFA* msg expected-nfa r)
  `(test* ,msg ,expected-nfa (regex-AST->NFA ,r) fa-equal?))

(test-regex-AST->NFA* ":empty" '(nfa ((0 :empty 1)) (1)) :empty)

(test-regex-AST->NFA* "a <char>" '(nfa ((0 #\a 1)) (1)) #\a)
(test-regex-AST->NFA* "a <string>" '(nfa ((0 #\a 1)) (1)) "a")
(test-regex-AST->NFA* "a <symbol>" '(nfa ((0 #\a 1)) (1)) 'a)
(test-regex-AST->NFA* "a (<char>)" '(nfa ((0 #\a 1)) (1)) '(#\a))
(test-regex-AST->NFA* "a (<string>)" '(nfa ((0 #\a 1)) (1)) '("a"))
(test-regex-AST->NFA* "a (<symbol>)" '(nfa ((0 #\a 1)) (1)) '(a))

(test-regex-AST->NFA* "ab (<char> <char>)" '(nfa ((0 #\a 1) (1 #\b 2)) (2)) '(#\a #\b))
(test-regex-AST->NFA* "ab <string>" '(nfa ((0 #\a 1) (1 #\b 2)) (2)) "ab")
(test-regex-AST->NFA* "ab <symbol>" '(nfa ((0 #\a 1) (1 #\b 2)) (2)) 'ab)
(test-regex-AST->NFA* "ab (<symbol> <symbol>)" '(nfa ((0 #\a 1) (1 #\b 2)) (2)) '(a b))
(test-regex-AST->NFA* "ab (<symbol>)" '(nfa ((0 #\a 1) (1 #\b 2)) (2)) '(ab))

(test-regex-AST->NFA* "abc <string>" '(nfa ((0 #\a 1) (1 #\b 2) (2 #\c 3)) (3)) "abc")
(test-regex-AST->NFA* "aabb <string>" '(nfa ((0 #\a 1) (1 #\a 2) (2 #\b 3) (3 #\b 4)) (4)) "aabb")

(test-regex-AST->NFA* "a*" '(nfa ((0 :empty 1) (0 :empty 3) (1 #\a 2) (2 :empty 1) (2 :empty 3)) (3)) '(* #\a))
(test-regex-AST->NFA* "a+" '(nfa ((0 :empty 1) (1 #\a 2) (2 :empty 1) (2 :empty 3)) (3)) '(+ #\a))
(test-regex-AST->NFA* "a?" '(nfa ((0 :empty 1) (0 :empty 3) (1 #\a 2) (2 :empty 3)) (3)) '(? #\a))

(test-regex-AST->NFA* "a|b (<char> <char>)"
				  '(nfa ((0 :empty 1) (1 #\a 2) (2 :empty 5)
						 (0 :empty 3) (3 #\b 4) (4 :empty 5))
						(5))
				  '(/ #\a #\b))

(test-regex-AST->NFA* "a|b (<symbol> <symbol>)"
				  '(nfa ((0 :empty 1) (1 #\a 2) (2 :empty 5)
						 (0 :empty 3) (3 #\b 4) (4 :empty 5))
						(5))
				  '(/ a b))

(test-regex-AST->NFA* "a|b <char-set>"
				  '(nfa ((0 :empty 1) (1 #\a 2) (2 :empty 5)
						 (0 :empty 3) (3 #\b 4) (4 :empty 5))
						(5))
				  #[ab])

(test-regex-AST->NFA* "a(b|c)d (<char> ...)"
				  '(nfa ((0 #\a 1)
						 (1 :empty 2) (2 #\b 3) (3 :empty 6)
						 (1 :empty 4) (4 #\c 5) (5 :empty 6)
						 (6 #\d 7))
						(7))
				  '(#\a (/ #\b #\c) #\d))

(test-regex-AST->NFA* "a[bc]d (<char> <char-set> <char>)"
				  '(nfa ((0 #\a 1)
						 (1 :empty 2) (2 #\b 3) (3 :empty 6)
						 (1 :empty 4) (4 #\c 5) (5 :empty 6)
						 (6 #\d 7))
						(7))
				  '(#\a #[bc] #\d))

(test-regex-AST->NFA* "ab|cd"
				  '(nfa ((0 :empty 1) (1 #\a 2) (2 #\b 3) (3 :empty 7)
						 (0 :empty 4) (4 #\c 5) (5 #\d 6) (6 :empty 7))
						(7))
				  '(/ ab cd))

(test-regex-AST->NFA* "(a|b)*abb (<char> ...)"
				  '(nfa ((0 :empty 1)
						 (0 :empty 7)
						 (1 :empty 2) (2 #\a 3) (3 :empty 6)
						 (1 :empty 4) (4 #\b 5) (5 :empty 6)
						 (6 :empty 1)
						 (6 :empty 7)
						 (7 #\a 8) (8 #\b 9) (9 #\b 10))
						(10))
				  '((* (/ #\a #\b)) #\a #\b #\b))

(test-regex-AST->NFA* "(a|b)*abb (<char-set>* <symbol>)"
				  '(nfa ((0 :empty 1)
						 (0 :empty 7)
						 (1 :empty 2) (2 #\a 3) (3 :empty 6)
						 (1 :empty 4) (4 #\b 5) (5 :empty 6)
						 (6 :empty 1)
						 (6 :empty 7)
						 (7 #\a 8) (8 #\b 9) (9 #\b 10))
						(10))
				  '((* #[ab]) abb))
				  
(test-regex-AST->NFA* "aa*|bb*"
				  '(nfa ((0 :empty 1)
						 (1 #\a 2)
						 (2 :empty 3)
						 (2 :empty 5)
						 (3 #\a 4)
						 (4 :empty 3)
						 (4 :empty 5)
						 (5 :empty 11)
						 (0 :empty 6)
						 (6 #\b 7)
						 (7 :empty 8)
						 (7 :empty 10)
						 (8 #\b 9)
						 (9 :empty 8)
						 (9 :empty 10)
						 (10 :empty 11))
						(11))
				  '(/ (#\a (* #\a)) (#\b (* #\b))))

(test-regex-AST->NFA* "abc|def (<string> <string>)"
				  '(nfa ((0 :empty 1) (1 #\a 2) (2 #\b 3) (3 #\c 4) (4 :empty 9)
						 (0 :empty 5) (5 #\d 6) (6 #\e 7) (7 #\f 8) (8 :empty 9))
						(9))
				  '(/ "abc" "def"))

(test-section "nfa-simulator")
(define-macro (test-NFA-with-string* nfa in-string str excepted-to-be-accepted?)
  `(let1 sim (NFA-simulator ,nfa)
	 (test* (format "~a with ~s" ,in-string ,str)
			,excepted-to-be-accepted?
			(with-input-from-string ,str sim))))

(define fig_3_26 (make-NFA '((0 :empty 1) (1 #\a 2) (2 #\a 2)
							 (0 :empty 3) (3 #\b 4) (4 #\b 4))
						   '(2 4)))
(define fig_3_29 (make-NFA '((0 #\a 0) (0 #\b 0) (0 #\a 1)
							 (1 #\a 1) (1 #\b 1) (1 #\a 2)
							 (2 #\a 2) (2 #\b 2) (2 :empty 0) (2 #\b 3))
						   '(3)))
(define fig_3_30 (make-NFA '((0 #\a 1) (0 :empty 3)
							 (1 #\b 2) (1 :empty 0)
							 (2 #\b 3) (2 :empty 1)
							 (3 #\a 0) (3 :empty 2))
						   '(3)))

;; Exercise 3.7.2
(test-NFA-with-string* fig_3_29 "Figure 3.29" "aabb" #t)
(test-NFA-with-string* fig_3_30 "Figure 3.30" "aabb" #t)


(let1 nfa (regex-AST->NFA '((* (/ #\a #\b)) #\a #\b #\b))
  ;;(let1 sim (nfa-simulator nfa (list f))
  (test-NFA-with-string* nfa "(a|b)*abb" "abb" #t)
  (test-NFA-with-string* nfa "(a|b)*abb" "aabb" #t)
  (test-NFA-with-string* nfa "(a|b)*abb" "aabc" #f)
  )

#;(let1 nfa (regex-AST->NFA '((* #[ab]) abb))
  (print (NFA->DFA nfa)))
(test-section "NFA->DFA")
(define-macro (test-NFA->DFA* msg expected-dfa nfa)
  `(test* ,msg ,expected-dfa (NFA->DFA ,nfa) fa-equal?))

(test-NFA->DFA* "(a|b)*abb"
				'(dfa ((0 #\a 1) (0 #\b 2)
					   (1 #\a 1) (1 #\b 3)
					   (2 #\a 1) (2 #\b 2)
					   (3 #\a 1) (3 #\b 4)
					   (4 #\a 1) (4 #\b 2))
					  (4))
				'(nfa ((0 :empty 1)
					   (0 :empty 7)
					   (1 :empty 2) (2 #\a 3) (3 :empty 6)
					   (1 :empty 4) (4 #\b 5) (5 :empty 6)
					   (6 :empty 1)
					   (6 :empty 7)
					   (7 #\a 8) (8 #\b 9) (9 #\b 10))
					  (10)) )

;; Exercise 3.7.1
(test-NFA->DFA* "Figure 3.26"
				'(dfa ((0 #\a 1) (0 #\b 2)
					   (1 #\b 3) (1 #\a 1)
					   (2 #\a 3) (2 #\b 2)
					   (3 #\a 3) (3 #\b 3))
					  (1 2))
				fig_3_26)

(test-NFA->DFA* "Figure 3.29"
				'(dfa ((0 #\a 1) (0 #\b 0)
					   (1 #\b 1) (1 #\a 2)
					   (2 #\a 2) (2 #\b 3)
					   (3 #\a 2) (3 #\b 3))
					  (3))
				fig_3_29)

(test-NFA->DFA* "Figure 3.30"
				'(dfa ((0 #\a 0) (0 #\b 0))
					  (0))
					  fig_3_30)


(define (regex-AST->DFA r) (NFA->DFA (regex-AST->NFA r)))
(test-section "regex-AST->DFA")
(define-macro (test-regex-AST->DFA* msg expected-dfa r)
  `(test* ,msg ,expected-dfa (regex-AST->DFA ,r) fa-equal?))

#|
;; Exercise 3.7.3
(print (regex-AST->DFA '(* #[ab])))
(print (regex-AST->DFA '(* (/ (* a) (* b)))))
(print (regex-AST->DFA '(* ((/ :empty #\a) (* #\b)))))
(print (regex-AST->DFA '((* #[ab]) a b b (* #[ab]))))
|#

;;; 3.8.2
#|
(test-section "3.8.2")
(test-regex-AST->NFA* "a" '(nfa ((0 #\a 1))
								(1))
					  'a)
(test-regex-AST->NFA* "abb" '(nfa ((0 #\a 1) (1 #\b 2) (2 #\b 3))
								  (3))
					  'abb)
(test-regex-AST->NFA* "a*b+" '(nfa ((0 #\a 0) (0 #\b 1)
									(1 #\b 1))
								   (1))
					  '((* a) (+ b)))
|#

;;;
(test-end)
