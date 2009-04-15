
(define g_4_1 '((E -> E #\+ T / T)
				(T -> T #\* F / F)
				(F -> #\( E #\) / <id>)
				))

(define g_4_2 '((E -> T E_)
				(E_ -> #\+ T E_ / ϵ)
				(T -> F T_)
				(T_ -> #\* F T_ / ϵ)
				(F -> #\( E #\) / <id>)
				))

(define g_4_3 '((E -> E #\+ E / E #\* E / #\( E #\) / <id>) ))

(define g_4_4 '((stmt -> "if" #\( expr #\) stmt "else" stmt) ))

(define g_4_5 '((expression -> expression #\+ term)
				   (expression -> expression #\- term)
				   (expression -> term)
				   (term -> term #\* factor)
				   (term -> term #\/ factor)
				   (term -> factor)
				   (factor -> #\( expression #\))
				   (factor -> <id>) ))

(define g_4_6 '((E -> E #\+ T / E #\- T / T)
				   (T -> T #\* F / T #\/ F / F)
				   (F -> #\( E #\) / <id>) ))

(define g_4_7 '((E -> E #\+ E / E #\* E / #\- E / #\( E #\) / <id>) ))

(define g_4_13 '((S -> #\( S #\) S / ϵ)))

(define g_ex_4_2_1 '((S -> S S #\+ / S S #\* / #\a)))
(define g_ex_4_2_2_a '((S -> 0 S 1 / 0 1)))
(define g_ex_4_2_2_b '((S -> #\+ S S / #\* S S / #\a)))
(define g_ex_4_2_2_c '((S -> S #\( S #\) S / ϵ)))
(define g_ex_4_2_2_d '((S -> S #\+ S / S S / #\( S #\) / S #\*)))
(define g_ex_4_2_2_e '((S -> #\( L #\) / #\a)
					   (L -> L #\, S / S)))
(define g_ex_4_2_2_f '((S -> #\a S #\b S / #\b S #\a S / ϵ)))
(define g_ex_4_2_2_g '((bexpr -> bexpr "or" bterm / bterm)
					   (bterm -> bterm "and" bfactor / bfactor)
					   (bfactor -> "not" bfactor / #\( bepr #\) / "true" / "false")))
(define g_ex_4_2_5 '((stmt -> <if> expr <then> stmt <else> stmt
						   / <if> stmt <then> stmt
						   / <begin> stmt-list <end>)
					 (stmt-list -> stmt #\; stmt-list / stmt)))
(define g_ex_4_2_7 '((S -> 0 / A)
					 (A -> A B)
					 (B -> 1)))
(define g_ex_4_2_8 '((stmt -> <declare> <id> option-list)
					 (option-list -> option-list option / ϵ)
					 (option -> mode / scale / precision / base)
					 (mode -> <real> / <complex>)
					 (scale -> <fixed> / <floating>)
					 (precision -> <single> / <double>)
					 (base -> <binary> / <decimal>)))

(define g_4_14 '((stmt -> <if> expr <then> stmt
					   / <if> expr <then> stmt <else> stmt
					   / <other>)))
(define g_fig_4_10 '((stmt -> matched-stmt
						   / open-stmt)
					 (matched-stmt -> <if> expr <then> matched-stmt <else> matched-stmt
								   / <other>)
					 (open-stmt -> <if> expr <then> stmt
								/ <if> expr <then> matched-stmt <else> open-stmt)))

#;(for-each dump (list g_4_1 g_4_2 g_4_3 g_4_4
					 g_4_5 g_4_6 g_4_7
					 g_4_13
					 g_ex_4_2_1
					 g_ex_4_2_2_a g_ex_4_2_2_b g_ex_4_2_2_c g_ex_4_2_2_d
					 g_ex_4_2_2_e g_ex_4_2_2_f g_ex_4_2_2_g
					 g_ex_4_2_5 g_ex_4_2_7 g_ex_4_2_8
					 g_4_14
					 g_fig_4_10
					 ))

(define g_433_a '((A -> A α / β)))
#;(define g_433_a2 '((A -> β A_)
				   (A_ -> α A_ / ϵ)))
(define g_433_b '((A -> A α1
					 / A α2
					 / A α3
					 / A αm
					 / β1
					 / β2
					 / β3
					 / βn)))

#;(check-left-recursion g_433_a)
#;(check-left-recursion g_433_b)

(define g_4_18 '((S -> A #\a / #\b)
				 (A -> A #\c / S #\d / :empty)))
(define g_4_18_expected '((S -> A #\a / #\b)
						  (A -> #\b #\d A# / A#)
						  (A# -> #\c A# / #\a #\d A# / :empty)))
#;(print (describe-production (algorithm_4_19 (parse-grammar-desc g_4_18))))
