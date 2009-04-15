(use gauche.test)
(test-start "lib")

(require "./lib")
(test-section "uniq")
(test* "(a b c)" '(a b c) (uniq '(a b c)))
(test* "(a a a)" '(a) (uniq '(a a a)))
(test* "(a a e)" '(a e) (uniq '(a e)))
(test* "(e)" '(e) (uniq '(e)))
(test* "()" '() (uniq '()))

(test-section "regular definition")
(define RD
  (regular-definition
   (rd 'digit (rd-union #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
   (rd 'digits (rd-concatenation 'digit (rd-zero-or-more 'digit)))
   (rd 'optional-fraction (rd-union (rd-concatenation #\. 'digits) rd-epsilon))
   (rd 'optional-exponent (rd-union (rd-concatenation #\E (rd-union #\+ #\- rd-epsilon) 'digits) rd-epsilon))
   (rd 'number (rd-concatenation 'digits 'optional-fraction 'optional-exponent))
   ))
;;(rd-print RD_ex_3_6)
(test* "symbols" '(digit digits optional-fraction optional-exponent number) (rd-symbols RD))
(test* "alphabets" '(#\= #\* id) (rd-alphabets RD))
(test* "definitions" '((S L #\= R) (S R) (L #\* R) (L id) (R L))
	   (rd-definitions RD))

;(require "./256")
(test-section "grammar")
(define G
  (grammar (production 'S (list 'L #\= 'R) (list 'R))
		   (production 'L (list #\* 'R) (list 'id))
		   (production 'R (list 'L))
		   ))
;(grammar-print G)
(test* "nonterminals" '(S L R) (grammar-nonterminals G))
(test* "start-nonterminal" 'S (grammar-start-nonterminal G))
(test* "terminals" '(#\= #\* id) (grammar-terminals G))
(test* "productions" '((S L #\= R) (S R) (L #\* R) (L id) (R L))
	   (grammar-productions G))
;(grammar-print (augment-grammar G))










(test-end)
