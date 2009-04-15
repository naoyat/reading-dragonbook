(require "./lib")

(define RD_ex_3_6
  (regular-definition
   (rd 'digit (rd-union #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
   (rd 'digits (rd-concatenation 'digit (rd-zero-or-more 'digit)))
   (rd 'optional-fraction (rd-union (rd-concatenation #\. 'digits) rd-epsilon))
   (rd 'optional-exponent (rd-union (rd-concatenation #\E (rd-union #\+ #\- rd-epsilon) 'digits) rd-epsilon))
   (rd 'number (rd-concatenation 'digits 'optional-fraction 'optional-exponent))
   ))
(rd-print RD_ex_3_6)

(define (make-parser regular-definition)
  ;(format #t "(make-parser rd=~a)\n" regular-definition)
  (lambda (string)
	(format #t "parsing ~s ... " string)
	(print 'accepted)
	))

(define parse-number (make-parser RD_ex_3_6))
;(print parse-number)
(parse-number "5280")
(parse-number "0.01234")
(parse-number "6.336E4")
(parse-number "1.89E-4")

(define RD_ex_3_6_simplified
  (regular-definition
   (rd 'digit #[0-9])
   (rd 'digits (rd-one-or-more 'digit))
   (rd 'number (rd-concatenation 'digits
								 (rd-zero-or-one (rd-concatenation #\. 'digits))
								 (rd-zero-or-one (rd-concatenation #\E
																   (rd-zero-or-one #[+-])
																   'digits))
								 ))
   ))

(define RD_ex_3_7
  (regular-definition
   (rd 'letter_ #[A-Za-z_])
   (rd 'digit #[0-9])
   (rd 'id (rd-concatenation 'letter_ (rd-zero-or-more (rd-union 'letter_ 'digit))))
   ))

(define RD_fig_3_10
  (regular-definition
   (rd 'stmt (rd-union (rd-concatenation 'if 'expr 'then 'stmt)
					   (rd-concatenation 'if 'expr 'then 'stmt 'else 'stmt)
					   (rd-concatenation rd-epsilon)))
   (rd 'expr (rd-union (rd-concatenation 'term 'relop 'term) 'term))
   (rd 'term (rd-union 'id 'number))
   ))

(define POT_fig_3_11
  (patterns-for-tokens
   (pattern 'digit #[0-9])
   (pattern 'digits (++ 'digit))
   (pattern 'number (~~ 'digits
						(?? (~~ #\. 'digits))
						(?? (~~ #\E (?? #[+-]) 'digits))
						))
   (pattern 'letter #[A-Za-z])
   (pattern 'id (~~ 'letter (** (// 'letter 'digit))))
   (pattern 'if "if")
   (pattern 'then "then")
   (pattern 'else "else")
   (pattern 'relop "<" ">" "<=" ">=" "=" "<>")
   ))

(define POT_
  (patterns-for-tokens
   (pattern 'blank #\space)
   (pattern 'tab #\tab)
   (pattern 'newline #\newline) ;; #\return
   (pattern 'ws (++ (// 'blank 'tab 'newline)))
   ))

