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
