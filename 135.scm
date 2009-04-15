(define (get-relop)
  (let loop ()
	(case state
	  [(0) (case (next-char)
			 [(#\<) (set! state 1)]
			 [(#\=) (set! state 5)]
			 [(#\>) (set! state 6)]
			 [else 'fail] )]
	  [(1) 'break]
	  [(8) (retract)
	   [(ret-token'set-attribute) 'GT]
	   ret-token]
	  )
	(loop)
  ))