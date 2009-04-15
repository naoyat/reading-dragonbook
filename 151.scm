;; p151 Simulating a DFA
(define (move s c)
  )
(define (figure-3-27)
  (let1 s (let loop ((s s0) (c (read-char)))
			(if (eof-object? c) s
				(loop (move s c) (read-char)) ))
	(and (memq s F) #t)
	))
