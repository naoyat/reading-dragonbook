(use srfi-1) ;iota
(use gauche.uvector) ; string->u8vector
(define *eof* (read-from-string ""))
(define *undef* (if #f #f))

(define (string->stream string)
  (let* ((buf (string->u8vector string))
		 (len (u8vector-length buf))
		 (pos 0))
	(define (set-pos! new-pos)
	  (set! pos (cond ((< new-pos 0) 0)
					  ((< new-pos len) new-pos) ; [0,len)
					  (else len))))
	(define (make-backtrack-proc pos)
	  (lambda () (set-pos! pos)))
	(define (first-char)
	  (if (< pos len)
		  (let1 ch (u8vector-ref buf pos)
			(set! pos (+ pos 1))
			(integer->char ch))
		  *eof*))

	(lambda (m) (case m
				  ((first-char) (first-char))
				  ((set-pos) set-pos!)
				  ((at-the-end?) (= pos len))
				  ((rewind) (set-pos! 0))
				  ((backtrack-proc) (make-backtrack-proc pos))
				  (else *undef*)))
	))

;(define string "aaaaaa")
;(define st (string->stream "abcdef"))

;[st'first-char]
;(define backtrack [st'backtrack-proc])
;[st'first-char]
;[st'rewind]
;([st'set-pos] 2)
;(backtrack)

(define (S st)
  (let1 backtrack [st'backtrack-proc]
	(if	(and (eq? #\a [st'first-char])
			 (S st)
			 (eq? #\a [st'first-char]))
		#t
		(begin
		  (backtrack)
		  (if (and (eq? #\a [st'first-char])
				   (eq? #\a [st'first-char]))
			  #t
			  (begin
				(backtrack)
				#f))))))

(define (test-string string)
  (let1 st (string->stream string)
	(and (S st) [st'at-the-end?])))

(define (test-string-with-length-n n)
  (and (test-string (make-string n #\a)) n))

(define (test till)
  (remove not (map test-string-with-length-n (iota (/ till 2) 2 2))))