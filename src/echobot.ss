(use srfi-9)
(use extras)

(define (read-action-type app arg1 arg2)
	(printf "~a\n" app)
	(printf "~a\n" arg1)
	(printf "~a\n" arg2)
  (flush-output)
	read-action-type)

(define (go handler)
  (let* ((next-app (read-line))
        (next-arg1 (read-line))
        (next-arg2 (read-line))
	      (next-handler (handler next-app next-arg1 next-arg2)))
		(go next-handler)))

(define (main args)
	(cond ((not (equal? (length args) 1)) (printf "Usage: <fn> <player-number>\n") (exit 1))
				(else
				 (let ((config-me (car args)))
					 (cond ((string=? config-me "0") (set! me 0) (set! them 1))
					((string=? config-me "1") (set! me 1) (set! them 0))
					(else (display "DIE IN A FIRE") (exit 1))))
				 (if (equal? me 0) (printf "1\nI\n0\n"))
         (flush-output)
				 (go read-action-type))))

(main (command-line-arguments))
