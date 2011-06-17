(use srfi-9)
(use extras)

(define (read-action-type action)
	(printf "~a\n" action)
	read-action-type)


(define (go handler)
	(let ((next-handler (handler (read-line))))
		(go next-handler)))

(define (main args)
	(cond ((not (equal? (length args) 1)) (printf "Usage: <fn> <player-number>\n") (exit 1))
				(else
				 (let ((config-me (car args)))
					 (cond ((string=? config-me "0") (set! me 0) (set! them 1))
					((string=? config-me "1") (set! me 1) (set! them 0))
					(else (display "DIE IN A FIRE") (exit 1))))
				 (if (equal? me 0) (printf "1\nI\n0\n"))
				 (go read-action-type))))

(main (command-line-arguments))
