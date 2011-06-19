(declare (uses ltg-proto))

(define (do-self-turn state)
  (apply-state-walk state
					(pick-best-path
					 (possibilities-from-state-d state 2))))

(define (err-printf . params)
  '())

(main (command-line-arguments))