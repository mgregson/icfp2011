(declare (uses ltg-proto))

(define (do-self-turn state)
  (apply-state-walk
   state
   (pick-best-path
	(fitness-dfs '()
				 (possibilities-from-state state)
				 96
				 0
				 fitness-of-state))))

(define (err-printf . params)
  '())
(define (dbg-printf . params)
  '())

(main (command-line-arguments))
