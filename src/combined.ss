(declare (uses ltg-proto))

(define retention-decay 1.4)

(define (fitness-of-state state)
  (- (fitness-of-player (vector-ref state me))
     (* 1.3 (fitness-of-player (vector-ref state them)))))

(define (fitness-of-player player)
  (let*
      ((playeraslist (vector->list player))
       (nonZombieSlots (filter (lambda (s) (not (is-zombie-slot s))) playeraslist))
       (nonDeadSlots (filter (lambda (s) (not (is-dead-slot s))) nonZombieSlots))
       (alive (count is-dead-slot playeraslist))
       (zombieCount (count is-zombie-slot playeraslist))
       (cardPresentCount (count has-fun-card nonZombieSlots))
       (happyness (fold (lambda (x y) (+ (stack-item-happyness (slot-field x)) y)) 0 (delete-duplicates nonDeadSlots (lambda (x y) (equal? (slot-field x) (slot-field y))))))
       (zombieCardPresentCount (count has-fun-card (filter (lambda (s) (is-zombie-slot s)) playeraslist)))
       (vitality (fold (lambda (x y) (+ (slot-vitality x) y)) 0 playeraslist)) 
       (vitalities (map (lambda (x) (slot-vitality x)) playeraslist))
       )
    (- (+ (* 6000 alive) (* 0 cardPresentCount) (* 40 vitality) (* 25 happyness) (* 80 (fold min 99999 vitalities) ) )
       (+ (* 10 zombieCount) (* 25 zombieCardPresentCount)))))

(define (do-self-turn state)
  (apply-state-walk
   state
   (pick-best-path
	(fitness-dfs '()
				 (let* ((from-d (possibilities-from-state-d state 2))
						(scores (zip (map (lambda (s) (fitness-of-state (state-walk-state s))) from-d) from-d))
						(keep (map cadr (take (qsort scores car) 10))))
				   keep)
				 10
				 0
				 fitness-of-state))))

(define (err-printf . params)
  '())

(define (dbg-printf . params)
  '())

(main (command-line-arguments))
