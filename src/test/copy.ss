(declare (uses ltg-proto))
(declare (uses ltg-cards))
(declare (uses ltg-stack))

(set! players (player-field! (player-field! players 0 0 (card-function zero)) 1 255 (make-stack-item "255" val 1 0 0)))

(set! players (car (eval-card-to-slot players copy 255)))

(if (and (string=? (stack-item-desc (player-field players 1 255))
									 "zero")
				 (equal? (player-vitality players 1 255)
								 10000))
		(printf "PASS\n")
		(printf "FAIL\n"))
