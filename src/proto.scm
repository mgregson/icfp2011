(declare (unit ltg-proto))

(declare (uses ltg-cards))
(declare (uses ltg-stack))

(use srfi-9)
(use srfi-1)
(use extras)

(define (qsort e f)
  (if (or (null? e) (<= (length e) 1)) e
      (let loop ((left '()) (right '())
                   (pivot (car e)) (rest (cdr e)))
            (if (null? rest)
                (append (append (qsort left f) (list pivot)) (qsort right f))
               (if (>= (f (car rest)) (f pivot));;reverse of normal eh
                    (loop (append left (list (car rest))) right pivot (cdr rest))
                    (loop left (append right (list (car rest))) pivot (cdr rest)))))))

(define-record-type :move
  (make-move type card slot)
  move?
  (type move-type)
  (card move-card)
  (slot move-slot))

(define-record-type :state-walk
  (make-state-walk k state)
  state-walk?
  (k state-walk-k state-walk-k!)
  (state state-walk-state state-walk-state!))

(define-record-type :slot
  (make-slot field vitality)
  slot?
  (field slot-field slot-field!)
  (vitality slot-vitality slot-vitality!))

(define (slot-copy slot)
  (make-slot (slot-field slot) (slot-vitality slot)))

(define (state-copy state)
  (list->vector
   (map
	(lambda (player-state)
	  (list->vector
	   (map slot-copy
			(vector->list player-state))))
	(vector->list state))))

(define test-interp-mode #f)
(define test-derp-mode #f)

(define current-stack-depth 0)

(define max-stack-depth 1000)

(define (valid-slot-id? id)
  (and (<= id 255) (>= id 0)))

(define (valid-vitality? x)
  (and (< x 65535)
	   (> x 0)))

(define start-state (lambda (ignored) (make-slot
                                       (card-function I)
                                       10000)))

(define (makeplayervector) 
  (list->vector (unfold zero? start-state (lambda (x) (- x 1)) 256 )))

(define players
  (vector
   (makeplayervector)
   (makeplayervector)))

(define me 0)
(define them 1)

(define current-player 1)
(define other-player 0)

										;  Given the name of a card and return the corresponding card record.
(define (name-to-card name)
  (car (filter
		(lambda (x)
		  (string=? name (card-name x)))
		cards)))

										;  Get the field value for the given player and slot.
(define (player-field state player slot)
  (let* ((player-vector (vector-ref state player)))
	(slot-field (vector-ref player-vector slot))))

										;  Get the vitality value for the given player and slot.
(define (player-vitality state player slot)
  (let* ((player-vector (vector-ref state player)))
	(slot-vitality (vector-ref player-vector slot))))

										;  Set the field value for the given player and slot.
(define (player-field! state player slot value)
  (let* ((new-state (state-copy state))
		 (player-vector (vector-ref new-state player)))
	(slot-field! (vector-ref player-vector slot) value)
	new-state))


										;  Set the vitality value for the given player and slot.
(define (player-vitality! state player slot value)
  (let* ((new-state (state-copy state))
		 (player-vector (vector-ref new-state player)))
	(slot-vitality! (vector-ref player-vector slot) value)
	new-state))

(define (apply-card-to-slot card slot)
  (display (string-append
			"1\n"
			(card-name card)
			"\n"
			(number->string slot)
			"\n")))

(define (acts card slot)
  (apply-card-to-slot card slot))

(define (apply-slot-to-card slot card)
  (display (string-append
            "2\n"
            (number->string slot)
            "\n"
            (card-name card)
            "\n")))

(define (astc slot card)
  (apply-slot-to-card slot card))

(define (my-turn)
  (set! current-stack-depth 0)
  (apply-zombies players me)
  (set! current-stack-depth 0)
  (do-self-turn))

(define (do-self-turn)
										; (display "Do some shit"))
  '())

(define (eval-zombie player)
  (lambda (slot state)
	(if (< (player-vitality state player slot) 0)
		(let* ((zcont (stack-item-zcont (player-field state player slot)))
			   (result-1 (zcont state (card-function I)))
			   (state-1 (player-vitality! (car result-1) player slot 0)))
		  (player-field! state-1
						 player
						 slot
						 (card-function I)))
		state)))

(define (apply-zombies state player)
  (fold (eval-zombie player) state (gen-indices (vector->list (vector-ref state player)))))

(define (checkForError result)
  (if (equal? -1 current-stack-depth) (cons (car result) (card-function I)) result))

(define (eval-card-to-slot state card slot)
  (set! current-stack-depth 0)
										;  (display "Got card to slot")
  (let* ((player-slot (player-field state them slot))
		 (result (checkForError (if (procedure? (stack-item-cont (card-function card)))
                  ((stack-item-cont (card-function card)) state player-slot) 
                                ;;Ok?
                                (cons state (card-function I)))
                  ))
		 (new-state (player-field! (car result) them slot (cdr result))))
	(my-turn)
	(cons new-state read-action-type)))

(define (eval-slot-to-card state slot card)
  (set! current-stack-depth 0)
										;  (display "Got slot to card")
  (let* ((player-slot (player-field state them slot))
		 (result (checkForError (if (procedure? (stack-item-cont player-slot))
                                    ((stack-item-cont player-slot) state (card-function card)) 
                                    ;;Ok?
                                    (cons state (card-function I)))
                                    ))
		 (new-state (player-field! (car result) them slot (cdr result))))
	(my-turn)
	(cons new-state read-action-type)))

(define (read-acts-card state card)
  (cons state
		(lambda (state slot)
		  (eval-card-to-slot state
							 (name-to-card card)
							 (string->number slot)))))

(define (read-astc-slot state slot)
  (cons state
		(lambda (state card)
		  (eval-slot-to-card state
							 (string->number slot)
							 (name-to-card card)))))

(define (read-action-type state action)
  (if test-interp-mode (display-player-states state))
  (set! current-stack-depth 0)
  (let ((state-1 (apply-zombies state them)))
	(set! current-stack-depth 0)
	(cond
	 ((equal? action #!eof)
	  (exit 0))
	 ((string=? action "1")
	  (cons state-1 read-acts-card))
	 ((string=? action "2")
	  (cons state-1 read-astc-slot))
	 (else (display "YOU FUCKING BASTARD") (cons state-1 read-action-type)))))

(define (show-interesting-states p player)
  (printf "player: ~a\n" p)
  (printf "current stack depth ~a\n" current-stack-depth)
  (printf "current fitness value ~a\n" (fitness-of-player player))
  (vector-for-each (lambda (i slot)
                     (cond ((not (and (equal? (stack-item-desc (slot-field slot)) (card-name I))
                                      (equal? (slot-vitality slot) 10000)))
                            (printf "~a:{~a,~a}\n" i (slot-vitality slot) (stack-item-desc (slot-field slot))))))
                   player))

(define (gen-indices lst)
  (unfold (lambda (x) (>= x (length lst))) (lambda (x) x) (lambda (x) (+ x 1)) 0))

(define (vfe f lst)
  (map (lambda (x) (f (car x) (car (cdr x)))) (zip (gen-indices lst) lst)))

(define (vector-for-each f vec)
  (if (list? vec)
	  (vfe f vec)
	  (vfe f (vector->list vec))))

(define (display-player-states state)
  (vector-for-each show-interesting-states state)
  (printf "possible state walks are ~a\n" 
          (possibilities-from-state-d state 2)
          ;;(state-space-bfs '() (possibilities-from-state state) 30) 
))

(define (go handler state)
  (let* ((input (read-line))
		 (foo (printf "input: ~a\n" input))
		 (handler-result (handler state input))
		 (next-state (car handler-result))
		 (next-handler (cdr handler-result)))
	(if test-derp-mode (compute-val-in-slot 0 554))
    (go next-handler next-state)))

(define (main args)
  (cond ((not (equal? (length args) 1)) (printf "Usage: <fn> <player-number>\n") (exit 1))
        (else
         (let ((config-me (car args)))
           (cond ((string=? config-me "0") (set! me 0) (set! them 1))
                 ((string=? config-me "1") (set! me 1) (set! them 0))
                 ((string=? config-me "t") (set! me 0) (set! them 1) (set! test-interp-mode #t))
                 ((string=? config-me "n") (set! me 0) (set! them 1) (set! test-derp-mode #t))
                 (else (display "DIE IN A FIRE") (exit 1)))
           (go read-action-type players)))))

(define (double-slot slot doubles)
  (acts dbl slot)
  (if (> doubles 0)
    (double-slot slot (- doubles 1))))

(define (succ-slot slot succs)
  (acts succ slot)
  (if (> succs 0)
    (succ-slot slot (- succs 1))))

(define (compute-val-in-slot slot val)
  (acts put slot) ;; overwrite slot to identity func
  (astc slot zero) ;; get a zero into the field
  (acts succ slot) ;; increment
  (let* ((pow2 (find-largest-pow-2 val 0)))
    (double-slot slot (- pow2 1))
    (succ-slot slot (- (- val 1) (expt 2 pow2)))))

(define (find-largest-pow-2 v x)
  (if (< v (expt 2 x)) (- x 1) (find-largest-pow-2 v (+ x 1))))

(define (fitness-of-state state)
  (- (fitness-of-player (vector-ref state me))
     (fitness-of-player (vector-ref state them))))

(define (is-dead-slot slot)
  (< (slot-vitality slot) 1))

(define (is-zombie-slot slot)
  (equal? -1 (slot-vitality slot)))

(define (has-fun-card slot)
  (not (equal? (stack-item-desc (slot-field slot))
			   (card-name I))))

(define (state-space-bfs visited new max-states)
  (printf "new states: ~a   old states: ~a\n" (length new) (length visited))
  (if (eq? 0 (length new)) (exit 1))
  (let* ((new-states (fold (lambda (s acc)
							 (append acc (possibilities-from-state (state-walk-state s))))
						   '()
						   new))
		 (keep-states (filter (lambda (state-walk)
								(not (member state-walk visited))) new-states))
		 (now-visited (append visited new)))
	(if (>= (+ (length now-visited) (length keep-states)) max-states)
		(append now-visited keep-states)
		(state-space-bfs now-visited keep-states max-states))))
	
  

(define (possibilities-from-state-d state maxdepth)
  (let ((results (delete-duplicates 
   (append 
    (heuristicSearch state (+ 2 maxdepth) 1) 
    (depthfirst state maxdepth 1))
   (lambda (x y) (equal? (state-walk-state x)
                         (state-walk-state y)))
   )))
  (set! current-stack-depth 0)
  results
  )
)
(define (depthfirst state maxdepth curdepth)
  (let ((newstates (possibilities-from-state  state)))
    (if (equal? curdepth maxdepth) newstates
        (fold append (list ) (map 
                              (lambda (curstatewalk) (map (lambda (x)
                                                            (state-walk-k! x (append (state-walk-k curstatewalk) (state-walk-k x)))
                                                            x) (depthfirst (state-walk-state curstatewalk) maxdepth (+ 1 curdepth))))
                              newstates
                              ))
        )
))
(define (heuristicSearch state maxdepth curdepth)
  (let* ((newstates (possibilities-from-state  state)) 
        (newstatesScores (zip (map (lambda (s) (fitness-of-state (state-walk-state s))) newstates) newstates))
        (newtoexplore (map cadr (take  (qsort newstatesScores car) (min 3 (length newstatesScores)) )) );;4 should be safe, but 3 to be cautious
        )
    (if (equal? curdepth maxdepth) newstates
        (fold append newtoexplore (map (lambda (x) (map (lambda (j) (state-walk-k! j (append (state-walk-k x)
                                                                                        (state-walk-k j)) )
                                                           j) (heuristicSearch (state-walk-state x) maxdepth (+ 1 curdepth)))) newtoexplore)))
        
))
(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))
 
(define (possibilities-from-state state)
  (let*
      ((measlist (vector->list (vector-ref state me)) )
       (measlistindex (zip (gen-indices measlist) measlist))
       (dedupme (delete-duplicates measlistindex (lambda (x y) (equal? (cdr x)
																	   (cdr y))))))
    (delete-duplicates (concatenate (map (lambda (slot)  
                                            (concatenate
											 (map
											  (lambda (card)
												(list (make-state-walk (list (make-move 'cs
																						(car slot)
																						card))
																	   (car (eval-card-to-slot state
																							   card
																							   (car slot))))
													  (make-state-walk (list (make-move 'sc
																						(car slot) 
																						card))
																	   (car (eval-slot-to-card state
																							   (car slot)
																							   card)))))
											  cards)))
										  dedupme))
					   (lambda (x y) (equal? (state-walk-state x)
											 (state-walk-state y))))))

(define (fitness-of-player player)
  (let*
      ((playeraslist (vector->list player))
       (nonZombieSlots (filter (lambda (s) (not (is-zombie-slot s))) playeraslist))
       (alive (count is-dead-slot playeraslist))
       (zombieCount (count is-zombie-slot playeraslist))
       (cardPresentCount (count has-fun-card nonZombieSlots))
       (happyness (fold (lambda (x y) (+ (stack-item-happyness (slot-field x)) y)) 0 nonZombieSlots))
       (zombieCardPresentCount (count has-fun-card (filter (lambda (s) (is-zombie-slot s)) playeraslist)))
       (vitality (fold (lambda (x y) (+ (slot-vitality x) y)) 0 playeraslist)))
    (- (+ (* 6000 alive) (* 10 cardPresentCount) vitality (* 100 happyness))
       (+ (* 10 zombieCount) (* 50 zombieCardPresentCount)))))

(define (pick-best-path paths)
  (fold
   (lambda (current best)
	 (if (equal? best 'worst-path)
		 current
		 (let ((fitness-best (fitness-of-player (state-walk-state best)))
			   (fitness-curr (fitness-of-player (state-walk-state current))))
		   (cond ((> fitness-curr fitness-best)
				  current)
				 ((< fitness-curr fitness-best)
				  best)
				 (else
				  (cond ((< (state-walk-k current) (state-walk-k best))
						 current)
						(else best)))))))
   'worst-path
   paths))

(define (apply-state-walk state-walk)
  (let ((move (car (state-walk-k state-walk))))
	(cond ((equal? 'cs (move-type move))
		   (acts (move-card move) (move-slot)))
		  (else
		   (astc (move-slot move) (move-card move))))))

