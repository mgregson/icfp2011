(use srfi-9)
(use srfi-1)
(use extras)
(use vector-lib)
(define-record-type :slot
  (make-slot field vitality)
  slot?
  (field slot-field slot-field!)
  (vitality slot-vitality slot-vitality!))
;;Don't really need type, but for debugging pandas
(define-record-type :error 
  (error type)
  error?
  (type error-type error-type!))
(define-record-type :card 
  (make-card name function)
  card?
  (name card-name card-name!)
  (function card-function card-function!))

(define-record-type :stack-item
  (make-stack-item desc type depth cont)
  stack-item?
  (desc stack-item-desc stack-item-desc!)
  (type stack-item-type stack-item-type!)
  (depth stack-item-depth stack-item-depth!)
  (cont stack-item-cont stack-item-cont!))

(define func "function")
(define val "value")

(define test-interp-mode #f)

(define I (make-card "I"
					 (make-stack-item "I"
									  func
									  0
									  (lambda (x) x))))

(define zero (make-card "zero"
						(make-stack-item "zero"
										 val
										 0
										 0)))
(define S (make-card "S"
					 (make-stack-item "S"
									  func
									  0
									  (lambda (f)
										(make-stack-item (string-append "S(" (stack-item-desc f) ")")
														 func
														 0
														 (lambda (g)
														   (make-stack-item (string-append "S("
																						   (stack-item-desc f)
																						   ","
																						   (stack-item-desc g)
																						   ")")
																			func
																			0
																			(lambda (x)
																			  ((f x) (g x))))))))))

(define start-state (make-slot
					 (card-function I)
					 10000))

(define cards (list I zero S))

(define players
  (make-vector
   2
   (make-vector 256 start-state)))

(define me 0)
(define them 1)

										;  Given the name of a card and return the corresponding card record.
(define (name-to-card name)
  (car (filter
		(lambda (x)
		  (string=? name (card-name x)))
		cards)))

										;  Get the field value for the given player and slot.
(define (player-field player slot)
  (let ((player-vector (vector-ref players player)))
	(slot-field (vector-ref player-vector slot))))

										;  Get the vitality value for the given player and slot.
(define (player-vitality player slot)
  (let ((player-vector (vector-ref players player)))
	(slot-vitality (vector-ref player-vector slot))))

										;  Set the field value for the given player and slot.
(define (player-field! player slot value)
  (let ((player-vector (vector-ref players player)))
	(slot-field! (vector-ref player-vector slot) value)))


										;  Set the vitality value for the given player and slot.
(define (player-vitality! player slot value)
  (let ((player-vector (vector-ref players player)))
	(slot-vitality! (vector-ref player-vector slot) value)))

(define (apply-card-to-slot card slot)
  (display (string-append
			"1\n"
			card
			"\n"
			(number->string slot)
			"\nn")))

(define (acts card slot)
  (apply-card-to-slot card slot))

(define (apply-slot-to-card slot card)
  (display (string-append
			"2\n"
			(number->string slot)
			"\n"
			card
			"\n")))

(define (astc slot card)
  (apply-slot-to-card slot card))

(define (do-self-turn)
  (display "Do some shit"))

(define (eval-card-to-slot card slot)
  (display "Got card to slot")
  (do-self-turn)
  read-action-type)

(define (eval-slot-to-card slot card)
  (display "Got slot to card")
  (let ((player-slot (player-field them slot)))
	(player-field! them slot ((stack-item-cont player-slot) (card-function card))))
  read-action-type)

(define (read-acts-card card)
  (lambda (slot)
	(eval-card-to-slot (name-to-card card)
					   (string->number slot))))

(define (read-astc-slot slot)
  (lambda (card)
	(eval-slot-to-card (string->number slot)
					   (name-to-card card))))



(define (read-action-type action)
  (cond
   ((string=? action "1")
	read-acts-card)
   ((string=? action "2")
	read-astc-slot)
   (else (display "YOU FUCKING BASTARD") read-action-type)))

(define (show-interesting-states p player)
  (printf "player: ~a\n" p)
  (vector-for-each (lambda (i slot)
					 (cond ((not (and (equal? (stack-item-desc (slot-field slot)) (card-name I))
									  (equal? (slot-vitality slot) 10000)))
							(printf "~a:{~a,~a}\n" i (slot-vitality slot) (stack-item-desc (slot-field slot))))))
				   player))

(define (display-player-states)
  (vector-for-each show-interesting-states players))

(define (go handler)
  (let ((next-handler (handler (read-line))))
	(if test-interp-mode (display-player-states))
	(go next-handler)))

(define (main args)
  (cond ((not (equal? (length args) 1)) (printf "Usage: <fn> <player-number>\n") (exit 1))
		(else
		 (let ((config-me (car args)))
		   (cond ((string=? config-me "0") (set! me 0) (set! them 1))
				 ((string=? config-me "1") (set! me 1) (set! them 0))
				 ((string=? config-me "t") (set! me 0) (set! them 1) (set! test-interp-mode #t))
				 (else (display "DIE IN A FIRE") (exit 1)))
		   (go read-action-type)))))

(main (command-line-arguments))
