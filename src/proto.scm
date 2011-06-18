(use srfi-9)
(use srfi-1)
(use extras)

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
  (make-stack-item desc type cont)
  stack-item?
  (desc stack-item-desc stack-item-desc!)
  (type stack-item-type stack-item-type!)
  (cont stack-item-cont stack-item-cont!))

(define func "function")
(define val "value")

(define (stack-item-val? item)
  (string=? val (stack-item-type item)))

(define test-interp-mode #f)

(define current-stack-depth 0)

(define (if-stack-depth f)
  (lambda (x)
    (set! current-stack-depth (+ 1 current-stack-depth))
    (if (> current-stack-depth 1000) (lambda (i) (error "too deep"))
        (f x))))

(define (valid-slot-id? id)
  (and (<= id 255) (>= id 0)))

(define I (make-card "I"
					 (make-stack-item "I"
									  func
									  (if-stack-depth
                                       (lambda (x) x)))))

(define zero (make-card "zero"
						(make-stack-item "zero"
										 val
										 0)))

(define succ (make-card "succ"
                        (make-stack-item "succ"
                                         func
                                         (if-stack-depth
                                          (lambda (n)
                                           (cond ((stack-item-val? n)
                                                  (let* ((input (stack-item-cont n))
                                                         (new (+ input 1)))
                                                    (make-stack-item (number->string new)
                                                                     val
                                                                     new)))
                                                 (else (printf "succ expects a value\n")
                                                       (lambda (i) (error "succ expects value")))))))))

(define succ (make-card "dbl"
                        (make-stack-item "dbl"
                                         func
                                         (if-stack-depth
                                          (lambda (n)
                                           (cond ((stack-item-val? n)
                                                  (let* ((input (stack-item-cont n))
                                                         (new (* input 2)))
                                                    (make-stack-item (number->string new)
                                                                     val
                                                                     new)))
                                                 (else (printf "dbl expects a value\n")
                                                       (lambda (i) (error "dbl expects value")))))))))

(define succ (make-card "get"
                        (make-stack-item "get"
                                         func
                                         (if-stack-depth
                                          (lambda (i)
                                           (cond ((stack-item-val? i)
                                                  (let ((input (stack-item-cont i)))
                                                    (cond ((valid-slot-id? input)
                                                           (let ((result (player-field current-player input)))
                                                             (make-stack-item (number->string new)
                                                                              val
                                                                              new)))
                                                          (else (printf "get expects valid slot id")
                                                                (lambda (i) (error "get expects valid slot id"))))))
                                                  (else (printf "get expects a value\n")
                                                        (lambda (i) (error "get expects value")))))))))

(define put (make-card "put"
                       (make-stack-item "put"
                                        func
                                        (if-stack-depth
                                         (lambda (x)
                                           (card-function I))))))

(define S (make-card "S"
					 (make-stack-item "S"
									  func
									  (if-stack-depth
                                       (lambda (f)
                                         (make-stack-item (string-append "S(" (stack-item-desc f) ")")
                                                          func
                                                          (if-stack-depth
                                                           (lambda (g)
                                                             (make-stack-item (string-append "S("
                                                                                             (stack-item-desc f)
                                                                                             ","
                                                                                             (stack-item-desc g)
                                                                                             ")")
                                                                              func
                                                                              (if-stack-depth
                                                                               (lambda (x)
                                                                                 ((stack-item-cont ((stack-item-cont f) x))
                                                                                  ((stack-item-cont g) x)))))))))))))


(define K (make-card "K"
             (make-stack-item "K"
                func
                (if-stack-depth
                  (lambda (f)
                    (make-stack-item (string-append
                                       "K("
                                       (stack-item-desc f)
                                       ")")
                                     func
                                     (if-stack-depth
                                       (lambda (g)
                                         ((stack-item-cont f))))))))))


(define start-state (lambda (ignored) (make-slot
                                       (card-function I)
                                       10000)))

(define cards (list I zero succ S))

(define (makeplayervector) 
  (list->vector (unfold zero? start-state (lambda (x) (- x 1)) 256 )))

(define players
  (vector
   (makeplayervector)
   (makeplayervector)
   ))

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
  (let ((player-slot (player-field them slot)))
	(player-field! them slot ((stack-item-cont (card-function card)) player-slot)))
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
  (set! current-stack-depth 0)
  (cond
   ((string=? action "1")
    read-acts-card)
   ((string=? action "2")
    read-astc-slot)
   (else (display "YOU FUCKING BASTARD") read-action-type)))

(define (show-interesting-states p player)
  (printf "player: ~a\n" p)
  (printf "current stack depth ~a\n" current-stack-depth)
  (vector-for-each (lambda (i slot)
                     (cond ((not (and (equal? (stack-item-desc (slot-field slot)) (card-name I))
                                      (equal? (slot-vitality slot) 10000)))
                            (printf "~a:{~a,~a}\n" i (slot-vitality slot) (stack-item-desc (slot-field slot))))))
                   player))

(define (vector-for-each f vec)
  (map f (zip (unfold (lambda (x) (> x (length (vector->list vec)))) (lambda (x) (x)) (lambda (x) (+ x 1)) 0) (vector->list vec))))

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
