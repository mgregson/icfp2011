(use srfi-9)
(use srfi-1)
(use extras)

(define-record-type :slot
  (make-slot field vitality)
  slot?
  (field slot-field slot-field!)
  (vitality slot-vitality slot-vitality!))

(define (slot-copy slot)
  (make-slot (slot-field slot) (slot-vitality slot)))

;;Don't really need type, but for debugging pandas
(define-record-type :card 
  (make-card name function)
  card?
  (name card-name card-name!)
  (function card-function card-function!))

(define-record-type :stack-item
  (make-stack-item desc type cont zcont)
  stack-item?
  (desc stack-item-desc)
  (type stack-item-type)
  (cont stack-item-cont)
  (zcont stack-item-zcont))

(define (state-copy state)
  (list->vector
   (map
	(lambda (player-state)
	  (list->vector
	   (map slot-copy
			(vector->list player-state))))
	(vector->list state))))

(define func "function")
(define val "value")
(define (runtime-error str)
  (set! current-stack-depth -1)
  (card-function I)
  )

(define (make-r-stack-item desc type cont) (make-stack-item desc type cont cont))

(define (stack-item-val? item)
  (string=? val (stack-item-type item)))

(define test-interp-mode #f)
(define test-derp-mode #f)

(define current-stack-depth 0)

(define max-stack-depth 1000)

(define (if-stack-depth f)
  (lambda (state x)
    (set! current-stack-depth (+ 1 current-stack-depth))
    (cond ((or (> current-stack-depth (- max-stack-depth 1)) (equal? -1 current-stack-depth))
        ;;We've gone too deep/it can't fit
		   (set! current-stack-depth -1)
		   (cons state (card-function I)))
		  (else (f state x)))))

(define (valid-slot-id? id)
  (and (<= id 255) (>= id 0)))

(define (valid-vitality? x)
  (and (< x 65535)
	   (> x 0)))

(define Ifun (if-stack-depth (lambda (state x) (cons state x))))

(define I (make-card "I"
					 (make-stack-item "I"
									  func
									  Ifun Ifun)))

(define zero (make-card "zero"
						(make-stack-item "zero"
										 val
										 0
                                         0
                                         )))

(define succFun (if-stack-depth
                 (lambda (state n)
                   (cond ((stack-item-val? n)
                          (let* ((input (stack-item-cont n))
                                 (new (+ input 1)))
                            (cons
							 state
							 (make-r-stack-item (number->string new)
												val
												new))))
                         (else (printf "succ expects a value\n")
                               (cons state (runtime-error "succ expects value")))) )))

(define succ (make-card "succ"
                        (make-stack-item "succ"
                                         func
                                         succFun
                                         succFun
                                         )))

(define dblFunc (if-stack-depth
                 (lambda (state n)
                   (cond ((stack-item-val? n)
                          (let* ((input (stack-item-cont n))
                                 (new (* input 2)))
							(cons
							 state
							 (make-r-stack-item (number->string new)
												val
												new))))
                         (else (printf "dbl expects a value\n")
                               (cons state (runtime-error "dbl expects value")))))))

(define dbl (make-card "dbl"
					   (make-stack-item "dbl"
										func
										dblFunc
										dblFunc
										)))

(define getFunc (if-stack-depth
                 (lambda (state i)
                   (cond ((stack-item-val? i)
                          (let ((input (stack-item-cont i)))
                            (cond ((valid-slot-id? input)
                                   (let ((result (player-field state current-player input)))
									 (cons state result)))
                                  (else (printf "get expects valid slot id")
                                        (cons state (runtime-error "get expects valid slot id"))))))
                         (else (printf "get expects a value\n")
                               (cons state (runtime-error "get expects value")))))))

(define get (make-card "get"
					   (make-stack-item "get"
										func
										getFunc
										getFunc
										)))

(define putFunc (if-stack-depth
                 (lambda (state x)
                   (cons state (card-function I)))))

(define put (make-card "put"
                       (make-stack-item "put"
                                        func
                                        putFunc
                                        putFunc
                                        )))

(define sFunc 									  
  (if-stack-depth
   (lambda (state f)
     (cons
	  state
	  (make-r-stack-item (string-append "S(" (stack-item-desc f) ")")
						 func
						 (if-stack-depth
						  (lambda (state g)
							(cons
							 state
							 (make-r-stack-item (string-append "S("
															   (stack-item-desc f)
															   ","
															   (stack-item-desc g)
															   ")")
												func
												(if-stack-depth
												 (lambda (state x)
												   (let* ((fx ((stack-item-cont f) state x))
														  (fx-state (car fx))
														  (fx-frame (cdr fx))
														  (gx ((stack-item-cont g) fx-state x))
														  (gx-state (car gx))
														  (gx-frame (cdr gx)))
													 ((stack-item-cont fx-frame) state gx-frame)))))))))))))

(define sFuncZombie 									  
  (if-stack-depth
   (lambda (state f)
     (cons
	  state
	  (make-r-stack-item (string-append "S(" (stack-item-desc f) ")")
						 func
						 (if-stack-depth
						  (lambda (state g)
							(cons
							 state
							 (make-r-stack-item (string-append "S("
															   (stack-item-desc f)
															   ","
															   (stack-item-desc g)
															   ")")
												func
												(if-stack-depth
												 (lambda (state x)
												   (let* ((fx ((stack-item-zcont f) state x))
														  (fx-state (car fx))
														  (fx-frame (cdr fx))
														  (gx ((stack-item-zcont g) fx-state x))
														  (gx-state (car gx))
														  (gx-frame (cdr gx)))
													 ((stack-item-zcont fx-frame) state gx-frame)))))))))))))
												   

(define S (make-card "S"
					 (make-stack-item "S"
									  func
                                      sFunc
                                      sFuncZombie
                                      )))

(define incFunc (if-stack-depth
                 (lambda (state i)
                   (cond ((stack-item-val? i)
                          (let ((idx (stack-item-cont i)))
                            (cond ((valid-slot-id? idx)
                                   (let ((vitality (player-vitality state current-player idx)))
                                     (cond ((and (< vitality 65535) (> vitality 0))
                                            (cons (player-vitality! state current-player idx
																	(+ vitality 1))
												  (card-function I)))
                                           (else (cons state (card-function I))))))
                                  (else
                                   (cons state (runtime-error "inc got invalid slot"))))))
                         (else (cons state (runtime-error "inc expected value; got function")))))))

(define incFuncZombie (if-stack-depth
					   (lambda (state i)
						 (cond ((stack-item-val? i)
								(let ((idx (stack-item-cont i)))
								  (cond ((valid-slot-id? idx)
										 (let ((vitality (player-vitality state current-player idx)))
										   (cond ((> vitality 0)
												  (cons (player-vitality! state current-player idx
																		  (- vitality 1))
														(card-function I)))
												 (else (cons state card-function I)))))
										(else
										 (cons state (runtime-error "inc got invalid slot"))))))
							   (else (cons state (runtime-error "inc expected value; got function")))))))

(define inc (make-card "inc"
					   (make-stack-item "inc"
										func
										incFunc incFuncZombie)))

(define decFunc
  (if-stack-depth
   (lambda (state i)
     (cond ((stack-item-val? i)
            (let ((idx (- 255 (stack-item-cont i))))
              (cond ((valid-slot-id? idx)
                     (let ((vitality (player-vitality state  other-player idx)))
                       (cond ((> vitality 0)
                              (cons (player-vitality! state other-player idx
													  (- vitality 1))
									(card-function I)))
                             (else (cons state (card-function I))))))
                    (else
                     (cons state (runtime-error "dec got invalid slot"))))))
           (else (cons state (runtime-error "dec expected value; got function")))))))

(define decFuncZombie 
  (if-stack-depth
   (lambda (state i)
     (cond ((stack-item-val? i)
            (let ((idx (- 255 (stack-item-cont i))))
              (cond ((valid-slot-id? idx)
                     (let ((vitality (player-vitality state other-player idx)))
                       (cond ((and (> vitality 0) (< vitality 65535)) 
                              (cons (player-vitality! state other-player idx
													  (+ vitality 1))
									(card-function I)))
                             (else (cons state (card-function I))))))
                    (else
                     (cons state (runtime-error "dec got invalid slot"))))))
           (else (cons state (runtime-error "dec expected value; got function")))))))

(define dec (make-card "dec"
					   (make-stack-item "dec"
										func
										decFunc
                                        decFuncZombie)))

(define attackFunc
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "attack" (stack-item-desc i) ")")
						 func
						 (if-stack-depth
						  (lambda (state j)
							(cons
							 state 
							 (make-r-stack-item (string-append "attack(" 
															   (stack-item-desc i) 
															   ","
															   (stack-item-desc j)
															   ")")
												func
												(if-stack-depth
												 (lambda (state n)
												   (if (or (not (stack-item-val? i)) 
														   (not (valid-slot-id? (stack-item-cont i)))
														   (not (stack-item-val? n))
														   (not (integer? (stack-item-cont n)))
														   (not (stack-item-val? j))
														   (> (stack-item-cont n) (player-vitality state current-player (stack-item-cont i))))
													   (cons state (runtime-error "attack failed"))
														   (let* ((jv (stack-item-cont j))
																  (iv (stack-item-cont i))
																  (nv (stack-item-cont n))
																  ;;Decrement our thing before checking j
																  (state-1 (player-vitality! state current-player iv (- (player-vitality state (current-player) iv) nv))))
															 ;;Make sure j is valid
															 (if (not (valid-slot-id? (- 255 jv))) 
																 (cons state-1 (runtime-error "attack failed, invalid j"))
																 (let ((opjsv (player-vitality state-1 other-player (- 255 jv))))
																   (cons (player-vitality! state-1
																						   other-player
																						   (- 255 jv)
																						   (inexact->exact
																							(max (min 0 opjsv)
																								 (-
																								  opjsv
																								  (floor (/ (* 9 nv) 10))))))
																		 (card-function I)))))))))))))))))

(define attackFuncZombie
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "attack" (stack-item-desc i) ")")
						 func
						 (if-stack-depth
						  (lambda (state j)
							(cons
							 state
							 (make-r-stack-item (string-append "attack(" 
															   (stack-item-desc i) 
															   ","
															   (stack-item-desc j)
															   ")")
												func
												(if-stack-depth
												 (lambda (state n)
												   (if (or (not (stack-item-val? i)) 
														   (not (valid-slot-id? (stack-item-cont i)))
														   (not (stack-item-val? n))
														   (not (integer? (stack-item-cont n)))
														   (not (stack-item-val? j))
														   (> (stack-item-cont n) (player-vitality state current-player (stack-item-cont i))))
													   (cons state (runtime-error "attack failed"))
													   (let* ((jv (stack-item-cont j))
															  (iv (stack-item-cont i))
															  (nv (stack-item-cont n))
															  ;;Decrement our thing before checking j
															  (state-1 (player-vitality! state current-player iv (- (player-vitality state current-player iv) nv))))
														 ;;Make sure j is valid
														 (if (not (valid-slot-id? (- 255 jv))) 
															 (cons state-1 (runtime-error "attack failed, invalid j"))
															 (let ((opjsv (player-vitality state-1 other-player (- 255 jv))))
															   (cons (player-vitality! state-1
																					   other-player 
																					   (- 255 jv) 
																					   (inexact->exact
																						(max (min 0 opjsv)
																							 (+
																							  opjsv
																							  (floor (/ (* 9 nv) 10))))))
																	 (card-function I)))))))))))))))))

(define attack (make-card "attack"
						  (make-stack-item "attack"
										   func
										   attackFunc
										   attackFunc)))

(define helpFunc
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "help(" (stack-item-desc i) ")")
						 func
						 (if-stack-depth
						  (lambda (state j)
							(cons
							 state
							 (make-r-stack-item (string-append "help("
															   (stack-item-desc i)
															   ","
															   (stack-item-desc j)
															   ")")
												func
												(if-stack-depth
												 (lambda (state n)
												   (cond
													((not (stack-item-val? i))
													 (cons state (runtime-error "help expected value; got function (i)")))
													((not (valid-slot-id? (stack-item-cont i)))
													 (cons state (runtime-error "help got invalid slot id (i)")))
													((not (stack-item-val? n))
													 (cons state (runtime-error "help expected vallue; got function (n)")))
													(else
													 (let* ((my-idx (stack-item-cont i))
															(my-v (player-vitality state current-player my-idx))
															(delta (stack-item-cont n)))
													   (cond
														((> delta my-v)
														 (cons state (runtime-error "help expected n < vitality i")))
														(else
														 (let ((state-1 (player-vitality! state-1 current-player my-idx (- my-v delta))))
														   (cond
															((not (stack-item-val? j))
															 (cons state-1 (runtime-error "help expected value; got function (j)")))
															((not (valid-slot-id? (stack-item-cont j)))
															 (cons state-1 (runtime-error "help got invalid slot id (j)")))
															(else
															 (let* ((other-idx (stack-item-cont j))
																	(other-v (player-vitality state-1 current-player other-idx)))
															   (cond
																((<= other-v 0)
																 (cons state-1 (card-function I)))
																(else
																 (cons (player-vitality! state-1
																						 current-player
																						 other-idx
																						 (inexact->exact
																						  (min 65535
																							   (+ other-v
																								  (floor (/ (* delta 11) 10))))))
																	   (card-function I))))))))))))))))))))))))

(define helpFuncZombie
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "help(" (stack-item-desc i) ")")
						func
						(if-stack-depth
						 (lambda (state j)
						   (cons
							state
							(make-r-stack-item (string-append "help("
															  (stack-item-desc i)
															  ","
															  (stack-item-desc j)
															  ")")
											   func
											   (if-stack-depth
												(lambda (state n)
												  (cond
												   ((not (stack-item-val? i))
													(cons state (runtime-error "help expected value; got function (i)")))
												   ((not (valid-slot-id? (stack-item-cont i)))
													(cons state (runtime-error "help got invalid slot id (i)")))
												   ((not (stack-item-val? n))
													(cons state (runtime-error "help expected vallue; got function (n)")))
												   (else
													(let* ((my-idx (stack-item-cont i))
														   (my-v (player-vitality state current-player my-idx))
														   (delta (stack-item-cont n)))
													  (cond
													   ((> delta my-v)
														(runtime-error "help expected n < vitality i"))
													   (else
														(let ((state-1 (player-vitality! state current-player my-idx (- my-v delta))))
														  (cond
														   ((not (stack-item-val? j))
															(cons state-1 (runtime-error "help expected value; got function (j)")))
														   ((not (valid-slot-id? (stack-item-cont j)))
															(cons state-1 (runtime-error "help got invalid slot id (j)")))
														   (else
															(let* ((other-idx (stack-item-cont j))
																   (other-v (player-vitality state-1 current-player other-idx)))
															  (cond
															   ((<= other-v 0)
																(cons state-1 (card-function I)))
															   (else
																(cons (player-vitality! state-1
																						current-player
																						other-idx
																						(max 65535
																							 (- other-v
																								(floor (/ (* delta 11) 10)))))
																	  (card-function I))))))))))))))))))))))))


(define help (make-card "help"
						(make-stack-item "help"
										 func
                                         helpFunc helpFuncZombie)))

(define kFunc (if-stack-depth
			   (lambda (state f)
				 (cons
				  state
				  (make-r-stack-item (string-append
									  "K("
									  (stack-item-desc f)
									  ")")
									 func
									 (if-stack-depth
									  (lambda (state g)
										(cons state f))))))))
(define kFuncZombie (if-stack-depth
					 (lambda (state f)
					   (cons
						state
						(make-r-stack-item (string-append
											"K("
											(stack-item-desc f)
											")")
										   func
										   (if-stack-depth
											(lambda (state g)
											  (cons state f))))))))

(define K (make-card "K"
					 (make-stack-item "K"
									  func
									  kFunc kFuncZombie)))

(define copyFunc
  (if-stack-depth
   (lambda (state i)
	 (cond
	  ((not (stack-item-val? i))
	   (cons state (runtime-error "copy expected value; got function (i)")))
	  ((not (valid-slot-id? (stack-item-cont i)))
	   (cons state (runtime-error "copy got invalid slot id (i)")))
	  (else
	   (cons state (player-field state other-player (stack-item-cont i))))))))

(define copyFuncZombie (if-stack-depth
						(lambda (state i)
						  (cond
						   ((not (stack-item-val? i))
							(cons state (runtime-error "copy expected value; got function (i)")))
						   ((not (valid-slot-id? (stack-item-zcont i)))
							(cons state (runtime-error "copy got invalid slot id (i)")))
						   (else
							(cons state (player-field state other-player (stack-item-zcont i))))))))

(define copy (make-card "copy"
						(make-stack-item "copy"
										 func
                                         copyFunc
                                         copyFuncZombie)))

(define reviveFunc
  (if-stack-depth
   (lambda (state i)
	 (cond
	  ((not (stack-item-val? i))
	   (cons state (runtime-error "revive expected value; got function (i)")))
	  ((not (valid-slot-id? (stack-item-cont i)))
	   (cons state (runtime-error "revive got invalid slot id (i)")))
	  (else
	   (let* ((slot (stack-item-cont i))
			  (vitality (player-vitality state current-player slot)))
		 (if (<= vitality 0)
			 (cons (player-vitality! state current-player slot 1)
				   (card-function I))
			 (cons state card-function I))))))))

(define reviveFuncZombie
  (if-stack-depth
   (lambda (state i)
	 (cond
	  ((not (stack-item-val? i))
	   (cons state (runtime-error "revive expected value; got function (i)")))
	  ((not (valid-slot-id? (stack-item-zcont i)))
	   (cons state (runtime-error "revive got invalid slot id (i)")))
	  (else
	   (let* ((slot (stack-item-cont i))
			  (vitality (player-vitality current-player slot)))
		 (if (<= vitality 0)
			 (cons (player-vitality! current-player slot 1)
				   (card-function I))
			 (cons state (card-function I)))))))))

(define revive
  (make-card "revive"
			 (make-stack-item "revive"
							  func
                              reviveFunc
                              reviveFuncZombie
							  )))

(define zombie
  (make-card "zombie"
			 (make-r-stack-item "zombie"
								func
								(if-stack-depth
								 (lambda (state i)
								   (cons
									state
									(make-r-stack-item (string-append "zombie(" (stack-item-desc i) ")")
													   func
													   (if-stack-depth
														(lambda (state x)
														  (cond
														   ((not (stack-item-val? i))
															(cons state (runtime-error "zombie expects a value; got a function (i)")))
														   (else
															(let* ((idx (- 255 (stack-item-cont i)))
																   (vitality (player-vitality state other-player idx)))
															  (cond
															   ((not (valid-slot-id? idx))
																(cons state runtime-error "zombie got invalid slot id (255-i)")))
															   ((> vitality 0)
																(cons state (card-function I)))
															   (else
																(let* ((state-1 (player-vitality! state other-player idx -1))
																	   (state-2 (player-field! state-1 other-player idx x)))
																  (cons state-2 (card-function I))))))))))))))))

(define start-state (lambda (ignored) (make-slot
                                       (card-function I)
                                       10000)))

(define cards (list I zero succ dbl get put S K inc dec attack help copy revive zombie))

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
			   (result-1 (zcont (card-function I)))
			   (state-1 (player-vitality! (car result-1) player slot 0)))
		  (player-field! state-1
						 player
						 slot
						 (card-function I)))
		state)))

(define (apply-zombies state player)
  (set! players (fold (eval-zombie player) state (gen-indices (vector->list (vector-ref state player))))))

(define (checkForError result)
  (if (equal? -1 current-stack-depth) (cons (car result) (card-function I)) result))

(define (eval-card-to-slot card slot)
										;  (display "Got card to slot")
  (let* ((player-slot (player-field players them slot))
		 (result (checkForError ((stack-item-cont (card-function card)) players player-slot))))
	(set! players (player-field! (car result) them slot (cdr result))))
  (my-turn)
  read-action-type)

(define (eval-slot-to-card slot card)
										;  (display "Got slot to card")
  (let* ((player-slot (player-field players them slot))
		 (result (checkForError ((stack-item-cont player-slot) players (card-function card)))))
	(set! players (player-field! (car result) them slot (cdr result))))
  (my-turn)
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
  (apply-zombies players them)
  (set! current-stack-depth 0)
  (cond
   ((equal? action #!eof)
	(exit 0))
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

(define (gen-indices lst)
  (unfold (lambda (x) (>= x (length lst))) (lambda (x) x) (lambda (x) (+ x 1)) 0))

(define (vfe f lst)
  (map (lambda (x) (f (car x) (car (cdr x)))) (zip (gen-indices lst) lst)))

(define (vector-for-each f vec)
  (if (list? vec)
	  (vfe f vec)
	  (vfe f (vector->list vec))))

(define (display-player-states)
  (vector-for-each show-interesting-states players))

(define (go handler)
  (let ((next-handler (handler (read-line))))
    (if test-interp-mode
      (display-player-states)
      (if test-derp-mode (compute-val-in-slot 0 554)))
    (go next-handler)))

(define (main args)
  (cond ((not (equal? (length args) 1)) (printf "Usage: <fn> <player-number>\n") (exit 1))
        (else
         (let ((config-me (car args)))
           (cond ((string=? config-me "0") (set! me 0) (set! them 1))
                 ((string=? config-me "1") (set! me 1) (set! them 0))
                 ((string=? config-me "t") (set! me 0) (set! them 1) (set! test-interp-mode #t))
                 ((string=? config-me "n") (set! me 0) (set! them 1) (set! test-derp-mode #t))
                 (else (display "DIE IN A FIRE") (exit 1)))
           (go read-action-type)))))

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


(main (command-line-arguments))
