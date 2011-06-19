(declare (unit ltg-cards))
(declare (uses ltg-stack))
(use srfi-9)
(use srfi-1)
(use extras)

(define-record-type :card 
  (make-card name function)
  card?
  (name card-name card-name!)
  (function card-function card-function!))

(define (runtime-error str)
  (err-printf "runtime error: ~a\n" str)
  (set! current-stack-depth -1)
  (card-function I))

(define (dbg-printf . params)
  (apply printf params))

(define (err-printf . params)
  (apply printf params))

(define Ifun (if-stack-depth (lambda (state x) (cons state x))))

(define I (make-card "I"
					 (make-stack-item "I"
									  func
                                      0;;happyness of 0
									  Ifun Ifun)))

(define zero (make-card "zero"
						(make-stack-item "zero"
										 val
                                         1;;happyness of 1
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
                                                (* 1.5 new);;happyness of 2 [same for all values]
												new))))
                         (else
						  (cons state (runtime-error "succ expects value")))) )))

(define succ (make-card "succ"
                        (make-stack-item "succ"
                                         func
                                         2;;happyness of 3
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
                                                2;;happyness of 2 [same for all values]
												new))))
                         (else
						  (cons state (runtime-error "dbl expects value")))))))

(define dbl (make-card "dbl"
					   (make-stack-item "dbl"
										func
                                        1;;happyness of 3
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
                                  (else
								   (cons state (runtime-error "get expects valid slot id"))))))
                         (else
						  (cons state (runtime-error "get expects value")))))))

(define get (make-card "get"
					   (make-stack-item "get"
										func
                                        1;;happyness of 3
										getFunc
										getFunc
										)))

(define putFunc (if-stack-depth
                 (lambda (state x)
                   (cons state (card-function I)))))

(define put (make-card "put"
                       (make-stack-item "put"
                                        func
                                        1;;happyness of 3
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
                         (if (procedure? (stack-item-cont f)) 
                             (if (equal? (stack-item-desc f) "I")
                                 1
                                 5;;If its a procedure yay
                             )
                             0;;If its not nay
                         )
						 (if-stack-depth
						  (lambda (state g)
							(cons
							 state
							 (make-stack-item (string-append "S("
															   (stack-item-desc f)
															   ","
															   (stack-item-desc g)
															   ")")
												func
                                                (if (and (procedure? (stack-item-cont f)) (procedure? (stack-item-cont g)))  
                                                    (if (or (equal? (stack-item-desc f) "I") (equal? (stack-item-desc g) "I"))
                                                        1
                                                        10;;If its a procedure yay
                                                    )
                                                    0;;If its not nay
                                                )
                                                (if-stack-depth
												 (lambda (state x)
                                                   (if (not (procedure? (stack-item-cont f)))
                                                       (cons state (runtime-error "S got value; expected function (f)"))
                                                       (let* ((fx ((stack-item-cont f) state x))
                                                              (fx-state (car fx))
                                                              (fx-frame (cdr fx)))
                                                         (if (not (procedure? (stack-item-cont g)))
                                                             (cons state (runtime-error "g is not a function in S"))
                                                             (let*
                                                                 ((gx ((stack-item-cont g) fx-state x))
                                                                  (gx-state (car gx))
                                                                  (gx-frame (cdr gx)))
                                                               (if (not (procedure? (stack-item-cont fx-frame)))
                                                                   (cons state (runtime-error "f(x) is not a function in S"))
                                                                   ((stack-item-cont fx-frame) state gx-frame))))))))
                                                (if-stack-depth
												 (lambda (state x)
                                                   (if (not (procedure? (stack-item-zcont f)))
                                                       (cons state (runtime-error "f is not function in S"))
                                                       (let* ((fx ((stack-item-zcont f) state x))
                                                              (fx-state (car fx))
                                                              (fx-frame (cdr fx)))
                                                         (if (not (procedure? (stack-item-cont g)))
                                                             (cons state (runtime-error "g is not a function in S"))
                                                             (let*
                                                                 ((gx ((stack-item-zcont g) fx-state x))
                                                                  (gx-state (car gx))
                                                                  (gx-frame (cdr gx)))
                                                               (if (not (procedure? (stack-item-zcont fx-frame)))
                                                                   (cons state (runtime-error "f(x) is not a function in S"))
                                                                   ((stack-item-zcont fx-frame) state gx-frame))))))))
                                                )))))))))

(define S (make-card "S"
					 (make-stack-item "S"
									  func
                                      2;;happyness of 4
                                      sFunc
                                      sFunc
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
                                        1;;happyness of 3
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
                                        1;;happyness of 4
										decFunc
                                        decFuncZombie)))

(define attackFunc
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "attack(" (stack-item-desc i) ")")
						 func
                         (if (and (stack-item-val? i) (valid-slot-id? (stack-item-cont i)))
                             4;;happyness of 4
                             -1)
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
                                                (if (and (stack-item-val? i) (valid-slot-id? (stack-item-cont i)) 
                                                         (stack-item-val? j) (valid-slot-id? (stack-item-cont j)))
                                                    5;;happyness of 5
                                                    -1
                                                )
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
																								 (-
																								  opjsv
																								  (floor (/ (* 9 nv) 10))))))
																		 (card-function I)))))))))))))))))

(define attackFuncZombie
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "attack(" (stack-item-desc i) ")")
						 func
                         -5;;happyness of -5
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
                                                -7;;happyness of -6
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
                                           1 ;; happyness of 3
										   attackFunc
										   attackFunc)))

(define helpFunc
  (if-stack-depth
   (lambda (state i)
	 (cons
	  state
	  (make-r-stack-item (string-append "help(" (stack-item-desc i) ")")
						 func
                         (if (and (stack-item-val? i) (valid-slot-id? (stack-item-cont i)))
                         4;;happyness of 4
                         -1
                         )
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
                                                (if (and (stack-item-val? i) (valid-slot-id? (stack-item-cont i)) 
                                                         (stack-item-val? j) (valid-slot-id? (stack-item-cont j))
                                                         )
													5;;happyness of 5
													-1
                                                )
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
                        -4;;happyness of -4
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
                                               -7;;happyness of -5
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
                                         2;;happyness of 4
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
									 (stack-item-happyness f)
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
                                           (stack-item-happyness f)
										   (if-stack-depth
											(lambda (state g)
											  (cons state f))))))))

(define K (make-card "K"
					 (make-stack-item "K"
									  func
                                      0;;happyness of 3
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
                                         1;;happyness of 3
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
			 (cons state (card-function I)))))))))

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
                              1;;happyness of 3
                              reviveFunc
                              reviveFuncZombie
							  )))

(define zombie
  (make-card "zombie"
			 (make-r-stack-item "zombie"
								func
                                3;;happyness of 3
								(if-stack-depth
								 (lambda (state i)
								   (cons
									state
									(make-r-stack-item (string-append "zombie(" (stack-item-desc i) ")")
													   func
                                                       (if (stack-item-val? i)
														   4;;happyness of 4
														   -1;;Sad if its a function cause asplode
                                                       )
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
																(cons state (runtime-error "zombie got invalid slot id (255-i)")))
															   ((> vitality 0)
																(cons state (runtime-error "zombie for live slot id (255-i)")))
															   (else
																(let* ((state-1 (player-vitality! state other-player idx -1))
																	   (state-2 (player-field! state-1 other-player idx x)))
																  (cons state-2 (card-function I)))))))))))))))))

(define cards (list I zero succ dbl get put S K inc dec attack help copy revive zombie))
