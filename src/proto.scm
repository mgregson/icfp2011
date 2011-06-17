(use srfi-9)
(use extras)
(define-record slot field vitality)
;;Don't really need type, but for debugging pandas
(define-record error type)
(define-record card-murh name thingy)

(define s 0)
(define (cardfun fun)
  (set! s (+ 1 s))
  (cond
   ;;If its more 10k fuckit
   ((<= 10000 s) (runningerror "too deep"))
   (else fun)
   )
)

(define (valid-slot-number num)
  (and (>= num 0) (< num 256)))

(define (numcardfun fun)
  (cardfun (lambda (n)
             (if (numeric? n) (fun n) (runningerror "not numeric"))
             ))
)

(define-record cardmurh name function)

(define I (make-cardmurh "I" (cardfun (lambda (i) i))))
(define zero (make-cardmurh "zero" 0))
(define succ (make-cardmurh "succ" 
                       (numcardfun (lambda (n) 
                                  (cond
                                   ((< n 65535) (+ 1 n))
                                   (else 65535)
                                   )))))
(define dbl (make-cardmurh "dbl"
                      (numcardfun (lambda (n)
                                    (cond
                                     ((< n 32768) (* n 2))
                                     (else 65535)
                                     ))
                                  )
                      ))
(define get (make-cardmurh "get"
                      (cardfun (lambda (i)
                                 (cond
                                  (((not (valid-slot-number i)) (error "invalid slot #")))
                                  ;;If alive something
                                  ;;If not alive something else
                                  (else (error "bah"))
                      )))))
(define put (make-cardmurh "put" (cardfun (lambda (i) (lambda (x) x)))))
(define S (make-cardmurh "S" (cardfun (lambda (f) 
                                   (lambda (g) 
                                     (lambda (x)
                                       (if ((not (procedure? g))
                                            (not (procedure? f))) (let
                                                                      ((h (f x))
                                                                      (y (g x)))
                                            (if (not (procedure? h)) (error "g x not fun") (h y))))))))))

(define K (make-cardmurh "K" (cardfun (lambda (x y) x))))
(define inc "inc" )
(define dec "dec")
(define attack "attack")
(define help "help")
(define copy "copy")
(define revive "revive")
(define zombie "zombie")

(define start-state (make-slot '(I) 10000))

(define players
  (make-vector
    2
    (make-vector 256 start-state)))

(define me 0)
(define them 1)

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

(define (eval-card-to-slot card slot)
	(display "Got card to slot")
	read-action-type)

(define (eval-slot-to-card slot card)
	(display "Got slot to card")
	read-action-type)

(define (read-acts-card card)
	(lambda (slot)
		(eval-card-to-slot card (string->number slot))))

(define (read-astc-slot slot)
	(current-read-interaction
	 (lambda (card)
		 (eval-slot-to-card (string->number slot) card)
		 (current-read-interaction read-action-type))))

(define (read-action-type action)
	(cond
	 ((string=? action "1")
		read-acts-card)
	 ((string=? action "2")
		read-astc-slot)
	 (else (display "YOU FUCKING BASTARD") read-action-type)))


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
				 (go read-action-type))))

(main (command-line-arguments))
