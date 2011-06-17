#!/usr/bin/env racket

#lang racket

(define-structure slot field vitality)

(define I "I")
(define zero "zero")
(define succ "succ")
(define dbl "dbl")
(define get "get")
(define put "put")
(define S "S")
(define K "K")
(define inc "inc")
(define dec "dec")
(define attack "attack")
(define help "help")
(define copy "copy")
(define revive "revive")
(define zombie "zombie")

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
	 [(string=? action "1")
		read-acts-card]
	 [(string=? action "2")
		read-astc-slot]
	 [else (display "YOU FUCKING BASTARD") read-action-type]))


(define (go handler)
	(let ((next-handler (handler (read-line))))
		(go next-handler)))

(go read-action-type)