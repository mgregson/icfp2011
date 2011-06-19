(declare (unit ltg-stack))

(use srfi-9)

(define-record-type :stack-item
  (make-stack-item desc type happyness cont zcont)
  stack-item?
  (desc stack-item-desc)
  (type stack-item-type)
  (happyness stack-item-happyness)
  (cont stack-item-cont)
  (zcont stack-item-zcont))

(define (make-r-stack-item desc type happyness cont) (make-stack-item desc type happyness cont cont))

(define (stack-item-val? item)
  (string=? val (stack-item-type item)))

(define (if-stack-depth f)
  (lambda (state x)
    (set! current-stack-depth (+ 1 current-stack-depth))
    (cond ((or (> current-stack-depth (- max-stack-depth 1)) (equal? -1 current-stack-depth))
        ;;We've gone too deep/it can't fit
		   (set! current-stack-depth -1)
		   (cons state (card-function I)))
		  (else (f state x)))))
