#lang rosette

(provide main)

(define (subThree n)
  (match n
    [0 3]
    [_ (- (subThree (- n 1)) 1)]))

(define (main . args)
  (begin
    (define-symbolic x integer?)
    (synthesize
     #:forall (list x)
     #:guarantee (assert (> (subThree x) 0)))))

(define (main . args)
  (begin
    (define-symbolic n integer?)
    (letrec
	([f (lambda (x)
	      (match x
		[0 3]
		[else ((f(x - 1)) - 1)]))])
      (solve (assert (= (3 - n) (f n)))))))
