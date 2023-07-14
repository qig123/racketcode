#lang racket
(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin
        (set-mcar! th #t)
        (set-mcdr! th ((mcdr th)))
        (mcdr th))))
;(my-force (my-delay (lambda () 5)))

(define nature-s
  (lambda () (cons 1  nature-s)))
(define nats
(letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
(lambda () (f 1))))
;(car (nature-s))
;(car ((cdr (nature-s))))
(car (nats))
(car ((cdr (nats))))
