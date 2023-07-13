#lang racket
(define (my-delay f)
  (mcons #f f)
  )
(define (my-force th)
(if (mcar th)
(mcdr th)
(begin (set-mcar! th #t)
(set-mcdr! th ((mcdr th)))
(mcdr th))))

(my-force (begin (print "hi") (my-delay (lambda () (+ 2 4)))))
