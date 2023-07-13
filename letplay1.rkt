#lang racket
(define x 5)
(define y 8)
(define (s-double x)
  (letrec ([x  (+ x 3)]
        [y (+ x 2)]
        )
    (+ x y -5)
    )
  )
(define (swap x y)
  (let ([x y]
        [y x]
        )
    (cons x y)
    )
  )
(define (f x)
  (let ([g (lambda (x) (+ x 5))])
    (+ x (g 5))
    )
  )
(define (g x)
(letrec ([x 1]
         [y (+ x 2)])
  (displayln y)))
(define (triple x)
(letrec ([y (+ x 2)]
[f (lambda (z) (+ z y w x))]
[w (+ x 7)])
(f -9)))
