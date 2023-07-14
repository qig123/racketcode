#lang racket
(define (silly-double x)
(let ([x (+ x 3)]
[y (+ x 2)])
(+ x y -5))
  )