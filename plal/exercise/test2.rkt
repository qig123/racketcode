#lang racket

(define x 2)
(define (main)
  (define (getx) x)
  (define y (getx))
  (define x 3)
  y
  )
(main)