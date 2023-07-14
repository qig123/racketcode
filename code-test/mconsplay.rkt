#lang racket
(define x
  (mcons 10 20)
  )
(define (m p)
  (set-mcar! p 30)
  )
