#lang racket
(define SUM 5)
(define coins (list 11 5 1))
(define (coins-value n)
  (cond
    [(= 1 n) 1]
    ;[(= 2 n) 5]
    ;[(= 3 n) 11]
    )
)
(define (change s n)
  (cond
    [(= s 0) 1]
    [(< s 0) 0]
    [(= n 0) 0]
    [#t (+ (change s (- n 1)) ( change (- s (coins-value n)) n))]
    )
  )


