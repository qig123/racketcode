#lang racket
(define funny-sum
  (lambda (xs)
    (cond [(null? xs) 0]
          [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
          [(string? (car xs)) (+ (string-length (car xs)) (funny-sum (cdr xs)))]
          [#t (error "expected number or string")])))
(funny-sum (list 2 "555" 5))