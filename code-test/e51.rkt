#lang racket
(define b 3)
(define f
  (let ([b b])
  (lambda (x) (* 1 (+ x b)))
  ))
(define c (+ b 4))
(set! b 5)
(define z (f 4))
(define w c)
(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))
(define fourteen (car y))
