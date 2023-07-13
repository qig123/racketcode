#lang racket
(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))
(define fourteen (car y))
(define pr (cons 1 (cons #t "hi")))