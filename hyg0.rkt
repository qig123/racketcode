#lang racket
(define-syntax double4
(syntax-rules ()
[(double4 e)
(let* ([zero 0]
[x e])
(+ x x zero))]))
(double4 14)
;(let ([zero 17]) (double4 zero))
(let ([zero 17])
(let* ([zero 0]
[x zero])
(+ x x zero)))