#lang racket
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define successor (lambda (n) (cons #t n)))
(define predecessor (lambda (n) (cdr n)))
;;client program
(define plus (lambda (x y) (if (is-zero? x) y (successor (plus (predecessor x) y)))))
(define one (successor (zero)))
(define two (successor one))
;(plus (zero) (one))

