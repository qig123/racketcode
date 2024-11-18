#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(define rember (lambda (a lat) (filter (lambda (x) (not (eq? x a))) lat)))
(define (rember2 a lat)
  (foldr (lambda (x acc) (if (eq? x a) acc (cons x acc))) '() lat))
(define (firsts lst)
  (foldr (lambda (x acc) (cons (car x) acc)) '() lst))
(define (insertR new old lat)
  (foldr (lambda (x acc) (if (eq? x old) (cons x (cons new acc)) (cons x acc))) '() lat))
(define (subst new old lat)
  (foldr (lambda (x acc)(if (eq? x old) (cons new acc ) (cons x acc))) '() lat))
(define subst2
(lambda (new o1 o2 lat)
  (foldr (lambda (x acc) (if (or (eq? x o1) (eq? x o2)) (cons new acc ) (cons x acc) )) '() lat)))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
