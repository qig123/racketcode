#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))
;a lat is a list of atoms
(define lat?
  (lambda (lst)
    (cond
      [(null? lst) #t]
      [(atom? (car lst)) (lat? (cdr lst))]
      [#t #f])))
(define lat2? (lambda (lst)(andmap atom? lst)))

(define member? (lambda (a lat) (ormap (lambda (x) (eq? x a) ) lat)))

;test
;(lat? '())
;(lat? '(Jack (Sprat could) eat))
;(lat2? '())
;(lat2? '(Jack (Sprat could) eat))
;(member? 'meat '(mashed potatoes and meat gravy))
;(member? 'poached '(fried eggs and scrambled eggs))