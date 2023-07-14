#lang racket
(define g
 (lambda (z)
  (let* ([x (+ z 1)]
        [y (+ x 2)]
        [f (lambda (t) (+ t t))])
      (+ x y (f z) ))))
;求列表的最大值
(define (max-list lst)
  (cond
    ((null? lst) (error "list is empty"))
    ((null? (cdr lst)) (car lst))
    ( else (let ( [max-rest (max-list (cdr lst))])
                  (if (> (car lst) max-rest) (car lst) max-rest)
                  ) ))
    )
  