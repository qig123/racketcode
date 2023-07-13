#lang racket
(define filter
 (lambda (predicate)
  (lambda (lst)
    (cond ((null? lst) '())
          ((predicate (car lst))
           (cons (car lst) ((filter predicate) (cdr lst))))
          (else ((filter predicate) (cdr lst)))))))
; 示例用法
(define even-filter (filter even?))
(even-filter '(1 2 3 4 5 6)) ; 返回结果为 '(2 4 6)

(define create-adder
 (lambda (n)
  (lambda (x)
    (+ x n))))
(define mupl-all-gt
  (lambda (x)
    (lambda (xs) ( (filter (lambda (y) (> y x))) xs ))
    )
  )
( (mupl-all-gt 3) '(1 2 3 4 5 6) )