#lang racket
(provide (all-defined-out))

(define two-in-a-row-me?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [#t (cond
            [(null? (cdr lat)) #f]
            [#t ( if (equal? (car lat) (car (cdr lat))) #t (two-in-a-row-me? (cdr lat)))])])))
;经过第一个null的判断后，我们知道这时候list里面必然有至少一个元素，假设设置这元素是a1,
;那么我们的问题就变成，我们要找到a1后面的下一个元素a2,判断a1和a2是否相等,当然下一个元素有可能不存在
(define is-first?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [#t (equal? (car lat) a)])))
(define two-in-a-row1?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [#t ( if (is-first? (car lat) (cdr lat)) #t (two-in-a-row1? (cdr lat))) ])))
(define two-in-a-row2?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [#t [is-first-b? (car lat) (cdr lat)]])))
(define is-first-b?
  (lambda (a lat)
    (cond
    [(null? lat) #f]
    [#t (or (equal? (car lat) a) (two-in-a-row2? (cdr lat)))])))
(define two-in-a-row-b?
  (lambda (p lat)
    (cond
      [(null? lat) #f]
      [#t (or (equal? p (car lat )) #t (two-in-a-row-b? (car lat) (cdr lat)))])))
;最终版本
(define two-in-a-row3?
  (lambda ( lat)
    (cond
      [(null? lat) #f]
      [#t (two-in-a-row-b? (car lat) (cdr lat))])))
;;求前面元素之和
(define sum-of-prefixed
  (lambda (tup)
    (cond
    [(null? tup) '()]
    [#t  ( cons (car tup)(sum-of-prefixed-b (car tup) (cdr tup)))])))
(define sum-of-prefixed-b
  (lambda (p tup)
    (cond
      [(null? tup) '()]
      [#t (cons (+ p (car tup)) (sum-of-prefixed-b (+ p (car tup)) (cdr tup)))])
    ))
(define (scramble l)
  (for/list ([index (in-naturals)]
             [element (in-list l)])
    (list-ref l (+ (- index element) 1))))
(define lst '(a b c ))
;(two-in-a-row3? lst)
;(two-in-a-row-me? lst)
(define tup1 '(2 1 9 17 0))
(define tup2 '(1 1 1 1 1))
(define s1 '(1 1 1 3 4 2 1 1 9 2) )
(scramble s1)
(sum-of-prefixed tup1)
(sum-of-prefixed tup2)

