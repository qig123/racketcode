#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require "ch2.rkt")
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) (cdr l)]
        [#t (cons (car l) ((rember-f test?) a (cdr l)))]))))
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(test? (car l) old) (cons new (cons old (cdr l)))]
        [#t (cons (car l) ((insertL-f test?) new old (cdr l)))]))))
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(test? (car l) old) (cons old (cons new (cdr l)))]
        [#t (cons (car l) ((insertR-f test?) new old (cdr l)))]))))
;;insert-g
(define insert-g
  (lambda (f)
    (lambda (new old l)
      (cond [(null? l) '()]
            [(eq? (car l) old) (f new old (cdr l)) ]
            [#t (cons (car l) ((insert-g f) new old (cdr l)))]))))
(define seqL
  (lambda (o1 o2 l)
    (cons o1 (cons o2 l))))
(define seqR
  (lambda (o1 o2 l)
    (cons o2 (cons o1 l))))
(define insertL2 (insert-g seqL))
(define insertR2 (insert-g seqR))
(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond [(null? l) '()]
            [(test? (car l) a) ((multirember-f test?) a (cdr l))]
            [#t (cons (car l) ((multirember-f test?) a (cdr l)))]))))
(define eq?-tuan
  (lambda (x) (eq? x 'tuna)))
(define multiremberT
  (lambda (test?)
    (lambda ( l)
      (cond [(null? l) '()]
            [(test? (car l)) ((multiremberT test?) (cdr l))]
            [#t (cons (car l) ((multiremberT test?) (cdr l)))]))))
(define multirember-f2
  (multiremberT eq?-tuan))
  
(define multirember&co
  (lambda (a lat col)
    (cond
      [(null? lat) (col (quote ()) (quote ()))]
      [(eq? (car lat) a)
       (multirember&co a (cdr lat)
         (lambda (newlat seen) (col newlat (cons (car lat) seen))))  ]
      [else (multirember&co a (cdr lat)
        (lambda (newlat seen) (col (cons (car lat) newlat) seen)))])))
(define a-friend
(lambda (x y)(length x)))
;(multirember&co 'tuna '(tuna) a-friend)
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
      [#t (cons (car lat)(multiinsertLR new oldL oldR (cdr lat)))])))
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond [(null? lat) (col '() 0 0)]
         [(eq? (car lat) oldL)
          (multiinsertLR&co new oldL oldR
           (cdr lat)
           (lambda (newlat L R) (col (cons new (cons oldL newlat)) (+ 1 L) R)))]
         [(eq? (car lat) oldR)
          (multiinsertLR&co new oldL oldR
           (cdr lat)
           (lambda (newlat L R)( col (cons oldR (cons new newlat)) L (+ 1 R))))]
         [#t
          (multiinsertLR&co new oldL oldR
           (cdr lat)
           (lambda (newlat L R)(col (cons (car lat) newlat) L R)))])))
(define insert-col
  (lambda (lat l r)
    lat)
  )
;(multiinsertLR&co 'topping 'cream 'fudge '(ice cream with fudge for cream dessert) insert-col)
(define evens-only*
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (if (even? (car l))
          (cons (car l) (evens-only* (cdr l)))
          (evens-only* (cdr l)))]
      [#t (cons (evens-only* (car l)) (evens-only* (cdr l)))])))
(define evens-only*&co
  (lambda (l col)
    (cond
      [(null? l) (col '() 0 1)]
      [(atom? (car l))
       (if (even? (car l))
           (evens-only*&co
            (cdr l)
            (lambda (newlst sum product)
              (col (cons (car l) newlst) sum (* (car l) product) )))
           (evens-only*&co
            (cdr l)
            (lambda (newlst sum product)
              (col newlst (+ sum (car l)) product))))]
      [#t (evens-only*&co (car l)
            (lambda (al as ap)
              (evens-only*&co (cdr l)
                (lambda (dl ds dp)
                  (col (cons al dl) (+ as ds)(* ap dp))))))])))
(define the-last-friend
  (lambda (newl sum product)
    (cons sum (cons product newl))))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)


