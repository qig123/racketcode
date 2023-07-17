#lang racket
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define N 16)
(define (successor n) 
  (cond [(null? n) ( cons 1 '())]
        [(= (+ 1 (car n)) N) (cons 0 (successor (cdr n)))]
        [#t (cons (+ 1 (car n)) (cdr n))]
        )
  )
(define predecessor (lambda (n)
                     (cond [(null? n) '()]
                           [(<  (- (car n) 1) 0) (cons (- N 1) ( predecessor ( cdr n)))]
                           [#t  (if (= 0 (- (car n) 1))
                                    (cdr n)
                                    (cons (- (car n) 1) (cdr n)))  ]
                           )
                      ))
(define ( getn n)
  (define (iter c n)
    (if (null? n) 0 ( + ( * (car n) (expt 16 c) ) (iter (+ c 1) (cdr n)))
    )
  )
 (iter 0 n)
  )
(define (convert-to-base16-list num)
  (define base 16)
  (define result '())
  
  (let loop ((n num))
    (if (zero? n)
        result
        (begin
          (set! result (cons (remainder n base) result))
          (loop (quotient n base))))))
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
( reverse (convert-to-base16-list 20))
( reverse (convert-to-base16-list 16))
( reverse (convert-to-base16-list 17))
( reverse (convert-to-base16-list 18))
( reverse (convert-to-base16-list 19))
( reverse (convert-to-base16-list 255))
( reverse (convert-to-base16-list 256))
( reverse (convert-to-base16-list 257))
( reverse (convert-to-base16-list 120))
( reverse (convert-to-base16-list 3628800))
;( reverse (convert-to-base16-list 33))
    
;(getn '(1 2))
;(getn '(15 15))
;(successor '(15))
;( define one (successor (zero)) )
;(predecessor one )
(factorial 4)
(factorial 10)
