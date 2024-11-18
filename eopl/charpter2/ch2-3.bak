#lang racket

(define (is-zero? n)
  (define (eval exp)
    (match exp
      [(list "one") 1]
      [`(,op ,e1 ,e2) (let ([v1 (eval e1)] [v2 (eval e2)]) (- v1 v2))]))
  (if (= 0 (eval n)) #t #f))
(define one (list "one")) ;1
(define zero (list "diff" one one))

(define successor (lambda (n) (list "diff" n (list "diff" (list "diff" one one) one))))
(define predecessor (lambda (n)
                    (list "diff" n (list "one"))  
                      ))
;test
(define (eval n)
  (define (eval exp)
    (match exp
      [(list "one") 1]
      [`(,op ,e1 ,e2) (let ([v1 (eval e1)] [v2 (eval e2)]) (- v1 v2))]))
  (eval n))

(is-zero? one)
(is-zero? (list "diff" one one)) ;0
(is-zero? (list "diff" (list "diff" one one) one)) ;-1
(define two (successor one))
(eval (successor zero))
(eval (predecessor two))
;(successor one)
;(eval (list "diff" (list "diff" one one) one) )
