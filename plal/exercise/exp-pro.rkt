#lang racket
;(struct foo (bar baz quux) #:transparent)
;This defines a new "struct" called
(struct bot () #:transparent)
(struct top (obj pie) #:transparent)
(struct anchovy () #:transparent)
(struct salmon () #:transparent)
(struct tuna () #:transparent)

;creat
(define p1 (top (salmon) (top (anchovy) (top (tuna) (top (anchovy) (bot))))))
(define p2 (top (anchovy) (bot)))
(define p3 (top 2 (top 3 (top 2 (bot)))))
(define p4 (top (anchovy) (top 3 (top 0 (bot)))))
;
(define (rem pie t)
  (cond
    [(bot? pie) (bot)]
    [(top? pie)
     (if (equal? t (top-obj pie)) (rem (top-pie pie) t) (top (top-obj pie) (rem (top-pie pie) t)))]
    [#t (error "type error")]))
(rem p2 (anchovy))
(rem p1 (anchovy))
(rem p3 2)
(rem p4 3)
;;(equal? (top-obj p2) (anchovy))
