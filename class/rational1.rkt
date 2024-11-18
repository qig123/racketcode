#lang racket
(define rational%
  (class object%
    (super-new)
    (init n)
    (init [d 1])
    (define denominator
      (cond [(= d 0) (error "rational% received zero denominator")]
            [(< d 0) (- d)]
            [#t d]))
    (define numerator
      (if (< d 0) (- n) n))
    ))