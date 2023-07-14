#lang racket
;(define x 6)
;env from before the let-exp
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))
;这里产生环境:count=0, counter-1=(lambda () (set! count (+ count 1)) count)
(define counter-1 (make-counter))
;这里产生环境:count=1, counter-1=(lambda () (set! count (+ count 1)) count)
(counter-1)
(counter-1)
