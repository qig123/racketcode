#lang racket
(define (my-delay f)
  (mcons #f f)
  )
;save a chunk f of value 
(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th)
             )
      )
  )
(define f
  (lambda () 1)
  )
(define (my-mult x y)
  (cond [(= x 0) 0]
        [(= x 1) y]
        [#t (+ y (my-mult (- x 1) y))]
        )
  )
(define (my-mult2 x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]
        )
  )