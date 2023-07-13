#lang racket
(define a 3) ;extends the top-level env so that a is bound to 3
(define b (+ a 2))
;lambda synatx define a function binds to cube1
(define cube1
  (lambda (x)
    (* x (* x x))
    )
  )
(define cube2
  (lambda (x)
    (* x x x)
    )
  )
(define pow
  (lambda (x y)
    (if ( = y 0)
        1
        (* x (pow x (- y 1)))
        )
    )
  )
;syntactic sugar for defining functions without Lambda keyword
(define (cube3 x) (* x x x))
(define (pow2 x y)
  (if (= y 0) 1 (* x (pow2 x (- y 1))))
  )
;use currying
(define pow3
  (lambda (x)
    (lambda (y)
      ( if (= y 0)
           1
           (* x ((pow3 x) ( - y 1)))
           )
      )
    )
  )
;closures
(define three-to-the (pow3 3))
(define eightone (three-to-the 4))
(define sixteen ((pow3 2) 4))
;sugar for defining a curried function
(define ((pow4 x) y)
  (if (= y 0)
      1
      (* x ((pow4 x) (- y 1)))
      )
  )
;list-processing functions
(define  sum
  (lambda (xs)
    (if (null? xs)
        0
        (+ (car xs) (sum (cdr xs)))
        )
    )
  )
(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))
      )
  )
(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))
      )
  )
;
(define (sum-all xs)
  (cond [(null? xs) 0]
        [(number? xs) xs]
        [(list? xs) (+ (sum-all (car xs)) (sum-all (cdr xs)) )]
        [#t 0]
        )
  )
;
(define (silly-double x)
  (let ([x  (+ x 3)]
        [y  (+ x 2)]
        )
    (+ x y -5)
    )
  )
