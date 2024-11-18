#lang racket
;;how to use marco steper

(define-syntax my-let1
  (syntax-rules ()
    [(my-let1 (var val) body)
     ( (lambda (var) body) val)]))
(my-let1 (x 5) (+ x x))
(define-syntax my-let2
  (syntax-rules ()
    [(my-let2 ([var val] ...) body)
     ((lambda (var ...) body) val ...)]))
(my-let2 ([x 1] [y 2]) (+ x y))
(define-syntax my-cond
  (syntax-rules ()
    [(my-cond) (error 'my-cond "should not get here")]
    [(my-cond [q0 a0] [q1 a1] ...)
     (if q0 a0 (my-cond [q1 a1] ...))]))
(define (sign n)
(my-cond
[(< n 0) "negative"]
[(= n 0) "zero"]
[(> n 0) "positive"]))
(sign -1)
(define-syntax unless
  (syntax-rules ()
    [(_ c body ...)
     (if (not c) (begin body ...) (void))]))
(let ([not (λ (v) v)])
(unless false
(println 1)
(println 2)))
(define-syntax or-2
  (syntax-rules ()
    [(_ e1 e2)
     ( let ([v e1])(if v v e2))]))
;(or-2 (member 'y '(x y z)) "not found")
;(or-2 (print "hello") "not found")
(let ([v 1])
(or-2 false v))
(define-syntax orN
(syntax-rules ()
[(_) false]
[(_ e1 e2 ...)
(let ([v e1])
(if v v (orN e2 ...)))]))
(orN 1 2 false)
(orN 1 false)
(orN false)
