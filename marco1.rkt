#lang racket
(define-syntax my-if
  (syntax-rules (then else)
   [(my-if e1 then e2 else e3)
    (if e1 e2 e3)
    ]))
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out e1 e2) e2]
    )
  )
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e) (mcons #f (lambda () e))]
    )
  )
(define-syntax my-force
(syntax-rules ()
[(my-force e)
(if (mcar e)
    (mcdr e)
(begin (set-mcar! e #t)
(set-mcdr! e ((mcdr e)))
(mcdr e)))]))
((my-force (begin (print "hi") (my-delay (+ 2 4)))))
;define-synatax is the special form
;my-if is the name of our macro
(define x 10)

(my-if (> x 5)
    then (display "x 大于 5")
    else (display "x 不大于 5"))
(comment-out (+ 1 2) (* 3 4))
((my-force (begin (print "hi") (my-delay (+ 2 4)))))
