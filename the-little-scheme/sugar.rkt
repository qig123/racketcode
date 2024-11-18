#lang racket
;;racket marco

;(strict-if true 1 2)
;(strict-if 0 1 2)
;(strict-if true 1 (/ 1 0))
;(if true 1 (/ 1 0))
(define-syntax strict-if
  (syntax-rules ()
    [(strict-if C T E)
     (if (boolean? C)
         (if C T E)
         (error 'strict-if' "expected a boolean"))]))
;(strict-if true 1 (/ 1 0))
(define-syntax my-let1
  (syntax-rules ()
    [(my-let1 (var val) body)
     ((lambda (var) body) val)]))

(my-let1 (x 3) (+ x x))
