#lang racket
;; empty environment
(define env0 '())
;; 
(define ext-env
  (lambda (x v env)
    (cons (cons x v) env)
    ))
(define (look-up x v)
  (cond
    [(null? v) (error "no value")]
    [ (eq? x (car (car v))) (cdr (car v))]
    [#t (look-up x (cdr v))]
    )
  )
(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(define (eval-exp e env)
  (cond
         [(const? e) e]
         [(negate? e) (const (- (const-int (eval-exp (negate-e e) env))))]
         [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e) env))]
                         [v2 (const-int (eval-exp (add-e2 e) env))]
                         ) (const (+ v1 v2)))]
         [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e) env))]
                         [v2 (const-int (eval-exp (multiply-e2 e) env))]
                         ) (const (* v1 v2)))]
         [#t (error "eval-exp expected an exp")]
         )
  )
(define test-exp (multiply (negate (add (const 2) (const 2))) (const 7)))
(define test-ans (eval-exp test-exp env0))
;test-ans
;look-up 'x env0
;(ext-env 'x 4 env0)
;(look-up 'x  (ext-env 'x 4 env0))