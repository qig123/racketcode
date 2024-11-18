#lang racket
(define empty-env '())
(define extend-env (lambda (var val env) (cons (cons var val) env)))
(define apply-env
  (lambda (env search-var)
    (cond
      [(null? env) (report-no-binding-found search-var)]
      [(eqv? (car (car env)) search-var) (cdr (car env))]
      [#t (apply-env (cdr env) search-var)])))
(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "No binding for ~s" search-var)))

;test
(define v1 (extend-env 'x 3 empty-env))
v1
(define v2 (extend-env 'y 5 v1))
v2
(apply-env v2 'x)
(apply-env v1 'x)
(apply-env v2 'y)
;(apply-env  v1 'y)
