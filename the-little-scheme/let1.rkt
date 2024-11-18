#lang plait
(define-type Exp
  (numE [n : Number])
  (boolE [n : Boolean])
  (plusE [left : Exp] [right : Exp])
  (cndE [test : Exp] [then : Exp] [else : Exp])
  (varE [name : Symbol])
  (let1E [var : Symbol] [value : Exp] [body : Exp] ))
(define-type Value
  (numV [the-number : Number])
  (boolV [the-boolean : Boolean]))
(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty))
;helper function
(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error s "not found")]
    [(some v) v]))
;(extend: (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))
;helper function
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
     [(numV n2) (numV (+ n1 n2))]
     [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))
(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [else (error 'if "Expects conditional to evaluate to be a boolean")]))
;parse
(define (parse s)
  (cond
    [(s-exp-number? s) (numE (s-exp->number s))]
    [(s-exp-boolean? s) (boolE (s-exp->boolean s)) ]
    [(s-exp-symbol? s) (varE (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(symbol=? '+ (s-exp->symbol (first l))) (plusE (parse (second l)) (parse (third l)))]
         [(symbol=? 'if (s-exp->symbol (first l))) (cndE (parse (second l)) (parse (third l)) (parse (fourth l)))]
         [(symbol=? 'let1 (s-exp->symbol (first l))) (let1E (s-exp->symbol (first (s-exp->list (second l))))
                                                            (parse (second (s-exp->list (second l))))
                                                            (parse (third l)))]
         [#t (error 's "parse error")]) )]
    [#t (error 's "parse error")]))
;(interp: (Exp Env -> Value))
(define (interp e nv)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(let1E var val body)
     (let ([new-env (extend nv var (interp val nv))])
       (interp body new-env))]
    [(cndE c t e) (if (boolean-decision (interp c nv))
                     (interp t nv)
                     (interp e nv))]))
(define (run s)
  (interp (parse s) mt-env))
;;testcode
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref i-ht 1)
(define l1 `{let1 {x 1}{+ x x}})
(define l2 `{let1 {x 1}{let1 {y 2} {+ x y}}})
(define l3 `{let1 {x 1}{let1 {y 2}{let1 {x 3}{+ x y}}}} )
(define l4 `{let1 {x 1}{+ x {let1 {x 2} x}}})
(first (s-exp->list l1))
(second (s-exp->list l1))
(third (s-exp->list l1))
(s-exp->list (second (s-exp->list l1)))
(s-exp-symbol? (first (s-exp->list (second  (s-exp->list l1)))) )
(s-exp-number? (first (s-exp->list (second  (s-exp->list l1)))) )
(parse (first (s-exp->list (second  (s-exp->list l1)))))
(parse l1)
(test (run l1) (numV 2))
(test (run l2) (numV 3))
(test (run l3) (numV 5 ))
(test (run l4) (numV 3 ))
