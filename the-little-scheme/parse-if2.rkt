#lang plait
;;extend conditionals
;;Ast
(define-type Exp
  (numE [n : Number])
  (boolE [n : Boolean])
  (plusE [left : Exp] [right : Exp])
  (cndE [test : Exp] [then : Exp] [else : Exp]))
(define-type Value
  (numV [the-number : Number])
  (boolV [the-boolean : Boolean]))
;parse
(define (parse s)
  (cond
    [(s-exp-number? s) (numE (s-exp->number s))]
    [(s-exp-boolean? s) (boolE (s-exp->boolean s)) ]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(symbol=? '+ (s-exp->symbol (first l))) (plusE (parse (second l)) (parse (third l)))]
         [(symbol=? 'if (s-exp->symbol (first l))) (cndE (parse (second l)) (parse (third l)) (parse (fourth l)))]
         [#t (error 's "parse error")]) )]
    [#t (error 's "parse error")]))
;(calc: (Exp->Value))
(define (calc e)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(plusE l r) (add (calc l) (calc r))]
    [(cndE c t e) (if (boolean-decision (calc c))
                     (calc t)
                     (calc e))]))
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
(define (run s)
  (calc (parse s)))
;test
(test (parse `1) (numE 1))
(test (parse `{+ 1 2}) (plusE (numE 1) (numE 2)))
(test (run `{+ 1 2} ) (numV 3) )
;(s-exp-boolean? `#f)
(define if-exp `{if #f  {+ 1 2} #t} )
;(s-exp-list? if-exp)
;(s-exp->list if-exp)
(parse if-exp)
(run if-exp)
;(symbol=? 'if (s-exp->symbol (first (s-exp->list if-exp) )))
;(first (s-exp->list if-exp))





