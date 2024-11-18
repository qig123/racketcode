#lang plait
;define abs syntax
(define-type Exp
  (numE [n : Number])
  (boolE [b : Boolean])
  (plusE [left : Exp] [right : Exp])
  (cndE [test : Exp] [then : Exp] [else : Exp]))
(define-type Value
  (numV [the-number : Number])
  (boolV [the-boolean : Boolean]))
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

;calc:(Exp->Value)
(define (calc e)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(plusE l r) (add (calc l) (calc r))]
    [(cndE c t e) (if (boolean-decision (calc c))
                     (calc t)
                     (calc e))]))
(define (parse s)
  (cond
    [(s-exp-number? s) (numE (s-exp->number s))]
    [(s-exp-boolean? s) (boolE (s-exp->boolean s)) ]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=?
            '+
            (if (s-exp-symbol? (first l)) (s-exp->symbol (first l)) (error 'parse "not a symbol")))
           (plusE (parse (second l)) (parse (third l)))
           (error 'parse "list not an addition")))]))
(define (run s)
  (calc (parse s)))
;test
(test/exn (calc (plusE (numE 4) (boolE #false))) "RHS")