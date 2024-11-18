#lang plait
;;add function binding, only one argument
;Ast
(define-type Exp
  (numE [n : Number])
  (boolE [n : Boolean])
  (plusE [left : Exp] [right : Exp])
  (cndE [test : Exp] [then : Exp] [else : Exp])
  (varE [name : Symbol])
  (let1E [var : Symbol] [value : Exp] [body : Exp] )
  (lamE [var : Symbol] [body : Exp])
  [appE [fun : Exp] [arg : Exp]])
(define-type Value
  (numV [the-number : Number])
  (boolV [the-boolean : Boolean])
  (funV [var : Symbol] [body : Exp][nv : Env]))
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
         [(and (s-exp-symbol?  (first l)) 
               (symbol=? '+ (s-exp->symbol (first l)))) (plusE (parse (second l)) (parse (third l)))]
         [(and (s-exp-symbol?  (first l)) 
               (symbol=? 'if (s-exp->symbol (first l)))) (cndE (parse (second l)) (parse (third l)) (parse (fourth l)))]
         [(and (s-exp-symbol?  (first l)) 
               (symbol=? 'let1 (s-exp->symbol (first l)))) (let1E (s-exp->symbol (first (s-exp->list (second l))))
                                                            (parse (second (s-exp->list (second l))))
                                                            (parse (third l)))]
         [(and (s-exp-symbol?  (first l)) 
               (symbol=? 'lam (s-exp->symbol (first l)))) ( lamE (s-exp->symbol (second l))
                                                           (parse (third l)))]
         ;;不知道怎样判断一个s表达式是函数调用，只能用这个办法
         [#t (appE (parse (first l)) (parse (second l)))]) )]
    [#t (error 's "parse error")]))
;(interp: (Exp Env -> Value))
(define (interp e nv)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(lamE v b) (funV v b nv)]
    [(appE f a) (let ([fv (interp f nv)]
                      [av (interp a nv)])
                  (type-case Value fv
                  [(funV v b nv) (interp b (extend nv v av))]
                  [else (error 'app "didn't get a function")])) ]
    [(let1E var val body)
     (let ([new-env (extend nv var (interp val nv))])
       (interp body new-env))]
    [(cndE c t e) (if (boolean-decision (interp c nv))
                     (interp t nv)
                     (interp e nv))]))
(define (run s)
  (interp (parse s) mt-env))
(define f1  `{{lam x {+ x x}} 5} )
(define f2 `{let1 {f {lam x {+ x x}}}{f 3}})
(define f3 `{let1 {x 3}{let1 {f {lam y {+ x y}}}{f 3}}})
(define f4 `{{let1 {y 3} {lam y {+ y 1}}} 5})
(test (parse f2) (let1E 'f (lamE 'x (plusE (varE 'x) (varE 'x)))(appE (varE 'f) (numE 3))))
(test (parse f3) (let1E 'x (numE 3)(let1E 'f (lamE 'y (plusE (varE 'x) (varE 'y)))
(appE (varE 'f) (numE 3)))))
(test (run f2) (numV 6))
(test (run f3) (numV 6))
(test (interp (appE (let1E 'x (numE 3)(lamE 'y (plusE (varE 'x) (varE 'y))))
(numE '4))mt-env)(numV 7))
(test (run f4) (numV 6) )

