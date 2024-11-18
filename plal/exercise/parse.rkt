#lang plait
(define-type Exp (num [n : Number]) (plus [left : Exp] [right : Exp]))
(define (parse s)
  (cond
    [(s-exp-number? s) (num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=?
            '+
            (if (s-exp-symbol? (first l)) (s-exp->symbol (first l)) (error 'parse "not a symbol")))
           (plus (parse (second l)) (parse (third l)))
           (error 'parse "list not an addition")))]))
(define (calc e)
  (type-case Exp e [(num n) n] [(plus l r) (+ (calc l) (calc r))]))
(define (run s)
  (calc (parse s)))
;test
(test (parse `1) (num 1))
(test (parse `2.3) (num 2.3))
(test (parse `{+ 1 2}) (plus (num 1) (num 2)))
(test (parse `{+ 1 {+ {+ 2 3} 4}}) (plus (num 1) (plus (plus (num 2) (num 3)) (num 4))))
(test/exn (parse `{1 + 2}) "not a symbol")
(test (run `1) 1)
(test (run `2.3) 2.3)
(test (run `{+ 1 2}) 3)
(test (run `{+ {+ 1 2} 3}) 6)
(test (run `{+ 1 {+ 2 3}}) 6)
(test (run `{+ 1 {+ {+ 2 3} 4}}) 10)
