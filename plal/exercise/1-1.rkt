#lang plait
(define-type Exp
  (num [n : Number])
  (plus [left : Exp] [right : Exp]))
(define (calc e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (calc l) (calc r))]))
;;test
(print-only-errors #true)
(test (calc (num 1)) 1)
(test (calc (num 2.3)) 2.3)
(test (calc (plus (num 1) (num 2))) 3)
(test (calc (plus (plus (num 1) (num 2))(num 3)))6)
(test (calc (plus (num 1)(plus (num 2) (num 3))))6)
(test (calc (plus (num 1)(plus (plus (num 2)(num 3))(num 4))))10)
(test (calc (plus (num 0.1) (num 0.2))) 0.3)
;(test (calc (plus (num 0.1) (num 0.2))) 1/3)
(s-exp->number `1)
(s-exp->list `{+ 1 2})
;(s-exp->number `{+ 1 2})
;(s-exp->list `1)
;(s-exp->list 9)
;(symbol=? '+ (s-exp->symbol (first  (list `+ `1 `2)) ))
