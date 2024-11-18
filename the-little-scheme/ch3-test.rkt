#lang racket
(require "ch3.rkt")
(require rackunit)
(define tests
  (test-suite "ch3 Tests"
    (check-equal? (remq 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly) "rember test")
    (check-equal? (remq 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato) "rember test")
    (check-equal? (rember2 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly) "rember test")
    (check-equal? (rember2 'toast '(bacon lettuce and tomato))
                  '(bacon lettuce and tomato)
                  "rember test")
    (check-equal? (firsts '((a b) (c d) (e f))) '(a c e) "firsts test")
    (check-equal? (firsts '()) '() "firsts test")
    (check-equal? (firsts '(((five plums) four) (eleven green oranges) ((no) more)))
                  '((five plums) eleven (no))
                  "firsts test")
    (check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
                  '(ice cream with fudge topping for dessert) "insertR test")
    (check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
                  '(ice cream with topping for dessert) "subst test")
    (check-equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
                  '(vanilla ice cream with chocolate topping) "subst2 test")
    ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
