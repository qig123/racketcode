#lang racket
(require "ch11.rkt")
(require rackunit)
(define tests
  (test-suite "ch11 Tests"
    (check-equal? (two-in-a-row? '(a b c c d ) )
                  #t
                  "two-in-a-row? test")
   
    ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)