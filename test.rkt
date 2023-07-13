#lang racket
(require rackunit)


(define (add x y)
  (+ x y))
(define test
(test-suite
  "An example suite"
  #:before (lambda () (display "Before"))
  #:after  (lambda () (display "After"))
  (test-case
    "An example test"
    (check-eq? 1 1))
  (test-suite "A nested test suite"
    (test-case "Another test"
      (check < 1 2))))
  )
(require rackunit/text-ui)
(run-tests test)