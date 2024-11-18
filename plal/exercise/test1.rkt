#lang racket
(define (g s)
  (+ 1 (or (string->number s) 0)))
(g "s")