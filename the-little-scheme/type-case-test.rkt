#lang plait
(define-type Shape
  (circle [radis : Number])
  (rec [w : Number][h : Number]))
(define (area [s : Shape])
  (type-case Shape s
    [(circle r) (* (* r r) 3.14)]
    [(rec w h) (* w h)]))
(area (circle 1))
(area (rec 2 3))