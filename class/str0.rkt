#lang racket
;自定义数据类型
(struct posn (x y) #:transparent)
(struct 3d-posn posn (z) #:transparent)
(define p1 (posn 1 2))
(define p2 (struct-copy posn p1 [x 3]))
(define p3 (3d-posn 1 2 3))