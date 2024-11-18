#lang racket
(define point%
  (class object%
    (display "point% before super-new\n")
    (super-new)
    (init x y)
    (display "point% init x y \n")
    (define x-coord x)
    (define y-coord y)
    ;(print x-coord)
    (define/public (get-x) x-coord)
    (define/public (get-y) y-coord)
    (define/public (dist-from-origin)
      (sqrt (+ (* x-coord x-coord)
               (* y-coord y-coord))))
    (define/public (dist-from-origin2)
      (sqrt (+ (* (get-x) (get-x))
               (* (get-y)(get-y)))))
    (define/public (->string)
      (begin
      (display "->string start in point")
      (string-append "(" (number->string (get-x)) "," (number->string (get-y)) ")")))
    ))
(define color-point%
  (class point%
    (super-new)
    (init [color "black"])
    (define current-color color)
    (define/public (get-color) current-color)
    (define/public (set-color! c) (set! current-color c))
    (define/override (->string)
      (string-append (get-color) ":" (super ->string)))))
;;
(define p (new point% [x 1] [y 1]))
(define cp (new color-point% [x 1] [y 1] [color "red"]))
; is-a? is generally "poor OOP style" (interact with objects by sending messages),
; but helps understand subclassing
(define y1 (is-a? p  object%))
(define y2 (is-a? p  point%))
(define n1 (is-a? p  color-point%))
(define y3 (is-a? cp object%))
(define y4 (is-a? cp point%))
(define y5 (is-a? cp color-point%))
(define print-at-build-point%
  (class point%
    (display "print-at-build-point% before super-new\n")
    (super-new)
    (display "print-at-build-point% after super-new\n")
    (inherit ->string)
    (print (->string))))
;(define print-p (new print-at-build-point% [x 2][y 4]))
;(string-append "x" "y")
;(print "x")
(define color-print-at-build-point-broken%
  (class print-at-build-point%
    (display "color-print-at-build-point-broken% before super-new\n")
    (super-new)
    (display "color-print-at-build-point-broken% after super-new\n")
    (init [color "black"])
    (define current-color color)

    (define/public (get-color) current-color)
    (define/public (set-color! c) (set! current-color c))

    (define/override (->string)
      (begin
      (display "->string start in color-print-at-build-point-broken ") 
      (string-append (get-color) ":" (super ->string))))))
;(define broken (new color-print-at-build-point-broken% [x 2][y 4]))




