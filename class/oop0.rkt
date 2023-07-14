#lang racket
;; (class superclass-expr  decl or expr)
;;
(define fish%
  (class object%
    (init size) ;
    (define current-size size) ;field
    (super-new) ;?
    (define/public (get-size) current-size) ;
    (define/public (grow amt) (set! current-size (+ amt current-size)))
    (define/public (eat other-fish) (grow (send other-fish get-size)))))
;;
(define charlie (new fish% [size 10]))
(send charlie grow 6)
(send charlie get-size)
(define hungary-fish%
  (class fish%
    (super-new)
    (define/public (eat-more fish1 fish2)
      (send this eat fish1)
      (send this eat fish2))))
;
(define hungry-fish (new hungary-fish% [size 10]))
(define hungary-fish2%
  (class fish%
    (super-new)
    (inherit eat)
    (define/public (eat-more fish1 fish2)
      (eat fish1)
      (eat fish2))))
(define get-fish-size (generic fish% get-size))
(send-generic charlie get-fish-size)
(define pick-fish%
  (class fish%
    (super-new)
    (define/override (grow amt) (super grow (* 3/4 amt)))))
(define daisy (new pick-fish% [size 20]))
(send daisy eat charlie)
(send daisy get-size)
