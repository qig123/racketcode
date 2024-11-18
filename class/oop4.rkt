#lang racket
(define fish-interface (interface () get-size grow eat))
(define fish% (class* object% (fish-interface)
  (init size);
  (define current-size size);field
  (super-new);superclass initialization
  (define/public (get-size)
    current-size);method decl
  (define/public (grow amt)
    (set! current-size (+ amt current-size)))
  (define/public (eat other-fish)
    (grow (send other-fish get-size)))
  ))
;
(define hungry-fish2 (class fish% (super-new)
                      (define/public (eat-more fish1 fish2)
                        (send this eat fish1)
                        (send this eat fish2))
                      ))
(define hungry-fish (class fish% (super-new)
                      (inherit eat)
                      (define/public (eat-more fish1 fish2)
                        (eat fish1)
                        (eat fish2))
                      ))
(define defalult-10-fish (class fish%
                           (init [size 10])
                           (super-new [size size])))
(define charlie (new fish% [size 10]))


;call object method
(send charlie grow 6)
(send charlie get-size)