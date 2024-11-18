#lang racket
;;创建了一个fish类,
( define fish% (class object%
  (init size) 
  (define current-size size) ;;private field
  (super-new);; super-new must be used,because a class must always invoke
  (define/public (get-size)
    current-size)
  (define/public (grow amt)
    (set! current-size (+ amt current-size)))
  (define/public (eat other-fish)
    (grow (send other-fish get-size)))))
;;创建了hungry-fish类
(define hungry-fish% (class fish% (super-new)
                       (inherit eat) ;;介绍了子类调用父类方法的一种方式
                       (define/public (eat-more fish1 fish2)
                       (send this eat fish1)
                       (send this eat fish2))))
;;创建了picky-fish%类
(define picky-fish% (class fish% (super-new)
                      (define/override (grow amt)
                      (super grow (* 3/4 amt)))))
;;使用new关键字创建instance
(define charlie (new fish% [size 10]))
(define daisy (new picky-fish% [size 20]))
;;?
(define get-fish-size (generic fish% get-size))
(send charlie grow 6)
(send charlie get-size)
(send-generic charlie get-fish-size)
(send daisy eat charlie)
(send daisy get-size)

