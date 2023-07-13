#lang racket
;(class [类名] [父类] [构造器/初始化参数][字段定义][方法定义])

(define rational%
  (class object%
    (super-new)
    (init n) ;no defalut, argument mandatory
    (init [d 1]) ;default, argument optional
    ;initialize private state using initialization arguments
    (define denominator
      (cond
        [(= d 0) (error "rational% received zero denominator")]
        [(< d 0) (- d)]
        [#t d]))
    (define numerator (if (< d 0) (- n) n))
    ;private  "method" (well, private "field holding a function")
    (define (reduce!)
      (if (= numerator 0)
          (set! denominator 1)
          (let ([g (gcd numerator denominator)])
            (set! numerator (/ numerator g))
            (set! denominator (/ denominator g)))))
    (reduce!) ;call？
    ;public methods, including getters(needed for add!)
    (define/public (get-numerator) numerator)
    (define/public (get-denominator) denominator)
    (define/public (add! other)
      (let ([n2 (send other get-numerator)] [d2 (send other get-denominator)])
        (begin
          (set! numerator (+ (* n2 denominator) (* d2 numerator)))
          (set! denominator (* denominator d2))
          (reduce!))))
    (define/public (->string)
      (if (= denominator 1)
          (number->string numerator)
          (string-append (number->string numerator) "/" (number->string denominator))))
    ;;methods can call other methods on this
    (define/public (whole?1) (= denominator 1))
    (define/public (whole?2) (= (send this get-denominator) 1))
    ;syntactic sugar for send to this
    (define/public (whole?3) (= (get-denominator) 1))
    (define/public (double!) (send this add! this))
    (define/public (double!2) (add! this))
    ;todo
    ))
(define (r+ r1 r2)
  (let ([ans (new rational% [n (send r1 get-numerator)] [d (send r1 get-denominator)])])
    (begin
      (send ans add! r2)
      ans)))

(define r1 (new rational% [n 7]))
(define r2 (new rational% [d 4] [n 9]))
(define s (send (r+ (r+ r1 r2) (new rational% [n 1] [d 4])) ->string)) ; "19/2"
;a couple subclass to show inheritance and the modest behavior of inherit
(define subrational%
  (class rational%
    (super-new)
    (define/public (quadruple!)
      (send this double!)
      (send this double!))
    (define/public (whole?4) (send this whole?3))
    (define/public (whole?5) (send this get-denominator))
    ;; fine method definition that fails when called
    (define/public (oops) (send this woiufoiewf))
    ;; does not work because state is private
    ;turns out denominator
    ; is a standard library function we get instead
    (define/public (whole?6-broken) (= denominator 1))
    ;编译错误
    ;(define/public (whole?7)
    ; (whole?3))
    ;;todo
    ))
(define r2_1 (new subrational% [n 7]))
;(send r2_1 whole?6-broken )
