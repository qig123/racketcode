#lang racket
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define N 16)
(define (successor n)
  (cond
    [(is-zero? n) (cons 1 '())]
    [(= (+ 1 (car n)) N) (cons 0 (successor (cdr n)))]
    [#t (cons (+ 1 (car n)) (cdr n))]))
(define predecessor
  (lambda (n)
    (cond
      [(is-zero? n) '()]
      [(< (- (car n) 1) 0) (cons (- N 1) (predecessor (cdr n)))]
      [#t (if (and (= 0 (- (car n) 1)) (null? (cdr n))) (cdr n) (cons (- (car n) 1) (cdr n)))])))
(define plus (lambda (x y) (if (is-zero? x) y (successor (plus (predecessor x) y)))))
(define mul (lambda (x y) (if (is-zero? y) (zero) (plus x (mul x (predecessor y))))))
(define fact (lambda (x) (if (is-zero? x) (successor (zero)) (mul x (fact (predecessor x))))))
(define print-n
  (lambda (n)
    (if (null? n)
        (display "end\n")
        (begin
          (print-n (predecessor n))
          (display (predecessor n))
          (display "\n")))))

;;client test

(predecessor '(1 1))
(define one (successor (zero)))
(define two (successor one))
(define three (successor two))
(define four (successor three))
(define five (plus two three))
(define ten (mul five two))
(define num20 (mul five four))
;(plus  num20 one)
;(fact ten)
;(define num100 (mul ten ten))
