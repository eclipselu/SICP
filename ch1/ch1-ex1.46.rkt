#lang racket
;; ex-1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  (define tolerance 0.0001)
  (define square (lambda (x) (* x x)))
  (define (average a b) (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f guess)
  (define tolerance 0.0001)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve x)
    (f x))
  ((iterative-improve good-enough? improve) 1.0))

(define (average a b)
  (/ (+ a b) 2))
    
(define (sqrt-fixed x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 2.0)
(sqrt-fixed 2.0)
  