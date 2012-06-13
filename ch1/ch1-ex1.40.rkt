#lang racket
;; ex-1.40
(define (square x) (* x x))
(define (cube x) (* x x x))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (fixed-point f start)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (iter start)
    (define next (f start))
    (if (close-enough? start next)
        start
        (iter next)))
  (iter start))
(define (newton-transform f)
  (lambda (x) 
    (- x (/ (f x)
            ((deriv f) x)))))

(define (fixed-point-of-transform f transform start)
  (fixed-point (transform f) start))

(define (newtons-method f start)
  (fixed-point-of-transform f newton-transform start))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; evaluation

(newtons-method (cubic 1 -1 -1) 2)