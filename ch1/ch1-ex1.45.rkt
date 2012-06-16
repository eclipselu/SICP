#lang racket
;; ex-1.45
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f times)
  (if (= times 0)
      (lambda (x) x)
      (compose f (repeated f (- times 1)))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (power a n)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (* a result))))
  (iter n 1))

(define (nth-root x n k)
  (fixed-point ((repeated average-damp k) (lambda (y) (/ x (power y (- n 1)))))
               1.0))       

  