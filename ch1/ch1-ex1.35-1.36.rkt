#lang racket

;; ex-1.35 1.36
(define (fixed-point f start)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (define (iter start)
    (define next (f start))
    (if (close-enough? start next)
        start
        (iter next)))
  (iter start))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 0.5)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 6)