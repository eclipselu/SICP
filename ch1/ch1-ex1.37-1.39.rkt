#lang racket

;; ex-1.37
(define (cont-frac n d k)
  (define (iter i result)
    (let ((next-res (/ (n i) (+ (d i) result))))
      (if (= i 0)
          result
          (iter (- i 1) next-res))))          
  (iter k 0))

(display "ex-1.37 Golden ratio: ")
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)

;; ex-1.38

(define (approx-e k)
  (define (n i) 1)
  (define (d i)
    (cond ((= (remainder (+ i 1) 3) 0) (* 2.0 (/ (+ i 1) 3)))
          (else 1)))
  (+ 2 (cont-frac n d k)))

(display "ex-1.38 Approximation of e: ")
(approx-e 100)

;; ex-1.39

(define (tan-cf x k)
  (/ (- (cont-frac (lambda (i) (- (* x x)))
                   (lambda (i) (+ 1 (* 2 (- i 1))))
                   k))
     x))

(display "ex-1.39 Tangent: ")
(tan-cf (/ 3.1415926 4) 10000)

