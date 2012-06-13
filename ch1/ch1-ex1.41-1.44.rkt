#lang racket

;; ex-1.41
(define inc add1)
(define (double f)
  (lambda (x)
    (f (f x))))


(((double (double double)) inc) 5)
;; ((double (double double)) inc) ->  (double (double (double (double inc)))) -> inc 2^4次

;; ex-1.42
(define (square x) (* x x))
(define (compose f g)
  (lambda (x)
    (f (g x))))
((compose square inc) 6)

;; ex-1.43
(define (repeated f times)
  (if (= times 0)
      (lambda (x) x)
      (compose f (repeated f (- times 1)))))

(define (repeated-iter f times)  ;; iterative version
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (compose f result))))
  (iter times (lambda (x) x)))

((repeated square 2) 5)
((repeated-iter square 2) 5)

;; ex-1.44
(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
(define (n-fold-smooth f n)
  (repeated (smooth f) n))

((n-fold-smooth sin 2) (/ 3.1415926 6))
((n-fold-smooth (lambda (x) x) 3) 1)
