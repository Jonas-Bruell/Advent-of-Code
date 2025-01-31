#lang racket

(require "../parser.rkt" "../config.rkt")
(define list-of-packages (parse "./Day-2.data.txt" to-strings newline))

(define (higher-order-loop list-of-packages function)
  (let loop ((packages list-of-packages) (sum 0))
    (if (null? packages)
        sum
        (let* ((package (map (λ (x) (string->number x))
                             (string-split (car packages) "x")))
               (l (car package)) (w (cadr package)) (h (caddr package)))
          (loop (cdr packages)
                (function package sum l w h))))))

; https://adventofcode.com/2015/day/2
(define (calculate-square-feet-of-wrapping-paper list-of-packages)
  (higher-order-loop
   list-of-packages
   (λ (package sum l w h)
     (+ sum (+ (* 2 l w) (* 2 w h) (* 2 h l)
               ((λ (x) (* (car x) (cadr x))) (remove (max l w h) package)))))))

(calculate-square-feet-of-wrapping-paper list-of-packages)

; https://adventofcode.com/2015/day/2#part2
(define (calculate-feet-of-ribbon list-of-packages)
  (higher-order-loop
   list-of-packages
   (λ (package sum l w h)
     (+ sum (* l w h)
        ((λ (x) (* 2 (+ (car x) (cadr x)))) (remove (max l w h) package))))))

(calculate-feet-of-ribbon list-of-packages)
