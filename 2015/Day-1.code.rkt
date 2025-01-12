#lang racket

(require "../parser.rkt")
(define confusing-directions (parse "./Day-1.data.txt"))

; https://adventofcode.com/2015/day/1
(define (santa-floor-calculator confusing-directions)
  (let loop ((directions confusing-directions) (floor 0))
    (cond ((null? directions) floor)
          ((eq? #\( (car directions)) (loop (cdr directions) (+ floor 1)))
          ((eq? #\) (car directions)) (loop (cdr directions) (- floor 1))))))

(santa-floor-calculator confusing-directions) ; 138

; https://adventofcode.com/2015/day/1#part2
(define (santa-first-time-in-basement-calculator confusing-directions)
  (let loop ((directions confusing-directions) (floor 0) (position 0))
    (cond ((null? directions) position)
          ((< floor 0) position)
          ((eq? #\( (car directions)) (loop (cdr directions) (+ floor 1) (+ position 1)))
          ((eq? #\) (car directions)) (loop (cdr directions) (- floor 1) (+ position 1))))))

(santa-first-time-in-basement-calculator confusing-directions) ; 1771