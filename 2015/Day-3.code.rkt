#lang racket

(require "../parser.rkt")
(define directions-with-too-much-eggnog (parse "./Day-3.data.txt"))

(define (insert-or-inc new-house visited-list) ; search from start list
  (let loop ((houses-list visited-list) (return-list '()))
    (cond
      ((null? houses-list) (cons new-house return-list))
      ((equal? (car new-house) (caar houses-list))
       (append (cdr houses-list)
               (cons (cons (caar houses-list) (+ (cdar houses-list) 1))
                     return-list)))
      (else (loop (cdr houses-list) (cons (car houses-list) return-list))))))

; https://adventofcode.com/2015/day/3
(define (santas-walk directions-with-too-much-eggnog)
  (let loop ((directions directions-with-too-much-eggnog)
             (santa-x 0) (santa-y 0)
             (visited-list '(((0 . 0) . 1)))) ;'((x . y) . amount-of-visits)
    (if (null? directions)
        (length visited-list)
        (let ((santa-x (cond ((eq? (car directions) #\>) (+ santa-x 1))
                             ((eq? (car directions) #\<) (- santa-x 1))
                             (else santa-x)))
              (santa-y (cond ((eq? (car directions) #\^) (+ santa-y 1))
                             ((eq? (car directions) #\v) (- santa-y 1))
                             (else santa-y))))
          (loop (cdr directions) santa-x santa-y
                (insert-or-inc `((,santa-x . ,santa-y) . 1) visited-list))))))

(santas-walk directions-with-too-much-eggnog) ; 2592

; https://adventofcode.com/2015/day/3#part2
(define (santas-walk-with-robosanta directions-with-too-much-eggnog)
  (let loop ((directions directions-with-too-much-eggnog)
             (santa? #t)
             (santa-x 0) (santa-y 0)
             (robo-x 0) (robo-y 0)
             (visited-list '(((0 . 0) . 1)))) ;'((x . y) . amount-of-visits)
    (if (null? directions)
        (length visited-list)
        (let* ((deliverer-x (if santa? santa-x robo-x))
               (deliverer-y (if santa? santa-y robo-y))
               (x (cond ((eq? (car directions) #\>) (+ deliverer-x 1))
                        ((eq? (car directions) #\<) (- deliverer-x 1))
                        (else deliverer-x)))
               (y (cond ((eq? (car directions) #\^) (+ deliverer-y 1))
                        ((eq? (car directions) #\v) (- deliverer-y 1))
                        (else deliverer-y))))
          (loop (cdr directions)
                (not santa?)
                (if santa? x santa-x)
                (if santa? y santa-y)
                (if santa? robo-x x)
                (if santa? robo-y y)
                (insert-or-inc `((,x . ,y) . 1) visited-list))))))

(santas-walk-with-robosanta directions-with-too-much-eggnog) ; 2360
