#lang racket

(require "../parser.rkt" "../config.rkt")
(define list-of-strings (parse "./Day-5.data.txt" to-strings newline))

(define (check-string? string)
  (let loop ((s string) (vowels 0) (twice-in-row 0) (last-letter "?"))
    (cond ((equal? s "?") (and (> vowels 2) (> twice-in-row 0)))
          ((or (member (substring s 0 2) '("ab" "cd" "pq" "xy"))) #f)
          (else (loop (substring s 1 (string-length s))
                      (if (member (substring s 0 1) '("a" "e" "i" "o" "u"))
                          (+ vowels 1)
                          vowels)
                      (if (equal? (string-ref s 0) last-letter)
                          (+ twice-in-row 1)
                          twice-in-row)
                      (string-ref s 0))))))

; https://adventofcode.com/2015/day/5
(define (check-for-nice-strings list-of-strings)
  (let loop ((strings (map (λ (x) (string-append x "?")) list-of-strings))
             (nice-strings 0))
    (cond ((null? strings) nice-strings)
          ((check-string? (car strings))
           (loop (cdr strings) (+ nice-strings 1)))
          (else
           (loop (cdr strings) nice-strings)))))

(check-for-nice-strings list-of-strings) ; 258
