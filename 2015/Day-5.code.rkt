#lang racket

(require "../parser.rkt" "../config.rkt")
(define list-of-strings (parse "./Day-5.data.txt" to-strings newline))

(define (check-for-nice-strings list-of-strings check?)
  (let loop ((strings (map (λ (x) (string-append x "?")) list-of-strings))
             (nice-strings 0))
    (cond ((null? strings) nice-strings)
          ((check? (car strings))
           (loop (cdr strings) (+ nice-strings 1)))
          (else
           (loop (cdr strings) nice-strings)))))

; https://adventofcode.com/2015/day/5
(define (check-string? string)
  (let loop ((s string) (vowels 0) (twice-in-row 0) (previous-letter "*"))
    (let ((current-letter (substring s 0 1)))
      (cond ((equal? s "?") (and (> vowels 2) (> twice-in-row 0)))
            ((member (substring s 0 2) '("ab" "cd" "pq" "xy")) #f) ;naughty
            (else (loop (substring s 1 (string-length s))
                        (if (member current-letter '("a" "e" "i" "o" "u"))
                            (+ vowels 1)
                            vowels)
                        (if (equal? current-letter previous-letter)
                            (+ twice-in-row 1)
                            twice-in-row)
                        current-letter))))))

(check-for-nice-strings list-of-strings check-string?) ; 258

; https://adventofcode.com/2015/day/5#part2
(define (better-check-string? string)
  (let loop ((s string) (pairs 0) (repeats 0) (pairs-list '()) (prev-pair "++"))
    (let* ((curr-char (substring s 0 1))
           (prev-char (substring prev-pair 1 2))
           (prpr-char (substring prev-pair 0 1))
           (curr-pair (string-append prev-char curr-char)))
      (cond ((equal? s "?") (and (> pairs 0) (> repeats 0)))
            (else (loop (substring s 1 (string-length s))
                        ; I don't like this complicated conditional, but it worked....
                        (cond ((equal? curr-pair prev-pair)
                               (if (member curr-pair (cdr (member curr-pair pairs-list)))
                                   (+ pairs 1)
                                   pairs))
                              ((member curr-pair pairs-list)
                               (+ pairs 1))
                              (else pairs))
                        (if (equal? prpr-char curr-char)
                            (+ repeats 1)
                            repeats)
                        (append (list curr-pair) pairs-list)
                        (string-append prev-char curr-char)))))))

(check-for-nice-strings list-of-strings better-check-string?) ; 53