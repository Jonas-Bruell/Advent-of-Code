#lang racket

(require "../parser.rkt" "../config.rkt" openssl/md5)
(define secret-key-file (parse "./Day-4.data.txt" to-strings newline))

; https://adventofcode.com/2015/day/4
(define (adventcoins-miner secret-key)
  (let loop ((index 0))
    (let* ((hash (md5 (open-input-string
                       (string-append secret-key (number->string index)))))
           (first-five-numbers-hash (substring hash 0 5)))
      (if (equal? first-five-numbers-hash "00000")
          index
          (loop (+ index 1))))))

; secret key is first element of list
(adventcoins-miner (car secret-key-file)) ; 346386