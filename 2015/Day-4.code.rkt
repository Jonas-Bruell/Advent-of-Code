#lang racket

(require "../parser.rkt" "../config.rkt" openssl/md5)
(define secret-key-file (parse "./Day-4.data.txt" to-strings newline))

; https://adventofcode.com/2015/day/4
(define (adventcoins-miner secret-key compare)
  (let loop ((index 0))
    (let* ((hash (md5 (open-input-string
                       (string-append secret-key (number->string index)))))
           (first-five-numbers-hash (substring hash 0 (string-length compare))))
      (if (equal? first-five-numbers-hash compare)
          index
          (loop (+ index 1))))))

; secret key is first element of list
(adventcoins-miner (car secret-key-file) "00000") ; 346386

; https://adventofcode.com/2015/day/4#part2
(adventcoins-miner (car secret-key-file) "000000") ; 9958218