#lang racket

(require "./config.rkt")
(provide parse to-strings)

(define (to-strings input separator)
  (define separator-tag (if (eq? separator newline) newline-tag separator))
  (define skip-separator (if (eq? separator newline) skip-newline cdr))
  (let loop ((char-list input) (output '()) (current-string ""))
    (cond ((null? char-list) (append output (list current-string)))
          ((eq? (car char-list) separator-tag) ; separator
           (loop (skip-separator char-list)
                 (append output (list current-string))
                 ""))
          (else
           (loop (cdr char-list)
                 output
                 (string-append current-string (string (car char-list))))))))

(define (format-data format input)
  (define f (if (null? format)
                (λ (input) input)
                (λ (input) ((car format) input (cadr format)))))
  (f (port->list (λ (next) (read-char next)) input)))

(define (parse file-path . format)
  (define input (open-input-file file-path))
  (define formated-data (format-data format input))
  (close-input-port input)
  formated-data)
