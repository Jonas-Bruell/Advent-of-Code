#lang racket

(provide parse)

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
