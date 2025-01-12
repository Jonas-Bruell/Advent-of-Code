#lang racket

(provide (all-defined-out))

(define newline (list #\return #\newline))
(define newline-tag (if (list? newline) (car newline) newline))
(define skip-newline (if (list? newline) cddr cdr))
