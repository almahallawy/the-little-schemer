#lang racket
;;The Little Schemer
;;1. Toys

(provide atom?)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))