;;The Little Schemer
;;2. Do it, Do It Again, and Again, and Again...
#lang racket

(require "01-toys.rkt")

(provide lat?)

(provide member?)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

