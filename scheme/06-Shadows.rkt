#lang racket
(require "04-NumbersGames.rkt")

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((or (eq? (car (cdr aexp)) (quote +))
           (or (eq? (car (cdr aexp)) (quote *))
               (eq? (car (cdr aexp)) (quote ^))))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      (else #f))))


(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered2? (car aexp))
            (numbered2? (car (cdr (cdr aexp)))))))))

;;Airthemtic express in infix notation
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (o+ (value (car nexp))
           (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote *))
       (o* (value (car nexp))
           (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ^))
       (o^ (value (car nexp))
           (value (car (cdr (cdr nexp))))))
      (else (quote())))))

;;Airthemtic expression in Prefix notation
(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) (quote +))
       (o+ (value (car (cdr nexp)))
           (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) (quote *))
       (o* (value (car (cdr nexp)))
           (value (car (cdr (cdr nexp))))))
      (else
       (o^ (value (car (cdr nexp)))
           (value (car (cdr (cdr nexp)))))))))
      
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (o+ (value3 (1st-sub-exp nexp))
           (value3 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *))
       (o* (value3 (1st-sub-exp nexp))
           (value3 (2nd-sub-exp nexp))))
      (else
       (o^ (value3 (1st-sub-exp nexp))
           (value3 (2nd-sub-exp nexp)))))))

;;for infix notation
(define 1st-sub-exp-in
  (lambda (aexp)
    (car aexp)))
;;for infix notation
(define operator-in
  (lambda (aexp)
    (car (cdr aexp))))

;;for infix notation
(define value4
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator-in nexp) (quote +))
       (o+ (value3 (1st-sub-exp-in nexp))
           (value3 (2nd-sub-exp nexp))))
      ((eq? (operator-in nexp) (quote *))
       (o* (value3 (1st-sub-exp-in nexp))
           (value3 (2nd-sub-exp nexp))))
      (else
       (o^ (value3 (1st-sub-exp-in nexp))
           (value3 (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define e+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (e+ n (zub1 m)))))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))