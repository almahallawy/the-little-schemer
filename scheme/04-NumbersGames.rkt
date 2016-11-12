#lang racket
;; 4. Numbers Games

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o1+
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (o1+ (add1 n) (sub1 m))))))

(provide o+)
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o1-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o1- (sub1 n) (sub1 m))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))


;;addtup builds a number by totaling all the numbers in a tup
(define addtup
  (lambda(tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

;;builds up a number by adding n up m times (or multiplication)
(provide o*)
(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

;;tup+ add each element of two tups. if one tup is (), the other is the result
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+(cdr tup1) (cdr tup2)))))))

;; compare for greater than
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

;; compare for less than 
(define o<
  (lambda(n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))


;;compare n with m and return true if n=m
(define o=
  (lambda (n m)
    (cond
      ((or (o< n m) (o> n m)) #f) ;;another solution: ((o> n m) #f) ((o< n m) #f)
      (else #t))))

;;calcualte n^m
(provide o^)
(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (o^ n (sub1 m)))))))

;; division
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

;;find the length of lat
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

;;chose item n from lat
(define pick1
  (lambda (n lat)
    (cond
      ((or (null? lat) (zero? n)) (quote()))
      ((zero? (sub1 n)) (car lat))
      (else (pick1 (sub1 n) (cdr lat))))))

;;chose item n from lat as defined in the book
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;;remove item number n from lat
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                   (rempick (sub1 n) (cdr lat)))))))

;;no-nums remove all the numbers from the lat
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat))
               (no-nums (cdr lat)))
              (else (cons (car lat)
                          (no-nums (cdr lat)))))))))

;;all-nums extracts a tup from a lat using all the numbers in the lat
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat))
                        (cons (car lat)
                              (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

;;eqan? is true if its two arguments (a1 and a2) are the same atom
(provide eqan?)
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (o= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))


;;occur counts the number of times an atom a apperas in a lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat) a)
          (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

;;(one? n) is #t if n is 1 and #f otherwise
(define one1?
  (lambda (n)
    (cond
      (else (eqan? n 1)))))

(define one2?
  (lambda (n)
    (cond
      (else (o= n 1)))))

(define one3?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(define one4?
  (lambda (n)
    (eqan? n 1)))

(define one?
  (lambda (n)
    (o= n 1)))

(define rempick1
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick1 (sub1 n)
                            (cdr lat)))))))

      