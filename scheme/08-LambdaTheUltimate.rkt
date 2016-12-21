;;8. Lambda the Ultimate
#lang racket

(require "05-OhMyGawdItsFullOfStars.rkt")

(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f1 test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define insertL1
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) old)
       (cons new l))
      (else (cons (car l)
                  (insertL1 new old (cdr l)))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                  ((insertR-f test?) new old (cdr l))))))))

(define insert-gg
  (lambda (test? kk)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old)
         (kk new old (cdr l)))
        (else (cons (car l)
                    ((insert-gg test? kk) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-LL (insert-gg equal? seqL))
(define insert-RR (insert-gg equal? seqR))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
(define inserL2
  (insert-g
   (lambda (new old l)
     (cons (new (cons old l))))))

(define subst
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((eq? (car l) old)
       (cons new (cdr l)))
      (else (cons (car l)
                  (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst1 (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) +)
      ((eq? x (quote *)) *)
      (else expt))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c (quote tuna)))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote())))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat)
                                   seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))
       
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat left right)
                           (col (cons new (cons oldL newlat))
                                (+ 1 left) right))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat left right)
                           (col (cons oldR (cons new newlat))
                                left (+ 1 right)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat left right)
                                (col (cons (car lat) newlat)
                                     left right)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))


(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))


(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col (quote()) 1 0))
       ((atom? (car l))
        (cond
          ((even? (car l))
           (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col (cons (car l) newl)
                                  (* (car l) p) s))))
          (else (evens-only*&co (cdr l)
                                (lambda (newl p s)
                                  (col newl p
                                       (+ (car l) s)))))))
       (else (evens-only*&co (car l)
                             (lambda (al ap as)
                               (evens-only*&co (cdr l)
                                               (lambda (dl dp ds)
                                                 (col (cons al dl)
                                                      (* ap dp)
                                                      (+ as ds))))))))))


(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))