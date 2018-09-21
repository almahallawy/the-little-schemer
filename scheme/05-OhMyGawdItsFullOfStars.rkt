;;5. *Oh My Gawd*:
;;  It's Full of Stars
#lang racket

(require "04-NumbersGames.rkt")

(define add1
  (lambda (n)
    (+ n 1)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

;; remove all occurrence of atom a from list l
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
               (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new
                          (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old
                            (cdr l))))))
      (else (cons
             (subst* new old (car l))
             (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old
                          (insertL* new old (cdr l)))))
         (else (cons (car l)
                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))


(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

;;eqlist determines if two lists are equal
;;my version
(define eqlist-k?
  (lambda (l1 l2)
    (cond
      ((or (null? l1) (null? l2))
       (and (null? l1) (null? l2)))
      ((or (atom? (car l1)) (atom? (car l2)))
       (and (and (atom? (car l1)) (atom? (car l2)))
            (and (eq? (car l1) (car l2))
                 (eqlist-k? (cdr l1) (cdr l2)))))
      (else (and (eqlist-k? (car l1) (car l2))
                 (eqlist-k? (cdr l1) (cdr l1)))))))

;(eqlist-k? '(1) '())
;(eqlist-k? '() '(1))
;(eqlist-k? '(strawberry ice cream) '(strawberry ice cream))
;(eqlist-k? '(strawberry ice cream) '(strawberry cream ice))
;(eqlist-k? '(banana ((split))) '((banana) (split)))
;(eqlist-k? '(beef ((sausage) (and (soda)))) '(beef ((salami) (and (soda)))))
;(eqlist-k? '(beef ((sausage) (and (soda)))) '(beef ((sausage) (and (soda)))))

(define eqlist1?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2))) #f)
      ((null? l1) #f)
      ((and (atom? (car l1)) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist1? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist1? (car l1) (car l2))
            (eqlist1? (cdr l1) (cdr l2)))))))

;(eqlist1? '(1) '())
;(eqlist1? '() '(1))
;(eqlist1? '(strawberry ice cream) '(strawberry ice cream))
;(eqlist1? '(strawberry ice cream) '(strawberry cream ice))
;(eqlist1? '(banana ((split))) '((banana) (split)))
;(eqlist1? '(beef ((sausage) (and (soda)))) '(beef ((salami) (and (soda)))))
;(eqlist1? '(beef ((sausage) (and (soda)))) '(beef ((sausage) (and (soda)))))


(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist2? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))


;(eqlist2? '(1) '())
;(eqlist2? '() '(1))
;(eqlist2? '(strawberry ice cream) '(strawberry ice cream))
;(eqlist2? '(strawberry ice cream) '(strawberry cream ice))
;(eqlist2? '(banana ((split))) '((banana) (split)))
;(eqlist2? '(beef ((sausage) (and (soda)))) '(beef ((salami) (and (soda)))))
;(eqlist2? '(beef ((sausage) (and (soda)))) '(beef ((sausage) (and (soda)))))

;(provide equal?)
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(1) '())
(eqlist? '() '(1))
(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage) (and (soda)))) '(beef ((salami) (and (soda)))))
(eqlist? '(beef ((sausage) (and (soda)))) '(beef ((sausage) (and (soda)))))


;; remove the first matching S-expression s in l
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))
