#lang racket

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define eternity
  (lambda (x)
    (eternity x)))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (a b)
    (cons a (cons b (quote ())))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
      ((null? x) #f)
      ((atom? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (* (weight* (first pora)) 2)
          (weight* (second pora)))))))

(define revpair
  (lambda (p)
    (build (second p)
           (first p))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n)(add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

;length0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

;; apply length0 to empty list
;((lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1 (eternity (cdr l))))))
; (quote ()))


;;length1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           ((lambda (l) ;;length0
              (cond
                ((null? l) 0)
                (else (add1 (eternity (cdr l))))))
            (cdr l))))))


;;(length1 '(1))
;((lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1
;           ((lambda (l) ;;length0
;             (cond
;               ((null? l) 0)
;               (else (add1 (eternity (cdr l))))))
;            (cdr l))))))
; '(1))

;;length2
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           ((lambda (l);; length1
              (cond
                ((null? l) 0)
                (else (add1
                       ((lambda (l) ;;length0
                          (cond
                            ((null? l) 0)
                            (else (add1 (eternity (cdr l))))))
                        (cdr l))))))
            (cdr l))))))

;;(length2 '(1 2))
;((lambda (l)
;   (cond
;     ((null? l) 0)
;     (else (add1
;            ((lambda (l);; length1
;               (cond
;                 ((null? l) 0)
;                 (else (add1
;                        ((lambda (l) ;;length0
;                           (cond
;                             ((null? l) 0)
;                             (else (add1 (eternity (cdr l))))))
;                         (cdr l))))))
;             (cdr l))))))
; '(1 2))

;;length0
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

;;(length0 (quote ()))
;(((lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
; eternity)
; (quote ()))

;;length1
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length) ;;length0
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  eternity))

;(length1 '(1))
;(((lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
; ((lambda (length) ;;length0
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
; eternity))
; '(1))

;;length2
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ;length1
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length) ;length0
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))

;(length2 '(1 2))
;;length2
;(((lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
; ;length1
; ((lambda (length)
;    (lambda (l)
;      (cond
;        ((null? l) 0)
;        (else (add1 (length (cdr l)))))))
;  ((lambda (length) ;length0
;     (lambda (l)
;       (cond
;         ((null? l) 0)
;         (else (add1 (length (cdr l)))))))
;   eternity)))
; '(1 2))

;lenght0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;(length0 (quote ()))
;(((lambda (mk-length)
;  (mk-length eternity))
; (lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l))))))))
; (quote ()))

;length1
((lambda (mk-length)
   (mk-length
    ;;length 0
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;;(length1 '(1))
;(((lambda (mk-length)
;    (mk-length
;     ;;length 0
;     (mk-length eternity)))
;  (lambda (length)
;    (lambda (l)
;      (cond
;        ((null? l) 0)
;        (else (add1 (length (cdr l))))))))
; '(1))

;length2
((lambda (mk-length)
   (mk-length
    ;length1
    (mk-length
     ;length0
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;(length2 '(1 2))
;(((lambda (mk-length)
;   (mk-length
;    ;length1
;    (mk-length
;     ;length0
;     (mk-length eternity))))
; (lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l))))))))
; '(1 2))

;length3
((lambda (mk-length)
   (mk-length
    ;length2
    (mk-length
     ;length1
     (mk-length
      ;length0
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;(length3 '(1 2 3))
;(((lambda (mk-length)
;   (mk-length
;    ;length2
;    (mk-length
;     ;length1
;     (mk-length
;      ;length0
;      (mk-length eternity)))))
; (lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l))))))))
; '(1 2 3))

;length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;;length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

;(length0 (quote ()))
;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (mk-length (cdr l))))))))
; (quote ()))

;length1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length eternity) (cdr l))))))))

;(length1 '(apples))
;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1
;              ((mk-length eternity) (cdr l))))))))
; '(apples))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 
               ((mk-length eternity) (cdr l))))))))
 '(apples))

(((lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1
               ((mk-length eternity) (cdr l)))))))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1
               ((mk-length eternity) (cdr l))))))))
 '(apples))

((lambda (l)
   (cond
     ((null? l) 0)
     (else (add1
            (((lambda (mk-length)
                (lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (add1
                           ((mk-length eternity) (cdr l))))))) eternity)
            (cdr l))))))
 '(apples))

((lambda (l)
   (cond
     ((null? l) 0)
     (else (add1
            ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (add1
                           ((eternity eternity) (cdr l))))))
             (cdr l))))))
 '(apples))


;length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length mk-length)
               (cdr l))))))))

;(length (1 2 3 4 56 7 8 9 0))
;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1
;              ((mk-length mk-length)
;               (cdr l))))))))
; '(1 2 3 4 5 6 7 8 9 0))

;length -> no end to it, because we keep applying mk-length to itself again and again and again..
;((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   ((lambda (length)
;      (lambda (l)
;        (cond
;          ((null? l) 0)
;          (else (add1 (length (cdr l)))))))
;    (mk-length mk-length))))

;length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((lambda (x)
                 ((mk-length mk-length) x))
               (cdr l))))))))


;(length '(1 2 3 4))
;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1
;              ((lambda (x)
;                 ((mk-length mk-length) x))
;               (cdr l))))))))
; '(1 2 3 4))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
 '(1 2))
(quote ==========================================)

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;(length '(1 2 3 4 5 6 7 8 9 0))
(((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
 '(1 2 3 4 5 6 7 8 9 0))

;function that makes length - Y combinator
(lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))

;Y combinator - Applicative-Order Y
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

((Y (lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l))))))))
 '(1 2 3 4 5 6 7 8 9 0 11))

(define pick_1
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick_1 (sub1 n) (cdr lat))))))

(pick_1 7 '(1 2 3 4 5 6 7 8))

(define pick_2
  (Y (lambda (pick)
       (lambda (n lat)
         (cond
           ((zero? (sub1 n)) (car lat))
           (else
            (pick (sub1 n) (cdr lat))))))))

;;This will fail
;;(pick_2 5 '(1 2 3 4 5 6 7 8))

;;https://stackoverflow.com/questions/26117386/why-scheme-requires-apply-in-y-combinator-implementation-but-racket-doesnt

;;Y combinator that accepts function with two arguments
(define Y1
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (a b) ((f f) a b)))))))

(define pick_3
  (Y1 (lambda (pick)
       (lambda (n lat)
         (cond
           ((zero? (sub1 n)) (car lat))
           (else
            (pick (sub1 n) (cdr lat))))))))

(pick_3 8 '(1 2 3 4 5 6 7 8))

;;Y combinator that accepts function with any number of arguments
(define Y2
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (g)
       (f (lambda args (apply (g g) args)))))))

(define pick_4
  (Y2 (lambda (pick)
       (lambda (n lat)
         (cond
           ((zero? (sub1 n)) (car lat))
           (else
            (pick (sub1 n) (cdr lat))))))))

(pick_4 6 '(1 2 3 4 5 6 7 8))
