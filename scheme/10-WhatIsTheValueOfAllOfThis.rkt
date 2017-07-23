#lang racket

;entry
;'((appetizer entree beverage)
; (pate boeuf vin))
;
;'((appetizer entree beverage)
;  (beer beer beer))
;
;'((beverage dessert)
;  ((food is) (number one with us)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

;(build '(appetizer entree beverage) '(pate boeuf vin))
;(build '(appetizer entree beverage) '(beer beer beer))
;(build '(beverage dessert) '((food is) (number one with us)))

(define new-entry build)

;(new-entry '(appetizer entree beverage) '(pate boeuf vin))
;(new-entry '(appetizer entree beverage) '(beer beer beer))
;(new-entry '(beverage dessert) '((food is) (number one with us)))


(define locate
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) 1)
      (else (add1 (locate a (cdr lat)))))))

;(locate 5 '(1 2 3 4 5))

(define lookup-in-entry_v1
  (lambda (name entry entry-f)
    (cond
      ((null? (first entry)) (entry-f name))
      ((eq? (car (first entry)) name)
       (car (second entry)))
      (else (lookup-in-entry_v1 name (build (cdr (first entry))
                                            (cdr (second entry)))
                                entry-f)))))


;(lookup-in-entry_v1 'entree '((appetizer entree beverage)(pate boeuf vin)) eq?)


(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
       (lookup-in-entry-help name
                             (cdr names)
                             (cdr values)
                             entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

;(lookup-in-entry 'beverage '((appetizer entree beverage)(pate boeuf vin)) eq?)


;Table or Environment
;'(((appetizer entree beverage)
; (pate boeuf vin))
;((beverage dessert)
;  ((food is) (number one with us))))

(define extend-tablel1
  (lambda (entry tablel)
    (cons entry tablel)))

;(extend-tablel1 '((appetizer entree beverage)(pate boeuf vin)) '())

(define extend-table cons)

;(extend-table '((appetizer entree beverage)(pate boeuf vin)) '())

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             (lambda (name)
                               (lookup-in-table name (cdr table) table-f)))))))



;(quote (a b c))
;
;(car (quote (a b c)))
;
;(cons 'a
;      (cons 'b
;            (cons 'c
;                  (quote ()))))
;
;(cons
; (cons 'a
;       (cons 'b
;             (cons 'c
;                   (quote ()))))
; (quote()))
;
;(cons 'car
;      (cons (cons (quote quote)
;                  (cons
;                   (cons 'a
;                         (cons 'b
;                               (cons 'c
;                                     (quote ()))))
;                   (quote ())))
;            (quote ())))
;
;(quote (car (quote (a b c))))
;
;
;((lambda (nothing)
;   (cons nothing (quote ())))
; (quote
;  (from nothing comes something)))
;
;((lambda (nothing)
;   (cond
;     (nothing (quote something))
;     (else (quote nothing))))
; #t)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))


(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(meaning
 '(lambda (x) (cons x y))
 '(((y z) ((8) 9))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)
(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

;(define *cond
;  (lambda (e table)
;    (evcon (cdr e) table)))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(*cond
 '(cond (coffee klatsch) (else party)) 
 '(((coffee) (t))
   ((klatsch party) (5 ((6))))))

(lookup-in-table 'coffee '(((coffee) (t))
                           ((klatsch party) (5 ((6)))))
                 initial-table)

(lookup-in-table 'klatsch '(((coffee) (t))
                            ((klatsch party) (5 ((6)))))
                 initial-table)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (applyz
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

;;Can not use "apply". Already available.
(define applyz
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun)  vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive)) #t)
      (else #f))))

(:atom? '(primitive car))

(:atom? '(non-primitive
	  ((((y z) ((8) 9))) (x) (cons x y))))


;;**************(car '(a b c))
(function-of '(car (quote (a b c))))
;car

(meaning 'car '())
;(primitive car)

(arguments-of '(car (quote (a b c))))
;((quote (a b c)))

(evlis '((quote (a b c))) '())
;((a b c))

(*application '(car (quote (a b c))) '())
;a

;;***********(cons 'a '(b))
(cons 'a '(b))

(function-of '(cons 'a '(b)))
;'cons

(meaning 'cons '())
;'(primitive cons)

(arguments-of '(cons 'a '(b)))
;'('a '(b))

(evlis '('a '(b)) '())
;'(a (b))

(*application '(cons 'a '(b)) '())
;'(a b)

;;******************(car (cdr (quote (a b c))))
(function-of '(car (cdr (quote (a b c)))))

(meaning 'car '())
;(primitive car)

(arguments-of '(car (cdr (quote (a b c)))))
;((cdr (quote (a b c))))

(evlis '((cdr (quote (a b c)))) '())
;((b c))

(*application '(car (cdr (quote (a b c)))) '())
;b

;;**********(atom? car)
(atom? car)
;#t
(function-of '(atom? car))
;atom?
(meaning 'atom? '())
;(primitive atom\?)

(arguments-of '(atom? car))
;(car)

(evlis '(car) '())
;((primitive car))

(first '((primitive car)))
;'(primitive car)

(:atom? '(primitive car))
;#t
(apply-primitive 'atom?
                 '((primitive car)))
;#t`
(applyz '(primitive atom?)
        '((primitive car)))
;#t

(*application '(atom? car) '())

;;**************(lambda (x y) (cons x y))
(lambda (x y) (cons x y))

(meaning '(lambda (x y) (cons x y)) '())
;'(non-primitive (() (x y) (cons x y)))

(atom? (lambda (x y) (cons x y)))
;#t

(function-of '(atom? (lambda (x y) (cons x y))))
;atom?
(meaning 'atom? '())
;(primitive atom\?)

(arguments-of '(atom? (lambda (x y) (cons x y))))
;'((lambda (x y) (cons x y)))

(evlis '((lambda (x y) (cons x y))) '())
;'((non-primitive (() (x y) (cons x y))))

(first '((non-primitive (() (x y) (cons x y)))))
;'(non-primitive (() (x y) (cons x y)))

(:atom? '(non-primitive (() (x y) (cons x y))))
;#t
(apply-primitive 'atom?
                 '((non-primitive (() (x y) (cons x y)))))
;#t
(applyz '(primitive atom?)
        '((non-primitive (() (x y) (cons x y)))))
;#t

(*application '(atom? (lambda (x y) (cons x y))) '())
;#t

;;************(first p)

;;------------------------------------------
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))

;;(cons x y)
;; table = (((x y) (1 (2))))
(function-of '(cons x y))
;cons

(meaning 'cons '(((x y) (1 (2)))))
;(primitive cons)

(arguments-of '(cons x y))
;(x y)

(meaning 'x '(((x y) (1 (2)))))
;1

(meaning 'y '(((x y) (1 (2)))))
;(2)

(evlis '(x y) '(((x y) (1 (2)))))
;(1 (2))

(*application '(cons x y) '(((x y) (1 (2)))))
;(1 2)

(meaning '(cons x y) '(((x y) (1 (2)))))
;(1 2)

(cons 1 '(2))
;(1 2)

;;-------------
;;---(cons z x) , z = 6, x = (a b c)
(meaning '(cons z x) '(((x y)
			((a b c) (d e f)))
		       ((u v w)
			(1 2 3))
		       ((x y z)
			(4 5 6))))
;(6 a b c)

(function-of '(cons z x))
;cons

(arguments-of '(cons z x))
;(z x)

(meaning 'z '(((x y)
	       ((a b c) (d e f)))
	      ((u v w)
	       (1 2 3))
	      ((x y z)
	       (4 5 6))))
;6

(meaning 'x '(((x y)
	       ((a b c) (d e f)))
	      ((u v w)
	       (1 2 3))
	      ((x y z)
	       (4 5 6))))
;(a b c)

(evlis '(z x) '(((x y)
		 ((a b c) (d e f)))
		((u v w)
		 (1 2 3))
		((x y z)
		 (4 5 6))))
;(6 (a b c))

(meaning 'cons '(((x y)
		  ((a b c) (d e f)))
		 ((u v w)
		  (1 2 3))
		 ((x y z)
		  (4 5 6))))
;(primitive cons)

(apply-primitive 'cons '(6 (a b c)))
;(6 a b c)

(applyz '(primitive cons) '(6 (a b c)))
;(6 a b c)

;;-----------------

(value '((lambda (x y)(cons x y)) 1 '(2)))

;From:  https://github.com/pkrumins/the-little-schemer/blob/master/10-value-of-all-of-this.ss
(value '(add1 6))                           ; 7
(value '(quote (a b c)))                    ; '(a b c)
(value '(car (quote (a b c))))              ; 'a
(value '(cdr (quote (a b c))))              ; '(b c)
(value
  '((lambda (x)
      (cons x (quote ())))
    (quote (foo bar baz))))                 ; '((foo bar baz))
(value
  '((lambda (x)
      (cond
        (x (quote true))
        (else
          (quote false))))
    #t))                                    ; 'true
