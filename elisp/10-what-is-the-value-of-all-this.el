;;Chapter 10 What is the Value of All of This?

'((appetirze entree beverage)
 (pate boeuf vin))

'((appetizer entree beverage)
 (beer beer beer))

'((beverage dessert)
  ((food is) (number one with us)))

(defun build (a b)
  (cons a (cons b (quote ()))))

(defun first (p)
  (car p))

(defun second (p)
  (car (cdr p)))

(defun third (l)
  (car (cdr (cdr l))))

(setq new-entry1 (function build))

(funcall new-entry1 '(appetirze entree beverage)
	   ' (pate boeuf vin))

(defun new-entry (names values)
  (cons names (cons values (quote ()))))

(new-entry '(appetirze entree beverage)
	   '(pate boeuf vin))

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name
			(first entry)
			(second entry)
			entry-f))

(defun lookup-in-entry-help (name names values entry-f)
  (cond
   ((null names)
    (funcall entry-f name))
   ((eq (car names) name)
    (car values))
   (t (lookup-in-entry-help name
			    (cdr names)
			    (cdr values)
			    entry-f))))

(lookup-in-entry 'entree
		 '((appetirze entree beverage)
		   (pate boeuf vin)) (function print))

(lookup-in-entry 'dessert
		 '((appetirze entree beverage)
		   (pate boeuf vin))
		 (lambda (name)
		   (cons name '())))

;;Table
'(((appetirze entree beverage)
   (pate boeuf vin))
 ((beverage dessert)
   ((food is) (number one with us))))

;(setq extend-table (function cons))
(defun extend-table (entry table)
  (cons entry table))

(extend-table '((appetizer entree beverage)
		(beer beer beer))
	      '(((appetirze entree beverage)
		 (pate boeuf vin))
		((beverage dessert)
		 ((food is) (number one with us)))))

(setq lexical-binding t)

;;Same trick as evens-only*&co collector in chapter 8
(defun lookup-in-table (name table table-f)
  (cond
   ((null table) (funcall table-f name))
   (t (lookup-in-entry name (car table)
		       (lambda (name)
			 (lookup-in-table name
					  (cdr table)
					  table-f))))))

(lookup-in-table 'entree
		 '(((entree dessert)
		    (spaghetti spumoni))
		   ((appetizer entree beverage)
		    (food tastes good)))
		 (function print))

(quote (a b c))

(car (quote (a b c)))

(cons 'a
      (cons 'b
	    (cons 'c
		  (quote ()))))

(cons
 (cons 'a
       (cons 'b
             (cons 'c
                   (quote ()))))
 (quote()))

(cons 'car
      (cons (cons 'quote
		  (cons
		   (cons 'a
			 (cons 'b
			       (cons 'c
				     (quote ()))))
		   (quote ())))
	    (quote ())))

(quote (car (quote (a b c))))

((lambda (nothing)
   (cons nothing (quote ())))
 (quote
  (from nothing comes something)))

((lambda (nothing)
   (cond
    (nothing (quote something))
    (t (quote nothing))))
 t)

(defun atom? (x)
  (not (listp x)))

(defun expression-to-action (e)
  (cond
   ((atom? e) (atom-to-action e))
   (t (list-to-action e))))

(defun atom-to-action (e)
  (cond
   ((numberp e) (function *const))
   ((eq e t) (function *const))
   ((eq e nil) (function *const))
   ((eq e (quote cons)) (function *const))
   ((eq e (quote car)) (function *const))
   ((eq e (quote cdr)) (function *const))
   ((eq e (quote null)) (function *const))
   ((eq e (quote eq)) (function *const))
   ((eq e (quote atom?)) (function *const))
   ((eq e (quote zerop)) (function *const))
   ((eq e (quote add1)) (function *const))
   ((eq e (quote sub1)) (function *const))
   ((eq e (quote numberp)) (function *const))
   (t (function *identifier))))

(defun list-to-action (e)
  (cond
   ((atom? (car e))
    (cond
     ((eq (car e) (quote quote))
      (function *quote))
     ((eq (car e) (quote lambda))
      (function *lambda))
     ((eq (car e) (quote cond))
      (function *cond))
     ((eq (car e) (quote function))
      (atom-to-action (second e)))
     (t (function *application))))
   (t (function *application))))

(list-to-action '(lambda (x y) (cons x y)))
;*lambda


(list-to-action '(function car))
;*const

(list-to-action '(function first))
;*identifier

(atom-to-action 'car)
;*const

(list-to-action '((lambda (nothing)
		    (cons nothing (quote ())))
		  (quote
		   (from nothing comes something))))
;*application


(list-to-action '((lambda (nothing)
		    (cond
		     (nothing (quote something))
		     (t (quote nothing))))
		  t))
;*application


(defun value (e)
  (meaning e (quote ())))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(defun *const (e table)
  (cond
   ((atom? e)
    (cond
     ((numberp e) e)
     ((eq e t) t)
     ((eq e nil) nil)
     (t (build (quote primitive) e))))
   (t (build (quote primitive) (second e)))))

(*const 'car '())
;(primitive car)

(*const '(function car) '())
;(primitive car)

(defun *quote (e table)
  (text-of e))

;;(setq text-of (function second))
;;Or the following to make it easier to call instead of using funcall 
(defun text-of (p)
  (car (cdr p)))

(defun *identifier (e table)
  (lookup-in-table e table (function initial-table)))

(defun initial-table (name)
  (car (quote ())))

(defun *lambda (e table)
  (build (quote non-primitive)
	 (cons table (cdr e))))

(meaning
 '(lambda (x) (cons x y))
 '(((y z) ((8) 9))))
;(non-primitive ((((y z) ((8) 9))) (x) (cons x y)))



;;Defines as in the book
;;(setq table-of (function first))
;;(setq formals-of (function second))
;;(setq body-of (function third))

;;The following to make it easier to call instead of using funcall 
(defun table-of (l)
  (car l))

(defun formals-of (l)
  (car (cdr l)))

(defun body-of (l)
  (car (cdr (cdr l))))

(defun evcon (lines table)
  (cond
   ((else? (question-of (car lines)))
    (meaning (answer-of (car lines)) table))
   ((meaning (question-of (car lines)) table)
    (meaning (answer-of (car lines)) table))
   (t (evcon (cdr lines) table))))

(defun else? (x)
  (cond
   ((atom? x) (eq x (quote t)))
   (t nil)))

(else? t)
(else? '(a b))

;;(setq question-of (function first))
;;(setq answer-of (function second))

(defun question-of (p)
  (car p))

(defun answer-of (p)
  (car (cdr p)))

(defun *cond (e table)
  (evcon (cond-lines-of e) table))

;;(setq cond-lines-of (function cdr))
(defun cond-lines-of (e)
  (cdr e))

(*cond
 '(cond (coffee klatsch) (t party)) ;;substitute (else party) by (t party)
 '(((coffee) (t))
   ((klatsch party) (5 ((6))))))
;5

(lookup-in-table 'coffee '(((coffee) (t))
			   ((klatsch party) (5 ((6)))))
		 (function initial-table))
;t

(lookup-in-table 'klatsch '(((coffee) (t))
			   ((klatsch party) (5 ((6)))))
		 (function initial-table))
;5

(defun evlis (args table)
  (cond
   ((null args) (quote ()))
   (t (cons (meaning (car args) table)
	    (evlis (cdr args) table)))))


(defun *application (e table)
  (applyz
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

;;(setq function-of (function car))
;;(setq arguments-of (function cdr))

(defun function-of (e)
  (car e))

(defun arguments-of (e)
  (cdr e))

(defun primitive? (l)
  (eq (first l) (quote primitive)))

(defun non-primitive? (l)
  (eq (first l) (quote non-primitive)))

;;cannot use "apply". Already available and will not evaluate. 
(defun applyz (fun vals)
  (cond
   ((primitive? fun)
    (apply-primitive (second fun) vals))
   ((non-primitive? fun)
    (apply-closure (second fun) vals))))

(defun apply-primitive (name vals)
  (cond
   ((eq name (quote cons))
    (cons (first vals) (second vals)))
   ((eq name (quote car))
    (car (first vals)))
   ((eq name (quote cdr))
    (cdr (first vals)))
   ((eq name (quote null))
    (null (first vals)))
   ((eq name (quote eq))
    (eq (first vals) (second vals)))
   ((eq name (quote atom?))
    (:atom? (first vals)))
   ((eq name (quote zerop))
    (zerop (frist vals)))
   ((eq name (quote add1))
    (1+ (first vals)))
   ((eq name (quote sub1))
    (1- (first vals)))
   ((eq name (quote numberp))
    (numberp (first vals)))))

(defun :atom? (x)
  (cond
   ((atom? x) t)
   ((null x) nil)
   ((eq (car x) (quote primitive)) t)
   ((eq (car x) (quote non-primitive)) t)
   (t nil)))

(:atom? '(primitive car))
;t

(:atom? '(non-primitive
	  ((((y z) ((8) 9))) (x) (cons x y))))
;t


;;---Examples
;;(car '(a b c))
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

;;---------------------------
;;(car (cdr (quote (a b c))))
(function-of '(car (cdr (quote (a b c)))))

(meaning 'car '())
;(primitive car)

(arguments-of '(car (cdr (quote (a b c)))))
;((cdr (quote (a b c))))

(evlis '((cdr (quote (a b c)))) '())
;((b c))

(*application '(car (cdr (quote (a b c)))) '())
;;b

;;***********(cons 'a '(b))
(cons 'a '(b))
;(a b)

(function-of '(cons 'a '(b)))
;cons

(meaning 'cons '())
;(primitive cons)

(arguments-of '(cons 'a '(b)))
;((quote a) (quote (b)))

(meaning '(quote a) '())
;a

(meaning '(quote (b)) '())
;(b)

(evlis '((quote a) (quote (b))) '())
;(a (b))

(*application '(cons 'a '(b)) '())
;(a b)

;;---------------------
(atom? (function car))
;t

(atom? (function (lambda (x y) (cons x y))))
;;nil, but in Scheme it is true!!

(function-of '(atom? (function car)))

(meaning 'atom? '())
;(primitive atom?)

(arguments-of '(atom? (function car)))
;((function car))

(evlis '((function car)) '())
;;((primitive car))

(first '((primitive car)))
;(primitive car)

(applyz '(primitive atom?)
	'((primitive car)))
;t

(apply-primitive 'atom?
		 '((primitive car)))
;t

(:atom? (first '((primitive car))))
;t

(*application '(atom? (function car)) '())
;t

;;-----------------------
(lambda (x y) (cons x y))

(meaning '(lambda (x y) (cons x y)) '())
;(non-primitive (nil (x y) (cons x y)))

(atom? (lambda (x y) (cons x y)))
;;nil(false) in elisp, but in scheme it is true.
;;I will implement as Scheme for now

(function-of '(atom? (lambda (x y) (cons x y))))
;atom?

(meaning 'atom? '())
;(primitive atom?)

(arguments-of '(atom? (lambda (x y) (cons x y))))
;((lambda (x y) (cons x y)))

(evlis '((lambda (x y) (cons x y))) '())
;((non-primitive (nil (x y) (cons x y))))

(first '((non-primitive (() (x y) (cons x y)))))
;(non-primitive (nil (x y) (cons x y)))

(:atom? '(non-primitive (() (x y) (cons x y))))
;t

(applyz '(primitive atom?)
        '((non-primitive (() (x y) (cons x y)))))
;t

(apply-primitive 'atom?
                 '((non-primitive (() (x y) (cons x y)))))
;t

(*application '(atom? (lambda (x y) (cons x y))) '())
;t

;;-------

(defun apply-closure (closure vals)
  (meaning (body-of closure)
	   (extend-table
	    (new-entry (formals-of closure)
		       vals)
	    (table-of closure))))

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

;;---------
;;closure
'((((u v w)
    (1 2 3))
   ((x y z)
    (4 5 6)))
  (x y)
  (cons z x)))

;;vals
'((a b c) (d e f))

(body-of '((((u v w)
	     (1 2 3))
	    ((x y z)
	     (4 5 6)))
	   (x y)
	   (cons z x)))
;;(cons z x)

(new-entry
 (formals-of '((((u v w)
		 (1 2 3))
		((x y z)
		 (4 5 6)))
	       (x y)
	       (cons z x)))
 '((a b c) (d e f)))
;((x y) ((a b c) (d e f)))

(extend-table
 (new-entry
  (formals-of '((((u v w)
		  (1 2 3))
		 ((x y z)
		  (4 5 6)))
		(x y)
		(cons z x)))
  '((a b c) (d e f)))
 (table-of '((((u v w)
	       (1 2 3))
	      ((x y z)
	       (4 5 6)))
	     (x y)
	     (cons z x))))
;(((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6)))

'(((x y)
   ((a b c) (d e f)))
  ((u v w)
   (1 2 3))
  ((x y z)
   (4 5 6)))

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


;;-------more examples
(value '((lambda (x y)(cons x y)) 1 '(2)))
;(1 2)

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
    t))                                    ; 'true














