;; Chapter 9.. and Again, and Again, and Again, ...
(setq lexical-binding t)

(defun pick (n lat)
  (cond
   ((zerop (1- n)) (car lat))
   (t (pick (1- n) (cdr lat)))))

(pick 6 '(6 2 4 caviar 5 7 3))

(defun keep-looking (a sorn lat)
  (cond
   ((numberp sorn)
    (keep-looking a (pick sorn lat) lat))
   (t (eq sorn a))))

(keep-looking 'caviar (pick 1 '(6 2 4 caviar 5 7 3)) '(6 2 4 caviar 5 7 3))


(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(looking 'caviar '(6 2 4 caviar 5 7 3))

(looking 'caviar '(6 2 grits caviar 5 7 3))

(looking 'caviar '(7 2 4 7 5 6 3))

(defun eternity (x)
  (eternity x))

(eternity 'a)

(defun first (p)
  (car p))

(defun second (p)
  (car (cdr p)))

(defun build (a b)
  (cons a (cons b (quote ()))))

;(cons 'a (cons (cons 'b (cdr '((a b) (c d)))) (quote ())))

(defun shift (pair)
  (build (first (first pair))
	 (build (second (first pair))
		(second pair))))

(shift '((a b) c))
(shift '((a b) (c d)))

(defun atom? (x)
  (not (listp x)))


(defun a-pair? (pair)
  (cond
   ((null pair) nil)
   ((atom? pair) nil)
   ((null (cdr pair)) nil)
   ((null (cdr (cdr pair))) t)
   (t nil)))

(defun align (pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (align (shift pora)))
   (t (build (first pora)
	     (align (second pora))))))

(align '((a b) (c d)))

(shift '((a b) (c d)))

(align '((a (b c))(d e)))

(shift '((a (b c))(d e)))

(defun length* (pora)
  (cond
   ((atom? pora) 1)
   (t (+ (length* (first pora))
	 (length* (second pora))))))

(length* '((a b) (c d)))

(defun weight* (pora)
  (cond
   ((atom? pora) 1)
   (t (+ (* (weight* (first pora)) 2)
	 (weight* (second pora))))))

(weight* '(a b))
(weight* '((a b) c))
(weight* '(a (b c)))
(weight* '((a b) (c d)))
(weight* '(a (b (c d))))

(defun revpair (pair)
  (build (second pair)
	 (first pair)))

(defun shuffle (pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (shuffle (revpair pora)))
   (t (build (first pora)
		(shuffle (second pora))))))
(shuffle '(a (b c)))
(shuffle '(a b))
(shuffle '((a b)(c d)))

(defun one? (n)
  (zerop (1- n)))

(one? 1)
(one? 5)

(defun even? (n)
  (= (* (/ n 2) 2) n))

(even? 4)
(even? 5)
(even? 0)

(defun C (n)
  (cond
   ((one? n) 1)
   (t (cond
       ((even? n) (C (/ n 2)))
       (t (C (1+ (* 3 n))))))))

(C 0)

(defun A (n m)
  (cond
   ((zerop n) (1+ m))
   ((zerop m) (A (1- n) 1))
   (t (A (1- n)
	 (A n (1- m))))))

(A 1 0)
(A 1 1)
(A 2 2)
(A 1 2)
(A 4 3)


(defun length (l)
  (cond
   ((null l) 0)
   (t (1+ (length (cdr l))))))


;;length0
(lambda (l)
  (cond
   ((null l) 0)
   (t (1+ (eternity (cdr l))))))

;;(length0 (quote()))
((lambda (l)
   (cond
    ((null l) 0)
    (t (1+ (eternity (cdr l))))))
 (quote ()))

;;length1
(lambda (l)
  (cond
   ((null l) 0)
   (t (1+
       ;;length0
       ((lambda (l)
	  (cond
	   ((null l) 0)
	   (t (1+ (eternity (cdr l))))))
	(cdr l))))))

;;(length1 '(1))
((lambda (l)
   (cond
    ((null l) 0)
    (t (1+
	;;length0
	((lambda (l)
	   (cond
	    ((null l) 0)
	    (t (1+ (eternity (cdr l))))))
	 (cdr l))))))
 '(1))

;;length2
(lambda (l)
  (cond
   ((null l) 0)
   (t (1+
       ;;length1
       ((lambda (l)
	  (cond
	   ((null l) 0)
	   (t (1+
	       ;;length0
	       ((lambda (l)
		  (cond
		   ((null l) 0)
		   (t (1+ (eternity (cdr l))))))
		(cdr l))))))
	(cdr l))))))

;;(length2 '(1 2))
((lambda (l)
   (cond
    ((null l) 0)
    (t (1+
	;;length1
	((lambda (l)
	   (cond
	    ((null l) 0)
	    (t (1+
		;;length0
		((lambda (l)
		   (cond
		    ((null l) 0)
		    (t (1+ (eternity (cdr l))))))
		 (cdr l))))))
	 (cdr l))))))
 '(1 2))

;;The following simple two anonymous function helped me  to understand how to call it
;;Anonyomous function that wrap length function.
((lambda (len)
   (lambda (l)
     (funcall len l)))
 (function length))

;;How to call the un-defun/un-define length function.
(funcall
 ((lambda (len)
    (lambda (l)
      (funcall len l)))
  (function length))
 '(1 2 3))

;;length0
((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l)))))))
 (function eternity))

;;(length0 '())
(funcall
 ((lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
  (function eternity))
 (quote ()))

;;length1
((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l)))))))
 ;;length0
 ((lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
  (function eternity)))


;;(length '(1))
(funcall
 ((lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
  ;;length0
  ((lambda (length)
     (lambda (l)
       (cond
	((null l) 0)
	(t (1+ (funcall length (cdr l)))))))
   (function eternity)))
 '(1))

;;length2
((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l)))))))
 ;;length1
 ((lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
  ;;length0
  ((lambda (length)
     (lambda (l)
       (cond
	((null l) 0)
	(t (1+ (funcall length (cdr l)))))))
   (function eternity))))

;;(length2 '(1 2))
(funcall
 ((lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
  ;;length1
  ((lambda (length)
     (lambda (l)
       (cond
	((null l) 0)
	(t (1+ (funcall length (cdr l)))))))
   ;;length0
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (funcall length (cdr l)))))))
    (function eternity))))
 '(1 2))

;;The following simple two anonymous function helped me  to understand how to call it
;;anonymous function that wrap length function
((lambda (mk-length)
   (funcall mk-length (function length)))
 (lambda (length)
   (lambda (l)
     (funcall length l))))

;;how to call this anonymous function
(funcall
 ((lambda (mk-length)
    (funcall mk-length (function length)))
  (lambda (length)
    (lambda (l)
      (funcall length l))))
 '(1 2 3 4))

;;length0
((lambda (mk-length)
   (funcall mk-length (function eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

;;(length0 '())
(funcall
 ((lambda (mk-length)
    (funcall mk-length (function eternity)))
  (lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))
 '())

;;length1
((lambda (mk-length)
   (funcall mk-length
	    (funcall mk-length (function eternity))));length0
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

;;(length1 '(1))
(funcall
 ((lambda (mk-length)
    (funcall mk-length
	     (funcall mk-length (function eternity))));;length0
  (lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))
 '(1))

;;length2
((lambda (mk-length)
   (funcall mk-length
	    (funcall mk-length;;length1
		     (funcall mk-length (function eternity)))));;length0
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

;;(length2 '(1 2))
(funcall
 ((lambda (mk-length)
    (funcall mk-length
	     (funcall mk-length;;length1
		      (funcall mk-length (function eternity)))));;length0
  (lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))
 '(1 2))

;;length3
((lambda (mk-length)
   (funcall mk-length
	    (funcall mk-length;;length2
		     (funcall mk-length;;length1
			      (funcall mk-length (function eternity))))));;length0
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

;;(length3 '(1 2 3))
(funcall
 ;;length3
 ((lambda (mk-length)
    (funcall mk-length
	     (funcall mk-length;;length2
		      (funcall mk-length;;length1
			       (funcall mk-length (function eternity))))));;length0
  (lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))
 '(1 2 3))


;;length0
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall mk-length (cdr l))))))))

;;(length0 '())
(funcall
 ((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall mk-length (cdr l))))))))
 '())


;;length1
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall
	      (funcall mk-length (function eternity))
	      (cdr l))))))))

;;Page: 166. Working out the execrise
;;(length1 '(apple))
(funcall
 ((lambda (mk-length)
    (funcall mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall
	       (funcall mk-length (function eternity))
	       (cdr l))))))))
 '(apple))

(funcall
 ((lambda (mk-length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall
	       (funcall mk-length (function eterntiy))
	       (cdr l)))))))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall
	       (funcall mk-length (function eternity))
	       (cdr l))))))))
 '(apples))


(funcall
 (lambda (l)
   (cond
    ((null l) 0)
    (t (1+ (funcall
	    (funcall (lambda (mk-length)
		       (lambda (l)
			 (cond
			  ((null l) 0)
			  (t (1+ (funcall
				  (funcall mk-length (function eternity))
				  (cdr l)))))))
		     (function eternity))
	    (cdr l))))))
 '(apple))

(funcall
 (lambda (l)
   (cond
    ((null l) 0)
    (t (1+ (funcall
	    (lambda (l)
	      (cond
	       ((null l) 0)
	       (t (1+ (funcall
		       (eternity (function eternity))
		       (cdr l))))))
	    (cdr l))))))
 '(apple))

;;length
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall
	      (funcall mk-length mk-length)
	      (cdr l))))))))

;;(length '(1 2 3 4 5 6))
(funcall
 ((lambda (mk-length)
    (funcall mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall
	       (funcall mk-length mk-length)
	       (cdr l))))))))
 '(1 2 3 4 5 6 ))


;;extract (funcall mk-length mk-length) to make it look like length function
;;length -> no end to it, because we keep applying mk-length to itself again and again and again..
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (length);;regular length function we are familiar with
     (lambda (l)
       (cond
	((null l) 0)
	(t (1+
	    (funcall length (cdr l)))))))
   (funcall mk-length mk-length)))

;;length -> no end to it, because we keep applying mk-length to itself again and again and again..
(funcall
 ((lambda (mk-length)
    (funcall mk-length mk-length))
  (lambda (mk-length)
    (lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+
	     (funcall length (cdr l)))))))
    (funcall mk-length mk-length)))
 '(apples))

;;length
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+
	  ((lambda (x)
	     (funcall (funcall mk-length mk-length) x))
	   (cdr l))))))))

;;(length '(1 2 3 4 5 6))
(funcall
 ((lambda (mk-length)
    (funcall mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+
	   ((lambda (x)
	      (funcall (funcall mk-length mk-length) x))
	    (cdr l))))))))
 '(1 2 3 4 5 6))

;;length
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (funcall length (cdr l)))))))
    (lambda (x)
      (funcall (funcall mk-length mk-length) x)))))

;;(length '(1 2 3 4 5 6))
(funcall
 ((lambda (mk-length)
    (funcall mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
	 (cond
	  ((null l) 0)
	  (t (1+ (funcall length (cdr l)))))))
     (lambda (x)
       (funcall (funcall mk-length mk-length) x)))))
 '(1 2 3 4 5 6))

;;length
((lambda (le)
   ((lambda (mk-length)
      (funcall mk-length mk-length))
    (lambda (mk-length)
      (funcall le (lambda (x)
		    (funcall (funcall mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

;;(length '(1 2 3 4 5 6))
(funcall
 ((lambda (le)
    ((lambda (mk-length)
       (funcall mk-length mk-length))
     (lambda (mk-length)
       (funcall le (lambda (x)
		     (funcall (funcall mk-length mk-length) x))))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))
 '(1 2 3 4 5 6))



;;function that makes length
(lambda (le)
  ((lambda (mk-length)
     (funcall mk-length mk-length))
   (lambda (mk-length)
     (funcall le (lambda (x)
		   (funcall (funcall mk-length mk-length) x))))))

;;Applicative-Order Y
(defun Y (le)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall le (lambda (x)
		   (funcall (funcall f f) x))))))


;;length function
(funcall (Y (lambda (length)
	      (lambda (l)
		(cond
		 ((null l) 0)
		 (t (1+ (funcall length (cdr l))))))))
	 '(1 2 3 4 5 6))
