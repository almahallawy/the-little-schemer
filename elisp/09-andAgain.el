;; Chapter 9.. and Again, and Again, and Again, ...


(defun pick (n lat)
  (cond
   ((zerop (1- n)) (car lat))
   (t (pick (1- n) (cdr lat)))))

(defun keep-looking (a sorn lat)
  (cond
   ((numberp sorn)
    (keep-looking a (pick sorn lat) lat))
   (t (eq sorn a))))

(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(defun eternity (x)
  (eternity x))

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

(align '((a (b c))(d e)))
(shift '((a (b c))(d e)))


(defun length* (pora)
  (cond
   ((atom? pora) 1)
   (t (+ (length* (first pora))
	 (length* (second pora))))))

(defun weight* (pora)
  (cond
   ((atom? pora) 1)
   (t (+ (* (weight* (first pora)) 2)
	 (weight* (second pora))))))

(defun revpair (pair)
  (build (second pair)
	 (first pair)))

(defun shuffle (pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (shuffle (revpair pora)))
   (else (build (first pora)
		(shuffle (second pora))))))
(defun A (n m)
  (cond
   ((zerop n) (1+ m))
   ((zerop m) (A (1- n) 1))
   (t (A (1- n)
	 (A n (1- m))))))

(defun length (l)
  (cond
   ((null l) 0)
   (t (1+ (length (cdr l))))))

;length0
(lambda (l)
  (cond
   ((null l) 0)
   (t (1+ (eternity (cdr l))))))

;length1
(lambda (l)
  (cond
   ((null l) 0)
   (t (1+
       ((lambda (l);;length0
	 (cond
	  ((null l) 0)
	  (t (1+ (eternity (cdr l))))))
	(cdr l))))))

;;length2
(lambda (l)
  (cond
   ((null l) 0)
   (t (1+
       ((lambda (l);;length1
	  (cond
	   ((null l) 0)
	   (t (1+
	       ((lambda (l);;length0
		  (cond
		   ((null l) 0)
		   (t (1+ (eternity (cdr l))))))
		(cdr l))))))
	(cdr l))))))

;;length0
((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l)))))))
 (function eternity))


;;length1
((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l)))))))
 ((lambda (length);;length0
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (length (cdr l)))))))
 (function eternity)))

;;length2
((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l)))))))
 ((lambda (length);;length1
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null l) 0)
	(t (1+ (length (cdr l)))))))
   (function eternity))))

;;length0
((lambda (mk-length)
   (funcall mk-length (function eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

;;length1
((lambda (mk-length)
   (funcall mk-length
    (funcall mk-length (function eternity))));;length0
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l))))))))

;;length2
((lambda (mk-length)
   (funcall mk-length
    (funcall mk-length ;length1
     (funcall mk-length (function eternity)))));length0
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l))))))))

;;length3
((lambda (mk-length)
   (funcall mk-length
    (funcall mk-length;length2
     (funcall mk-length;length1
      (funcall mk-length (function eternity))))));length0
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l))))))))


;;length0
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+
	     (mk-length (cdr l))))))))


;;length1
((lambda (mk-length)
   (funcall mk-length  mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (else (1+
	     ((mk-length eternity)
	      (cdr l))))))))

(setq lexical-binding t)

;;length
((lambda (mk-length)
   (funcall mk-length (function mk-length)))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+
	     ((funcall mk-length (function mk-length))
	      (cdr l))))))))

(((lambda (mk-length)
    (funcall mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+
	   ((funcall mk-length mk-length)
	    (cdr l))))))))
 '(a b c d e f g h i j))

;;length - same as before but extract (mk-length mk-length)
((lambda (mk-length)
   (funcall mk-length  mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (else (1+ (length (cdr l)))))))
    (function (mk-length mk-lenght)))))

;; value of above for (apple)
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
	 (cond
	  ((null l) 0)
	  (else (1+ (length (cdr l)))))))
     (mk-length mk-lenght))))
 '(apple))


;;(mk-length mk-length)
((lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (length (cdr l)))))))
    (function (mk-length mk-lenght))))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (length (cdr l)))))))
    (function (mk-length mk-lenght)))))


((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l)))))))
 ;;(mk-length mk-length)
 ((lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (length (cdr l)))))))
    (function (mk-length mk-lenght))))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ (length (cdr l)))))))
    (function (mk-length mk-lenght))))))

((lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null l) 0)
       (t (1+ (length (cdr l)))))))
  ;;(mk-length mk-length)
  ((lambda (mk-length)
     ((lambda (length)
	(lambda (l)
	  (cond
	   ((null l) 0)
	   (t (1+ (length (cdr l)))))))
      (function (mk-length mk-lenght))))
   (lambda (mk-length)
     ((lambda (length)
	(lambda (l)
	  (cond
	   ((null l) 0)
	   (t (1+ (length (cdr l)))))))
      (function (mk-length mk-lenght)))))))
 
;;length
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+
	     ((mk-length mk-length)
	      (cdr l))))))))
;;length
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+
	  ((lambda (x)
	     ((funcall mk-length mk-length) x))
	   (cdr l))))))))

((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null l) 0)
	 (t (1+ length  (cdr l))))))
    (lambda (x)
      ((funcall mk-lenght mk-length) x)))))


((lambda (le)
   ((lambda (mk-length)
      (funcall mk-length mk-length))
    (lambda (mk-length)
      (funcall le (lambda (x)
		    ((funcall mk-length mk-lenght) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null l) 0)
      (t (1+ (funcall length (cdr l))))))))

(lambda (le)
  ((lambda (mk-length)
     (funcall mk-length mk-length))
   (lambda (mk-length)
     (funcall le (lambda (x)
		   ((funcall mk-length mk-lenght) x))))))

(defun Y (le)
  (function
   ((lambda (f) (funcall f f))
    (lambda (f)
      (funcall le (lambda (x) ((funcall f f) x)))))))












       



