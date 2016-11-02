;; 4. Numbers Games

;;  add two numbers
(defun o+ (n m)
  (cond
   ((zerop m) n)
   (t (1+ (o+ n (1- m))))))

(defun o1+ (n m)
  (cond
   ((zerop m) n)
   (t (o1+ (1+ n) (1- m)))))

;;substract two numbers
(defun o- (n m)
  (cond
   ((zerop m) n)
   (t (1- (o- n (1- m))))))

(defun o1- (n m)
  (cond
   ((zerop m) n)
   (t (o1- (1- n) (1- m)))))


;;addtup builds a number by totaling all the numbers in a tup
(defun addtup (tup)
  (cond
   ((null tup) 0)
   (t (o+ (car tup)
	  (addtup (cdr tup))))))


;;builds a number by adding n up m times (multiplication)
(defun o*(n m)
  (cond
   ((zerop m) 0)
   (t (o+ n (o* n (1- m))))))

;;add two tups
(defun tup+ (tup1 tup2)
  (cond
   ((null tup1) tup2)
   ((null tup2) tup1)
   (t (cons (o+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2))))))

;;true when n>m
(defun o> (n m)
  (cond
   ((zerop n) nil)
   ((zerop m) t)
   (t (o> (1- n) (1- m)))))

;;true when n<m
(defun o< (n m)
  (cond
   ((zerop m) nil)
   ((zerop n) t)
   (t (o< (1- n) (1- m)))))

;;true when n=m
(defun o= (n m)
  (cond
   ((o< n m) nil)
   ((o> n m) nil)
   (t t)))

;;calculate n^m
(defun o^ (n m)
  (cond
   ((zerop m) 1)
   (t (o* n (o^ n (1- m))))))

; divisioin
(defun o/ (n m)
  (cond
   ((o< n m) 0)
   (t (1+ (o/ (o- n m) m)))))

;;get the lenght of lat
(defun length (lat)
  (cond
   ((null lat) 0)
   (t (1+ (length (cdr lat))))))


;;pick an item from lat
(defun pick (n lat)
  (cond
   ((zerop (1- n)) (car lat))
   (t (pick (1- n) (cdr lat)))))

;;remove item # n from the lat
(defun rempick1 (n lat)
  (cond
   ((zerop (1- n)) (cdr lat))
   (t (cons (car lat)
	    (rempick1 (1- n) (cdr lat))))))

;;gives as a final value a lat obtained by removing all the numbers from the lat
(defun no-nums (lat)
  (cond
   ((null lat) (quote()))
   (t (cond
       ((numberp (car lat)) (no-nums (cdr lat)))
       (t (cons (car lat)
		(no-nums (cdr lat))))))))

;;extracts a tup from a lat using all the numbers in the lat
(defun all-nums (lat)
  (cond
   ((null lat) (quote()))
   (t (cond
       ((numberp (car lat))
	(cons (car lat)
	      (all-nums (cdr lat))))
       (t (all-nums (cdr lat)))))))

;;eqan? is true if its two arguments (a1 and a2) are the same atom
(defun eqan? (a1 a2)
  (cond
   ((and (numberp a1) (numberp a2))
    (o= a1 a2))
   ((or (numberp a1) (numberp a2)) nil)
   (t (eq a1 a2))))

;; counts the number of times an atom a appears in a lat
(defun occurs (a lat)
  (cond
   ((null lat) 0)
   (t
    (cond
     ((eq (car lat) a)
      (1+ (occurs a (cdr lat))))
     (t (occurs a (cdr lat)))))))

;;one? is true if n is 1
(defun one1? (n)
  (cond
   ((zerop n) nil)
   (t (zerop (1- n)))))

(defun one2? (n)
  (cond
   (t (o= n 1))))

(defun one? (n)
  (o= n 1))

;;rempick removes the nth atom from a lat
(defun rempick (n lat)
  (cond
   ((one? n) (cdr lat))
   (t (cons (car lat)
	    (rempick (1- n) (cdr lat))))))

















