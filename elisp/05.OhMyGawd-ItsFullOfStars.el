;;5. *Oh My Gawd*:
;;It's Full of Stars

(load-file "04-NumbersGames.el")

(defun atom? (x)
  (not (listp x)))

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))


(defun rember* (a l)
  (cond
   ((null l) (quote()))
   ((atom? (car l))
    (cond
     ((eq (car l) a)
      (rember* a (cdr l)))
     (t (cons (car l)
	      (rember* a (cdr l))))))
   (t (cons (rember* a (car l))
	    (rember* a (cdr l))))))

(defun insertR* (new old l)
  (cond
   ((null l) (quote()))
   ((atom? (car l))
    (cond
     ((eq (car l) old)
      (cons old
	    (cons new
		  (insertR* new old (cdr l)))))
     (t (cons (car l)
	      (insertR* new old (cdr l))))))
   (t (cons (insertR* new old (car l))
	    (insertR* new old (cdr l))))))

(defun occur* (a l)
  (cond
   ((null l) 0)
   ((atom? (car l))
    (cond
     ((eq (car l) a)
      (1+ (occur* a (cdr l))))
     (t (occur* a (cdr l)))))
   (t (+ (occur* a (car l))
	 (occur* a (cdr l))))))

(defun subst* (new old l)
  (cond
   ((null l) (quote()))
   ((atom? (car l))
    (cond
     ((eq (car l) old)
      (cons new
	    (subst* new old (cdr l))))
     (t (cons (car l)
	      (subst* new old (cdr l))))))
   (t (cons (subst* new old (car l))
	    (subst* new old (cdr l))))))

(defun insertL* (new old l)
  (cond
   ((null l) (quote()))
   ((atom? (car l))
    (cond
     ((eq (car l) old)
      (cons new (cons old
		      (insertL* new old (cdr l)))))
     (t (cons (car l)
	      (insertL* new old (cdr l))))))
   (t (cons (insertL* new old (car l))
	    (insertL* new old (cdr l))))))

(defun member* (a l)
  (cond
   ((null l) nil)
   ((atom? (car l))
    (or (eq (car l) a)
	(member* a (cdr l))))
   (t (or (member* a (car l))
	  (member* a (cdr l))))))

(defun leftmost (l)
  (cond
   ((null l) nil)
   ((atom? (car l)) (car l))
   (t (leftmost (car l)))))

(defun eqlist? (l1 l2)
  (cond
   ; l1 empty,  l2 empty
   ((and (null l1) (null l2)) t)
   ; l1 empty, l2 atom consed onto a list
   ((and (null l1) (atom? (car l2))) nil)
   ; l1 empty, l2 list consed onto a list
   ((null l1) nil)
   ; l1 atom consed onto list, l2 empty
   ((and (atom? (car l1)) (null l2)) nil)
   ; l1 atom consed onto list, l2 atom consed onto a list
   ((and (atom? (car l1)) (atom? (car l2)))
    (and (eqan? (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2))))
   ; l1 atom consed onto list, l2 list consed onto a list
   ((atom? (car l1)) nil)
   ; l1 list consed onto list, l2 empty
   ((null l2) nil)
   ; l1 list consed onto list, l2 atom consed onto list
   ((atom? (car l2)) nil)
   ; l1 list consed onto list, l2 list consed ontol list
   (t (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))))

(defun eqlist2? (l1 l2)
  (cond
   ((and (null l1) (null l2)) t)
   ((or (null l1) (null l2)) nil)
   ; l1 atom consed onto list, l2 empty
   ((and (atom? (car l1)) (null l2)) nil)
   ; l1 atom consed onto list, l2 atom consed onto a list
   ((and (atom? (car l1)) (atom? (car l2)))
    (and (eqan? (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2))))
   ; l1 atom consed onto list, l2 list consed onto a list
   ((atom? (car l1)) nil)
   ; l1 list consed onto list, l2 empty
   ((null l2) nil)
   ; l1 list consed onto list, l2 atom consed onto list
   ((atom? (car l2)) nil)
   ; l1 list consed onto list, l2 list consed ontol list
   (t (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))))

(defun equal? (s1 s2)
  (cond
   ((and (atom? s1) (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1) (atom? s2)) nil)
   (t (eqlist? s1 s2))))


(defun eqlist3? (l1 l2)
  (cond
   ((and (null l1) (null l2)) t)
   ((or (null l1) (null l2)) nil)
   (t (and (equal? (car l1) (car l2))
	   (eqlist3? (cdr l1) (cdr l2))))))

(defun rember (s l)
  (cond
   ((null l) (quote()))
   ((equal? (car l) s) (cdr l))
   (t (cons (car l)
	    (rember s (cdr l))))))
