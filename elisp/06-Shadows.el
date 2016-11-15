;;Chapter 6. shadows
(load-file "04-NumbersGames.el")


(defun atom? (x)
  (not (listp x)))

(defun numbered1? (aexp)
  (cond
   ((atom? aexp) (numberp aexp))
   ((eq (car (cdr aexp)) (quote +))
    (and (numbered1? (car aexp))
	 (numbered1? (car (cdr (cdr aexp))))))
   ((eq (car (cdr aexp)) (quote *))
    (and (numbered1? (car aexp))
	 (numbered1? (car (cdr (cdr aexp))))))
   ((eq (car (cdr aexp)) (quote ^))
    (and (numbered1? (car aexp))
	 (numbered1? (car (cdr (cdr aexp))))))))

(defun numbered? (aexp)
  (cond
   ((atom? aexp) (numberp aexp))
   (t (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))))

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq (car (cdr nexp)) (quote +))
    (o+ (value (car nexp))
	(value (car (cdr (cdr nexp))))))
   ((eq (car (cdr nexp)) (quote *))
    (o* (value (car nexp))
	(value (car (cdr (cdr nexp))))))
   (t
    (o^ (value (car nexp))
	(value (car (cdr (cdr nexp))))))))


(defun value-pre1 (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq (car nexp) (quote +))
    (o+ (value-pre1 (car (cdr nexp)))
	(value-pre1 (car (cdr (cdr nexp))))))
   ((eq (car nexp) (quote *))
    (o* (value-pre1 (car (cdr nexp)))
	(value-pre1 (car (cdr (cdr nexp))))))
   (t
    (o^ (value-pre1 (car (cdr nexp)))
	(value-pre1 (car (cdr (cdr nexp))))))))

(defun 1st-sub-exp-pre (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp-pre (aexp)
  (car (cdr (cdr aexp))))

(defun operator-pre (aexp)
  (car aexp))

(defun value-pre (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq (operator-pre nexp) (quote +))
    (o+ (value-pre (1st-sub-exp-pre nexp))
	(value-pre (2nd-sub-exp-pre nexp))))
   ((eq (operator-pre nexp) (quote *))
    (o* (value-pre (1st-sub-exp-pre nexp))
	(value-pre (2nd-sub-exp-pre nexp))))
   (t
    (o^ (value-pre (1st-sub-exp-pre nexp))
	(value-pre (2nd-sub-exp-pre nexp))))))

(defun 1st-sub-exp (aexp)
  (car aexp))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car (cdr aexp)))

(defun value-in (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq (operator nexp) (quote +))
    (o+ (value-in (1st-sub-exp nexp))
	(value-in (2nd-sub-exp nexp))))
   ((eq (operator nexp) (quote *))
    (o* (value-in (1st-sub-exp nexp))
	(value-in (2nd-sub-exp nexp))))
   (t
    (o^ (value-in (1st-sub-exp nexp))
	(value-in (2nd-sub-exp nexp))))))

(defun sero? (n)
  (null n))


(defun edd1 (n)
  (cons (quote()) n))

(defun zub1 (n)
  (cdr n))

(defun e+ (n m)
  (cond
   ((sero? m) n)
   (t (edd1 (e+ n (zub1 m))))))
