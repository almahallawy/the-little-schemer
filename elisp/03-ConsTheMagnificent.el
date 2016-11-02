;; The Little Schemer
;; 3. Cons the Magnificent

;; rember remove the first occurence of the atom -a- from -lat-
(defun rember(a lat)
    (cond
     ((null lat) nil)
     ((eq (car lat) a) (cdr lat))
     (t (cons (car lat)
	      (rember a (cdr lat))))))

;;multirember gives as its final value the lat with
;; all occurrences of a removed
(defun multirember(a lat)
    (cond
     ((null lat) nil)
     ((eq (car lat) a)
      (multirember a (cdr lat)))
     (t (cons (car lat)
	      (multirember a (cdr lat))))))

;; firsts builds another list composed of the first S-expression of each internal list
(defun firsts(l)
  (cond
   ((null l) nil)
   (t (cons (car (car l))
	    (first (cdr l))))))

;; insertR builds a lat with -new- inserted to the righ
;; of the first occurrence of old
(defun insertR(new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old)
    (cons old
	  (cons new (cdr lat))))
   (t (cons (car lat)
	    (insertR new old
		     (cdr lat))))))

(defun multiinsertR (new old lat)
  (cond
   ((null lat) nil)
   (t (cond
       ((eq (car lat) old)
	(cons old
	      (cons new
		    (multiinsertR new old
				  (cdr lat)))))
       (t (cons (car lat)
		(multiinsertR new old
			      (cdr lat))))))))

;; insertL builds a lat with -new- inserted to the left
;; of the first occurrence of -old-
(defun insertL (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old)
	(cons new lat))
   (t (cons (car lat)
	    (insertL new old
		     (cdr lat))))))

(defun multiinsertL (new old lat)
  (cond
   ((null lat) nil)
   (t (cond
       ((eq (car lat) old)
	(cons new
	      (cons old
		    (multiinsertL new old
				  (cdr lat)))))
       (t (cons (car lat)
		(multiinsertL new old
			      (cdr lat))))))))

;;subst replace the first occurrence of old in the lat with new
(defun subst (new old lat)
  (cond
   ((null lat) nil)
   (t (cond
       ((eq (car lat) old)
	(cons new (cdr lat)))
       (t (cons (car lat)
		(subst new old
		       (cdr lat))))))))

(defun multisubst (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old)
    (cons new
	  (multisubst new old
		      (cdr lat))))
   (t (cons (car lat)
	    (multisubst new old
			(cdr lat))))))

;;subst2 replaces either the first occurrence of o1 or
;;the first ocurrence of o2 by new
(defun subst2 (new o1 o2 lat)
  (cond
   ((null lat) nil)
   (t (cond
       ((or (eq (car lat) o1) (eq (car lat) o2))

	(cons new (cdr lat)))
       (t (cons (car lat)
		(subst2 new o1 o2 (cdr lat))))))))
