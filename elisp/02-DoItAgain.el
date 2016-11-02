;; The Little Schemer
;; 2. Do it, Do It Again, and Again, and Again...

(load-file "01-toys.el")

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))


(defun member? (a lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) a)
	  (member? a (cdr lat))))))
