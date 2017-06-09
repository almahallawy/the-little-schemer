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

(setq  new-entry (function build))

(build '(appetirze entree beverage)
 '(pate boeuf vin))

(funcall new-entry '(appetirze entree beverage)
 '(pate boeuf vin))

(setq lexical-binding t)


(defun lookup-in-entry-help (name names values entry-f)
  (cond   
   ((null names) (funcall entry-f name))
   ((eq (car names) name) (car values))
   (t (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))


(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name
			(first entry)
			(second entry)
			entry-f))

(lookup-in-entry 'beverage
		 '((appetirze entree beverage)
		   (food tastes good))
		 (function null))


(lookup-in-entry-help 'beveragee nil nil (function null))


;;table
'(((appetirze entree beverage)
 (pate boeuf vin))
((beverage dessert)
 ((food is) (number one with us))))

(setq extend-table (function cons))


(defun lookup-in-table (name table table-f)
  (cond
   ((null table) (funcall table-f name))
   (t (lookup-in-entry name (car table)
			     (lambda (name)
			       (lookup-in-table name
						(cdr table)
						table-f))))))

(lookup-in-table 'beverage
		 '(((entree dessert)
		    (spaghetti spumoni))
		   ((appetizer entree beverage)
		    (food tastes good)))
		 (function null))


