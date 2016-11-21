;;Chapter 7 Friends and Relations

(load-file "05.OhMyGawd-ItsFullOfStars.el")

(defun atom? (x)
  (not (listp x)))


(defun member? (a lat)
  (cond
   ((null lat) nil)
   ((equal? (car lat) a) t)
   (t (member? a (cdr lat)))))


(defun multirember (a lat)
  (cond
   ((null lat) (quote ()))
   ((equal? (car lat) a)
    (multirember a (cdr lat)))
   (t (cons (car lat)
	    (multirember a (cdr lat))))))

(defun set? (lat)
  (cond
   ((null lat) t)
   ((member? (car lat) (cdr lat)) nil)
   (t (set? (cdr lat)))))


(defun makeset1 (lat)
  (cond
   ((null lat) (quote ()))
   ((member? (car lat) (cdr lat))
    (makeset1 (cdr lat)))
   (t (cons (car lat)
	    (makeset1 (cdr lat))))))


(defun makeset (lat)
  (cond
   ((null lat) (quote ()))
   (t (cons (car lat)
	    (makeset
	     (multirember (car lat) (cdr lat)))))))


(defun subset? (set1 set2)
  (cond
   ((null set1) t)
   (t (and (member? (car set1) set2)
	   (subset? (cdr set1) set2)))))


(defun eqset? (set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))


(defun intersect? (set1 set2)
  (cond
   ((null set1) nil)
   (t (or (member? (car set1) set2)
	  (intersect? (cdr set1) set2)))))

(defun intersect (set1 set2)
  (cond
   ((null set1) (quote ()))
   ((member? (car set1) set2)
    (cons (car set1)
	  (intersect (cdr set1) set2)))
   (t (intersect (cdr set1) set2))))


(defun union (set1 set2)
  (cond
   ((null set1) set2)
   ((member? (car set1) set2)
    (union (cdr set1) set2))
   (t (cons (car set1)
	    (union (cdr set1) set2)))))

(defun set-difference (set1 set2)
  (cond
   ((null set1) (quote ()))
   ((member? (car set1) set2)
    (set-difference (cdr set1) set2))
   (t (cons (car set1)
	    (set-difference (cdr set1) set2 )))))


(defun intersectall (l-set)
  (cond
   ((null (cdr l-set)) (car l-set))
   (t (intersect (car l-set)
		 (intersectall (cdr l-set))))))

(defun a-pair (x)
  (cond
   ((atom? x) nil)
   ((null x) nil)
   ((null (cdr x)) nil)
   ((null (cdr (cdr x))) t)
   (t nil)))

(defun first (p)
  (car p))

(defun second (p)
  (car (cdr p)))

(defun build (s1 s2)
  (cons s1 (cons s2 (quote ()))))

(defun third (l)
  (car (cdr (cdr l))))

;; from Chapter 3
(defun firsts (l)
  (cond
   ((null l) (quote()))
   (t (cons (car (car l))
	    (firsts (cdr l))))))

(defun seconds (l)
  (cond
   ((null l) (quote ()))
   (t (cons (car (cdr (car l)))
	    (seconds (cdr l))))))

(defun fun? (rel)
  (set? (firsts rel)))


(defun revrel1 (rel)
  (cond
   ((null rel) (quote()))
   (t (cons (build
	     (second (car rel))
	     (first (car rel)))
	    (revrel1 (cdr rel))))))

(defun revpair (p)
  (build (second p) (first p)))

(defun revrel (rel)
  (cond
   ((null rel) (quote ()))
   (t (cons (revpair (car rel))
	    (revrel (cdr rel))))))

(defun fullfun? (fun)
  (fun? (revrel fun)))

(defun fullfun1? (fun)
  (set? (seconds fun)))
