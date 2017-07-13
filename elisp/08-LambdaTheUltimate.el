;;chapter 8. Lambda the Ultimate

(load-file "05.OhMyGawd-ItsFullOfStars.el")

(defun rember-f1 (test? a l)
  (cond
   ((null l) (quote ()))
   ((funcall test? (car l) a)
    (cdr l))
   (t (cons (car l)
	    (rember-f1 test? a (cdr l))))))
(rember-f1 (function eq) 5 '(6 2 5 3))
(setq eqer (function eq))
(rember-f1 eqer 5 '(6 2 5 3))



;;We need to enable Lexical binding in elisp. The default binding is Dynamic
;;check the following: https://m.reddit.com/r/emacs/comments/1voaz9/why_this_code_snippet_doesnt_work_in_emacs_lisp/
;;https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding
(setq lexical-binding t)


(defun eq?-c (a)
  (function
   (lambda (x)
     (eq x a))))

(eq?-c 'salad)

(setq eq?-salad (eq?-c 'salad))

(funcall eq?-salad 'salad)

(funcall (eq?-c 'salad) 'tuna)


(defun rember-f (test?)
  (function
   (lambda (a l)
     (cond
      ((null l) (quote ()))
      ((funcall test? (car l) a) (cdr l))
      (t (cons (car l)
	       (funcall (rember-f test?) a (cdr l))))))))

(setq rember-eq? (rember-f (function eq)))

(funcall rember-eq? 'tuna '(tuna salad is good))
(funcall (rember-f (function eq)) 'tuna '(tuna salad is good))

(funcall (rember-f (function eq)) 'eq '(equal? eq eqan? eqlist? eqpair? ))


(defun insertL1 (new old l)
  (cond
   ((null l) (quote ()))
   ((equal? (car l) old)
    (cons new l))
   (t (cons (car l)
	    (insertL1 new old (cdr l))))))

(defun insertL-f (test?)
  (function
   (lambda (new old l)
     (cond
      ((null l) (quote ()))
      ((funcall test? (car l) old)
       (cons new (cons old (cdr l))))
      (t (cons (car l)
	       (funcall (insertL-f test?)
			new old (cdr l))))))))

(funcall (insertL-f (function eq)) 5 6  '(1 2 3 4 6))

(defun insertR-f (test?)
  (function
   (lambda (new old l)
     (cond
      ((null l) (quote ()))
      ((funcall test? (car l) old)
       (cons old (cons new (cdr l))))
      (t (cons (car l)
	       (funcall (insertR-f test?)
			new old (cdr l))))))))

(funcall (insertR-f (function eq)) 6 5 '(1 2 3 4 5))

(defun seqL (new old l)
  (cons new (cons old l)))

(defun seqR (new old l)
  (cons old (cons new l)))

(defun insert-g (seq)
  (function
   (lambda (new old l)
     (cond
      ((null l) (quote ()))
      ((eq (car l) old)
       (funcall seq new old (cdr l)))
      (t (cons (car l)
	       (funcall (insert-g seq) new old (cdr l))))))))

(setq insertL (insert-g (function seqL)))

(setq insertR (insert-g (function seqR)))

(funcall insertL 'a 'b '(1 2 3 bb c d b))

(funcall insertR 'a 'b '(1 2 4 bb c d b))
 

(setq insertL2
      (insert-g
       (function
	(lambda (new old l)
	  (cons new (cons old l))))))

(funcall insertL2 'a 'b '(1 2 3 db c d b))

(defun subst (new old l)
  (cond
   ((null l) (quote ()))
   ((eq (car l) old)
    (cons new (cdr l)))
   (t (cons (car l)
	    (subst new old (cdr l))))))

(defun seqS (new old l)
  (cons new l))

(setq subst1 (insert-g (function seqS)))

(funcall subst1 'a 'b '(1 2 3 4 b 6 7 ))
(subst 'a 'b '(1 2 3 4 b 6 7))


(defun seqrem (new old l)
  l)

(defun yyy (a l)
  (funcall (insert-g (function seqrem)) nil a l))

(yyy 5 '(1 2 3 a 5 6))


(defun atom? (x)
  (not (listp x)))

(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car aexp))

(defun value_old (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq (operator nexp) (quote +))
    (+ (value_old (1st-sub-exp nexp))
       (value_old (2nd-sub-exp nexp))))
   ((eq (operator nexp) (quote *))
    (* (value_old (1st-sub-exp nexp))
       (value_old (2nd-sub-exp nexp))))
   (t (expt (value_old (1st-sub-exp nexp))
		(value_old (2nd-sub-exp nexp))))))

(value_old '(^ 2 3))

(defun atom-to-function (x)
  (cond
   ((eq x (quote +)) (function +))
   ((eq x (quote *)) (function *))
   (t (function expt))))

(atom-to-function (operator '(+ 5 3)))

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   (t (funcall (atom-to-function (operator nexp))
	       (value (1st-sub-exp nexp))
	       (value (2nd-sub-exp nexp))))))

(value '(+ 5 3))

;; Note the following
(funcall (function  +) 5 3)
;;versus 
((function +) 5 3)
;;versus
(+ 5 3)

;;so in value you can not do
;; (t ((atom-to-function (operator nexp))
;;      (value (1st-sub-exp nexp))
;;      (value (2nd-sub-exp nexp))))))
;; you have to preceed with funcall

(defun multirember (a lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) a)
    (multirember a (cdr lat)))
   (t (cons (car lat)
	    (multirember a (cdr lat))))))

(multirember 'a '(1 a 2 a 3 a 4 a 5))

(defun multirember-f (test?)
  (function
   (lambda (a lat)
     (cond
      ((null lat) (quote ()))
      ((funcall test? (car lat) a)
       (funcall (multirember-f test?) a (cdr lat)))
      (t (cons (car lat)
	       (funcall (multirember-f test?) a (cdr lat))))))))


(funcall (multirember-f (function  eq)) 'a '(1 a 2 a 3 a 4 a 5))
(funcall (multirember-f (function  eq)) 'tuna '(shrimp salad tuna salad and tuna))

(setq multirember-eq? (multirember-f (function eq)))
(funcall multirember-eq? 'a '(1 a 2 a 3 a 4 a 5))

(setq eq?-tuna
      (eq?-c (quote tuna)))

(funcall eq?-tuna 'tuna)

;;combining test? and a
(defun multirember_tuna (lat)
  (cond
   ((null lat) (quote ()))
   ((funcall eq?-tuna (car lat))
    (multirember_tuna (cdr lat)))
   (t (cons (car lat)
	    (multirember_tuna (cdr lat))))))

(multirember_tuna '(shrimp salad tuna salad and tuna))

(defun multiremberT (test? lat)
  (cond
   ((null lat) (quote ()))
   ((funcall test? (car lat))
    (multiremberT test? (cdr lat)))
   (t (cons (car lat)
	    (multiremberT test? (cdr lat))))))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(defun multirember&co (a lat col)
  (cond
   ((null lat)
    (funcall col (quote ()) (quote ())))
   ((eq (car lat) a)
    (multirember&co a (cdr lat)
		    (function
		     (lambda (newlat seen)
		       (funcall col newlat
			    (cons (car lat) seen))))))
   (t (multirember&co a (cdr lat)
		      (function
		       (lambda (newlat seen)
			 (funcall col (cons (car lat) newlat)
			      seen)))))))
(defun a-friend (x y)
  (null y))

(a-friend 5 nil)

(multirember&co 'tuna '(strawberries tuna and swordfish)
		(function a-friend))

(multirember&co 'tuna '() (function a-friend))

(multirember&co 'tuna '(tuna) (function a-friend))

(multirember&co 'tuna '(and tuna) (function a-friend))

(defun last-friend (x y)
  (length x))

(multirember&co 'tuna '(stawberries tuna and swordfish) (function last-friend))


(defun multiinsertL (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons new
	  (cons old
		(multiinsertL new old (cdr lat)))))
   (t (cons (car lat)
	    (multiinsertL new old (cdr lat))))))

(multiinsertL 'a 'b '(1 b 2 b 3 b ))

(defun multiinsertR (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons old
	  (cons new
		(multiinsertR new old (cdr lat)))))
   (t (cons (car lat)
	    (multiinsertR new old (cdr lat))))))

(multiinsertR 'b 'a '(1 a 2 a 3 a))

(defun multiinsertLR (new oldL oldR lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) oldL)
    (cons new
	  (cons oldL
		(multiinsertLR new oldL oldR (cdr lat)))))
   ((eq (car lat) oldR)
    (cons oldR
	  (cons new
		(multiinsertLR new oldL oldR (cdr lat)))))
   (t (cons (car lat)
	    (multiinsertLR new oldL oldR (cdr lat))))))

(multiinsertLR 'a 'b 'c '(1 b 2 c 3 b 4 c))
(multiinsertLR 'a 'b 'b '(1 b 2 c 3 b 4 c))

(defun multiinsertLR&co (new oldL oldR lat col)
  (cond
   ((null lat)
    (funcall col (quote ()) 0 0))
   ((eq (car lat) oldL)
    (multiinsertLR&co new oldL oldR (cdr lat)
		      (function
		       (lambda (newlat L R)
			 (funcall col
				  (cons new
					(cons oldL newlat))
				  (1+ L) R)))))
   ((eq (car lat) oldR)
    (multiinsertLR&co new oldL oldR (cdr lat)
		      (function
		       (lambda (newlat L R)
			 (funcall col
				  (cons oldR
					(cons new newlat))
				  L (1+ R))))))
   (t (multiinsertLR&co new oldL oldR (cdr lat)
			(function
			 (lambda (newlat L R)
			   (funcall col
				    (cons (car lat)
					  newlat)
				    L R)))))))

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
		  (function
		   (lambda (newlat l r)
		     (print newlat)
		     (print l)
		     (print r))))


(defun even? (n)
  (= (* (/ n 2) 2) n))

(even? 4)

(even? 9)

(defun evens-only* (l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (cons (car l)
	    (evens-only* (cdr l))))
     (t (evens-only* (cdr l)))))
   (t (cons (evens-only* (car l))
	    (evens-only* (cdr l))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))


(defun evens-only*&co (l col)
  (cond
   ((null l)
    (funcall col (quote ()) 1 0))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (evens-only*&co (cdr l)
		      (function
		       (lambda (newl p s)
			 (funcall col
				  (cons (car l) newl)
				  (* (car l) p) s)))))
     (t (evens-only*&co (cdr l)
			(function
			 (lambda (newl p s)
			   (funcall col
				    newl p
				    (+ (car l) s))))))))
   (t (evens-only*&co (car l)
		      (function
		       (lambda (al ap as)
			 (evens-only*&co (cdr l)
					 (function
					  (lambda (dl dp ds)
					    (funcall col
						     (cons al dl)
						     (* ap dp)
						     (+ as ds)))))))))))

(defun the-last-friend (newl product sum)
  (cons sum (cons product newl)))


(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
		(function the-last-friend))










