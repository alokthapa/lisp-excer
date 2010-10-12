;;1
(defun my-last(lst)
  (if (null (cdr lst))
      (car lst)
    (my-last (cdr lst))))

;;2
(defun my-but-last (lst)
  (if (null (cddr lst))
      (car lst)
    (my-but-last (cdr lst))))

;;3
(defun element-at (lst pos)
  (if (zerop pos)
      (car lst)
    (element-at (cdr lst) (1- pos))))

;;4
(defun length1 (lst)
  (if (null lst)
      0
    (1+ (length1 (cdr lst)))))

;;5
(defun revlist (lst)
  (let ((retlst '()))
	(mapcar #'(lambda (n)
		    (push n retlst)) lst)
	retlst))


;;6
(defun last1 (lst)
 (car (last lst)))

(defun palin1 (lst)
       (if (equal nil lst)
       t
       (let ((start (first lst)) (finish (last1 lst)))
         (if (equal start finish)
         (palin1 (rest (butlast lst)))
         nil))))

;;7
(defun flatten (lst)
  (if (eql lst nil)
      nil
    (if (listp (car lst))
	(append (flatten (car lst)) (flatten (cdr lst)))
      (append (cons (car lst) nil) (flatten (cdr lst))))))

;;8
(defun compress (lst)
  (if (null lst)
      '()
      (if (and (listp lst) (not (null (second lst))))
	  (if (eql (first lst) (second lst))
	      (compress (cdr lst))
	    (append (list (first lst)) (compress (cdr lst))))
	lst)))
 
;;9
(defun brkeql (lst1 acc el)
  (if (and (listp lst1) (eql (first lst1) el))
      (brkeql (cdr lst1) (append acc (list (first lst1))) el)
    (list acc lst1)))

(defun pack (lst)
  (if (null lst)
      '()
      (let ((div (brkeql lst '() (car lst))))
	(append (list (first div)) (pack (cadr div))))))

;;10
(defun encode (lst)
  (let ((pkd (pack lst)))
    (mapcar #'(lambda (lst1)
		  (list (length lst1) (car lst1)))
	    pkd)))

;;11
(defun encode-mod (lst)
  (let ((pkd (pack lst)))
    (mapcar #'(lambda (lst1)
		(let ((lgt (length lst1)))
		  (if (= lgt 1)
		      (car lst1)
		      (list lgt (car lst1)))))
		pkd)))

;;12
(defun gettimes (n val)
	   (labels ((rec (n val acc)
		    (if (= n 0)
			acc
			(rec (1- n) val (push val acc)))))
	     (rec n val '())))
(defun decode (lst)
	   (flatten 
	    (mapcar #'(lambda (val)
			(if (listp val)
			    (gettimes (car val) (cadr val))
			    val)) lst)))

;;13 
;;not interesting...


;;14
 (defun dupli (lst)
	   (let ((ret '()))
	     (mapcar #'(lambda (val)
			 (push val ret)
			 (push val ret))
		     lst)
	     (reverse ret)))

;;15
(defun repli (lst)
	   (let ((ret '()) (times (cadr lst)))
	     (mapcar #'(lambda (val)
			 (dotimes (v times val)
			   (push val ret)))
		     (car lst))
	     (reverse ret)))		     