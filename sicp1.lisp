 SICP LEARNING


(defun square (n) (* n n))
(defun fast-pow (x n)
"this is a fast version of power function"
  (cond ((= n 0) 1)
	((evenp n) (square (fast-pow x (/ n 2))))
	(t (* x (fast-pow x (1- n))))))


(defun my-gcd (a b )
"calculate the gcd of two numbers using Euclid's Algorithm"
  (if (zerop b)
      a
    (my-gcd b (mod a b))))



Counting Change

the number of ways to change amount a using n kinds of coins equals
the number of ways to change amount a using all but first kind of coin plus
the number of ways to change the amount a -d using all kinds of coins, where d is the denomination of the coin

(defun cc (amount kind-of-coin)
	   (cond ((= amount 0) 1)
		 ((or (< amount 0) (= kind-of-coin 0)) 0)
		 (t (+ (cc amount (1- kind-of-coin))
		       (cc (- amount (denom-of kind-of-coin)) kind-of-coin)))))

(defun denom-of (coin)
	   (cond ((= coin 1) 1)
		 ((= coin 2) 5)
		 ((= coin 3) 10)
		 ((= coin 4) 25)
		 ((= coin 5) 50)))


Exercise 1.11. A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n -3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
	       
(defun f (n)
       (cond ((< n 3) n)
       	     (t (+ (f (1- n))  (* 2 (f (- n 2))) (* 2 (f (- n 3)))))))



Exercise 1.12. The following pattern of numbers is called Pascal's triangle.

mathematical definition of p-elem is as follows:
p(height, elem) = 1 if elem == 0
                = 1 if height ==elem 
                = p(height -1, elem -1) + p (height - 1, elem)



height belongs to 0-n
elem belongs to 0-n, height >= elem

(defun p-elem (height elem)
	   (cond ((= elem 0) 1)
		 ((= elem height) 1)
		 (t (+ (p-elem (1- height) (1- elem)) 
			  (p-elem (1- height) elem)))))





Half-interval method to find roots


(defun average (x y) (/ (+ x y) 2))
(defun close-enough? (a b)
	   (< (abs (- a b )) 0.0001))

(defun mid-search ( f neg pos)
	   (let ((mid (average neg pos)))
	     (if (close-enough? neg pos)
		 mid
		 (let ((test-val (funcall f mid)))
		   (cond ((plusp test-val) (mid-search f neg mid))
			 ((not (plusp test-val)) (mid-search f mid pos))
			 (t mid))))))

(defun half-interval (f a b)
	   (let ((a-val (funcall f a))
		 (b-val (funcall f b)))
	     (cond ((and (minusp a-val) (plusp b-val))(mid-search f a b))
		   ((and (minusp b-val) (plusp a-val))(mid-search f b a))
		   (t (error "Not valid data provided.")))))



Fixed-point function

(defun fixed-point (f first-guess)
	   (defun try (guess)
	     (let ((next (funcall f guess)))
	       (if (close-enough? guess next)
		   next
		   (try next))))
	   (try first-guess))


Functions as first class citizens

(defun sum (term a next b)
	   (if (> a b)
	       0
	       (+ (funcall term a)
		  (sum term (funcall next a) next b))))

(defun cube (x ) (* x x x ))

(defun integral (f a b dx)
  (labels((add-dx (x) (+ x dx)))
  (* (sum f (+ a (/ dx 2.0)) #'add-dx b)
     dx)))


(defun deriv (g)
	   #'(lambda (x)
	       (/ (- (funcall g (+ x 0.0001)) (funcall g x))
	       0.0001)))


Rational Numbers Example

(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(defun numer (x) (car x))
(defun denom (x) (cdr x))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
  
Define our own cons,car and cdr functions. Notice that the "data" is actually a function.

(defun our-cons (x y)
	   #'(lambda (m)
	     (cond ((= m 0) x)
		   ((= m 1) y)
		   (t (error "Argument not 0 or 1")))))
(defun our-car (z) (funcall z 0))
(defun our-cdr (z) (funcall z 1))


Interval Arithmetic

(defun make-interval (a b) (cons a b))

(defun lower-bound (x) (min (car x) (cdr x)))
(defun upper-bound (x) (max (car x) (cdr x)))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))


(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))


(defun width-interval (x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(= (width-interval (add-interval (make-interval 2 4) (make-interval 2 7)))
	    (+ (width-interval (make-interval 2 4))
	       (width-interval (make-interval 2 7))))


(defun div-interval (x y)
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
      (error "Invalid interval for dividing")
    (mul-interval x 	     	    		
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))
(defun eq-interval? (x y)
  (and (= (upper-bound x) (upper-bound y))
       (= (lower-bound x) (lower-bound y))))



Divide into 9 different cases

lower < upper for any x

(defun mul-org-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

             lower x upper x  lower y upper y   p1   p2   p3   p4
1                -     -       -       -        >a   +    +    <a
2                -     -       -       +        >a   <a   +    -
3                -     -       +       +        -    >a   <a   -
4                -     +       -       -        >a   +    <a   -
5                -     +       -       +        +    -    -    +  ;calculate to find out which is greater
6                -     +       +       +        -    <a   +    >a
7                +     +       -       -        -    >a   <a    -
8                +     +       -       +        -    +    <a   >a
9                +     +       +       +        <a   +    +    >a

(defun mul-interval (x y)
  (let ((xl (lower-bound x)) (xu (upper-bound x))
	(yl (lower-bound y)) (yu (upper-bound y)))
    (cond ((and (minusp xu) (minusp yu))                       (make-interval (* xl yl) (* xu yu)))
	  ((and (minusp xu) (minusp yl) (plusp yu))            (make-interval (* xl yl) (* xl yu)))
	  ((and (minusp xu) (plusp yl))                        (make-interval (* xl yu) (* xu yl)))
	  ((and (minusp xl) (plusp xu) (minusp yu))            (make-interval (* xl yl) (* xu yl)))
	  ((and (minusp xl) (plusp xu) (minusp yl) (plusp yu)) (make-interval (max (* xu yu) (* xl yl)) (min (* xl yu) (* xu yl))))
	  ((and (minusp xl) (plusp xu) (plusp yu))             (make-interval (* xu yu) (* xl yu)))
	  ((and (plusp xl)  (minusp yu))                       (make-interval (* xl yu) (* xu yl)))
	  ((and (plusp xl) (minusp yl) (plusp yu))             (make-interval (* xu yl) (* xu yu)))
	  ((and (plusp xl) (plusp yl))                         (make-interval (* xl yl) (* xu yu))))))

test with different intervals here

(dolist (x-val  '((-2 . -1) (-1 . 3) (3 . 4)))
  (dolist (y-val '((-5 . -3) (-3 . 2) (2 . 6)))
    (let ((xl (car x-val)) (xu (cdr x-val)) (yl (car y-val)) (yu (cdr y-val)))
      (let ((org-val (mul-org-interval (make-interval xl xu) (make-interval yl yu)))
	    (new-val (mul-interval (make-interval xl xu) (make-interval yl yu))))
	(format t "~a ~a : ~a ~a ~%"
		(lower-bound org-val) (upper-bound org-val)
		(lower-bound new-val) (upper-bound new-val))
	(if (eq-interval? org-val new-val) 
	    t
	  (error "Error in multiplication"))))))


2.2

(defun list-ref (items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(defparameter *squares* (list 1 4 9 16 25))

(defun mylength (items)
  (if (null items)
      0
      (+ 1 (length (cdr items)))))


(defun myappend (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; ex 2.17
(defun last-pair (list)
  (if (null (cdr list))
      list
      (last-pair (cdr list))))

(last-pair '(1 2 3 4))

;; ex 2.18

(defun myreverse-aux  (list acc)
  (if (null list)
      acc
      (myreverse-aux (cdr list) (cons (car list) acc))))

(defun myreverse (list)
  (myreverse-aux list '()))

(myreverse (list 1 2 3 4 5 ))

;;2.19 revisited

Counting Change

the number of ways to change amount a using n kinds of coins equals
the number of ways to change amount a using all but first kind of coin plus
the number of ways to change the amount a -d using all kinds of coins, where d is the denomination of the coin

(defun cc (amount kind-of-coin)
	   (cond ((= amount 0) 1)
		 ((or (< amount 0) (= kind-of-coin 0)) 0)
		 (t (+ (cc amount (1- kind-of-coin))
		       (cc (- amount (denom-of kind-of-coin)) kind-of-coin)))))

(defun denom-of (coin)
	   (cond ((= coin 1) 1)
		 ((= coin 2) 5)
		 ((= coin 3) 10)
		 ((= coin 4) 25)
		 ((= coin 5) 50)))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(defun first-denomination (list))
(defun except-first-denomination (list))
(defun no-more? (list))

;; ex 2.20

(defun same-parity (&rest args)
  (when args
    (if (evenp (car args))
	(remove-if-not #'evenp args)
	(remove-if-not #'oddp  args))))

(same-parity  2 3 4 5 6 7)


;; ex 2.21

(defun square-list (list)
  (mapcar #'(lambda (x) (* x x )) list))

(square-list (list 1 2 3 4))


;; ex 2.22

(defun square (x) (* x x))

(defun iter (things answer)
  (if (null things)
      (reverse answer)
      (iter (cdr things)
	    (cons (square (car things)) answer))))
  
(defun sq-list-error (items)
  (iter items nil))
  
(sq-list-error '(1 2 3 4))


;; ex 2.23
(for-each (lambda (x)  
	    (format t "~a" x))
	  (list 4 5 6))

(defun for-each (fn lst)
  (if (null lst)
      'true
      (progn 
	(funcall fn (car lst))
	(for-each fn (cdr lst)))))


;;2.2.2 Hierarchial Structures

(defparameter *x* (cons (list 1 2) (list 3 4)))

(length *x*)

(count-leaves *x*)

(length (list *x* *x*))

(defun count-leaves (x)
  (cond ((null x) 0)
	((not (consp x)) 1)
	(t (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(count-leaves *x*)
(count-leaves (list *x* *x*))


;; 2.27

there are three conditions
when the given list is empty, return back the accumulator
when the given list is a fringe, then append the value in front of the accumulator
when the list is not empty or a fringe,then
the accumulator will be the appended list of the deep-reverse of the car of the value and the accumulator
and the result is the deep reverse of the cdr of the 

(defun deep-reverse-aux  (val acc)
  (cond ((null val) acc)
	((not (consp val)) (cons val acc))
	 (t (deep-reverse-aux (cdr val)
			       (deep-reverse-aux (car val) acc)))))

(defun deep-reverse (list)
  (deep-reverse-aux list '()))

(deep-reverse (list (list 1 2 3) (list 4 5 6)))

;; 2.28

(defun single (x)
  (not (consp x)))

(defun fringe-aux  (list acc )
  (cond ((null list) acc)
	((single list) (append acc (list list)))
	(t (fringe-aux (cdr list) 
		       (fringe-aux 
			(car list) 
			acc))))) 
(defun fringe (list)
  (fringe-aux list '()))
	       
(fringe *x*)

;;2.29

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (mobile)
  (first mobile))

(defun right-branch (mobile)
  (second mobile))

(defun branch-length (branch)
  (first branch))

(defun branch-structure(branch)
  (second branch))

(defparameter *mobile* (make-mobile 
			(make-branch 5
				     (make-mobile
				      (make-branch 2 5)
				      (make-branch 4 3)))
			(make-branch 4
				     (make-mobile
				      (make-branch 5 2)
				      (make-branch 7 9)))))


A mobile is structually a tree alternates between a mobile and a mobile structure. Thus the total-weight function calls total-branch-weight and vice versa.

(defun is-simple-branch (mobile)
  (numberp (branch-structure mobile)))

(defun total-branch-weight  (mobile-branch acc)
  (cond ((null mobile-branch) acc)
	((is-simple-branch mobile-branch) (+ acc (branch-structure mobile-branch)))

	(t (+ acc (total-weight (branch-structure mobile-branch))))))

(defun total-weight (mobile)
  (+
   (total-branch-weight (left-branch  mobile) 0)
   (total-branch-weight (right-branch mobile) 0)))

;;MAPPING OVER TREES 
(defun scale-tree (tree factor)
  (cond ((not tree) nil)
	((single tree) (* tree factor))
	(t (cons (scale-tree (car tree) factor)
		 (scale-tree (cdr tree) factor)))))

(defun scale-tree (tree factor)
  (mapcar #'(lambda (sub-tree)
	      (if (consp sub-tree)
		  (scale-tree2 sub-tree factor)
		(* sub-tree factor)))
	  tree))
;;NOTE: use M-/ to auto-complete word

;;ex 2.30

(defun square-tree (list)
  (mapcar #'(lambda (sub-list)
	      (if (consp sub-list)
		  (square-tree sub-list)
		(* sub-list sub-list)))
	  list))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


;;ex 2.31

(defun tree-map (fn tree)
  (mapcar #'(lambda (sub-tree)
	      (if (consp sub-tree)
		  (tree-map fn sub-tree)
		(funcall fn sub-tree)))
	  tree))

(tree-map #'(lambda (x) (* x x )) 
	  (list 1
		(list 2 (list 3 4) 5)
		(list 6 7)))

;;ex 2.32

(defun subsets (s)
  (if (null s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (mapcar
		    #'(lambda (x)  
			(if (null x)
			    (list (car s))
			  (append x (list  (car s)))))
		    rest)))))

(subsets '(1 2 3))

;;2.2.3 Sequeces
(defun single (x)
  (not (consp x)))

		

