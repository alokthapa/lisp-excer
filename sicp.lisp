
SICP PROBLEMS

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




Exercise 1.17. The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:

(define (* a b)
   (if (= b 0)
         0
         (+ a (* a (- b 1)))))


This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition,operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.


(defun our-* (a b)
	   (if (= b 0)
	       0
	       (+ a (our-* a (1- b)))))

(defun our-* (a b)
  (cond ((= b 0) 0)
	((evenp b) (double (our-* a (/ b 2))))
	(t (+ a (our-* a (1- b))))))

