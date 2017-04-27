(eval-one-exp '
	      (letrec ([fact (lambda (x)
			       (if (zero? x) 
				   1
				   (* x (fact (- x 1)))))])
		(map fact '(0 1 2 3 4 5))))

(eval-one-exp '
	      (letrec ([even? (lambda (n)
				(if (zero? n) 
				    #t
				    (odd? (- n 1))))]
		       [odd? (lambda (m)
			       (if (zero? m)
				   #f
				   (even? (- m 1))))])
		(list (odd? 3) (even? 3) (odd? 4) (even? 4))))