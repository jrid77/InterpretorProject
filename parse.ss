;;; Parser which creates datastructures from scheme code

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(literal? datum) (if (list? datum) (lit-exp (2nd datum)) (lit-exp datum))]
     [(list? datum)
      (case (1st datum)
		['lambda 
			(cond [(< (length datum) 3) (eopl:error 'parse-exp "Lambda Expression: ~s Incorrect Length" datum)]
			  [(and (list? (2nd datum)) (andmap symbol? (2nd datum)))
			   (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
			  [(symbol? (2nd datum))
			   (lambda-exp-one-var (list (2nd datum)) (map parse-exp (cddr datum)))]
			  [(and (pair? (2nd datum)) (ilos? (2nd datum)))
			   (lambda-exp-improper-list (flatten (2nd datum)) (map parse-exp (cddr datum)))]
			  [else (eopl:error 'parse-exp "Lambda Expression: ~s Incorrect Use of Arguments" datum)])]
		['while
		 (while-exp (syntax-expand (parse-exp (2nd datum))) (map syntax-expand (map parse-exp (cddr datum))))]
		['if 
		 (cond [(= 3 (length datum))
			(if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
			   [(= 4 (length datum))
			(if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
			   [else 
			(eopl:error 'parse-exp "If Expression: ~s Incorrect Length" datum)])]
		['let
			(if (symbol? (2nd datum))
			(if (< (length datum) 4)
				(eopl:error 'parse-exp "Named Let: ~s Incorrect Length" datum)
				(named-let-exp (var-exp (2nd datum)) (let-parse-helper (3rd datum)) (map parse-exp (cdddr datum))))
			(if (< (length datum) 3)
				(eopl:error 'parse-exp "Let Expression: ~s Incorrect Length" datum)
				(let-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum)))))]
		['letrec
			(if (< (length datum) 3)
			(eopl:error 'parse-exp "Letrec Expression: ~s Incorrect Length" datum)
			(letrec-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
		['let*
			(if (< (length datum) 3)
			(eopl:error 'parse-exp "Let* Expression: ~s Incorrect Length" datum)
			(let*-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
		['set!
			(if (= 3 (length datum))
			(if (symbol? (2nd datum))
				(set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
				(eopl:error 'parse-exp "Set! Expression: ~s Incorrect Argument Type" datum))
			(eopl:error 'parse-exp "Set! Expression: ~s Incorrect Number of Arguments" datum))]
		['begin
		  (begin-exp (map parse-exp (cdr datum)))]
		['case
			(case-exp (parse-exp (2nd datum))
				  (map (lambda (x) (map parse-exp (1st x)))
				   (filter (lambda (x) (not (eqv? 'else (1st x)))) (cddr datum)))
				  (map (lambda (x) (parse-exp (2nd x))) (cddr datum)))]
		['and
		 (and-exp (map parse-exp (cdr datum)))]
		['or
		 (or-exp (map parse-exp (cdr datum)))]
		['cond
		 (cond-exp (map (lambda (x) (parse-exp (1st x)))
				(filter (lambda (x) (not (eqv? 'else (1st x)))) (cdr datum)))
			   (map (lambda (x) (parse-exp (2nd x))) (cdr datum)))]
		[else (app-exp (parse-exp (1st datum))
				   (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define let-parse-helper
  (lambda (ls)
    (let inner-helper ((rest ls))
      (if (null? rest)
	  '()
	  (if
	   (not
	    (or 
	     (list? rest)
	     (list? (car rest))
	     (symbol? (caar rest))
	     (= 2 (length (car rest)))))
	   (eopl:error 'parse-exp "incorrect let expression ~s" rest)
	   (cons 
	    (let-declaration-exp (parse-exp (caar rest)) (parse-exp (cadar rest)))
	    (inner-helper (cdr rest))))))))

(define literal?
  (lambda (exp)
    (or 
     (boolean? exp)
     (number? exp)
     (string? exp)
     (null? exp)
     (vector? exp)
     (char? exp)
     (equal? 'quote (car exp)))))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lit-exp (id) id]
	   [lambda-exp (declaration body) 
		       (append 
			(list 
			 'lambda 
			 declaration
			 (map unparse-exp body)))]
	   [lambda-exp-one-var (declaraion body)
			       (append
				(list 'lambda
				      (1st declaration))
				(map unparse-exp body))]
	   [lambda-exp-improper-list (declaration body)
				     (append
				      (list 'lambda
					    (let loop ([decs declaration])
					      (if (null? (cdr decs))
						  (1st decs)
						  (cons (1st decs) (loop (cdr decs))))))
				      (map unparse-exp body))]
	   [if-exp (con then)
		   (append
		    (list 'if (unparse-exp con))
		    (list (unparse-exp then)))]
	   [if-else-exp (con then els) 
			(append 
			 (list 'if (unparse-exp con)) 
			 (list (unparse-exp then))
			 (list (unparse-exp els)))]
	   [named-let-exp (name declaration body)
			  (append
			   (list 'let name (map unparse-exp declaration))
			   (map unparse-exp body))]
	   [let-exp (declaration body)
		    (append 
		     (list 'let (map unparse-exp declaration))
		     (map unparse-exp body))]
	   [let-declaration-exp (var binding)
				(list (unparse-exp var) (unparse-exp binding))]
	   [letrec-exp (declaration body)
		       (append
			(list 'letrec (map unparse-exp declaration))
			(map unparse-exp body))]
	   [let*-exp (declaration body)
		     (append
		      (list 'let* (map unparse-exp declaration))
		      (map unparse-exp body))]
	   [set!-exp (id body)
		     (list 'set! id (map unparse-exp body))]
	   [app-exp (rator rands) (cons (unparse-exp rator) (map unparse-exp rands))]
	   [begin-exp (bodies) (append (list 'begin) (map unparse-exp bodies))]
	   [while-exp (test bodies)
		      (cons* 'while (unparse-exp test) (map unparse-exp bodies))]
	   [case-exp (id keys bodies)
		     (cons 'case 
			   (cons (map unparse-exp id)
				 (append (map list 
					      (append (map unparse-exp preds) (list 'else))
					      (map unparse-exp bodies)))))]
	   [cond-exp (preds bodies)
		     (cons 'cond
			   (append (map list 
					(append (map unparse-exp preds) (list 'else))
					(map unparse-exp bodies))))]
	   [and-exp (bodies)
		    (cons 'and
			  (map unparse-exp bodies))]
	   [or-exp (bodies)
		   (cons 'or
			 (map unparse-exp bodies))])))



