(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[let-exp (declaration body) 
				(app-exp 
					(lambda-exp
						(map unparse-exp (map extract-let-vars declaration)) (map syntax-expand body)) 
					(map syntax-expand (map extract-let-bindings declaration)))]
			[let*-exp (declarations bodies)
				(syntax-expand 
					(letrec ([helper (lambda (decs)
										(if (null? decs)
											(let-exp '() bodies)
											(let-exp 
												(list (car decs))
												(list (helper (cdr decs))))))])
						(helper declarations)))]
			[begin-exp (bodies)
				(syntax-expand 
					(let-exp '() bodies))]
			[app-exp (rator rands)
				(cond 
					[(equal? (unparse-exp rator) 'cond)
						(let ([rands (map unparse-exp rands)])
							(letrec ([helper 
										(lambda (ls)
											(cond
												[(equal? (caar ls) 'else) (parse-exp (cadar ls))]												
												[(null? (cdr ls)) (if-exp 
																	(syntax-expand (parse-exp (caar ls)))
																	(syntax-expand (parse-exp (cadar ls))))]											
												[else
													(if-else-exp 
														(syntax-expand (parse-exp (caar ls)))
														(syntax-expand (parse-exp (cadar ls)))
														(helper (cdr ls)))]))])
							(helper rands))
						)]
					[else (app-exp
							(syntax-expand rator)
							(map syntax-expand rands))]
			)]
			[if-exp (con then) 
				(if-exp 
					(syntax-expand con) 
					(syntax-expand then))]
			[if-else-exp (con then els)
				(if-else-exp
					(syntax-expand con)
					then
					els)]
			[and-exp (bodies)
				(cond [(null? bodies)
						(lit-exp #t)]
					  [(null? (cdr bodies))
						(syntax-expand (car bodies))]
					  [else
						(if-else-exp
							(syntax-expand (car bodies))
							(syntax-expand (and-exp (cdr bodies)) 
							(lit-exp #f)))])]
		   [or-exp (bodies) 
				(cond
					[(null? bodies)
					 (lit-exp #f)]
					[(null? (cdr bodies))
					 (syntax-expand (car bodies))]
					[else
					  (if-else-exp
						 (syntax-expand (car bodies))
						 (syntax-expand (car bodies))
						 (syntax-expand (or-exp (cdr bodies))))])]
			[case-exp (id keys bodies)
		     (if (null? keys)
			 (syntax-expand (1st bodies))
			 (if-else-exp
			  (app-exp
			   (var-exp 'member)
			   (list id (lit-exp (map unparse-exp (1st keys)))))
			  (syntax-expand (1st bodies))
				(syntax-expand (case-exp id (cdr keys) (cdr bodies)))))]
			[else exp])))

(define extract-let-vars
  (lambda (x)
    (cases expression x
      [let-declaration-exp (var binding) var]
      [else (eopl:error 'eval-exp "Bad Let Declaration ~s Parse Error" x)])))

;;; Pulling bindings out of let-exp
(define extract-let-bindings
  (lambda (x)
    (cases expression x
      [let-declaration-exp (var binding) binding]
      [else (eopl:error 'eval-exp "Bad Let Declaration ~s Parse Error" x)])))

(define format-nested-lets
	(lambda (exp og)
		(if (null? (cdr exp))
			(list 'let (list (car exp)) (caddr og))
			(list 'let (list (car exp)) (format-nested-lets (cdr exp) og)))))

(define let*->let
	(lambda (exp)
		(format-nested-lets (cadr exp) exp)))

