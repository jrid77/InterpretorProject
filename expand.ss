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
					[(equal? (unparse-exp rator) 'and)
						(let ([rands (map unparse-exp rands)])
							(if (null? rands) (parse-exp #t)
								(letrec ([helper 
											(lambda (ls)
												(if (null? (cdr ls))
													(if-else-exp 
														(syntax-expand (parse-exp (car ls)))
														(syntax-expand (parse-exp (car ls)))
														(parse-exp #f))
													(if-else-exp
														(syntax-expand (parse-exp (car ls)))
														(helper (cdr ls))
														(parse-exp #f))))])
								(helper rands))))]
					[(equal? (unparse-exp rator) 'or)
						(let ([rands (map unparse-exp rands)])
							(if (null? rands) (parse-exp #f)
								(letrec ([helper 
											(lambda (ls)
												(if (null? (cdr ls))
													(if-else-exp 
														(syntax-expand (parse-exp (car ls)))
														(syntax-expand (parse-exp (car ls)))											
														(parse-exp #f))
													(if-else-exp
														(syntax-expand (parse-exp (car ls)))
														(syntax-expand (parse-exp (car ls)))
														(helper (cdr ls))
														)))])
								(helper rands))))]
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

