(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[let-exp (declaration body) 
				(app-exp 
					(lambda-exp
						(map unparse-exp (map extract-let-vars declaration)) (map syntax-expand body)) 
					(map syntax-expand (map extract-let-bindings declaration)))]
			[let*-exp (declaration body)
				(syntax-expand (parse-exp (let*->let (unparse-exp exp))))]
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
					[(equal? (unparse-exp rator) 'case)
						(let-exp
							(list (let-declaration-exp
								(var-exp 'val)
								(car rands)))
							(list (syntax-expand (app-exp
											(var-exp 'cond)
											(map (lambda (v)
												(if (equal? (car v) 'else)
													(app-exp (var-exp 'else) (list (parse-exp (cadr v))))
													(let ([to-parse (list (list 'member 'val (quasiquote (quote ,(car v))) (car (cdr v))))])
														(newline)
														(display to-parse)
														(newline)
														(parse-exp to-parse)))) ;TODO: CLEAN
												(map unparse-exp (cdr rands)))))))]
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
					(syntax-expand then)
					(syntax-expand els))]
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

