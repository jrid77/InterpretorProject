;;;Expands supported syntax by replacing our specialized parse expressions into more basic expressions
;; i.e. cond -> nested ifs

(define syntax-expand
  (lambda (exp)
    (cases expression exp
       [lambda-exp (declaration body)
       	 (lambda-exp
       	 	declaration
       	 	(map syntax-expand body))]
       [lambda-exp-one-var (declaration body)
         (lambda-exp-one-var
         	declaration
         	(map syntax-expand body))]
       [lambda-exp-improper-list (declaration body)
         (lambda-exp-improper-list
         	declaration
         	(map syntax-expand body))]
	   [let-exp (declaration body) 
		    (app-exp 
		     (lambda-exp (map unparse-exp (map extract-let-vars declaration))
				 (map syntax-expand body)) 
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
	   [letrec-exp (declaration body)
		(syntax-expand
			(let ([vars (map extract-let-vars declaration)]
					[bindings (map extract-let-bindings declaration)])
				(let-exp 
					(map (lambda (x) 
							(let-declaration-exp x (lit-exp 'WhoCares?))) vars)
						(append (make-set!-exps vars bindings)
							body))))]
	   [named-let-exp (name vars bindings bodies)	   		 
	   			(syntax-expand
				  (letrec-exp
					(list (let-declaration-exp (var-exp name) 
										(lambda-exp vars bodies)))
					(list (app-exp (var-exp name) bindings))))]
	   [begin-exp (bodies)
		      (syntax-expand 
		       (let-exp '() bodies))]
	   [if-exp (con then) 
		   (if-exp 
		    (syntax-expand con) 
		    (syntax-expand then))]
	   [if-else-exp (con then els)
			(if-else-exp
			 (syntax-expand con)
			 (syntax-expand then)
			 (syntax-expand els))]
	   [cond-exp (preds bodies)
		     (if (null? preds)
			 (if (null? bodies)
			     (app-exp (var-exp 'void) '())
			     (syntax-expand (car bodies)))
			 (if-else-exp 
			  (syntax-expand (car preds))
			  (syntax-expand (car bodies))
			  (syntax-expand (cond-exp (cdr preds) (cdr bodies)))))]
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
	   [app-exp (rator rands)
		    (app-exp (syntax-expand rator) (map syntax-expand rands))]
	   [set!-exp (id body)
	   	(set!-exp
	   		id
	   		(syntax-expand body))]
	   [while-exp (test bodies)
	   	(while-exp
	   		(syntax-expand test)
	   		(map syntax-expand bodies))]
	   [define-exp (var val)
	   	(define-exp
	   		var
	   		(syntax-expand val))]
	   [else exp])))

(define extract-let-vars
  (lambda (x)
    (cases expression x
	   [let-declaration-exp (var binding) var]
	   [else (eopl:error 'eval-exp "Bad Let Declaration ~s Parse Error" x)])))

(define extract-let-bindings
  (lambda (x)
    (cases expression x
	   [let-declaration-exp (var binding) binding]
	   [else (eopl:error 'eval-exp "Bad Let Declaration ~s Parse Error" x)])))

(define make-set!-exps
	(lambda (vars bindings)
		(if (null? vars)
			'()
			(cons (set!-exp (unparse-exp (1st vars)) (1st bindings))
				(make-set!-exps (cdr vars) (cdr bindings))))))