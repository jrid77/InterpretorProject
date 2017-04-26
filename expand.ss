;;;Expands supported syntax by replacing our specialized parse expressions into more basic expressions
;; i.e. cond -> nested ifs

(define syntax-expand
  (lambda (exp)
    (cases expression exp
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
			 then
			 els)]
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
