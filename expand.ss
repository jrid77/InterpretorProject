(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[let-exp (declaration body) 
				(app-exp 
					(lambda-exp
						(map unparse-exp (map extract-let-vars declaration)) body) 
					(map extract-let-bindings declaration))]
			[begin-exp (bodies)
				(syntax-expand 
					(let-exp '() bodies))]
			[cond-exp (tests bodies)
				(letrec 
					[(helper)
						;; Need to iterate over each list and make if-exp
						;; Base case is else or null. Treat each differently
						]
					)]
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

