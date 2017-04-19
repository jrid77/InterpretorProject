;;; Main "read-eval-print" loop.
(define rep      
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: when creating closures, display nothing
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

;;; Evaluates one expression in the global environment for the grading server
(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))

;;; Evaluates a form in the global environment
;; later we may add things that are not expressions.
(define top-level-eval
  (lambda (form)
    (eval-exp form (empty-env))))

;;; Main component of the interpreter
(define eval-exp
 (let ([identity-proc (lambda (x) x)])
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id) ;look up its value.
		(apply-env env 
		           id 
      	           identity-proc ; procedure to call if id is in the environment
				   (lambda () ; procedure to call if id not in env
					  (apply-env global-env
					             id
								 identity-proc
								(lambda () (eopl:error 'apply-env 
									"variable not found in environment: ~s"
									id)))))]
	  [let-exp (declaration body)
		(eval-bodies body
					 (extend-env (map unparse-exp (map extract-let-vars declaration)) (eval-rands (map extract-let-bindings declaration) env) env))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))


;;; Pulling vars out of let-exp
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


;;; Evaluate the list of operands (expressions), putting results into a list
(define eval-rands
  (lambda (rands env)
	(map (lambda (e)
			(eval-exp e env))
			rands)))
	
;;; Evaluate the bodies returning the value of the last
(define eval-bodies
	(lambda (bodies env)
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env)
			(begin
				(eval-exp (car bodies) env)
				(eval-bodies (cdr bodies) env)))))
				
;;; Apply a procedure to its arguments.
;;  TODO: User-defined procedures
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
	  [closure (ids bodies env)
		(eval-bodies bodies
					 (extend-env ids args env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

;; Establishing which primitives we support
(define *prim-proc-names* '(+ - * add1 sub1 cons =))

;; Initializes a global environment with only primitives
(define global-env
  (extend-env            
     *prim-proc-names*   
     (map prim-proc *prim-proc-names*)
     (empty-env)))

;;; Cases out our primitive procedures
(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))