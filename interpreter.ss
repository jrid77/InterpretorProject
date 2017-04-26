;;; Main "read-eval-print" loop.
(define rep      
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: when creating closures, display nothing also figure out this void thing
      (eopl:pretty-print answer) (newline)
      (rep))))

;;; Evaluates one expression in the global environment for the grading server
(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

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
				   (extend-env (map unparse-exp (map extract-let-vars declaration))
					       (eval-rands (map extract-let-bindings declaration) env)
					       env))]
	     [app-exp (rator rands)
		      (let ([proc-value (eval-exp rator env)]
			    [args (eval-rands rands env)])
			(apply-proc proc-value args))]
	     [if-else-exp (con then els)
			  (if (eval-exp con env)
			      (eval-exp then env)
			      (eval-exp els env))]
	     [if-exp (con then)
		     (if (eval-exp con env)
			 (eval-exp then env)
			 (void))]
	     [lambda-exp (declaration body) 
			 (closure 
			  declaration
			  body
			  env)]
	     [while-exp (test bodies)
			(if (eval-exp test env)
			    (eval-bodies (append bodies (list exp)) env))]
	     [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

;;; Evaluate the list of operands (expressions), putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (e) (eval-exp e env)) rands)))

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
		    (eval-bodies
		     bodies
		     (cond [(list? ids) (extend-env ids args env)]
				   [(symbol? ids) (extend-env (list ids) (list args) env)]
				   [else 
					(let ([idslist (flatten ids)])
					  (letrec ([helper 
						(lambda (args ls)
						  (if (null? (cdr ls))
							  (list args)
							  (cons (1st args) (helper (cdr args) (cdr ls)))))])
					   (extend-env idslist (helper args idslist) env)))]))]
	   [else (error 'apply-proc
			"Attempt to apply bad procedure: ~s" 
			proc-value)])))

(define flatten
  (lambda (ls)
    (cond 
     [(null? ls) '()]
     [(pair? ls) (append (flatten (car ls)) (flatten (cdr ls)))]
     [else (list ls)])))					
					
;; Establishing which primitives we support
(define *prim-proc-names*
  '(+ - * / add1 sub1 cons = < > <= >= not
      car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr 
      list null? assq eq? equal? atom? length list->vector list? pair? procedure?
      vector->list vector make-vector vector-ref vector? number? symbol? zero?
      set-car! set-cdr! vector-set! display newline map apply member quotient void))

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
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(<=) (apply <= args)]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(list) args]
      [(null?) (null? (1st args))]
      [(void) (void)]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (if (null? (cdr args)) (make-vector (1st args)) (make-vector (1st args) (2nd args)))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(member) (member (1st args) (2nd args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (if (null? (cdr args)) (display (1st args)) (display (1st args) (2nd args)))]
      [(newline) (if (null? args) (newline) (newline (1st args)))]
      [(quotient) (apply quotient args)]
      [(map) (letrec 
				 [(helper (lambda (ls)
						(if (null? ls)
						'()
						(cons (apply-proc (1st args) (list (1st ls)))
							  (helper (cdr ls))))))]
				   (helper (2nd args)))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
		   "Bad primitive procedure name: ~s" 
		   prim-op)])))
