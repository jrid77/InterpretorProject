;;; Main "read-eval-print" loop.
(define rep      
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      (eopl:pretty-print answer) (newline)
      (rep))))

;;; Evaluates one expression in the global environment for the grading server
(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

;;; Evaluates a form in the global environment
;; later we may add things that are not expressions.
;;TODO
(define top-level-eval
  (lambda (form)
    (cases expression form
     (define-exp (var val) 
        (begin
          (mutate-global-env var (eval-exp val global-env))
          (void)))
     (else (eval-exp form (empty-env))))))

;;; Main component of the interpreter
(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env k)
      (cases expression exp
	     [lit-exp (datum) (apply-k k datum)]
	     [var-exp (id) ;look up its value.
		    (apply-env env id k (var-fail-k id k))]
	     [app-exp (rator rands)
                (eval-exp rator env (rator-k rands env k))
	     [if-else-exp (con then els)
			          (eval-exp con env (test-two-arm-k then els env k))]
	     [if-exp (con then)
		            (eval-exp con env (test-one-arm-k then env k))]
	     [lambda-exp (declaration body) 
			          (apply-k k (closure declaration body env))]
	     [lambda-exp-one-var (declaration body)
				        (apply-k k (closure-one-var declaration body env))]
	     [lambda-exp-improper-list (declaration body)
				       (apply-k k (closure-improper-list declaration body env))]
  		 [set!-exp (id exp)
  				(apply-env-ref env 
  								id
  							  (ref-k exp env k)
                  (set-fail-k id exp env k))]	
	     ;;TODO
       [while-exp (test bodies)
  			(if (eval-exp test env)
  			    (eval-bodies (append bodies (list exp)) env))]
       ;;TODO
       [define-exp (var val)
         (top-level-eval exp)]
	     [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define apply-k
  (lambda (k . vals)
    (cases continuation k
      [rator-k (rands env k)
        (eval-rands rands env (rands-k (car vals) k))]
      [rands-k (proc-val k)
        (apply-proc proc-val (car vals) k)]
      [test-two-arm-k (then els env k)
        (if (car vals)
          (eval-exp then env k)
          (eval-exp els env k))]
      [test-one-arm-k (then env k)
        (if (car vals)
          (eval-exp then env k)
          (apply-k k))]
      [ref-k (exp env k)
        (eval-exp exp env (set-ref-k (car vals) k))]
      [set-ref-k (ref k)
        (set-ref! ref (car vals))]
      [find-pos-k (ls env sym succeed fail)
        (if (number? (car vals))
            (apply-k succeed (list-ref ls (car vals)))
            (apply-env-ref env sym succeed fail))]
      [index-cdr-res-k (k)
        (if (number? (car vals))
          (apply-k k (+ 1 (car vals)))
          (apply-k k #f))]
      [ref-to-deref-k (k)
          (apply-k k (deref (car vals)))]
      [var-fail-k (id k)
          (apply-env-ref global-env id k (lookup-error-k id))]
      [set-fail-k (id exp env k)
         (apply-env-ref 
          global-env 
          id 
          (ref-k exp env k) 
          (lookup-error-k id))]
      [lookup-error-k (id)
          (eopl:error 'apply-env 
                     "variable not found in environment: ~s"
                     id)]      
      )))

;;; Evaluate the list of operands (expressions), putting results into a list
(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (e) (eval-exp e env k)) rands (lambda (v) v))))


;;; Evaluate the bodies returning the value of the last
;;TODO
(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env)
	(begin
	  (eval-exp (car bodies) env)
	  (eval-bodies (cdr bodies) env)))))

;;; Apply a procedure to its arguments.
;;TODO
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc op args)]
	   [closure (ids bodies env) 
		    (eval-bodies
		     bodies
		     (extend-env ids args env))]
	   [closure-one-var (ids bodies env)
			    (eval-bodies
			     bodies
			     (extend-env ids (list args) env))]
	   [closure-improper-list (ids bodies env)
				  (eval-bodies
				   bodies
				   (extend-env
				    ids
				    (let loop ([new-ids ids] [args args])
				      (if (null? (cdr new-ids))
					  (list args)
					  (cons (1st args) (loop (cdr new-ids) (cdr args)))))
				    env))]
	   [else (error 'apply-proc
			"Attempt to apply bad procedure: ~s" 
			proc-value)])))

(define map-cps
  (lambda (proc ls k)
    (if (null? ls)
      (apply-k k '())
      (proc (car ls)
        (lambda (applied-car)
          (map-cps (cdr ls)
            (lambda (mapped-cdr)
              (apply-k k (cons applied-car mapped-cdr)))))))))		
					
;; Establishing which primitives we support
(define *prim-proc-names*
  '(+ - * / add1 sub1 cons = < > <= >= not
      car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list-tail
      list null? assq eq? eqv? equal? atom? length list->vector list? pair? procedure?
      vector->list vector make-vector vector-ref vector? number? symbol? zero? append
      set-car! set-cdr! vector-set! display newline map apply member quotient void))

(define *global-env-vars* (append *prim-proc-names* '()))

;; Initializes a global environment with only primitives
(define make-init-env
  (lambda ()
     (extend-env            
      *prim-proc-names*   
      (map prim-proc *prim-proc-names*)
      (empty-env))))

(define global-env (make-init-env))

(define mutate-global-env
  (lambda (sym val)
    (cases environment global-env
      (extended-env-record (syms vals env)
        (set! global-env (extend-env 
                          (cons sym syms)
                          (cons val (map unbox vals))
                          (empty-env))))
      (else (eopl:error 'mutate-global-env "How the hell did we get here? ~s" global-env)))))

(define reset-global-env
  (lambda ()
    (set! global-env (make-init-env))))

;;; Cases out our primitive procedures
;;TODO
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
      [(eqv?) (eqv? (1st args) (2nd args))]
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
      [(append) (apply append args)]
      [(list-tail) (list-tail (1st args) (2nd args))]
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
