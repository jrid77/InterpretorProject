;;; Main "read-eval-print" loop.
(define rep      
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))) (init-k))])
      (eopl:pretty-print answer) (newline)
      (rep))))

;;; Evaluates one expression in the global environment for the grading server
(define eval-one-exp
  (lambda (x) 
    (top-level-eval (syntax-expand (parse-exp x)) (init-k))))

;;; Evaluates a form in the global environment
;; later we may add things that are not expressions.
(define top-level-eval
  (lambda (form k)
    (cases expression form
     (define-exp (var val)
        (eval-exp val global-env (define-global-k var k)))     
      (else (eval-exp form (empty-env) k)))))

;;; Main component of the interpreter
(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env k)
      (cases expression exp
	     [lit-exp (datum) (apply-k k datum)]
	     [var-exp (id) ;look up its value.
		    (apply-env env id k (var-fail-k id k))]
	     [app-exp (rator rands)
                (eval-exp rator env (rator-k rands env k))]
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
       [while-exp (test bodies)
  			 (eval-exp test env (while-test-k bodies exp env k))]
       [define-exp (var val)
         (top-level-eval exp k)]
	     [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define apply-k
  (lambda (k . vals)
    (cases continuation k
      [init-k () (car vals)]
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
      [eval-car-k (bodies env k)
          (eval-bodies bodies env k)]
      [while-test-k (bodies exp env k)
          (if (car vals)
            (append-cps bodies (list exp) (append-while-k env k)))]
      [append-while-k (env k)
            (eval-bodies (car vals) env k)]
      [append-rest-result-k (first k)
            (apply-k k (cons first (car vals)))]
      [define-global-k (var k)
            (mutate-global-env var (car vals) k)]
      [closure-k (bodies k)
            (eval-bodies bodies (car vals) k)]
      [truncated-k (first k)
            (apply-k k (cons first (car vals)))]
      [improper-k (bodies ids env k)
            (extend-env
              ids
              (car vals)
              env
              (closure-k bodies k))]
      [extend-env-k (syms env k)
          (begin
            (extended-env-record syms (car vals) env)
            (apply-k k))]
      )))

;;; Evaluate the list of operands (expressions), putting results into a list
(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (e) (eval-exp e env k)) rands (lambda (v) v))))


;;; Evaluate the bodies returning the value of the last
(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
	   (eval-exp (car bodies) env k)
	   (eval-exp (car bodies) env (eval-car-k (cdr bodies) env k)
	  ))))

;;; Apply a procedure to its arguments.
(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc op args k)]
	   [closure (ids bodies env)
        (extend-env id args env (closure-k bodies k))]
	   [closure-one-var (ids bodies env)
        (extend-env id (list args) env (closure-k bodies k))]
	   [closure-improper-list (ids bodies env)
          (truncate-args
            ids
            args
            (improper-k bodies ids env k))]
	   [else (error 'apply-proc
			"Attempt to apply bad procedure: ~s" 
			proc-value)])))

(define truncate-args
  (lambda (new-ids args k)
    (if (null? (cdr new-ids))
      (apply-k k (list args))
      (truncate-args (cdr new-ids) (cdr args) (truncated-k (car args) k)) 
        )))

;; TODO
(define map-cps
  (lambda (proc ls k)
    (if (null? ls)
      (apply-k k '())
      (proc (car ls)
        (lambda (applied-car)
          (map-cps (cdr ls)
            (lambda (mapped-cdr)
              (apply-k k (cons applied-car mapped-cdr)))))))))

(define append-cps
  (lambda (l1 l2 k)
    (if (null? l1)
      (apply-k k l2)
      (append-cps (cdr l1) l2 (append-rest-result-k (car l1) k)))))
					
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
  (lambda (sym val k)
    (cases environment global-env
      (extended-env-record (syms vals env)
        (set!-cps global-env (extend-env 
                          (cons sym syms)
                          (cons val (map unbox vals))
                          (empty-env)))
                  k)
      (else (eopl:error 'mutate-global-env "How the hell did we get here? ~s" global-env)))))

(define set!-cps
  (lambda (var val k)
    (begin
      (set! var val)
      (apply-k k))))

(define reset-global-env
  (lambda ()
    (set! global-env (make-init-env))))

;;; Cases out our primitive procedures
;;TODO
(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(=) (apply-k k (apply = args))]
      [(<) (apply-k k (apply < args))]
      [(>) (apply-k k (apply > args))]
      [(>=) (apply-k k (apply >= args))]
      [(<=) (apply-k k (apply <= args))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(car) (apply-k k (car (1st args)))]
      [(cdr) (apply-k k (cdr (1st args)))]
      [(caar) (apply-k k (caar (1st args)))]
      [(cadr) (apply-k k (cadr (1st args)))]
      [(cdar) (apply-k k (cdar (1st args)))]
      [(cddr) (apply-k k (cddr (1st args)))]
      [(caaar) (apply-k k (caaar (1st args)))]
      [(caadr) (apply-k k (caadr (1st args)))]
      [(cadar) (apply-k k (cadar (1st args)))]
      [(caddr) (apply-k k (caddr (1st args)))]
      [(cdaar) (apply-k k (cdaar (1st args)))]
      [(cdadr) (apply-k k (cdadr (1st args)))]
      [(cddar) (apply-k k (cddar (1st args)))]
      [(cdddr) (apply-k k (cdddr (1st args)))]
      [(list) (apply-k k args)]
      [(null?) (apply-k k (null? (1st args)))]
      [(void) (apply-k k (void))]
      [(assq) (apply-k k (assq (1st args) (2nd args)))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(atom?) (apply-k k (atom? (1st args)))]
      [(length) (apply-k k (length (1st args)))]
      [(list->vector) (apply-k k (list->vector (1st args)))]
      [(list?) (apply-k k (list? (1st args)))]
      [(pair?) (apply-k k (pair? (1st args)))]
      [(procedure?) (apply-k k (proc-val? (1st args)))]
      [(vector->list) (apply-k k (vector->list (1st args)))]
      [(vector) (apply-k k (apply vector args)]
      [(make-vector) (apply-k k (if (null? (cdr args)) (make-vector (1st args)) (make-vector (1st args) (2nd args))))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(vector?) (apply-k k (vector? (1st args)))]
      [(number?) (apply-k k (number? (1st args)))]
      [(symbol?) (apply-k k (symbol? (1st args)))]
      [(member) (apply-k k (member (1st args) (2nd args)))]
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(display) (apply-k k (if (null? (cdr args)) (display (1st args)) (display (1st args) (2nd args))))]
      [(newline) (apply-k k (if (null? args) (newline) (newline (1st args))))]
      [(quotient) (apply-k k (apply quotient args))]
      [(append) (apply-k k (apply append args))]
      [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
      ;; TODO
      [(map)]
      [(apply) (apply-proc (1st args) (2nd args) k)]
      [else (error 'apply-prim-proc 
		   "Bad primitive procedure name: ~s" 
		   prim-op)])))
