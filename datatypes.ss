
;;; Parsed expression datatypes
(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [let-exp
		(var (list-of symbol?))
		(exps (list-of expression?))
		(bodies (list-of expression?))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))])

	
;;; Datatype for procedures
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
	(ids (list-of symbol?))
	(bodies (list-of expression?))
	(env  environment?)])
	 
	
;;; Environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
   
(define scheme-value?
  (lambda (x) #t))
