;;;Defines datatypes to be used in the interpreter

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (id lit?)]
  [lambda-exp
   (declaration (list-of symbol?))
   (body (list-of expression?))]
  [lambda-exp-one-var
   (declaration (list-of symbol?))
   (body (list-of expression?))]
  [lambda-exp-improper-list
   (declaration (list-of symbol?))
   (body (list-of expression?))]
  [if-else-exp
   (con expression?)
   (then expression?)
   (els expression?)]
  [if-exp
   (con expression?)
   (then expression?)]
  [let-exp
   (declaration (list-of expression?))
   (body (list-of expression?))]
  [let-declaration-exp
   (var expression?)
   (binding expression?)]
  [named-let-exp
   (name symbol?)
   (vars (list-of symbol?))
   (bindings (list-of expression?))
   (let-bodies (list-of expression?))]
  [let*-exp
   (declaration (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp
   (declaration (list-of expression?))
   (body (list-of expression?))]
  [set!-exp
   (id symbol?)
   (body expression?)]
  [and-exp
   (preds (list-of expression?))]
  [or-exp
   (preds (list-of expression?))]
  [begin-exp
   (bodies (list-of expression?))]
  [while-exp
   (test expression?)
   (bodies (list-of expression?))]
  [cond-exp
   (tests (list-of expression?))
   (bodies (list-of expression?))]
  [case-exp
   (id expression?)
   (keys (list-of (list-of lit-exp?)))
   (bodies (list-of expression?))]
  [define-exp
   (var symbol?)
   (val expression?)]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))])	

(define lit?
  (lambda (exp)
    (or (boolean? exp)
	(number? exp)
	(string? exp)
	(null? exp)
	(vector? exp)
	(char? exp)
	(list? exp)
	(symbol? exp))))

(define ilos?
  (lambda (ls)
    (cond [(null? ls) #f]
          [(not (pair? (cdr ls))) (and (symbol? (cdr ls)) (symbol? (car ls)))]
          [else (and (symbol? (car ls)) (ilos? (cdr ls)))])))

(define lit-exp?
  (lambda (exp)
    (cases expression exp
	   [lit-exp (id) #t]
	   [else #f])))

;;; Environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of box?))
   (env environment?)))

;;; Datatype for procedures
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (ids (list-of symbol?))
   (bodies (list-of expression?))
   (env  environment?)]
  [closure-one-var
   (ids (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)]
  [closure-improper-list
   (ids (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)]
  [continuation-proc 
   (k continuation?)])

(define-datatype continuation continuation?
	[init-k]
	[rator-k 
		(rands (list-of expression?))
		(env environment?)
		(k continuation?)]
	[rands-k
		(proc-val scheme-value?)
		(k continuation?)]
	[test-two-arm-k
		(then expression?)
		(els expression?)
		(env environment?)
		(k continuation?)]
	[test-one-arm-k
		(then expression?)
		(env environment?)
		(k continuation?)]
	[ref-k
		(exp expression?)
		(env environment?)
		(k continuation?)]
	[set-ref-k
		(ref scheme-value?)
		(k continuation?)]
	[find-pos-k
		(vals (list-of scheme-value?))
		(env environment?)
		(sym symbol?)
		(succeed continuation?)
		(fail continuation?)]
	[index-cdr-res-k 
		(k continuation?)]
	[ref-to-deref-k
		(k continuation?)]
	[var-fail-k
		(id scheme-value?)
		(k continuation?)]
	[lookup-error-k
		(id scheme-value?)]
	[set-fail-k
		(id scheme-value?)
		(exp expression?)
		(env environment?)
		(k continuation?)]
	[eval-car-k
		(bodies (list-of expression?))
		(env environment?)
		(k continuation?)]
	[while-test-k
		(bodies (list-of expression?))
		(exp expression?)
		(env environment?)
		(k continuation?)]
	[append-while-k
		(env environment?)
		(k continuation?)]
	[append-rest-result-k
		(first scheme-value?)
		(k continuation?)]
	[define-global-k
		(var scheme-value?)
		(k continuation?)]
	[closure-k
		(bodies (list-of expression?))
		(k continuation?)]
	[truncated-k
		(first scheme-value?)
		(k continuation?)]
	[improper-k
		(bodies (list-of expression?))
		(ids (list-of scheme-value?))
		(env environment?)
		(k continuation?)]
	[extend-env-k
		(syms (list-of scheme-value?))
		(env environment?)
		(k continuation?)]
	[map-car-k
		(rest-of-list (list-of scheme-value?))
		(proc procedure?)
		(k continuation?)]
	[mapped-cdr-k 
		(first-of-list scheme-value?)
		(k continuation?)]
	[set-global-env-k 
		(k continuation?)]
	)
  
(define scheme-value?
  (lambda (x) #t))
