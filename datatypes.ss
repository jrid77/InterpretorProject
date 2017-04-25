(define lit?
  (lambda (exp)
    (or 
      (boolean? exp)
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
          [else 
            (and (symbol? (car ls))
                  (ilos? (cdr ls)))])))

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
    (id lit?)]
  [lambda-exp
    (declaration (lambda (x) (or (list-of symbol?) (symbol? x) (ilos? x))))
    (body (list-of expression?))]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))]
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
    (name expression?)
    (declaration (list-of expression?))
    (body (list-of expression?))]
  [let*-exp
    (declaration (list-of expression?))
    (body (list-of expression?))]
  [letrec-exp
    (declaration (list-of expression?))
    (body (list-of expression?))]
  [set!-exp
    (id symbol?)
    (body expression?)]
  [begin-exp
    (bodies (list-of expression?))]
  )	 
	
;;; Environment type definitions
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
 
;;; Datatype for procedures
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
	(ids (lambda (x) (or (list-of symbol?) (symbol? x) (ilos? x))))
	(bodies (list-of expression?))
	(env  environment?)]) 
 
(define scheme-value?
  (lambda (x) #t))
