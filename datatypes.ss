
; ;;; Parsed expression datatypes
; (define-datatype expression expression?
;   [var-exp        ; variable references
;    (id symbol?)]
;   [lit-exp        ; "Normal" data.  Did I leave out any types?
;    (datum
;     (lambda (x)
;       (ormap 
;        (lambda (pred) (pred x))
;        (list number? vector? boolean? symbol? string? pair? null?))))]
;   [let-exp
; 		(var (list-of symbol?))
; 		(exps (list-of expression?))
; 		(bodies (list-of expression?))]
;   [app-exp        ; applications
;    (rator expression?)
;    (rands (list-of expression?))])

(define lit?
  (lambda (exp)
    (or 
      (number? exp)
      (string? exp)
      (null? exp)
      (vector? exp)
      (char? exp)
      (and (equal? 'quote (car exp)) (list? (cdr exp))))))

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
    (id lit?)]
  [lambda-exp
    (declaration (lambda (x) (or (list-of expression?) (symbol? x))))
    (body (list-of expression?))]
  [lambda-var-exp
    (body (lambda (x)
        (cond 
          [(symbol? x) #t]
          [(list? x)
            (let recursive-helper ([rest x])
              (if (or (null? rest) (symbol? rest))
                #t
                (and (symbol? (car rest)) (recursive-helper (cdr rest)))))]
          [else #f])))]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))]
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
  [let-single-exp
    (var expression?)
    (body expression?)]
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
    (body expression?)])

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
