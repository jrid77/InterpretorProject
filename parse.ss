; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
; (define 1st car)
; (define 2nd cadr)
; (define 3rd caddr)

; (define parse-exp         
;   (lambda (datum)
;     (cond
;      [(symbol? datum) (var-exp datum)]
;      [(number? datum) (lit-exp datum)]
;      [(pair? datum)
;       (cond
; 	   [(eqv? 'let (1st datum))
; 		(let-exp (map car (2nd datum))
; 				 (map parse-exp (map cadr (2nd datum)))
; 				 (map parse-exp (cddr datum)))]
;        [else (app-exp (parse-exp (1st datum))
; 		      (map parse-exp (cdr datum)))])]
;      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lambda-exp (declaration body) 
        (append 
          (list 
            'lambda 
            (if (list? declaration)
              (map unparse-exp declaration)
              declaration))
          (map unparse-exp body))]
      [lambda-var-exp (body) (append (list 'lambda) (map unparse-exp exp))]
      [app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
      [if-else-exp (con then els) 
        (append 
          (list 'if (unparse-exp con)) 
          (list (unparse-exp then)) 
          (list (unparse-exp els)))]
      [if-exp (con then)
        (append
          (list 'if (unparse-exp con))
          (list (unparse-exp then)))]
      [lit-exp (id) id]
      [named-let-exp (name declaration body)
        (append
          (list 'let name (map unparse-exp declaration))
          (map unparse-exp body))]
      [let-exp (declaration body)
        (append 
          (list 'let (map unparse-exp declaration))
          (map unparse-exp body))]
      [letrec-exp (declaration body)
        (append
          (list 'letrec (map unparse-exp declaration))
          (map unparse-exp body))]
      [let*-exp (declaration body)
        (append
          (list 'let* (map unparse-exp declaration))
          (map unparse-exp body))]
      [set!-exp (id body)
        (list 'set! id (map unparse-exp body))]
      [let-single-exp (var body)
        (list (unparse-exp var) (unparse-exp body))]
      )))

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


; TO PARSE 
; var-exp 
; lit-exp
; lambda-exp
; lambda-var-exp
; app-exp
; if-else-exp
; if-exp
; lit-exp
; let-single-exp
; let-exp
; named-let-exp
; let*-exp
; letrec-exp
; set!-exp
(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(lit? datum) (lit-exp datum)]
     [(list? datum)
      (cond ; Everthing in here is a list of expressions. Where the bulk is.
        [(equal? (car datum) 'lambda) ; lambda-exp
          (if (< (length datum) 3)
            (eopl:error 'parse-exp "lambda expression ~s used incorrectly" datum)
          (if (and (list? (2nd datum)) (andmap symbol? (2nd datum)))
            (lambda-exp (map parse-exp (2nd datum)) (map parse-exp (cddr datum)))
            (if (symbol? (2nd datum))
              (lambda-exp (2nd datum) (map parse-exp (cddr datum)))
              (eopl:error 'parse-exp "lambda expression ~s used incorrectly" datum))))]
      [(equal? (car datum) 'if) ; if exp
        (cond 
          [(= 3 (length datum))
            (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
          [(= 4 (length datum))
            (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
          [else 
            (eopl:error 'parse-exp "Incorrect use of if expression ~s" datum)])]
      [(member (car datum) '(let let* letrec)) ; All of the lets
        (cond
          [(symbol? (2nd datum))
            (if (or (= 3 (length datum)) (not (equal? 'let (1st datum))))
              (eopl:error 'parse-exp "incorrect use of named let ~s" datum)
              (named-let-exp (var-exp (2nd datum)) (let-parse-helper (3rd datum)) (map parse-exp (cdddr datum))))]
          [(equal? (car datum) 'let)
            (if (< (length datum) 3)
              (eopl:error 'parse-exp "incorrect length of let ~s" datum)
              (let-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
          [(equal? (car datum) 'letrec)
            (if (< (length datum) 3)
              (eopl:error 'parse-exp "incorrect length of let ~s" datum)
                (letrec-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
          [(equal? (car datum) 'let*)
            (if (< (length datum) 3)
              (eopl:error 'parse-exp "incorrect length of let ~s" datum)
              (let*-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
            )]
      [(equal? (1st datum) 'set!)
        (if (= 3 (length datum))
          (if (symbol? (2nd datum))
            (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
            (eopl:error 'parse-exp "set! ~s must take a symbol as second arg" datum))
          (eopl:error 'parse-exp "Incorrect number of args to set! ~s" datum))]
      [else (app-exp (parse-exp (1st datum))
         (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define let-parse-helper
  (lambda (l)
    (let inner-helper ((r l))
      (if (null? r)
        '()
        (if (or 
          (not (list? r))
          (not (list? (car r)))
          (not (symbol? (caar r)))
          (not (= 2 (length (car r)))))
            (eopl:error 'parse-exp "incorrect let expression ~s" r)
            (cons 
              (let-single-exp (parse-exp (caar r)) (parse-exp (cadar r)))
              (inner-helper (cdr r))))))))