; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)



(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(literal? datum) (if (list? datum) (lit-exp (2nd datum)) (lit-exp datum))]
     [(list? datum)
      (cond ; Everthing in here is a list of expressions. Where the bulk is.
        [(equal? (car datum) 'lambda) ; lambda-exp
          (cond 
              [(< (length datum) 3) (eopl:error 'parse-exp "Lambda Expression: ~s Incorrect Length" datum)]
              [(and (list? (2nd datum)) (andmap symbol? (2nd datum)))
                  (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
              [(symbol? (2nd datum))
                  (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
              [(and (pair? (2nd datum)) (ilos? (2nd datum)))
                  (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
              [else (eopl:error 'parse-exp "Lambda Expression: ~s Incorrect Use of Arguments" datum)]
            )]
        [(equal? (car datum) 'while)
          (while-exp (syntax-expand (parse-exp (2nd datum))) (map syntax-expand (map parse-exp (cddr datum))))]
        [(equal? (car datum) 'if) ; if exp
          (cond 
            [(= 3 (length datum))
              (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
            [(= 4 (length datum))
              (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            [else 
              (eopl:error 'parse-exp "If Expression: ~s Incorrect Length" datum)])]
        [(equal? (car datum) 'let)
          (if (symbol? (2nd datum))
            (if (< (length datum) 4)
             (eopl:error 'parse-exp "Named Let: ~s Incorrect Length" datum)
             (named-let-exp (var-exp (2nd datum)) (let-parse-helper (3rd datum)) (map parse-exp (cdddr datum)))))
            (if (< (length datum) 3)
              (eopl:error 'parse-exp "Let Expression: ~s Incorrect Length" datum)
              (let-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
        [(equal? (car datum) 'letrec)
          (if (< (length datum) 3)
            (eopl:error 'parse-exp "Letrec Expression: ~s Incorrect Length" datum)
              (letrec-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
        [(equal? (car datum) 'let*)
          (if (< (length datum) 3)
            (eopl:error 'parse-exp "Let* Expression: ~s Incorrect Length" datum)
            (let*-exp (let-parse-helper (2nd datum)) (map parse-exp (cddr datum))))]
        [(equal? (1st datum) 'set!)
          (if (= 3 (length datum))
            (if (symbol? (2nd datum))
              (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
              (eopl:error 'parse-exp "Set! Expression: ~s Incorrect Argument Type" datum))
            (eopl:error 'parse-exp "Set! Expression: ~s Incorrect Number of Arguments" datum))]
        [(equal? (1st datum) 'begin)
          (begin-exp (map parse-exp (cdr datum)))]
        [else (app-exp (parse-exp (1st datum))
           (map parse-exp (cdr datum)))])]
       [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define let-parse-helper
  (lambda (ls)
    (let inner-helper ((rest ls))
      (if (null? rest)
        '()
        (if (or 
          (not (list? rest))
          (not (list? (car rest)))
          (not (symbol? (caar rest)))
          (not (= 2 (length (car rest)))))
            (eopl:error 'parse-exp "incorrect let expression ~s" r)
            (cons 
              (let-declaration-exp (parse-exp (caar rest)) (parse-exp (cadar rest)))
              (inner-helper (cdr rest))))))))
			  
(define literal?
  (lambda (exp)
    (or 
      (boolean? exp)
      (number? exp)
      (string? exp)
      (null? exp)
      (vector? exp)
      (char? exp)
      (equal? 'quote (car exp)))))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (declaration body) 
        (append 
          (list 
            'lambda 
            declaration
           (map unparse-exp body)))]
      [if-exp (con then)
        (append
          (list 'if (unparse-exp con))
          (list (unparse-exp then)))]
      [if-else-exp (con then els) 
        (append 
          (list 'if (unparse-exp con)) 
          (list (unparse-exp then))
          (list (unparse-exp els)))]
      [named-let-exp (name declaration body)
        (append
          (list 'let name (map unparse-exp declaration))
          (map unparse-exp body))]
      [let-exp (declaration body)
        (append 
          (list 'let (map unparse-exp declaration))
          (map unparse-exp body))]
      [let-declaration-exp (var binding)
        (list (unparse-exp var) (unparse-exp binding))]
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
      [app-exp (rator rands) (cons (unparse-exp rator) (map unparse-exp rands))]
      [begin-exp (bodies) (append (list 'begin) (map unparse-exp bodies))]
      [while-exp (test bodies)
        (cons* 'while (unparse-exp test) (map unparse-exp bodies))]
      )))



