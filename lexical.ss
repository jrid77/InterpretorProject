;;;Problem 3 lexical-address
(define lexical-address
  (lambda (exp)
    (let loop ([exp exp] [ls '()])
      (cond [(null? exp) '()]
            [(symbol? exp) (lookup-address ls exp)]
            [(eq? 'lambda (car exp))
             (cons 'lambda (cons (cadr exp)
				 (loop (cddr exp) (cons (cadr exp) ls))))]
            [(eq? 'if (car exp))
             (cons 'if (loop (cdr exp) ls))]
            [(eq? 'set! (car exp))
             (append (list 'set! (cadr exp))
		     (loop (cddr exp) ls))]
            [(eq? 'let (car exp))
             (cons 'let
		   (cons (map list 
			      (map car (cadr exp))
			      (map (lambda (x)
				     (loop (cadr x) ls))
				   (cadr exp)))
			 (loop (cddr exp) (cons (map car (cadr exp)) ls))))]
            [else
	     (cons (loop (car exp) ls)
		   (loop (cdr exp) ls))]))))

(define lookup-address
  (lambda (ls exp)
    (letrec ([lookup
	      (lambda (ls exp)
		(let loop ([ls ls] [depth 0])
		  (cond [(null? ls) (list ': 'free exp)]
			[else (let ([pos (find-position exp (car ls))])
				(if (eq? -1 pos)
				    (loop (cdr ls) (add1 depth))
				    (list ': depth pos)))])))]
	     [find-position
	      (lambda (obj ls)
		(let loop ([ls ls] [pos 0])
		  (cond [(null? ls) -1]
		      [(eq? obj (car ls)) pos]
		      [else (loop (cdr ls) (add1 pos))])))])
      (lookup ls exp))))
