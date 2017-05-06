; ;;;Problem 3 lexical-address
; (define lexical-address
;   (lambda (exp)
;     (let loop ([exp exp] [ls '()])
;       (cond [(null? exp) '()]
;             [(symbol? exp) (lookup-address ls exp)]
;             [(eq? 'lambda (car exp))
;              (cons 'lambda (cons (cadr exp)
; 				 (loop (cddr exp) (cons (cadr exp) ls))))]
;             [(eq? 'if (car exp))
;              (cons 'if (loop (cdr exp) ls))]
;             [(eq? 'set! (car exp))
;              (append (list 'set! (cadr exp))
; 		     (loop (cddr exp) ls))]
;             [(eq? 'let (car exp))
;              (cons 'let
; 		   (cons (map list 
; 			      (map car (cadr exp))
; 			      (map (lambda (x)
; 				     (loop (cadr x) ls))
; 				   (cadr exp)))
; 			 (loop (cddr exp) (cons (map car (cadr exp)) ls))))]
;             [else
; 	     (cons (loop (car exp) ls)
; 		   (loop (cdr exp) ls))]))))

; lambda
; if / if else
; app exp
; set!
; while


(define lexical-address
	(lambda (exp)
		(let loop ([exp exp] [ls '()])
			(if (symbol? exp)
				(lookup-address ls exp)
				(cases expression exp
					[lambda-exp (declaration body)
						(lambda-exp
							declaration
							(map (lambda (x)
									(loop x (cons declaration ls)))
								body))]
					[if-exp (con then)
						(if-exp
							(loop con ls)
							(loop then ls))]
					[if-else-exp (con then els)
						(if-else-exp
							(loop con ls)
							(loop then ls)
							(loop els ls))]
					[app-exp (rator rands)
						(app-exp
							(loop rator ls)
							(map (lambda (body)
									(loop body ls))
								rands))]
					[set!-exp (var val)
						(set!-exp
							(loop var ls)
							(loop val ls))]
					[var-exp (id)
						(lookup-address ls id)]
					[begin-exp (bodies)
						(begin-exp
							(map (lambda (body)
								(loop body ls)) bodies))]
					[define-exp (var val)
						(define-exp
							var
							(loop val ls))]
					[else exp])))))

; (define lookup-address
;   (lambda (ls exp)
;     (letrec ([lookup
; 	      (lambda (ls exp)
; 		(let loop ([ls ls] [depth 0])
; 		  (cond [(null? ls) (list ': 'free exp)]
; 			[else (let ([pos (find-position exp (car ls))])
; 				(if (eq? -1 pos)
; 				    (loop (cdr ls) (add1 depth))
; 				    (list ': depth pos)))])))]
; 	     [find-position
; 	      (lambda (obj ls)
; 		(let loop ([ls ls] [pos 0])
; 		  (cond [(null? ls) -1]
; 		      [(eq? obj (car ls)) pos]
; 		      [else (loop (cdr ls) (add1 pos))])))])
;       (lookup ls exp))))

(define lookup-address
	(lambda (ls exp)
		(letrec ([lookup
			(lambda (ls exp)
				(let loop ([ls ls] [depth 0])
					(cond 
						[(null? ls) (lexical-exp 'free exp)]
						[else (let ([pos (find-position exp (car ls))])
							(if (= -1 pos)
								(loop (cdr ls) (add1 depth))
								(lexical-exp depth pos)))])))]
			[find-position
				(lambda (obj ls)
					(let loop ([ls ls] [pos 0])
						(cond
							[(null? ls) -1]
							[(and (list? (car ls)) (eq? obj (cadar ls))) pos]
							[(eq? obj (car ls)) pos]
							[else (loop (cdr ls) (add1 pos))])))])
		(lookup ls exp))))