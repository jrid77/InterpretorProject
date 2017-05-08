 ;;; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define apply-env
	(lambda (env sym succeed fail)
		(apply-env-ref env sym (ref-to-deref-k succeed) fail)))

(define deref unbox)
(define set-ref! set-box!)
		
(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
	   (empty-env-record () (apply-k fail))
	   (extended-env-record (syms vals env)
	   		(list-find-position sym syms 
	   			(find-pos-k vals env sym succeed fail)		
     )))))

(define list-find-position
  (lambda (sym los k)
  	(list-index sym los k)))

(define list-index
  (lambda (sym ls k)
  	(cond 
  		[(null? ls) (apply-k k #f)]
  		[(eqv? sym (car ls)) (apply-k k 0)]
  		[(else (list-index sym (cdr ls)
  							(index-cdr-res-k k)))])))