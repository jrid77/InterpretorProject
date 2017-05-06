;;; Loads all files associated with the interpreter project              
;; Created by Claude Anderson
;; Modified by Christopher Keinsley and Jarrett Alexander

(load "chez-init.ss") 

;;; Reload all files
(define load-all
  (lambda ()
    (load "datatypes.ss")
    (load "lexical.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "expand.ss")
    (load "interpreter.ss")))

(load-all)

(define l load-all)
