;;; Loads the InterpretorProject and then runs all tests starting with the oldest
(load "main.ss")

(define test
  (lambda ()
    (begin
      (l)
      (display "13 Tests")
      (newline)
      (load "13_test.ss")
      (r)
      (newline)
      
      (display "14 Tests")
      (newline)
      (load "14_test.ss")
      (r)
      (newline)
      
      (display "16 Tests")
      (newline)
      (load "16_test.ss")
      (r)
      (newline)
      
      (display "17 Tests")
      (newline)
      (load "17_test.ss")
      (r)
      (newline)
      )
    )
  )
