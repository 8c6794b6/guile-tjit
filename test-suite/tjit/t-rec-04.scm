;;; Recursive fibonacci procedure.

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(list (fib 20)
      (fib 20)
      (fib 20))
