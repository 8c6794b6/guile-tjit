;;; Fibonacci with flonum.

(define (fib n)
  (if (< n 2.0)
      n
      (+ (fib (- n 1.0))
         (fib (- n 2.0)))))

(fib 20.0)
