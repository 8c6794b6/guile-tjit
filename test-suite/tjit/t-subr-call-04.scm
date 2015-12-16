;;; Loop with subr-call, returned value from subr-call has Scheme double
;;; type. Number of iterations is kept low so that garbage collector
;;; won't run.

(define (f n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (exact->inexact n))))))

(f 100)
