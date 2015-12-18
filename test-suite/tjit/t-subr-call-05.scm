;;; Loop with subr-call, returned value from subr-call has Scheme double
;;; type. Number of iterations is large enough to trigger a call to
;;; garbage collector.

(define (f n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (exact->inexact n))))))

(f #e1e7)
