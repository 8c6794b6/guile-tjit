;;; Loop with remainder, with positive divident and postive divisor.

(define (loop n x)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (remainder n x) acc)))))

(loop 100 17)
