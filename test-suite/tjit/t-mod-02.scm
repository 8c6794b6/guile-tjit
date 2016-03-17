;;; Loop with modulo, with positive divident and negative divisor.

(define (loop n x)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (modulo n x) acc)))))

(loop 100 -17)
