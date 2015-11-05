;;; Multiple calls to cons with same argument.

(define (loop n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (let lp ((acc acc) (sum 0))
          (if (null? acc)
              sum
              (lp (cdr acc) (+ sum (car acc)))))
        (lp (- n 1) (cons n (cons n (cons n (cons n acc))))))))

(loop #e1e3)
