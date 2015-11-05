;;; Multiple calls to cons with different arguments.

(define (loop n a b)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (let lp ((acc acc) (sum 0))
          (if (null? acc)
              sum
              (lp (cdr acc) (+ sum (car acc)))))
        (lp (- n 1) (cons n (cons (+ n a) (cons (+ n b) acc)))))))

(loop #e1e3 1 2)
