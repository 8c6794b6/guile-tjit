;;; Multiple calls to cons with different arguments. The trace contained
;;; a pattern with volatile register of second argument matched to ARG1
;;; register, which was leading to unexpected overwriting.

(define (loop n a)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (let lp ((acc acc) (sum 0))
          (if (null? acc)
              sum
              (lp (cdr acc) (+ sum (car acc)))))
        (lp (- n 1) (cons n (cons (+ n a 2) (cons (+ n a 3) acc)))))))

(loop #e1e3 123)
