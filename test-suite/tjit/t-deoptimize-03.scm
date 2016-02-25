;; Deoptimization with nested loop.

(define (inner lst acc)
  (let lp ((lst lst) (acc acc))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (car lst))))))

(define (outer lst n)
  (let lp ((n n) (r #f))
    (if (= n 0)
        r
        (lp (- n 1) (inner lst 0)))))

(let* ((n #e1e3)
       (l1 (make-list n 1))
       (l2 (make-list n 1.0)))
  (list (outer l1 100)
        (outer l2 100)))
