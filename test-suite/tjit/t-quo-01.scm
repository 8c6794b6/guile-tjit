(define (loop n q)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (quotient n q))))))

(loop #e1e3 41)
