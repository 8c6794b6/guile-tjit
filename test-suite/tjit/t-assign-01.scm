(define (loop)
  (let lp ((n 0) (acc 0))
    (if (< n #e1e5)
        (lp (+ n 1) (+ acc n))
        acc)))

(loop)
