(define (loop limit incr)
  (let lp ((n 0) (acc 0))
    (if (< n limit)
        (lp (+ n 1) (+ acc incr))
        acc)))

(loop #e1e5 1)
