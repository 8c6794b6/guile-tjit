(define (micro n incr)
  (let lp ((n n) (acc 0.0))
    (if (< 0 n)
        (lp (- n 1) (+ acc incr))
        acc)))

(micro #e1e6 1.23)
