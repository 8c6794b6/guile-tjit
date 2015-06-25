(define (micro n incr)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (+ acc incr))
        acc)))

(list (micro #e1e3 125)
      (micro #e1e3 1.25))
