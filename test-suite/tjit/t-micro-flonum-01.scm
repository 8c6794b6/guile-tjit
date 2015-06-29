(define (micro n)
  (let lp ((n n))
    (if (< 0.0 n)
        (lp (- n 1.0))
        n)))

(micro #e1e6)
