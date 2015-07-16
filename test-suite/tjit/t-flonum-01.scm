(define (micro n)
  (let lp ((n n))
    (if (< 42.0 n)
        (lp (- n 1.0))
        n)))

(micro #e1e6)
