(define (micro n)
  (let lp ((n n))
    (if (< 0 n)
        (lp (- n 1))
        n)))

(micro #e1e3)
