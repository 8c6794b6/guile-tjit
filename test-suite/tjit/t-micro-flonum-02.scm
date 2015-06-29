(define (micro n incr)
  (let lp ((k 0.0))
    (if (< k n)
        (lp (+ k incr))
        k)))

(micro 1.25e6 1.25)
