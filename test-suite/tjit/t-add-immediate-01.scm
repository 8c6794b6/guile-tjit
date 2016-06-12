(define (loop n acc)
  (let lp ((n n) (acc acc))
    (if (zero? n)
        acc
        (lp (- n 1) (+ acc 1)))))

(loop 100 100.0)
