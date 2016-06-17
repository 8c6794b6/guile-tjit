;;; Loop containing addition with arithmetic overflow.

(define (loop n acc inc)
  (let lp ((n n) (acc acc))
    (if (zero? n)
        acc
        (lp (- n 1) (+ acc inc)))))

(loop 1000 (- most-positive-fixnum 200) 1)
