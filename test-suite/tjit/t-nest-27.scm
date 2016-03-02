;;; Nested loop, taking fixnum and flonum from argument.

(define (f n acc inc)
  (let lp ((n n) (acc acc))
    (if (<= n 0)
        acc
        (lp (- n 1) (+ acc inc)))))

(define (g n acc inc)
  (let lp ((n n) (acc acc))
    (if (<= n 0)
        acc
        (lp (- n 1) (f n acc inc)))))

(list (g 1000 0 1)
      (g 1000 0.0 1.0))
