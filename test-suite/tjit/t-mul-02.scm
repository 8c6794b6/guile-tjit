;;; Loop containing `mul' with fixnums.

(define (f n a)
  (let lp ((n n) (acc '()))
    (if (< 0 n)
        (lp (- n 1) (cons (* n a) acc))
        acc)))

(f 1000 2)
