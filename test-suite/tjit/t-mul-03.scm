;;; Loop containing `mul' with negative and positive fixnums.

(define (f n a)
  (let lp ((n n) (acc '()))
    (if (< 0 n)
        (let ((m (if (even? n)
                     n
                     (- n))))
          (lp (- n 1) (cons (* m a) acc)))
        acc)))

(f 1000 -2)
