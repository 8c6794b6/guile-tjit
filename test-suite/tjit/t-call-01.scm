(define (my-add a b)
  (+ a b))

(define (f n)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (my-add acc n))
        acc)))

(f #e1e5)
