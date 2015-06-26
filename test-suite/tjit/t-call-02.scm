(define (my-add a b)
  (+ a b))

(define (my-sub1 a)
  (- a 1))

(define (f n)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (my-sub1 n) (my-add acc n))
        acc)))

(f #e1e5)
