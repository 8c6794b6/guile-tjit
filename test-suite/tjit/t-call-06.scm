;; Loop containing procedure calls. `my-add2' contains tail-call to
;; `my-add'.

(define (my-add a b)
  (+ a b))

(define (my-add2 a b)
  (my-add (my-add a b) b))

(define (f n incr)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (my-add2 acc incr))
        acc)))

(f #e1e7 2)
