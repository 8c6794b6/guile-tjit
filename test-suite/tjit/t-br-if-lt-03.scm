;; Another simple loop with `br-if-<', with flonum.

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (<= 0.0 n)
        (lp (- n 1.0) (+ acc 1))
        acc)))

(loop 1.0e6)
