;; Simple loop with `br-if-<=', with flonum.

(define (loop n)
  (let lp ((i 0.0) (acc 0))
    (if (<= n i)
        acc
        (lp (+ i 1.0) (+ acc 1)))))

(loop 1.0e6)
