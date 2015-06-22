(define (sum-tailcall n)
  (let lp ((n n) (acc 0))
    (if (= 0 n)
        acc
        (lp (- n 1) (+ acc n)))))

(sum-tailcall #e1e3)
