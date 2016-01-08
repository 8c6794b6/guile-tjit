;;; Simple loop with `box-ref', `box-set!', and `box'.

(define (inner n)
  (let ((acc 0))
    (do ((j n (- j 1))) ((<= j 0))
      (set! acc (+ acc 1)))
    acc))

(define (outer n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (inner n))))))

(outer 100)
