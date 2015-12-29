;;; Simple test for `vector-set!/immediate'.

(define (f n v0)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (let ((v1 (vector-copy v0)))
          (vector-set! v1 0 100)
          (vector-set! v1 1 101)
          (vector-set! v1 2 102)
          (lp (- n 1) (cons v1 acc))))))

(f 100 (make-vector 3 0))
