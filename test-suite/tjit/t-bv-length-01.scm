(use-modules (rnrs bytevectors))

(define (my-bv-length bv)
  (bytevector-length bv))

(define (f bv)
  (let lp ((n 100) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (my-bv-length bv))))))

(f (make-bytevector 123))
