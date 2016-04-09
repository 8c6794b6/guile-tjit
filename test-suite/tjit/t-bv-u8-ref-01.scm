(use-modules (rnrs bytevectors))

(define (f bv)
  (let lp ((n 0) (acc 0))
    (if (< n (bytevector-length bv))
        (lp (+ n 1) (+ acc
                       (bytevector-u8-ref bv n)))
        acc)))

(let ((bv (make-bytevector #e1e5 #xff)))
  (f bv))
