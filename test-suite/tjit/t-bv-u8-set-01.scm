(use-modules (ice-9 time)
             (rnrs bytevectors))

(define (f bv)
  (let lp ((n 0))
    (when (< n (bytevector-length bv))
      (bytevector-u8-set! bv n 2)
      (lp (+ n 1)))))

(let ((bv (make-bytevector 300 0)))
  (f bv)
  bv)
