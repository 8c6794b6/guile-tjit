;;; Loop containing bv-u8-ref with constant index.

(use-modules (rnrs bytevectors))

(define (f bv n)
  (let lp ((n n) (acc 0))
    (if (zero? n)
        acc
        (lp (- n 1) (+ acc (bytevector-u8-ref bv 128))))))

(let ((bv (make-bytevector 300 0)))
  (bytevector-u8-set! bv 128 123)
  (let lp ((n 1000) (v #f))
    (if (zero? n)
        v
        (lp (- n 1) (f bv 1000)))))
