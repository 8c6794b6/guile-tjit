(use-modules (rnrs bytevectors))

(define (f bv)
  (let lp ((n 0) (acc 0))
    (if (< n (bytevector-length bv))
        (lp (+ n 1) (+ acc
                       (bytevector-u8-ref bv n)))
        acc)))

(let* ((n #e1e3)
       (bv (make-bytevector n 0)))
  (do ((i 0 (+ i 1)))
      ((= i n))
    (bytevector-u8-set! bv i #x3f))
  (f bv))
