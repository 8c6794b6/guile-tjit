;;; Bytecode compilation with procedure definition and procedure call.
;;; Defined procedure will generate trace during the call.

(compile '(begin
            (define (sieve size)
              (let ((primes (make-vector size #t)))
                (let lp1 ((i 2))
                  (when (< i size)
                    (when (vector-ref primes i)
                      (let lp2 ((k (+ i i)))
                        (when (< k size)
                          (vector-set! primes k #f)
                          (lp2 (+ k i)))))
                    (lp1 (+ i 1))))
                (let lp ((i 0) (acc 0))
                  (if (< i size)
                      (lp (+ i 1)
                          (if (vector-ref primes i)
                              (+ acc i)
                              acc))
                      acc))))
            (sieve #e1e3)))
