;;; Sum of prime numbers using vector.

(define (sieve size)
  (let ((primes (make-vector size #t)))
    (do ((i 2 (+ i 1)))
        ((= size i))
      (when (vector-ref primes i)
        (do ((k (+ i i) (+ k i)))
            ((<= size k))
          (vector-set! primes k #f))))
    (do ((i 0 (+ i 1))
         (acc 0 (if (vector-ref primes i) (+ acc i) acc)))
        ((= size i) acc))))

(sieve #e1e7)
