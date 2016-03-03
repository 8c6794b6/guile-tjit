;; Nested loop, taking fixnum in first call, flonum in second call.

(define (loop n inc)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1)
            (let* ((acc (let lp ((j n) (acc acc))
                          (if (< 0 j)
                              (lp (- j 1) (+ acc inc))
                              acc)))
                   (acc (let lp ((k n) (acc acc))
                          (if (< 0 k)
                              (lp (- k 1) (+ acc inc))
                              acc))))
              acc))
        acc)))

(list (loop 100 1)
      (loop 100 1.0))
