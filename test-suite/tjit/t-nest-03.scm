(define (nest n)
  (let loop ((i n) (acc 0))
    (if (< 0 i)
        (loop (- i 1)
              (let* ((acc (let loop ((j n) (acc acc))
                            (if (< 0 j)
                                (loop (- j 1) (+ acc 1))
                                acc)))
                     (acc (let loop ((k n) (acc acc))
                            (if (< 0 k)
                                (loop (- k 1) (+ acc 1))
                                acc))))
                acc))
        acc)))

(nest 100)
