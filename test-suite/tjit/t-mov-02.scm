;;; Another simple loop containing bytecode `mov' with u64 elements.

(define (loop)
  (let outer ((i 100))
    (if (< 0 i)
        (let inner ((j 100))
          (if (< 0 j)
              (inner (- j 1))
              (outer (- i 1))))
        i)))
(loop)
