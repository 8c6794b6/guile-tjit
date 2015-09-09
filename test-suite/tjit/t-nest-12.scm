;;; More inlined procedure in nested loop.
;;;
;;; Calling procedure containing loop twice, adding results. Return
;;; address of the first call to `loop1' is different from the second
;;; call to `loop1'.

(define (loop1a n acc)
    (let lp ((i n) (acc acc))
      (if (< 0 i)
          (lp (- i 1) (+ acc 2))
          acc)))

(define (loop1b n acc)
    (let lp ((i n) (acc acc))
      (if (< 0 i)
          (lp (- i 1) (+ acc 3))
          acc)))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (list (loop1a n 0)
                          (loop1b n 0)))
        acc)))

(loop2 100)
