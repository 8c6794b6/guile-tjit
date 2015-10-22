;;; Another inlined procedures in nested loop. Procedure `loop3' calls
;;; `loop1' and `loop2', and sums the results. Procedure `loop2' adds 2,
;;; to avoid the use of bytecode operation `add1'.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (+ acc 1))
        acc)))

(define (loop2 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (+ acc 2))
        acc)))

(define (loop3 n)
  (let lp ((i n) (acc #f))
    (if (< 0 i)
        (lp (- i 1) (+ (loop1 n 0)
                       (loop2 n 0)))
        acc)))

(loop3 100)
