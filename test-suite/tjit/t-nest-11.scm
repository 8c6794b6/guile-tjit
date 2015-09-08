;;; Another inlined procedures in nested loop. Procedure `loop3' calls
;;; `loop1' and `loop2', and sums the results.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (= 0 i)
        acc
        (lp (- i 1) (+ acc 1)))))

;; `loop2' adds 2, to avoid the use of bytecode OP `add1'.
(define (loop2 n acc)
  (let lp ((i n) (acc acc))
    (if (= 0 i)
        acc
        (lp (- i 1) (+ acc 2)))))

(define (loop3 n)
  (let lp ((i n) (acc #f))
    (if (= 0 i)
        acc
        (lp (- i 1) (+ (loop1 n 0)
                       (loop2 n 0))))))

(loop3 100)
