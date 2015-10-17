;;; Another inter-procedure nested loop. The procedure `loop2' calls
;;; `loop1' from locally nested loop.

(use-modules (ice-9 time))

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (= i 0)
        acc
        (lp (- i 1) (+ acc 1)))))

(define (loop2 n acc)
  (let lp ((i n) (acc acc))
    (if (= 0 i)
        acc
        (lp (- i 1)
            (let lp ((j n) (acc acc))
              (if (= 0 j)
                  acc
                  (lp (- j 1) (loop1 n acc))))))))

(loop2 100 0)
