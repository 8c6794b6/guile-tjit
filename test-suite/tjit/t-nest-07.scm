;; More inlined procedures in nested loop. Callee has three loops,
;; and calling the loop twice to return a list.

(define (loop1 n acc)
    (let lp ((i n) (acc acc))
      (if (= 0 i)
          acc
          (lp (- i 1)
              (let lp ((i n) (acc acc))
                (if (= 0 i)
                    acc
                    (lp (- i 1)
                        (let lp ((i n) (acc acc))
                          (if (= 0 i)
                              acc
                              (lp (- i 1) (+ acc 1)))))))))))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (loop1 n acc))
        acc)))

(list (loop2 10)
      (loop2 100))
