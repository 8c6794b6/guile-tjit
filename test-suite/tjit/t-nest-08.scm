;;; Inlined inter-procedure nested loop, wrapped with `list'.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (= 1 i)
        acc
        (lp (- i 1) (+ acc 1)))))

(define (loop2 n)
  (let lp ((i n) (acc #f))
    (if (= 0 i)
        acc
        (lp (- i 1) (list (loop1 n 0))))))

(map loop2 (iota 100))
