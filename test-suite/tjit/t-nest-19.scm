;;; More nested loops, nesting 5 simple procedures.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc 1)))))

(define (loop2 n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (loop1 n acc)))))

(define (loop3 n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (loop2 n acc)))))

(define (loop4 n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (loop3 n acc)))))

(define (loop5 n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (loop4 n acc)))))

(loop5 100 0)
