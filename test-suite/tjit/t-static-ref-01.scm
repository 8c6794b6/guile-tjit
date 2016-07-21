;;;; Simple loop adding -1.

(define (inner n acc)
  (let lp ((n n) (acc acc))
    (if (zero? n)
        acc
        (lp (- n 1) (+ acc -1)))))

(define (outer n)
  (let lp ((n n) (acc 0))
    (if (zero? n)
        acc
        (lp (- n 1) (inner n acc)))))

(outer 1000)
