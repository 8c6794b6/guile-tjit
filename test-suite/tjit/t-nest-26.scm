;; Nested loops with condition and `box-set!', outer loop is locally
;; nested.

(define (inner n acc)
  (if (<= 20 n)
      (let lp ((n n) (acc acc))
        (if (= n 0)
            acc
            (lp (- n 1) (+ acc n))))
      (+ acc 1)))

(define (outer acc)
  (do ((i 0 (+ i 1)))
      ((< 30 i))
    (do ((j 0 (+ j 1)))
        ((< 30 j))
      (set! acc (inner j acc))))
  acc)

(outer 1)
