;; Nested loops with condition and `box-set!', outer loop is locally
;; nested.

(use-modules (ice-9 time))

(define (inner n acc)
  (if (<= 20 n)
      (let lp ((n n) (acc acc))
        (if (= n 0)
            acc
            (lp (- n 1) (+ acc n))))
      (+ acc 1)))

(define (outer n acc)
  (do ((i 0 (+ i 1)))
      ((< n i))
    (do ((j 0 (+ j 1)))
        ((< n j))
      (set! acc (inner j acc))))
  acc)

(do ((i 0 (+ i 1))
     (acc '() (cons (outer 30 i) acc)))
    ((= i 20) acc))
