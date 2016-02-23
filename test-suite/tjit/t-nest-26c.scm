;; Nested loops with condition and `box-set!', outer loop is locally
;; nested.

(define (inner n acc)
  (let lp ((n n) (acc acc))
    (cond
     ((<= n 0)   acc)
     ((<= n 100) (lp (- n 1) (+ acc 2)))
     (else       (lp (- n 1) (+ acc 1))))))

(define (outer acc)
  (do ((i 0 (+ i 1)))
      ((< 300 i))
    (do ((j 0 (+ j 1)))
        ((< 300 j))
      (set! acc (inner j acc))))
  acc)

(outer 1)
