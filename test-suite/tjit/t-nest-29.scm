;; Sample complex nesed loop, contains procedure calls in between nests.

(define (inner n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc 1)))))

(define (f a b)
  (if (< a b)
      (+ b 1)
      (+ a b)))

(define (outer n)
  (let lp ((n n) (acc 0))
    (cond
     ((= n 0) acc)
     ((< n 200)
      (lp (- n 1) (+ (truncate acc) (truncate (f (inner n 800) n)))))
     (else
      (lp (- n 1) (f (inner n acc) n))))))

(outer 1000)
