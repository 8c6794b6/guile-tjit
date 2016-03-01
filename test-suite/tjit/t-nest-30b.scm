;; Complicated nested trace.

(define (inner n acc)
  (let lp ((i 0) (acc acc))
    (cond
     ((= i n) acc)
     ((even? i)
      (lp (+ i 1) (+ acc 1)))
     (else
      (lp (+ i 1) (+ acc 2))))))

(define (outer n acc)
  (let lp ((i 0) (acc acc))
    (cond
     ((= i n) acc)
     ((<= i 20)
      (lp (+ i 1) (+ (inner i acc) 1)))
     (else
      (lp (+ i 1) (inner i acc))))))

(outer 100 0)
