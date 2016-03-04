;; Complicated nested trace.

(define (inner n acc)
  (let lp ((i 0) (acc acc))
    (cond
     ((= i n) acc)
     ((even? i)
      (lp (+ i 1) (+ acc 1)))
     (else
      (lp (+ i 1) (+ acc 2))))))

(define (middle n acc)
  (let lp ((i 0) (acc acc))
    (cond
     ((= i n) acc)
     ((even? i)
      (lp (+ i 1) (+ (inner i acc) 1)))
     (else
      (lp (+ i 1) (inner i acc))))))

(define (outer n)
  (let lp ((i 0) (acc 0))
    (cond
     ((= i n) acc)
     ((even? i)
      (lp (+ i 1) (+ (middle i acc) 1)))
     (else
      (lp (+ i 1) (middle i acc))))))

(outer 17)
