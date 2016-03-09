;; More complicated nested trace.

(define (inner n acc)
  (let lp ((i 0) (acc acc))
    (if (< i n)
        (lp (+ i 1) (+ acc i))
        acc)))

(define (f a b)
  (if (<= 10 a)
      (+ a (g a b))
      (+ a b 1)))

(define (g a b)
  (if (<= a 15)
      (+ a 1)
      (+ a b)))

(define (middle n acc)
  (let lp ((i 0) (acc acc))
    (cond
     ((= i n) acc)
     ((<= i 20)
      (lp (+ i 1) (inner i (inner i acc))))
     (else
      (lp (+ i 1) (inner i (f i acc)))))))

(define (outer n)
  (let lp ((i 0) (acc 0))
    (if (< i n)
        (lp (+ i 1) (middle i (middle (f i i) acc)))
        acc)))

(outer 100)
