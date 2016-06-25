;;; Mutually recursive loop with top level definition. The top level
;;; procedure `f' is modified after setting `r1'.

(define r1 #f)

(define r2 #f)

(define (f n acc)
  (if (= n 0)
      acc
      (g (- n 1) (+ acc n))))

(define (g n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (f (- n 1) acc)))))

(set! r1 (g 20 0))

(define (f n acc)
  (if (= n 0)
      acc
      (g (- n 1) (+ acc n 1))))

(set! r2 (g 20 0))

(list r1 r2)
