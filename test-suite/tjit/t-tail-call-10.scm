;; Tail call loop with mutually recursive top-level definition.

(define (f n acc)
  (if (= n 0)
      acc
      (g (- n 1) (cons n acc))))

(define (g n acc)
  (let lp ((n n) (acc acc))
    (if (= n 0)
        acc
        (lp (- n 1) (f (- n 1) acc)))))

(let lp ((n 15) (v 0))
  (if (= n 0)
      v
      (lp (- n 1) (g 30 '()))))
