;;; Nested tail-call loop, calling the callee twice. Outer loop `f2'
;;; uses same local index to hold `f1' in two `call' bytecode
;;; operations.

(define (f1 n acc)
  (if (= n 0)
      acc
      (f1 (- n 1) (+ acc n))))

(define (f2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (f1 100 (f1 100 acc))))))

(f2 100)
