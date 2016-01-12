;;; Same procedures as defined in `t-tail-call-05', passing same
;;; argument as in `t-tail-call-05b', but calling `f2' multiple times.

(define (f1 n acc)
  (if (= n 0)
      acc
      (f1 (- n 1) (+ acc n))))

(define (f2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (f1 n (f1 n acc))))))

(list (f2 13)
      (f2 13)
      (f2 13)
      (f2 13)
      (f2 13))
