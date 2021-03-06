;;; Same procedures as defined in `t-tail-call-05', but passing
;;; different argument to record different trace.

(define (f1 n acc)
  (if (= n 0)
      acc
      (f1 (- n 1) (+ acc n))))

(define (f2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (f1 n (f1 n acc))))))

(f2 13)
