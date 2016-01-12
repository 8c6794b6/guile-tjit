;;; Almost same as t-tail-call-05.scm, but with number of iterations in
;;; `f2' inlined and different.

(define (f1 n acc)
  (if (= n 0)
      acc
      (f1 (- n 1) (+ acc n))))

(define (f2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (f1 n (f1 n acc))))))

(list (f2 100)
      (f2 100)
      (f2 100)
      (f2 100)
      (f2 100))
