;;; Simple loop with `div' bytecode instruction, taking Scheme flonum as
;;; arguments.

(define (loop n denom)
  (let lp ((n n) (acc 1))
    (if (= n 0)
        acc
        (lp (- n 1) (/ acc denom)))))

(loop #e1e6 1.000001)
