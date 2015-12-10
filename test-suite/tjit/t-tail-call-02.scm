;; Simple tail-call with top-level procedure.

(define (sum n acc)
  (if (= 100 n)
      acc
      (sum (- n 1) (+ acc n))))

(sum #e1e3 0)
