;;; Simple non-tail-recursive procedure.

(define (sum n)
  (if (< 0 n)
      (+ n (sum (- n 1)))
      0))

(list (sum 100)
      (sum 100)
      (sum 100))
