;;; Simple loop to deoptimize. Passing small integers to procedure with
;;; tail-call loop, and then pass flonums to the same procedure.

(define (loop n inc acc)
  (if (zero? n)
      acc
      (loop (- n 1) inc (+ acc inc))))

(list (loop #e1e3 1 0)
      (loop #e1e3 1.23 0.0)
      (loop #e1e3 2 0)
      (loop #e1e3 4.56 0.0))
