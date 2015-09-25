;;; Test for deoptimization. Running procedure `loop' with small integer
;;; arguments for first time, then re-run the procedure with floating
;;; point number as arguments.

(define (loop n incr)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (+ acc incr))
        acc)))

(list (loop #e1e3 125)
      (loop #e1e3 1.25)
      (loop #e1e3 125)
      (loop #e1e3 1.25))
