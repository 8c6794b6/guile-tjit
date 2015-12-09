;;; Simple loop containing `fmul'.

(define (loop n)
  (let lp ((n n) (acc 1.0))
    (if (= n 0)
        acc
        (lp (- n 1) (* acc 1.2345)))))

(loop #e1e3)
