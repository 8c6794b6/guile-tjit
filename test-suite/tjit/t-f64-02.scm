;;; Simple loop containing `fsub'.

(define (loop n)
  (let lp ((n n) (acc 100.0))
    (if (= n 0)
        acc
        (lp (- n 1) (- acc 1.2345)))))

(loop #e1e3)
