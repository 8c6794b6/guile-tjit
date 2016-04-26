;;; Simple loop containing `br-if-logtest'.

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (zero? n)
        acc
        (let ((acc (if (zero? (logand n #b10))
                       acc
                       (+ acc 1))))
          (lp (- n 1) acc)))))

(loop 1000)
