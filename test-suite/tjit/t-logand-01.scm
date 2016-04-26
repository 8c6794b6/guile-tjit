;;; Simple loop containing `logand'.

(define (loop n acc)
  (if (zero? n)
      acc
      (loop (- n 1) (+ acc (logand n acc)))))

(loop 1000 1)
