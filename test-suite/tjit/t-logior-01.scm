;;; Simple loop containing `logand'.

(define (loop n acc)
  (if (zero? n)
      acc
      (loop (- n 1) (+ acc (logior n #xff)))))

(loop 1000 1)
