;;; Simple loop containing `ulogand'.

(define (loop n acc)
  (if (zero? n)
      acc
      (loop (- n 1) (+ acc (logand (modulo n 17) acc)))))

(loop 1000 255)
