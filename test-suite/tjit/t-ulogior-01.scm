;;; Simple loop containing `ulogand'.

(define (loop n acc)
  (if (zero? n)
      acc
      (loop (- n 1) (+ acc (logior (modulo n 17) (modulo n 16))))))

(loop 1000 255)
