(define (loop n)
  (let ((res 0))
    (do ((i 0 (+ i 1))) ((= i n))
      (set! res (+ res 0.12)))
    res))

(loop #e1e5)
