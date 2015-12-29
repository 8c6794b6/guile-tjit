;;; Simple test for `vector-set!'.

(define (f n)
  (do ((i 0 (+ i 1))
       (v (make-vector n #f)))
      ((= i n) v)
    (vector-set! v i i)))

(f 500)
