;;; Simple loop containing `usub'.

(define (loop1 n v)
  (define (f a)
    (let ((l (vector-length a)))
      (if (< l 1000)
          l
          (- l 800))))
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (f v))))))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (loop1 n (make-vector n)))))))

(loop2 1000)
