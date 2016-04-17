;;; Simple loop containing `string-length'.

(define (loop1 n str)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (string-length str))))))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (loop1 n "foo bar buzz"))))))

(loop2 100)
