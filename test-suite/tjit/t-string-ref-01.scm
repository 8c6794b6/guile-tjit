;;; Simple loop containing `string-ref'.

(define (loop1 n str)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (string-ref str 0) acc)))))

(define (loop2 n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (loop1 n "foo bar buzz") acc)))))

(loop2 100)
