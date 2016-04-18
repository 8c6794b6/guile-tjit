;;; Simple loop containing `string->symbol'.

(define (loop1 n str)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (string->symbol str) acc)))))

(define (loop2 n)
  (let lp ((i n) (acc '()))
    (if (= i 0)
        acc
        (lp (- i 1) (loop1 n "foo")))))

(loop2 100)
