;;; Loop containing div with fixnums.

(define (loop1 n a b)
  (do ((n n (- n 1)))
      ((= n 0) (/ a b))
    (/ a b)))

(define (loop2 n)
  (let lp ((i n) (acc '()))
    (if (= i 0)
        acc
        (lp (- i 1) (loop1 n 1554 37)))))

(loop2 #e1e3)
