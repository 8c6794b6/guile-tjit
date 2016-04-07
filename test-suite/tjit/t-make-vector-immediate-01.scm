;;; Simple loop containing `make-vector/immediate'.

(define (loop n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (make-vector 3 n) acc)))))

(loop 1000)
