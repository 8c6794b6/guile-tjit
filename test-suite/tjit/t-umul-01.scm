;;; Loop containing `umul'.

(define (random-state n)
  (cons n #f))

(define (rand state)
  (let ((seed (car state))
        (A 2813) ; 48271
        (M 8388607) ; 2147483647
        (Q 2787) ; 44488
        (R 2699)) ; 3399
    (let* ((hi (quotient seed Q))
           (lo (modulo seed Q))
           (test (- (* A lo) (* R hi)))
           (val (if (> test 0) test (+ test M))))
      (set-car! state val)
      val)))

(define (random-int n state)
  (modulo (rand state) n))

(define (loop n random-state)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (random-int n random-state))))))

(loop #e1e4 (random-state 20))
