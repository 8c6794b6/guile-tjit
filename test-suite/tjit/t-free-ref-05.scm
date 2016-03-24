;; Loop containing free-ref and tail-call, not aligned.

(set! *random-state* (seed->random-state 31))

(define (make-closure k a0 a1 a2 a3 a4 a5)
  (let ((r (random 6)))
    (cond
     ((= r 0) (lambda (x) (k (+ x a0))))
     ((= r 1) (lambda (x) (k (+ x a0 a1))))
     ((= r 2) (lambda (x) (k (+ x a0 a1 a2))))
     ((= r 3) (lambda (x) (k (+ x a0 a1 a2 a3))))
     ((= r 4) (lambda (x) (k (+ x a0 a1 a2 a3 a4))))
     ((= r 5) (lambda (x) (k (+ x a0 a1 a2 a3 a4 a5))))
     (else (lambda (x) (k x))))))

(define (go n)
  (let lp ((n n) (g (lambda (a) a)))
    (if (= n 0)
        g
        (lp (- n 1) (make-closure g n 1 2 3 4 5)))))

((go 300) 0)
