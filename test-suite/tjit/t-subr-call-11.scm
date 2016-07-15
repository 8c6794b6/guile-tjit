;;; Test for inlined subr-call `cadr'.

(define (loop n proc)
  (let lp ((n n) (acc 0))
    (if (zero? n)
        acc
        (lp (- n 1) (+ acc (proc '(1 2 3 4 5)))))))

(loop 100 cadr)
