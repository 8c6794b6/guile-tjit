;;; cpstak from racket benchmark

(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))
  (tak x y z (lambda (a) a)))

;;; call: (cpstak 18 12 6)

(define (run k)
  (let loop ((n 2) (v 0))
    (if (zero? n)
        v
        (loop (- n 1) (cpstak 18 12 k)))))

(run 2)
