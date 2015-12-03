;;; Yet another loop with side-exit.

(define (loop n)
  (let lp ((n n) (acc1 0) (acc2 0))
    (cond
     ((= n 0)
      (list acc1 acc2))
     ((< 800 n)
      (lp (- n 1) acc1 acc2))
     (else
      ;; Traced loop will contain VM operation `box' if below line with
      ;; `set!' were uncommented.

      ;; (set! acc1 (+ acc1 1))
      (cond
       ((< n 100)
        (lp (- n 1) acc1 acc2))
       (else
        (lp (- n 1) acc1 (+ acc2 1))))))))

(loop #e1e3)
