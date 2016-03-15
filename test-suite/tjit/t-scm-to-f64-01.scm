;;; Loop containing `scm->f64' and `f64->scm'.

(define (loop1)
  (let lp ((n 100) (acc 0.0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc n)))))

(loop1)
