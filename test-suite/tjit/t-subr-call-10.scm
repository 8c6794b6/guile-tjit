;;; Nested call to C subroutine `exact->inexact'.

(define (inner n)
  (let lp ((n n) (acc 0.0))
    (if (= n 0)
        acc
        (lp (- n 1)
            (+ (exact->inexact
                (exact->inexact
                 (exact->inexact
                  (exact->inexact
                   (exact->inexact n)))))
               acc)))))

(define (outer n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (inner n))))))

(outer 100)
