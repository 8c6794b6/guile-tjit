;;; Simple nested loop containing `car' and `cdr'. Uses shorter list
;;; than the one used in car01.scm, to view the dumped local in
;;; bytecode. Loop is nested to make the inner loop hot without using
;;; list containing less elements.

(define lst
  (let-syntax ((gen
                (lambda (x)
                  (syntax-case x ()
                    ((k n)
                     (let ((is (iota (syntax->datum #'n))))
                       #`'#,(datum->syntax #'k is)))))))
    (gen 11)))

(define (loop n lst)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1)
            (let lp ((lst lst) (acc acc))
              (if (null? lst)
                  acc
                  (lp (cdr lst) (+ acc (car lst)))))))))

(loop #e1e3 lst)
