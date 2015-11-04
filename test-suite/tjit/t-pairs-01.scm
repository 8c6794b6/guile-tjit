;;;; Simple loop containing `car' and `cdr'.

(define lst
  (let-syntax ((gen
                (lambda (x)
                  (syntax-case x ()
                    ((k n)
                     (let ((is (iota (syntax->datum #'n))))
                       #`'#,(datum->syntax #'k is)))))))
    (gen #e1e3)))

(define (loop lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (car lst))))))

(loop lst)
