;;; Loop containing `div' with fixnum, result type is fraction.

(define (loop lst)
  (let lp ((lst lst) (acc '()))
    (if (null? lst)
        (reverse! acc)
        (let ((a (/ (car lst) 7)))
          (lp (cdr lst) (cons a acc))))))

(let ((lst (iota 100)))
  (loop lst))
