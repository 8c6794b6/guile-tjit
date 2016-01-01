;;; Simple loop containing `br-if-null', `()' and small integer `772'.
;;; Pointer value of `()' is #x304, which is identical to unboxed small
;;; integer `772'.

(define (loop lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (if (null? (car lst))
                          acc
                          (+ acc 1))))))
(define (nulls n)
  (make-list n '()))

(loop (append! (iota 1000) (nulls 100) (iota 1000) (nulls 50)))
