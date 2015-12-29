;;; Simple loop containing `vector-ref', with vector containing small
;;; fixnums.

(define (f v)
  (let lp ((i 0) (acc 0))
    (if (< i (vector-length v))
        (lp (+ i 1) (+ acc (vector-ref v i)))
        acc)))

(f (make-vector #e1e3 2))
