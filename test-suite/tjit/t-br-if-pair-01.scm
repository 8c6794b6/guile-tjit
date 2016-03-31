;;; Simple loop with `br-if-pair'.

(define (loop lst acc)
  (if (pair? lst)
      (loop (cdr lst) (+ acc (car lst)))
      acc))

(loop (iota #e1e3) 0)
