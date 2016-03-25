;;; Loop with (@@ (guile) and-map) with primitive procedures. Contains
;;; tail-call.

(define (loop f lst acc)
  (if (null? lst)
      acc
      (loop f (cdr lst) (cons (and-map f (car lst)) acc))))

(let* ((symbols '(a b c d e f g))
       (numbers '(1 2 3 4 5 6))
       (lst (apply append (make-list 100 (list symbols numbers)))))
  (list (loop symbol? lst '())
        (loop number? lst '())))
