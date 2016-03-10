;;; Loop to catch non-fixnum element with `br-if-u64-=-scm'.

(define (loop lst acc)
  (cond
   ((null? lst)
    acc)
   ((= (car lst) 1234)
    (loop (cdr lst) 0))
   (else
    (loop (cdr lst) (+ acc (car lst))))))

(define (wrap lst acc)
  (catch #t
    (lambda ()
      (loop lst acc))
    (lambda args
      (format #f "~a" args))))

(list (wrap (iota 1000) 0)
      (wrap (append (iota 10000) '(#f) (iota 100)) 0))
