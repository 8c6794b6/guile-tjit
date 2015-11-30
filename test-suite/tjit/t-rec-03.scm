;;; Non-tail-recursive procedure with side exit.

(define (rec n)
  (cond
   ((= 0 n)
    0)
   ((< 100 n)
    (+ 1 (rec (- n 1))))
   (else
    (+ 2 (rec (- n 1))))))

(list (rec 200)
      (rec 200)
      (rec 200))
