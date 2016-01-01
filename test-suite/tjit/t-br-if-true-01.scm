;;; Simple loop containing `br-if-true', `#f' and small integer `4'.
;;; Pointer value of `#f' is #x4, which is identical to unboxed small
;;; integer `4'.

(define (loop lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (if (car lst)
                          (+ acc 1)
                          acc)))))
(define (falses n)
  (make-list n #f))

(list (loop (append! (iota 100) (falses 100) (iota 100) (falses 50)))
      (loop (iota 100))
      (loop (make-list 100 3))
      (loop (make-list 100 4))
      (loop '(1 #f #f #f #f))
      (loop '(1 2 3 1 #f 1 2 3 2 #f))
      (loop '(1 2 3 4 #f 1 2 3 4 #f)))
