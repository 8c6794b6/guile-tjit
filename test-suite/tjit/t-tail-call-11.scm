;;; Simple loop containing `br-if-tc7' with tail-call.

(define (loop lst acc)
  (if (null? lst)
      acc
      (let ((x (car lst))
            (lst (cdr lst)))
        (loop lst (if (vector? x)
                      (+ acc 1)
                      acc)))))

(let* ((vs (make-list 100 #(1 2 3)))
       (ls (make-list 100 '(1 2 3)))
       (lst (append ls vs ls ls ls vs ls ls ls ls vs ls ls ls vs ls ls)))
  (loop lst 0))
