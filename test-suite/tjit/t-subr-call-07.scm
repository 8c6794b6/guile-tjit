;;; Loop with subr-call, with side exit.

(define (f n lst1 lst2)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ (assq-ref (if (< n #e5e4) lst1 lst2) 1)
                       acc)))))

(f #e1e5
   '((0 . 10) (1 . 11))
   '((0 . 100) (1 . 110)))
