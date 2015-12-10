;;; Simple `tail-call-label'. Inner procedure `f' is called multiple
;;; times so that byte compiled code contain `tail-call-label'.

(define (loop n)
  (define (f i acc)
    (if (= i 0)
        acc
        (f (- i 1) (+ acc i))))
  (list (f n 0) (f n 0) (f n 0)))

(loop #e1e3)
