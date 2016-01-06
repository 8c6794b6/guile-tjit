;;; Simple loop containing `reset-frame'.

(define (loop n t)
  (let lp ((n n))
    (when (< 0 n)
      (hashq-set! t n n)
      (lp (- n 1)))))

(let ((t (make-hash-table)))
  (loop #e1e3 t)
  (hash-fold (lambda (k v acc)
               (+ acc v))
             0 t))
