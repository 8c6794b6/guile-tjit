;;; Simple nested loop containing `reset-frame'.

(define (loop n t)
  (let outer ((i n))
    (if (< 0 i)
        (let inner ((j n))
          (if (< 0 j)
              (begin
                (hashq-set! t n n)
                (inner (- j 1)))
              (outer (- i 1))))
        t)))

(let ((t (make-hash-table)))
  (loop #e1e3 t)
  (hash-fold (lambda (k v acc)
               (+ acc v))
             0 t))
