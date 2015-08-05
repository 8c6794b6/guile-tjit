;;; Inlined nested loop, called from different procedure.
;;;
;;; XXX: Doing recompilation more than necessary. Replace first bailout
;;; code of side trace instead of removing the entire side trace.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (+ acc 1))
        acc)))

(define (loop2 n)
  (let lp ((i n) (v 0) (w 0))
    (if (< 0 i)
        (lp (- i 1) (loop1 n 0) #f)
        (list v w))))

(define (loop3 n)
  (let lp ((i n) (v 0) (w 0))
    (if (< 0 i)
        (lp (- i 1) (loop1 n 0) #t)
        (list v w))))

(list (loop2 100)
      (loop3 100)
      (loop2 100)
      (loop3 100))
