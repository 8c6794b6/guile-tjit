;;; Inlined nested loop, called from different procedure.
;;;
;;; Test to replace first bailout code of side trace.  Procedure `loop1'
;;; is called from different procedures.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (+ acc 1))
        acc)))

(define (loop2 n)
  (let lp ((i n) (v 0))
    (if (< 0 i)
        (lp (- i 1) (+ v (loop1 n 0)))
        v)))

(define (loop3 n)
  (let lp ((i n) (v 1))
    (if (< 0 i)
        (lp (- i 1) (+ v (loop1 n 0)))
        v)))

(define (loop4 n)
  (let lp ((i n) (v 2))
    (if (< 0 i)
        (lp (- i 1) (+ v (loop1 n 0)))
        v)))

(define (loop5 n)
  (let lp ((i n) (v 3))
    (if (< 0 i)
        (lp (- i 1) (+ v (loop1 n 0)))
        v)))

(list (loop2 100)
      (loop3 100)
      (loop4 100)
      (loop5 100)
      (loop2 100)
      (loop3 100)
      (loop4 100)
      (loop5 100))
