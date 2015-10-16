;;; More inlined procedures in nested loop. Calling loop3 which calls
;;; loop2 which calls loop1.
;;;
;;; XXX: Getting correct result, but traces made more than necessary.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (= 0 i)
        acc
        (lp (- i 1) (+ acc 1)))))

(define (loop2 n acc)
  (let lp ((i n) (acc acc))
    (if (= 0 i)
        acc
        (lp (- i 1) (loop1 n acc)))))

(define (loop3 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (loop2 n acc))
        acc)))

(loop3 100 0)
