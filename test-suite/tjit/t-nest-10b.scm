;;; A loop procedure calling loop procedures example. Procedure `loop2'
;;; calls `loop1' multiple times. Procedure `loop2' is called multiple
;;; times.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (+ acc 1))
        acc)))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (+ (loop1 n 0)
                       (loop1 n 0)))
        acc)))

(list (loop2 #e1e3)
      (loop2 #e2e3)
      (loop2 #e3e3))
