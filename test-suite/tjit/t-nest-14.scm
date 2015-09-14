;;; Inter-procedure nested loop. Outer loop calls inner loop via medium
;;; procedure.
;;;
;;; The calls to `loop2' were initially not working. Addition in
;;; procedure `f' was not taken from the second call. After taking
;;; snapshot with bytecode operation `return' and filtering the locals
;;; to take in snapshot with lowest and highest offset , the missing
;;; result for procedure `f' was stored to snapshot.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (= 0 n)
        acc
        (lp (- n 1) (+ acc 1)))))

(define (f n acc)
  (+ (loop1 n acc) 123))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (= 0 i)
        acc
        (lp (- i 1) (f n acc)))))

(list (loop2 100)
      (loop2 100)
      (loop2 100))
