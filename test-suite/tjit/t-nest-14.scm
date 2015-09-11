;;; Inter-procedure nested loop. Outer loop calls inner loop via medium
;;; procedure.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (= 0 n)
        acc
        (lp (- n 1) (+ acc 1)))))

(define (f n acc)
  (+ (loop1 n acc) 1))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (= 0 i)
        acc
        (lp (- i 1) (f n acc)))))

(loop2 100)

;;; XXX: Not working.
;; (display (list (loop2 100)
;;                (loop2 100)))
;; (newline)
