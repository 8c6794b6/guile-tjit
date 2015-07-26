;; Test for type based unbox operation in entry clause of root trace.
;; Local 4 contains toplevel-box initialy, filled in by
;; `make-short-immediate' VM operation.

(define (f1 acc)
  (+ acc 1))

(define (f2 acc)
  (+ acc 2))

(define (loop low high)
  (let lp ((i 0) (acc 0))
    (cond
     ((= i #e1e5)
      acc)
     ((< high i)
      (lp (+ i 1) (f1 acc)))
     ((< low i)
      (lp (+ i 1) (f2 acc)))
     (else
      (lp (+ i 1) acc)))))

(loop 400 800)
