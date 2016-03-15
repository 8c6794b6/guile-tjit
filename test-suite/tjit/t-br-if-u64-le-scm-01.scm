;;; Simple root trace with `br-if-u64-=-scm'. Last bytecode in recorded
;;; trace gets out from the loop.

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (<= i 0)
        acc
        (lp (- i 1) (+ acc 1)))))

(do ((i 0 (+ i 1))
     (acc '() (cons (loop 3) acc)))
    ((= i 65) acc))
