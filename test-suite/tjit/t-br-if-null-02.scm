;;; Loop with `br-if-null', taking small list.
;;;
;;; The argument `(0 1 2 3), passed to `loop', will make the recorded
;;; locals to contain `()' at the end of root trace with default number
;;; of `tjit-hot-loop'. In such case, snapshot taken with `br-if-null'
;;; at the end of recorded bytecode was once pointing to the bytecode IP
;;; going back to the beginning of trace instead of the IP getting out
;;; from the loop. The loop in this file will test for such case.

(define (loop lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc 1)))))

(do ((i 0 (+ i 1))
     (acc '() (cons (loop '(0 1 2 3)) acc)))
    ((= i 65) acc))
