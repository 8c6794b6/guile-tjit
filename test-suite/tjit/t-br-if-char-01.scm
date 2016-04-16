;;; Simple loop containing `br-if-char'

(define (loop lst acc)
  (if (null? lst)
      acc
      (loop (cdr lst) (if (char? (car lst))
                          (+ acc 1)
                          acc))))

(let* ((fs (make-list 100 #f))
       (ns (make-list 100 12345))
       (ps (make-list 100 '(1 2 3)))
       (cs (make-list 100 #\x))
       (lst (append fs cs ns cs ps cs fs ns ps cs)))
  (loop lst 0))
