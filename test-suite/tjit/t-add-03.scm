;;; Simple loop containing `add', modifying top level value after making
;;; compiling native code..

(define incr 123)

(define (loop lst ini)
  (let lp ((lst lst) (acc ini))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (car lst) incr)))))

(let ((r1 (let ((lst (make-list 1000 1)))
            (loop lst 0)))
      (r2 (let ((lst (make-list 1000 1.23)))
            (loop lst 0))))
  (set! incr 123.456)
  (let ((r3 (let ((lst (make-list 1000 1)))
              (loop lst 0)))
        (r4 (let ((lst (make-list 1000 1.23)))
              (loop lst 0))))
    (list r1 r2 r3 r4)))
