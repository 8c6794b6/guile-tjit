;; Deciding the branch with heap object. Definition of `loop1' and
;; `loop2' is same, though passed argument are different, which results
;; in different recorded bytecode.

(define (loop1 lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (if (car lst)
                          (+ acc 1)
                          acc)))))

(define (loop2 lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (if (car lst)
                          (+ acc 1)
                          acc)))))

(let* ((n #e1e6)
       (lst1 (append (make-list n #f)
                     (make-list n #t)))
       (lst2 (reverse lst1)))
  (list (loop1 lst1)
        (loop1 lst2)
        (loop2 lst2)
        (loop2 lst1)))
