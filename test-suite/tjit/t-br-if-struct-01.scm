;;; Simple loop containing `br-if-struct'.

(use-modules (srfi srfi-9))

(define-record-type <foo>
  (make-foo x)
  foo?
  (x foo-x set-foo-x!))

(define (loop lst acc)
  (if (null? lst)
      acc
      (let ((head (car lst))
            (tail (cdr lst)))
        (loop tail (if (struct? head)
                       (+ acc 1)
                       acc)))))

(let* ((n 100)
       (foo (make-foo 0))
       (foos (make-list n foo))
       (falses (make-list n #f))
       (lst (append falses foos falses foos falses foos)))
  (loop lst 0))
