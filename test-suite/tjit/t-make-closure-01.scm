;;; Simple loop containing `make-closure'.

(define (loop1 n acc)
  (if (= n 0)
      acc
      (let ((cls (lambda (x)
                   (+ x n))))
        (loop1 (- n 1) (cons cls acc)))))

(define (loop2 lst acc)
  (if (null? lst)
      acc
      (loop2 (cdr lst) ((car lst) acc))))

(loop2 (loop1 200 '()) 0)
