;;; Loop with closure, contains root trace with stack pointer shift.

(define (loop n f acc)
  (if (= n 0)
      acc
      (loop (- n 1) f (f n acc))))

(letrec ((f (lambda (n x)
              (loop (- n 1) f (+ x 1)))))
  (loop 20 f 0))
