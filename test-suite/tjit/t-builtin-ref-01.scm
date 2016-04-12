;;; Loop to emit `builtin-ref'.

(define-syntax define-loop
  (syntax-rules ()
    ((_ name builtin)
     (define (name n)
       (let lp ((n n) (acc '()))
         (if (= n 0)
             acc
             (lp (- n 1) (cons builtin acc))))))))

(define-loop apply-loop apply)
(define-loop values-loop values)
(define-loop abort-to-prompt-loop abort-to-prompt)
(define-loop call-with-values-loop call-with-values)
(define-loop call/cc-loop call-with-current-continuation)

(define (loop2 n f)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (f n) acc)))))

(append (loop2 100 apply-loop)
        (loop2 100 values-loop)
        (loop2 100 abort-to-prompt-loop)
        (loop2 100 call-with-values-loop)
        (loop2 100 call/cc-loop))
