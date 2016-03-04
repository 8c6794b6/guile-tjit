;;; Bytecode compilation containing procedure definition and procedure
;;; call.

(compile '(begin
            (define (loop n l)
              (let lp ((n n) (acc 0))
                (if (< 0 n)
                    (lp (- n 1) (+ acc (length l)))
                    acc)))
            (loop #e1e3 '(1 2 3))))
