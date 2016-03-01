;; Test found in REPL, for format.

(use-modules (system vm disassembler))

(define (fib n)
  (letrec ((f (lambda (x)
                (if (< x 2)
                    x
                    (+ (f (- x 1)) (f (- x 2)))))))
    (f n)))

(call-with-output-string
  (lambda (port)
    (disassemble-program fib port)))
