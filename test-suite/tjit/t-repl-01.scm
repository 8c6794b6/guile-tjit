;;; Simple loop calling (@@ (system repl command) lookup-command).

(define lookup-command
  (@@ (system repl command) lookup-command))

(define (loop n sym)
  (let lp ((n n) (acc #f))
    (if (zero? n)
        acc
        (lp (- n 1) (lookup-command sym)))))

(loop #e1e3 'time)
