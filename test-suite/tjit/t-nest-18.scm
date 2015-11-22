;;; Simple inter-procedure nested loop.

(define (loop1 n)
  (let lp1 ((i1 1) (ret 0))
    (if (> i1 n)
        ret
        (let lp2 ((i2 1) (ret ret))
          (if (> i2 n)
              (lp1 (+ i1 1) ret)
              (let lp3 ((i3 1) (ret ret))
                (if (> i3 n)
                    (lp2 (+ i2 1) ret)
                    (let lp4 ((i4 1) (ret ret))
                      (if (> i4 n)
                          (lp3 (+ i3 1) ret)
                          (let lp5 ((i5 1) (ret ret))
                            (if (> i5 n)
                                (lp4 (+ i4 1) ret)
                                (let lp6 ((i6 1) (ret ret))
                                  (if (> i6 n)
                                      (lp5 (+ i5 1) ret)
                                      (lp6 (+ i6 1) (+ ret 1)))))))))))))))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (+ acc (loop1 n)))
        acc)))

(let-syntax ((gen-loops
              (lambda (x)
                (syntax-case x ()
                  ((_ n)
                   #`(list
                      #,@(let lp ((n (syntax->datum #'n)) (acc '()))
                           (if (= n 0)
                               acc
                               (lp (- n 1)
                                   (cons #'(loop2 10) acc))))))))))
  (gen-loops 15))
