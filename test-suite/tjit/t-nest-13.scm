;;; Inter-procedure nested loop, with more nested loop in callee
;;; procedure.

(define (func-loops n)
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

(define (run-nestedloop n)
  (let loop ((n n) (v 0))
    (if (zero? n)
        v
        (loop (- n 1) (+ v (func-loops 15))))))

(list (run-nestedloop 15)
      (run-nestedloop 15))
