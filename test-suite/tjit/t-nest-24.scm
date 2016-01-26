;;; Similar to `t-nest-18.scm'. Does not contain `resolve' in outer most
;;; loop.

(define (loop1 n)
  (let lp1 ((i1 1) (ret 0))
    (if (> i1 n)
        ret
        (let lp2 ((i2 1) (ret ret))
          (if (> i2 n)
              (lp1 (+ i1 1) ret)
              (lp2 (+ i2 1) (+ ret 1)))))))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (+ acc (loop1 n)))
        acc)))

(define-syntax list-of-loops
  (lambda (x)
    (syntax-case x ()
      ((k n)
       #`(list
           #,@(let lp ((n (syntax->datum #'n)) (acc '()))
                (if (= n 0)
                    acc
                    (lp (- n 1) (cons #'(loop2 100) acc)))))))))

(list-of-loops 20)
