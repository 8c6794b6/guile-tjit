;;; `Point' example translated to Scheme, from:
;;;
;;;  http://wiki.luajit.org/Allocation-Sinking-Optimization
;;;

(define (point x y)
  (lambda (method . args)
    (cond
     ((eq? method 'x) x)
     ((eq? method 'y) y)
     ((eq? method 'add)
      (let ((b (car args)))
        (point (+ x (b 'x)) (+ y (b 'y))))))))

(let lp ((i 1000) (a (point 1.5 2.5)) (b (point 3.25 4.75)))
  (if (< 0 i)
      (lp (- i 1) ((a 'add b) 'add b) b)
      (cons (a 'x) (a 'y))))
