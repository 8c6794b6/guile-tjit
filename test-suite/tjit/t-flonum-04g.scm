;; Variant of `t-flonum-04.scm'. Size is fixed to 40, `mandelbrot-main'
;; does not take argument.

(define *bailout* 16.0)
(define *max-iterations* 1000)

(define (mandelbrot x y)
  (let ((cr (- y 0.5))
        (ci x)
        (zi 0.0)
        (zr 0.0))
    (let lp ((i 0) (zr zr) (zi zi))
      (if (< *max-iterations* i)
          0
          (let ((zi2 (* zi zi))
                (zr2 (* zr zr)))
            (if (< *bailout* (+ zi2 zr2))
                i
                (lp (+ i 1)
                    (+ (- zr2 zi2) cr)
                    (+ (* 2.0 zr zi) ci))))))))

(define (mandelbrot-main)
  (let loop ((y -39) (acc '()))
    (if (not (= y 39))
        (let ((line
               (let loop ((x -39) (output '()))
                 (if (= x 39)
                     (list->string output)
                     (let* ((fx (/ x 40.0))
                            (fy (/ y 40.0))
                            (c (if (zero? (mandelbrot fx fy))
                                   #\*
                                   #\space)))
                       (loop (+ x 1) (cons c output)))))))
          (loop (+ y 1) (cons line acc)))
        (reverse! acc))))

(mandelbrot-main)
