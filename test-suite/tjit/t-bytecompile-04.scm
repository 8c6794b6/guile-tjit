(compile
 '(begin
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

    (define (mandelbrot-main size)
      (let loop ((y (- (- size 1))) (acc '()))
        (if (not (= y (- size 1)))
            (let ((line
                   (let loop ((x (- (- size 1))) (output '()))
                     (if (= x (- size 1))
                         (list->string output)
                         (let* ((fx (/ x (exact->inexact size)))
                                (fy (/ y (exact->inexact size)))
                                (c (if (zero? (mandelbrot fx fy))
                                       #\*
                                       #\space)))
                           (loop (+ x 1) (cons c output)))))))
              (loop (+ y 1) (cons line acc)))
            (reverse! acc))))

    (define-syntax-rule (time exp)
      (let* ((t1 (gettimeofday))
             (ret exp)
             (t2 (gettimeofday))
             (sec-start (car t1))
             (usec-start (cdr t1))
             (sec-end (car t2))
             (usec-end (cdr t2)))
        (format #t "Elapsed: ~s~%"
                (+ (- sec-end sec-start)
                   (/ (- usec-end usec-start) 1000000.0)))
        ret))

    (define (main n)
      (do ((i 0 (+ i 1)))
          ((= i n) (mandelbrot-main 40))
        (mandelbrot-main 40)))

    (time (main 1))))
