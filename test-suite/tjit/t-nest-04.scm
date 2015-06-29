(define (loops n)
  (let ((result 0))
    (let loop1 ((i1 1))
      (if (> i1 n)
          'done
          (begin
            (let loop2 ((i2 1))
              (if (> i2 n)
                  'done
                  (begin
                    (let loop3 ((i3 1))
                      (if (> i3 n)
                          'done
                          (begin
                            (let loop4 ((i4 1))
                              (if (> i4 n)
                                  'done
                                  (begin
                                    (let loop5 ((i5 1))
                                      (if (> i5 n)
                                          'done
                                          (begin
                                            (let loop6 ((i6 1))
                                              (if (> i6 n)
                                                  'done
                                                  (begin
                                                    (set! result (+ result 1))
                                                    (loop6 (+ i6 1)))))
                                            (loop5 (+ i5 1)))))
                                    (loop4 (+ i4 1)))))
                            (loop3 (+ i3 1)))))
                    (loop2 (+ i2 1)))))
            (loop1 (+ i1 1)))))
    result))

(loops 18)
