;;; Simple top-level loop containing no argument. Will result with
;;; bytecode containing `mov' operation with u64 elements.

(do ((n 0 (+ n 1))) ((< 100 n))
  (do ((j 0 (+ j 1))) ((< 100 j))))
