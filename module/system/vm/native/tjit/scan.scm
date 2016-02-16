;;;; Stack element scanner

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; A module containing procedure to scan through stack elements before IR
;;; compilation.
;;;
;;; Code:

(define-module (system vm native tjit scan)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit snapshot)
  #:export (scan-locals))

(define (scan-locals ol op prev-op ip dl locals)
  ;; Compute local indices and stack element types in op.
  ;;
  ;; The stack used by VM interpreter grows down. Lower frame data is saved at
  ;; the time of accumulation.  If one of the guard operation appeared soon
  ;; after bytecode sequence `return' or `receive', snapshot does not know the
  ;; value of locals in lower frame. When recorded bytecode contains `return'
  ;; before `call', snapshot will recover a frame higher than the one used to
  ;; enter the native call.
  ;;
  ;; This procedure does a lookup of accumulating procedure stored in
  ;; *index-scanners* and apply the procedure when found. Value in the hash
  ;; table might be a list, in such case, the element of list is a pair of
  ;; (test-procedure . work-procedure).
  ;;
  (define-syntax-rule (nyi)
    (begin
      (debug 1 "NYI: ~a~%" (car op))
      (values #f (car op))))

  ;; Look for the type of returned value from C function.
  (unless (outline-initialized? ol)
    (debug 1 ";;; [scan-locals] op=~s~%" op)
    (let* ((ret-types (outline-ret-types ol))
           (fill-false
            (lambda ()
              (set-outline-ret-types! ol (cons #f ret-types)))))
      (if (eq? 'subr-call prev-op)
          (match op
            (('receive dst proc nlocals)
             (let* ((stack-size (vector-length locals))
                    (idx (- stack-size proc 2))
                    (val (stack-element locals idx 'scm))
                    (type (type-of val)))
               (set-outline-ret-types! ol (cons type ret-types))))
            (('receive-values proc _ nvalues)
             (if (= nvalues 1)
                 (let* ((stack-size (vector-length locals))
                        (idx (- stack-size proc 2))
                        (val (stack-element locals idx 'scm))
                        (type (type-of val)))
                   (set-outline-ret-types! ol (cons type ret-types)))
                 (fill-false)))
            (_
             (fill-false)))
          (fill-false))))

  (match (hashq-ref *scan-procedures* (car op))
    ((? list? procs)
     (let lp ((procs procs))
       (match procs
         (((test . work) . procs)
          (if (apply test (list op locals))
              (apply work ip dl locals ol (cdr op))
              (lp procs)))
         (_ (nyi)))))
    (_ (nyi))))
