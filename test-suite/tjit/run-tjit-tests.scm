;;; run-tjit-tests.scm -- Run test suite for `vm-tjit' engine.
;;;
;;; Copyright 2005, 2009, 2010, 2013 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

(use-modules (srfi srfi-1)
             (system vm vm)
             (system vm loader)
             (system vm native tjit)
             (system base compile)
             (system base language))

(define (read-sexp-from-file path)
  (call-with-input-file path
    (lambda (port)
      (let lp ((exp (read port))
               (acc '()))
        (if (eof-object? exp)
            (cons 'begin (reverse! acc))
            (lp (read port) (cons exp acc)))))))

(define (compile-file-to-thunk path)
  (let* ((exp (read-sexp-from-file path))
         (bytecode (compile exp #:from 'scheme #:to 'bytecode)))
    (load-thunk-from-memory bytecode)))

(define (call-with-vm-tjit thunk)
  (call-with-values (lambda ()
                      (set-vm-engine! 'tjit)
                      (call-with-vm thunk))
    (lambda vals
      (set-vm-engine! 'regular)
      (apply values vals))))

(define (run-tjit-test path)
  (let ((thunk (compile-file-to-thunk path)))
    (let ((result-regular (thunk))
          (result-tjit (call-with-vm-tjit thunk)))
      (if (equal? result-regular result-tjit)
          #f
          (list result-regular result-tjit)))))

(define (run-tjit-tests paths)
  (init-vm-tjit #t)
  (let ((results (map run-tjit-test paths)))
    (cond
     ((and (every not results)
           (< 0 (assq-ref (tjit-stats) 'num-hot-loops)))
      (exit 0))
     (else
      (format #t "vm-regular: ~a~%vm-tjit:    ~a~%"
              (caar results) (cadar results))
      (exit 1)))))
