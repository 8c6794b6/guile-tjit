#!./meta/guile \
-e main -s
!#
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

(use-modules (ice-9 getopt-long)
             (ice-9 regex)
             ((srfi srfi-1) #:select (every))
             (srfi srfi-11)
             (system vm coverage)
             (system vm vm)
             (system vm loader)
             (system vm native debug)
             (language trace dump)
             (language trace error)
             (language trace parameters)
             (system vm native tjitc)
             (system base compile))

(define (read-sexp-from-file path)
  (call-with-input-file path
    (lambda (port)
      (let lp ((exp (read port)) (acc '()))
        (if (eof-object? exp)
            (cons 'begin (reverse! acc))
            (lp (read port) (cons exp acc)))))))

(define (compile-file-to-thunk path)
  (let* ((exp (read-sexp-from-file path))
         (bytecode (compile exp #:from 'scheme #:to 'bytecode)))
    (load-thunk-from-memory bytecode)))

(define-inlinable (call-with-vm-tjit thunk)
  (call-with-values (lambda ()
                      (set-vm-engine! 'tjit)
                      (call-with-vm thunk))
    (lambda vals
      (set-vm-engine! 'regular)
      (apply values vals))))

(define (ensure-absolute-path path)
  (if (absolute-file-name? path)
      path
      (string-append (getcwd) file-name-separator-string path)))

(define (run-tjit-test coverage)
  (lambda (path)
    (when coverage
      (format #t "Running: ~a~%" path))
    (call-with-output-file *null-device*
      (lambda (nowhere)
        (parameterize ((current-output-port (if coverage
                                                nowhere
                                                (current-output-port))))
          (let* ((path (ensure-absolute-path path))
                 (thunk (compile-file-to-thunk path))
                 (result-regular (thunk))
                 (result-tjit (call-with-vm-tjit thunk)))
            (if (equal? result-regular result-tjit)
                #f
                (list result-regular result-tjit))))))))

(define (main args)
  (let* ((spec '((coverage (required? #f)
                           (value #f)
                           (single-char #\c))
                 (out (required? #f)
                      (value #t)
                      (single-char #\o))))
         (opts (getopt-long args spec))
         (paths (option-ref opts '() '()))
         (coverage (option-ref opts 'coverage #f))
         (out (option-ref opts 'out "tjit-tests.info")))
    (init-vm-tjit coverage)
    (when coverage
      (lightning-verbosity 2)
      (set-tjit-dump-option! "jbovdstn")
      (tjit-dump-log (open-output-file "tjit-tests.log")))
    (let* ((run (lambda ()
                  (map (run-tjit-test coverage)
                       (if coverage
                           (let ((rx (make-regexp "t-.*\\.scm")))
                             (filter (lambda (path)
                                       (regexp-exec rx path))
                                     paths))
                           paths))))
           (results (if coverage
                        (let-values (((data results)
                                      (with-code-coverage run)))
                          (call-with-output-file out
                            (lambda (port)
                              (coverage-data->lcov data port)))
                          results)
                        (run)))
           (nfs (assq-ref (tjit-stats) 'num-fragments))
           (nerrors (hash-count (const #t) (tjitc-errors))))
      (cond
       ((and (every not results)
             (number? nfs)
             (< 0 nfs)
             (zero? nerrors))
        (exit 0))
       ((<= nfs 0)
        (format #t "num-fragments: ~s~%" nfs)
        (exit 1))
       ((< 0 nerrors)
        (format #t "tjitc error~%")
        (exit 1))
       (else
        (if (and (pair? results) (pair? (car results)))
            (let ((result (car results)))
              (format #t "vm-regular: ~s~%vm-tjit:    ~s~%"
                      (car result) (cadr result)))
            (format #t "results: ~s~%" results))
        (exit 1))))))
