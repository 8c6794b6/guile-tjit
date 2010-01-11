;;; User interface messages

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; This module provide a simple interface to send messages to the user.
;;; TODO: Internationalize messages.
;;;
;;; Code:

(define-module (system base message)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (*current-warning-port* warning

            warning-type? warning-type-name warning-type-description
            warning-type-printer lookup-warning-type

            %warning-types))


;;;
;;; Source location
;;;

(define (location-string loc)
  (if (pair? loc)
      (format #f "~a:~a:~a"
              (or (assoc-ref loc 'filename) "<stdin>")
              (1+ (assoc-ref loc 'line))
              (assoc-ref loc 'column))
      "<unknown-location>"))


;;;
;;; Warnings
;;;

(define *current-warning-port*
  ;; The port where warnings are sent.
  (make-fluid))

(fluid-set! *current-warning-port* (current-error-port))

(define-record-type <warning-type>
  (make-warning-type name description printer)
  warning-type?
  (name         warning-type-name)
  (description  warning-type-description)
  (printer      warning-type-printer))

(define %warning-types
  ;; List of know warning types.
  (map (lambda (args)
         (apply make-warning-type args))

       `((unsupported-warning ;; a "meta warning"
          "warn about unknown warning types"
          ,(lambda (port unused name)
             (format port "warning: unknown warning type `~A'~%"
                     name)))

         (unused-variable
          "report unused variables"
          ,(lambda (port loc name)
             (format port "~A: warning: unused variable `~A'~%"
                     loc name)))

         (unused-toplevel
          "report unused local top-level variables"
          ,(lambda (port loc name)
             (format port "~A: warning: possibly unused local top-level variable `~A'~%"
                     loc name)))

         (unbound-variable
          "report possibly unbound variables"
          ,(lambda (port loc name)
             (format port "~A: warning: possibly unbound variable `~A'~%"
                     loc name)))

         (arity-mismatch
          "report procedure arity mismatches (wrong number of arguments)"
          ,(lambda (port loc name certain?)
             (if certain?
                 (format port
                         "~A: warning: wrong number of arguments to `~A'~%"
                         loc name)
                 (format port
                         "~A: warning: possibly wrong number of arguments to `~A'~%"
                         loc name)))))))

(define (lookup-warning-type name)
  "Return the warning type NAME or `#f' if not found."
  (find (lambda (wt)
          (eq? name (warning-type-name wt)))
        %warning-types))

(define (warning type location . args)
  "Emit a warning of type TYPE for source location LOCATION (a source
property alist) using the data in ARGS."
  (let ((wt   (lookup-warning-type type))
        (port (fluid-ref *current-warning-port*)))
    (if (warning-type? wt)
        (apply (warning-type-printer wt)
               port (location-string location)
               args)
        (format port "~A: unknown warning type `~A': ~A~%"
                (location-string location) type args))))

;;; message.scm ends here
