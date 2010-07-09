;;; Guile VM debugging facilities

;;; Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.
;;;
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

;;; Code:

(define-module (system repl debug)
  #:use-module (system base pmatch)
  #:use-module (system base syntax)
  #:use-module (system base language)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module ((system vm inspect) #:select ((inspect . %inspect)))
  #:use-module (system vm program)
  #:export (<debug>
            make-debug debug? debug-frames debug-index
            print-locals print-frame print-frames frame->module
            stack->vector narrow-stack->vector))

;;; FIXME: add more repl meta-commands: continue, inspect, etc...

;;;
;;; Debugger
;;;
;;; The actual interaction loop of the debugger is run by the repl. This module
;;; simply exports a data structure to hold the debugger state, along with its
;;; accessors, and provides some helper functions.
;;;

(define-record <debug> frames index)



(define (reverse-hashq h)
  (let ((ret (make-hash-table)))
    (hash-for-each
     (lambda (k v)
       (hashq-set! ret v (cons k (hashq-ref ret v '()))))
     h)
    ret))

(define* (print-locals frame #:optional (port (current-output-port))
                       #:key (width 72) (per-line-prefix ""))
  (let ((bindings (frame-bindings frame)))
    (cond
     ((null? bindings)
      (format port "~aNo local variables.~%" per-line-prefix))
     (else
      (format port "~aLocal variables:~%" per-line-prefix)
      (for-each
       (lambda (binding)
         (format port "~a~4d ~a~:[~; (boxed)~] = ~v:@y\n"
                 per-line-prefix
                 (binding:index binding)
                 (binding:name binding)
                 (binding:boxed? binding)
                 width
                 (let ((x (frame-local-ref frame (binding:index binding))))
                   (if (binding:boxed? binding)
                       (variable-ref x)
                       x))))
       (frame-bindings frame))))))

(define* (print-frame frame #:optional (port (current-output-port))
                      #:key index (width 72) (full? #f) (last-source #f))
  (define (source:pretty-file source)
    (if source
        (or (source:file source) "current input")
        "unknown file"))
  (let* ((source (frame-source frame))
         (file (source:pretty-file source))
         (line (and=> source source:line)))
    (if (and file (not (equal? file (source:pretty-file last-source))))
        (format port "~&In ~a:~&" file))
    (format port "~:[~*~6_~;~5d:~]~:[~*~3_~;~3d~] ~v:@y~%"
            line line index index width (frame-call-representation frame))
    (if full?
        (print-locals frame #:width width
                      #:per-line-prefix "     "))))

(define* (print-frames frames
                       #:optional (port (current-output-port))
                       #:key (width 72) (full? #f) (forward? #f) count)
  (let* ((len (vector-length frames))
         (lower-idx (if (or (not count) (positive? count))
                        0
                        (max 0 (+ len count))))
         (upper-idx (if (and count (negative? count))
                        (1- len)
                        (1- (if count (min count len) len))))
         (inc (if forward? 1 -1)))
    (let lp ((i (if forward? lower-idx upper-idx))
             (last-source #f))
      (if (<= lower-idx i upper-idx)
          (let* ((frame (vector-ref frames i)))
            (print-frame frame port #:index i #:width width #:full? full?
                         #:last-source last-source)
            (lp (+ i inc) (frame-source frame)))))))

;; Ideally here we would have something much more syntactic, in that a set! to a
;; local var that is not settable would raise an error, and export etc forms
;; would modify the module in question: but alack, this is what we have now.
;; Patches welcome!
(define (frame->module frame)
  (let ((proc (frame-procedure frame)))
    (if (program? proc)
        (let* ((mod (or (program-module proc) (current-module)))
               (mod* (make-module)))
          (module-use! mod* mod)
          (for-each
           (lambda (binding)
             (let* ((x (frame-local-ref frame (binding:index binding)))
                    (var (if (binding:boxed? binding) x (make-variable x))))
               (format #t
                       "~:[Read-only~;Mutable~] local variable ~a = ~70:@y\n"
                       (binding:boxed? binding)
                       (binding:name binding)
                       (if (variable-bound? var) (variable-ref var) var))
               (module-add! mod* (binding:name binding) var)))
           (frame-bindings frame))
          mod*)
        (current-module))))


;; TODO:
;;
;; eval expression in context of frame
;; set local variable in frame
;; step until next instruction
;; step until next function call/return
;; step until return from frame
;; step until different source line
;; step until greater source line
;; watch expression
;; break on a function
;; remove breakpoints
;; set printing width
;; display a truncated backtrace
;; go to a frame by index
;; (reuse gdb commands perhaps)
;; disassemble a function
;; disassemble the current function
;; inspect any object
;; hm, trace via reassigning global vars. tricksy.
;; (state associated with vm ?)

(define (stack->vector stack)
  (let* ((len (stack-length stack))
         (v (make-vector len)))
    (if (positive? len)
        (let lp ((i 0) (frame (stack-ref stack 0)))
          (if (< i len)
              (begin
                (vector-set! v i frame)
                (lp (1+ i) (frame-previous frame))))))
    v))

(define (narrow-stack->vector stack . args)
  (stack->vector (apply make-stack (stack-ref stack 0) args)))

