;;; Guile VM frame functions

;;; Copyright (C) 2001, 2005, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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

(define-module (system vm frame)
  #:use-module (system base pmatch)
  #:use-module (system vm program)
  #:use-module (system vm debug)
  #:use-module (ice-9 match)
  #:export (frame-bindings
            frame-lookup-binding
            frame-binding-ref frame-binding-set!
            frame-call-representation
            frame-environment
            frame-object-binding frame-object-name))

(define (frame-bindings frame)
  (let ((p (frame-procedure frame)))
    (program-bindings-for-ip p (frame-instruction-pointer frame))))

(define (frame-lookup-binding frame var)
  (let lp ((bindings (frame-bindings frame)))
    (cond ((null? bindings)
           #f)
          ((eq? (binding:name (car bindings)) var)
           (car bindings))
          (else
           (lp (cdr bindings))))))

(define (frame-binding-set! frame var val)
  (frame-local-set! frame
                    (binding:index
                     (or (frame-lookup-binding frame var)
                         (error "variable not bound in frame" var frame)))
                    val))

(define (frame-binding-ref frame var)
  (frame-local-ref frame
                   (binding:index
                    (or (frame-lookup-binding frame var)
                        (error "variable not bound in frame" var frame)))))


;; This function is always called to get some sort of representation of the
;; frame to present to the user, so let's do the logical thing and dispatch to
;; frame-call-representation.
(define (frame-arguments frame)
  (cdr (frame-call-representation frame)))



;;;
;;; Pretty printing
;;;

;; Basically there are two cases to deal with here:
;;
;;   1. We've already parsed the arguments, and bound them to local
;;      variables. In a standard (lambda (a b c) ...) call, this doesn't
;;      involve any argument shuffling; but with rest, optional, or
;;      keyword arguments, the arguments as given to the procedure may
;;      not correspond to what's on the stack. We reconstruct the
;;      arguments using e.g. for the case above: `(,a ,b ,c). This works
;;      for rest arguments too: (a b . c) => `(,a ,b . ,c)
;;
;;   2. We have failed to parse the arguments. Perhaps it's the wrong
;;      number of arguments, or perhaps we're doing a typed dispatch and
;;      the types don't match. In that case the arguments are all on the
;;      stack, and nothing else is on the stack.

(define (frame-call-representation frame)
  (let* ((ip (frame-instruction-pointer frame))
         (info (find-program-debug-info ip))
         (nlocals (frame-num-locals frame))
         (closure (frame-procedure frame)))
    (define (local-ref i)
      (if (< i nlocals)
          (frame-local-ref frame i)
          ;; Let's not error here, as we are called during backtraces.
          '???))
    (define (reconstruct-arguments nreq nopt kw has-rest? local)
      (cond
       ((positive? nreq)
        (cons (local-ref local)
              (reconstruct-arguments (1- nreq) nopt kw has-rest? (1+ local))))
       ((positive? nopt)
        (cons (local-ref local)
              (reconstruct-arguments nreq (1- nopt) kw has-rest? (1+ local))))
       ((pair? kw)
        (cons* (caar kw) (local-ref (cdar kw))
               (reconstruct-arguments nreq nopt (cdr kw) has-rest? (1+ local))))
       (has-rest?
        (local-ref local))
       (else
        '())))
    (cons
     (or (and=> info program-debug-info-name)
         (procedure-name closure)
         (and info
              ;; No need to give source info, as backtraces will already
              ;; take care of that.
              (format #f "#<procedure ~a>"
                      (number->string (program-debug-info-addr info) 16)))
         (procedure-name closure)
         closure)
     (cond
      ((find-program-arity ip)
       => (lambda (arity)
            ;; case 1
            (reconstruct-arguments (arity-nreq arity)
                                   (arity-nopt arity)
                                   (arity-keyword-args arity)
                                   (arity-has-rest? arity)
                                   1)))
      ((and (primitive? closure)
            (program-arguments-alist closure ip))
       => (lambda (args)
            (match args
              ((('required . req)
                ('optional . opt)
                ('keyword . kw)
                ('allow-other-keys? . _)
                ('rest . rest))
               ;; case 1
               (reconstruct-arguments (length req) (length opt) kw rest 1)))))
      (else
       ;; case 2
       (map local-ref
            ;; Cdr past the 0th local, which is the procedure.
            (cdr (iota nlocals))))))))



;;; Misc
;;;

(define (frame-environment frame)
  (map (lambda (binding)
	 (cons (binding:name binding) (frame-binding-ref frame binding)))
       (frame-bindings frame)))

(define (frame-object-binding frame obj)
  (do ((bs (frame-bindings frame) (cdr bs)))
      ((or (null? bs) (eq? obj (frame-binding-ref frame (car bs))))
       (and (pair? bs) (car bs)))))

(define (frame-object-name frame obj)
  (cond ((frame-object-binding frame obj) => binding:name)
	(else #f)))
