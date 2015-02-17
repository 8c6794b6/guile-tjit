;;; GDB debugging support for Guile.
;;;
;;; Copyright 2014, 2015 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guile-gdb)
  #:use-module (system base types)
  #:use-module (system vm debug)
  #:use-module ((gdb) #:hide (symbol? frame?))
  #:use-module ((gdb) #:select ((symbol? . gdb:symbol?) (frame? . gdb:frame?)))
  #:use-module (gdb printing)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-41)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:export (%gdb-memory-backend
            display-vm-frames))

;;; Commentary:
;;;
;;; This file defines GDB extensions to pretty-print 'SCM' objects, and
;;; to walk Guile's virtual machine stack.
;;;
;;; This file is installed under a name that follows the convention that
;;; allows GDB to auto-load it anytime the user is debugging libguile
;;; (info "(gdb) objfile-gdbdotext file").
;;;
;;; Code:

(define (type-name-from-descriptor descriptor-array type-number)
  "Return the name of the type TYPE-NUMBER as seen in DESCRIPTOR-ARRAY, or #f
if the information is not available."
  (let ((descriptors (lookup-global-symbol descriptor-array)))
    (and descriptors
         (let ((code (type-code (symbol-type descriptors))))
           (or (= TYPE_CODE_ARRAY code)
               (= TYPE_CODE_PTR code)))
         (let* ((type-descr (value-subscript (symbol-value descriptors)
                                             type-number))
                (name       (value-field type-descr "name")))
           (value->string name)))))

(define %gdb-memory-backend
  ;; The GDB back-end to access the inferior's memory.
  (let ((void* (type-pointer (lookup-type "void"))))
    (define (dereference-word address)
      ;; Return the word at ADDRESS.
      (value->integer
       (value-dereference (value-cast (make-value address)
                                      (type-pointer void*)))))

    (define (open address size)
      ;; Return a port to the SIZE bytes starting at ADDRESS.
      (if size
          (open-memory #:start address #:size size)
          (open-memory #:start address)))

    (define (type-name kind number)
      ;; Return the type name of KIND type NUMBER.
      (type-name-from-descriptor (case kind
                                   ((smob) "scm_smobs")
                                   ((port) "scm_ptobs"))
                                 number))

    (memory-backend dereference-word open type-name)))


;;;
;;; GDB pretty-printer registration.
;;;

(define scm-value->string
  (lambda* (value #:optional (backend %gdb-memory-backend))
    "Return a representation of value VALUE as a string."
    (object->string (scm->object (value->integer value) backend))))

(define %scm-pretty-printer
  (make-pretty-printer "SCM"
                       (lambda (pp value)
                         (let ((name (type-name (value-type value))))
                           (and (and name (string=? name "SCM"))
                                (make-pretty-printer-worker
                                 #f              ; display hint
                                 (lambda (printer)
                                   (scm-value->string value %gdb-memory-backend))
                                 #f))))))

(define* (register-pretty-printer #:optional objfile)
  (prepend-pretty-printer! objfile %scm-pretty-printer))

(register-pretty-printer)


;;;
;;; VM stack walking.
;;;

(define ip-type (type-pointer (lookup-type "scm_t_uint32")))
(define fp-type (type-pointer (lookup-type "SCM")))
(define sp-type (type-pointer (lookup-type "SCM")))

(define-record-type <vm-frame>
  (make-vm-frame ip sp fp saved-ip saved-fp)
  vm-frame?
  (ip vm-frame-ip)
  (sp vm-frame-sp)
  (fp vm-frame-fp)
  (saved-ip vm-frame-saved-ip)
  (saved-fp vm-frame-saved-fp))

;; See libguile/frames.h.
(define* (vm-frame ip sp fp #:optional (backend %gdb-memory-backend))
  "Return the components of the stack frame at FP."
  (make-vm-frame ip
                 sp
                 fp
                 (value-dereference (value-cast (value-sub fp 1)
                                                (type-pointer ip-type)))
                 (value-dereference (value-cast (value-sub fp 2)
                                                (type-pointer fp-type)))))

(define (vm-engine-frame? frame)
  (let ((sym (frame-function frame)))
    (and sym
         (member (symbol-name sym)
                 '("vm_debug_engine" "vm_regular_engine")))))

(define (find-vp)
  "Find the scm_vm pointer for the current thread."
  (let loop ((frame (newest-frame)))
    (and frame
         (if (vm-engine-frame? frame)
             (frame-read-var frame "vp")
             (loop (frame-older frame))))))

(define (newest-vm-frame)
  "Return the newest VM frame or #f."
  (let ((vp (find-vp)))
    (and vp
         (vm-frame (value-field vp "ip")
                   (value-field vp "sp")
                   (value-field vp "fp")))))

(define* (vm-frame-older frame #:optional (backend %gdb-memory-backend))
  (let ((ip (vm-frame-saved-ip frame))
        (sp (value-sub (vm-frame-fp frame) 3))
        (fp (vm-frame-saved-fp frame)))
    (and (not (zero? (value->integer fp)))
         (vm-frame ip sp fp backend))))

(define (vm-frames)
  "Return a SRFI-41 stream of the current VM frame stack."
  (stream-unfold identity
                 vm-frame?
                 vm-frame-older
                 (newest-vm-frame)))

(define (vm-frame-locals frame)
  (let ((fp (vm-frame-fp frame))
        (sp (vm-frame-sp frame)))
    (let lp ((slot 0) (ptr fp))
      (if (value<=? ptr sp)
          (acons (string-append "v" (number->string slot))
                 (value-dereference ptr)
                 (lp (1+ slot) (value-add ptr 1)))
          '()))))

(define (lookup-symbol-or-false name)
  (match (lookup-symbol name)
    (#f #f)
    ((sym _) sym)))

(define (find-mapped-elf-image addr)
  (let ((array (lookup-symbol-or-false "mapped_elf_images"))
        (count (lookup-symbol-or-false "mapped_elf_images_count")))
    (and array count
         (let ((array (symbol-value array))
               (count (value->integer (symbol-value count))))
           (let lp ((start 0) (end count))
             (if (< start end)
                 (let ((n (+ start (ash (- end start) -1))))
                   (if (value<? addr (value-field (value-add array n) "end"))
                       (lp start n)
                       (lp (1+ n) end)))
                 (let ((mei (value-add array start)))
                   (and (value<=? (value-field mei "start") addr)
                        mei))))))))

(define (vm-frame-program-debug-info frame)
  (let ((addr (vm-frame-ip frame)))
    (and=> (find-mapped-elf-image addr)
           (lambda (mei)
             (let* ((start (value->integer (value-field mei "start")))
                    (size (- (value->integer (value-field mei "end"))
                             start))
                    (mem-port (open-memory #:start start #:size size))
                    (bv (get-bytevector-all mem-port))
                    (ctx (debug-context-from-image bv)))
               ;; The image is in this process at "bv", but in the
               ;; inferior at mei.start.  Therefore we relocate addr
               ;; before we look for the PDI.
               (let ((addr (+ (value->integer addr)
                              (- (debug-context-base ctx) start))))
                 (find-program-debug-info addr ctx)))))))

(define (vm-frame-function-name frame)
  (define (default-name)
    (format #f "0x~x" (value->integer (vm-frame-ip frame))))
  (cond
   ((vm-frame-program-debug-info frame)
    => (lambda (pdi)
         (or (and=> (program-debug-info-name pdi) symbol->string)
             (default-name))))
   (else
    (let ((ip (vm-frame-ip frame)))
      (define (ip-in-symbol? name)
        (let ((sym (lookup-symbol-or-false name)))
          (and sym
               (let* ((val (symbol-value sym))
                      (size (type-sizeof (value-type val)))
                      (char* (type-pointer (arch-char-type (current-arch))))
                      (val-as-char* (value-cast val char*)))
                 (and (value<=? val-as-char* ip)
                      (value<? ip (value-add val-as-char* size)))))))
      (cond
       ((ip-in-symbol? "vm_boot_continuation_code") "[boot continuation]")
       ;; FIXME: For subrs, read the name from slot 0 in the frame.
       ((ip-in-symbol? "subr_stub_code") "[subr call]")
       ((ip-in-symbol? "vm_builtin_apply_code") "apply")
       ((ip-in-symbol? "vm_builtin_values_code") "values")
       ((ip-in-symbol? "vm_builtin_abort_to_prompt_code") "abort-to-prompt")
       ((ip-in-symbol? "vm_builtin_call_with_values_code") "call-with-values")
       ((ip-in-symbol? "vm_builtin_call_with_current_continuation_code")
        "call-with-current-continuation")
       ((ip-in-symbol? "continuation_stub_code") "[continuation]")
       ((ip-in-symbol? "compose_continuation_code") "[delimited continuation]")
       ((ip-in-symbol? "foreign_stub_code") "[ffi call]")
       (else (default-name)))))))

(define* (dump-vm-frame frame #:optional (port (current-output-port)))
  (format port "  name: ~a~%" (vm-frame-function-name frame))
  (format port "  ip: 0x~x~%" (value->integer (vm-frame-ip frame)))
  (format port "  fp: 0x~x~%" (value->integer (vm-frame-fp frame)))
  (for-each (match-lambda
             ((name . val)
              (let ((obj (scm->object (value->integer val) %gdb-memory-backend)))
                (format port "    ~a: ~a~%" name obj))))
            (vm-frame-locals frame)))

(define* (display-vm-frames #:optional (port (current-output-port)))
  "Display the VM frames on PORT."
  (stream-for-each (lambda (frame)
                     (dump-vm-frame frame port))
                   (vm-frames)))

;;; libguile-2.2-gdb.scm ends here
