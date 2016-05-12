;;; Ports
;;; Copyright (C) 2016 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Implementation of input/output routines over ports.
;;;
;;; Note that loading this module overrides some core bindings; see the
;;; `replace-bootstrap-bindings' invocation below for details.
;;;
;;; Code:


(define-module (ice-9 ports)
  #:use-module (rnrs bytevectors)
  #:export (;; Definitions from ports.c.
            %port-property
            %set-port-property!
            current-input-port current-output-port
            current-error-port current-warning-port
            set-current-input-port set-current-output-port
            set-current-error-port
            port-mode
            port?
            input-port?
            output-port?
            port-closed?
            eof-object?
            close-port
            close-input-port
            close-output-port
            ;; These two are currently defined by scm_init_ports; fix?
            ;; %default-port-encoding
            ;; %default-port-conversion-strategy
            port-encoding
            set-port-encoding!
            port-conversion-strategy
            set-port-conversion-strategy!
            read-char
            peek-char
            unread-char
            unread-string
            setvbuf
            drain-input
            force-output
            char-ready?
            seek SEEK_SET SEEK_CUR SEEK_END
            truncate-file
            port-line
            set-port-line!
            port-column
            set-port-column!
            port-filename
            set-port-filename!
            port-for-each
            flush-all-ports
            %make-void-port

            ;; Definitions from fports.c.
            open-file
            file-port?
            port-revealed
            set-port-revealed!
            adjust-port-revealed!
            ;; note: %file-port-name-canonicalization is used in boot-9

            ;; Definitions from ioext.c.
            ftell
            redirect-port
            dup->fdes
            dup2
            fileno
            isatty?
            fdopen
            primitive-move->fdes
            fdes->ports

            ;; Definitions in Scheme
            file-position
            file-set-position
            move->fdes
            release-port-handle
            dup->port
            dup->inport
            dup->outport
            dup
            duplicate-port
            fdes->inport
            fdes->outport
            port->fdes
            OPEN_READ OPEN_WRITE OPEN_BOTH
            *null-device*
            open-input-file
            open-output-file
            open-io-file
            call-with-input-file
            call-with-output-file
            with-input-from-port
            with-output-to-port
            with-error-to-port
            with-input-from-file
            with-output-to-file
            with-error-to-file
            call-with-input-string
            with-input-from-string
            call-with-output-string
            with-output-to-string
            with-error-to-string
            the-eof-object
            inherit-print-state))

(define (replace-bootstrap-bindings syms)
  (for-each
   (lambda (sym)
     (let* ((var (module-variable the-scm-module sym))
            (mod (current-module))
            (iface (module-public-interface mod)))
       (unless var (error "unbound in root module" sym))
       (module-add! mod sym var)
       (when (module-local-variable iface sym)
         (module-add! iface sym var))))
   syms))

(replace-bootstrap-bindings '(open-file
                              open-input-file
                              set-port-encoding!
                              eof-object?
                              force-output
                              call-with-output-string
                              close-port
                              current-error-port
                              current-warning-port))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_ice_9_ports")
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_ice_9_fports")
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_ice_9_ioext")



(define (port-encoding port)
  "Return, as a string, the character encoding that @var{port} uses to
interpret its input and output."
  (symbol->string (%port-encoding port)))



(define-syntax-rule (port-buffer-bytevector buf) (vector-ref buf 0))
(define-syntax-rule (port-buffer-cur buf) (vector-ref buf 1))
(define-syntax-rule (port-buffer-end buf) (vector-ref buf 2))
(define-syntax-rule (port-buffer-has-eof? buf) (vector-ref buf 3))

(define-syntax-rule (set-port-buffer-cur! buf cur)
  (vector-set! buf 1 cur))
(define-syntax-rule (set-port-buffer-end! buf end)
  (vector-set! buf 2 end))
(define-syntax-rule (set-port-buffer-has-eof?! buf has-eof?)
  (vector-set! buf 3 has-eof?))

(define (make-port-buffer size)
  (vector (make-bytevector size 0) 0 0 #f))

(define (write-bytes port src start count)
  (let ((written ((port-write port) port src start count)))
    (unless (<= 0 written count)
      (error "bad return from port write function" written))
    (when (< written count)
      (write-bytes port src (+ start written) (- count written)))))

(define (flush-output port)
  (let* ((buf (port-write-buffer port))
         (cur (port-buffer-cur buf))
         (end (port-buffer-end buf)))
    (when (< cur end)
      ;; Update cursors before attempting to write, assuming that I/O
      ;; errors are sticky.  That way if the write throws an error,
      ;; causing the computation to abort, and possibly causing the port
      ;; to be collected by GC when it's open, any subsequent close-port
      ;; or force-output won't signal *another* error.
      (set-port-buffer-cur! buf 0)
      (set-port-buffer-end! buf 0)
      (write-bytes port (port-buffer-bytevector buf) cur (- end cur)))))

(define (read-bytes port dst start count)
  (let ((read ((port-read port) port dst start count)))
    (unless (<= 0 read count)
      (error "bad return from port read function" read))
    read))

(define utf8-bom #vu8(#xEF #xBB #xBF))
(define utf16be-bom #vu8(#xFE #xFF))
(define utf16le-bom #vu8(#xFF #xFE))
(define utf32be-bom #vu8(#x00 #x00 #xFE #xFF))
(define utf32le-bom #vu8(#xFF #xFE #x00 #x00))

(define (clear-stream-start-for-bom-read port io-mode)
  (define (maybe-consume-bom bom)
    (and (eq? (peek-byte port) (bytevector-u8-ref bom 0))
         (call-with-values (lambda ()
                             (fill-input port (bytevector-length bom)))
           (lambda (buf buffered)
             (and (<= (bytevector-length bom) buffered)
                  (let ((bv (port-buffer-bytevector buf))
                        (cur (port-buffer-cur buf)))
                    (let lp ((i 1))
                      (if (= i (bytevector-length bom))
                          (begin
                            (set-port-buffer-cur! buf (+ cur i))
                            #t)
                          (and (eq? (bytevector-u8-ref bv (+ cur i))
                                    (bytevector-u8-ref bom i))
                               (lp (1+ i)))))))))))
  (when (and (port-clear-stream-start-for-bom-read port)
             (eq? io-mode 'text))
    (case (%port-encoding port)
      ((UTF-8)
       (maybe-consume-bom utf8-bom))
      ((UTF-16)
       (cond
        ((maybe-consume-bom utf16le-bom)
         (specialize-port-encoding! port 'UTF-16LE))
        (else
         (maybe-consume-bom utf16be-bom)
         (specialize-port-encoding! port 'UTF-16BE))))
      ((UTF-32)
       (cond
        ((maybe-consume-bom utf32le-bom)
         (specialize-port-encoding! port 'UTF-32LE))
        (else
         (maybe-consume-bom utf32be-bom)
         (specialize-port-encoding! port 'UTF-32BE)))))))

(define* (fill-input port #:optional (minimum-buffering 1))
  (clear-stream-start-for-bom-read port 'text)
  (let* ((buf (port-read-buffer port))
         (cur (port-buffer-cur buf))
         (buffered (- (port-buffer-end buf) cur)))
    (cond
     ((or (<= minimum-buffering buffered) (port-buffer-has-eof? buf))
      (values buf buffered))
     (else
      (unless (input-port? port)
        (error "not an input port" port))
      (when (port-random-access? port)
        (flush-output port))
      (let ((bv (port-buffer-bytevector buf)))
        (cond
         ((< (bytevector-length bv) minimum-buffering)
          (let ((buf* (make-port-buffer minimum-buffering)))
            (bytevector-copy! bv cur (port-buffer-bytevector buf*) 0 buffered)
            (set-port-buffer-end! buf* buffered)
            (set-port-read-buffer! port buf*)
            (fill-input port minimum-buffering)))
         (else
          (when (< 0 cur)
            (bytevector-copy! bv cur bv 0 buffered)
            (set-port-buffer-cur! buf 0)
            (set-port-buffer-end! buf buffered))
          (let ((buffering (max (port-read-buffering port) minimum-buffering)))
            (let lp ((buffered buffered))
              (let* ((count (- buffering buffered))
                     (read (read-bytes port bv buffered count)))
                (cond
                 ((zero? read)
                  (set-port-buffer-has-eof?! buf #t)
                  (values buf buffered))
                 (else
                  (let ((buffered (+ buffered read)))
                    (set-port-buffer-end! buf buffered)
                    (if (< buffered minimum-buffering)
                        (lp buffered)
                        (values buf buffered)))))))))))))))

(define-inlinable (peek-bytes port count kfast kslow)
  (let* ((buf (port-read-buffer port))
         (cur (port-buffer-cur buf))
         (buffered (- (port-buffer-end buf) cur)))
    (if (<= count buffered)
        (kfast buf (port-buffer-bytevector buf) cur buffered)
        (call-with-values (lambda () (fill-input port count))
          (lambda (buf buffered)
            (kslow buf (port-buffer-bytevector buf) (port-buffer-cur buf)
                   buffered))))))

(define (peek-byte port)
  (peek-bytes port 1
              (lambda (buf bv cur buffered)
                (bytevector-u8-ref bv cur))
              (lambda (buf bv cur buffered)
                (and (> buffered 0)
                     (bytevector-u8-ref bv cur)))))

(define* (%lookahead-u8 port)
  (define (fast-path buf bv cur buffered)
    (bytevector-u8-ref bv cur))
  (define (slow-path buf bv cur buffered)
    (if (zero? buffered)
        the-eof-object
        (fast-path buf bv cur buffered)))
  (peek-bytes port 1 fast-path slow-path))

(define* (%get-u8 port)
  (define (fast-path buf bv cur buffered)
    (set-port-buffer-cur! buf (1+ cur))
    (bytevector-u8-ref bv cur))
  (define (slow-path buf bv cur buffered)
    (if (zero? buffered)
        (begin
          (set-port-buffer-has-eof?! buf #f)
          the-eof-object)
        (fast-path buf bv cur buffered)))
  (peek-bytes port 1 fast-path slow-path))

(define (decoding-error subr port)
  ;; GNU/Linux definition; fixme?
  (define EILSEQ 84)
  (throw 'decoding-error subr "input decoding error" EILSEQ port))

(define-inlinable (decode-utf8 bv start avail u8_0 kt kf)
  (cond
   ((< u8_0 #x80)
    (kt (integer->char u8_0) 1))
   ((and (<= #xc2 u8_0 #xdf) (<= 2 avail))
    (let ((u8_1 (bytevector-u8-ref bv (1+ start))))
      (if (= (logand u8_1 #xc0) #x80)
          (kt (integer->char
               (logior (ash (logand u8_0 #x1f) 6)
                       (logand u8_1 #x3f)))
              2)
          (kf))))
   ((and (= (logand u8_0 #xf0) #xe0) (<= 3 avail))
    (let ((u8_1 (bytevector-u8-ref bv (+ start 1)))
          (u8_2 (bytevector-u8-ref bv (+ start 2))))
      (if (and (= (logand u8_1 #xc0) #x80)
               (= (logand u8_2 #xc0) #x80)
               (case u8_0
                 ((#xe0) (>= u8_1 #xa0))
                 ((#xed) (>= u8_1 #x9f))
                 (else #t)))
          (kt (integer->char
               (logior (ash (logand u8_0 #x0f) 12)
                       (ash (logand u8_1 #x3f) 6)
                       (logand u8_2 #x3f)))
              3)
          (kf))))
   ((and (<= #xf0 u8_0 #xf4) (<= 4 avail))
    (let ((u8_1 (bytevector-u8-ref bv (+ start 1)))
          (u8_2 (bytevector-u8-ref bv (+ start 2)))
          (u8_3 (bytevector-u8-ref bv (+ start 3))))
      (if (and (= (logand u8_1 #xc0) #x80)
               (= (logand u8_2 #xc0) #x80)
               (= (logand u8_3 #xc0) #x80)
               (case u8_0
                 ((#xf0) (>= u8_1 #x90))
                 ((#xf4) (>= u8_1 #x8f))
                 (else #t)))
          (kt (integer->char
               (logior (ash (logand u8_0 #x07) 18)
                       (ash (logand u8_1 #x3f) 12)
                       (ash (logand u8_2 #x3f) 6)
                       (logand u8_3 #x3f)))
              4)
          (kf))))
   (else (kf))))

(define (bad-utf8-len bv cur buffering first-byte)
  (define (ref n)
    (bytevector-u8-ref bv (+ cur n)))
  (cond
   ((< first-byte #x80) 0)
   ((<= #xc2 first-byte #xdf)
    (cond
     ((< buffering 2) 1)
     ((not (= (logand (ref 1) #xc0) #x80)) 1)
     (else 0)))
   ((= (logand first-byte #xf0) #xe0)
    (cond
     ((< buffering 2) 1)
     ((not (= (logand (ref 1) #xc0) #x80)) 1)
     ((and (eq? first-byte #xe0) (< (ref 1) #xa0)) 1)
     ((and (eq? first-byte #xed) (< (ref 1) #x9f)) 1)
     ((< buffering 3) 2)
     ((not (= (logand (ref 2) #xc0) #x80)) 2)
     (else 0)))
   ((<= #xf0 first-byte #xf4)
    (cond
     ((< buffering 2) 1)
     ((not (= (logand (ref 1) #xc0) #x80)) 1)
     ((and (eq? first-byte #xf0) (< (ref 1) #x90)) 1)
     ((and (eq? first-byte #xf4) (< (ref 1) #x8f)) 1)
     ((< buffering 3) 2)
     ((not (= (logand (ref 2) #xc0) #x80)) 2)
     ((< buffering 4) 3)
     ((not (= (logand (ref 3) #xc0) #x80)) 3)
     (else 0)))
   (else 1)))

(define (peek-char-and-len/utf8 port first-byte)
  (define (bad-utf8 len)
    (if (eq? (port-conversion-strategy port) 'substitute)
        (values #\? len)
        (decoding-error "peek-char" port)))
  (if (< first-byte #x80)
      (values (integer->char first-byte) 1)
      (call-with-values (lambda ()
                          (fill-input port
                                      (cond
                                       ((<= #xc2 first-byte #xdf) 2)
                                       ((= (logand first-byte #xf0) #xe0) 3)
                                       (else 4))))
        (lambda (buf buffering)
          (let* ((bv (port-buffer-bytevector buf))
                 (cur (port-buffer-cur buf)))
            (define (bad-utf8)
              (let ((len (bad-utf8-len bv cur buffering first-byte)))
                (when (zero? len) (error "internal error"))
                (if (eq? (port-conversion-strategy port) 'substitute)
                    (values #\? len)
                    (decoding-error "peek-char" port))))
            (decode-utf8 bv cur buffering first-byte values bad-utf8))))))

(define (peek-char-and-len/iso-8859-1 port first-byte)
  (values (integer->char first-byte) 1))

(define (peek-char-and-len/iconv port first-byte)
  (let lp ((prev-input-size 0))
    (let ((input-size (1+ prev-input-size)))
      (call-with-values (lambda () (fill-input port input-size))
        (lambda (buf buffered)
          (cond
           ((< buffered input-size)
            ;; Buffer failed to fill; EOF, possibly premature.
            (cond
             ((zero? prev-input-size)
              (values the-eof-object 0))
             ((eq? (port-conversion-strategy port) 'substitute)
              (values #\? prev-input-size))
             (else
              (decoding-error "peek-char" port))))
           ((port-decode-char port (port-buffer-bytevector buf)
                              (port-buffer-cur buf) input-size)
            => (lambda (char)
                 (values char input-size)))
           (else
            (lp input-size))))))))

(define (peek-char-and-len port)
  (let ((first-byte (peek-byte port)))
    (if (not first-byte)
        (values the-eof-object 0)
        (case (%port-encoding port)
          ((UTF-8)
           (peek-char-and-len/utf8 port first-byte))
          ((ISO-8859-1)
           (peek-char-and-len/iso-8859-1 port first-byte))
          (else
           (peek-char-and-len/iconv port first-byte))))))

(define* (%peek-char #:optional (port (current-input-port)))
  (define (slow-path)
    (call-with-values (lambda () (peek-char-and-len port))
      (lambda (char len)
        char)))
  (define (fast-path buf bv cur buffered)
    (let ((u8 (bytevector-u8-ref bv cur))
          (enc (%port-encoding port)))
      (case enc
        ((UTF-8) (decode-utf8 bv cur buffered u8 (lambda (char len) char)
                              slow-path))
        ((ISO-8859-1) (integer->char u8))
        (else (slow-path)))))
  (peek-bytes port 1 fast-path
              (lambda (buf bv cur buffered) (slow-path))))

(define* (%read-char #:optional (port (current-input-port)))
  (define (update-position! char)
    (case char
      ((#\alarm) #t) ; No change.
      ((#\backspace)
       (let ((col (port-column port)))
         (when (> col 0)
           (set-port-column! port (1- col)))))
      ((#\newline)
       (set-port-line! port (1+ (port-line port)))
       (set-port-column! port 0))
      ((#\return)
       (set-port-column! port 0))
      ((#\tab)
       (let ((col (port-column port)))
         (set-port-column! port (- (+ col 8) (remainder col 8)))))
      (else
       (set-port-column! port (1+ (port-column port)))))
    char)
  (define (slow-path)
    (call-with-values (lambda () (peek-char-and-len port))
      (lambda (char len)
        (let ((buf (port-read-buffer port)))
          (set-port-buffer-cur! buf (+ (port-buffer-cur buf) len))
          (if (eq? char the-eof-object)
              (begin
                (set-port-buffer-has-eof?! buf #f)
                char)
              (update-position! char))))))
  (define (fast-path buf bv cur buffered)
    (let ((u8 (bytevector-u8-ref bv cur))
          (enc (%port-encoding port)))
      (case enc
        ((UTF-8)
         (decode-utf8 bv cur buffered u8
                      (lambda (char len)
                        (set-port-buffer-cur! buf (+ cur len))
                        (update-position! char))
                      slow-path))
        ((ISO-8859-1)
         (set-port-buffer-cur! buf (+ cur 1))
         (update-position! (integer->char u8)))
        (else (slow-path)))))
  (peek-bytes port 1 fast-path
              (lambda (buf bv cur buffered) (slow-path))))



;;; Current ports as parameters.
;;;

(define current-input-port
  (fluid->parameter %current-input-port-fluid
                    (lambda (x)
                      (unless (input-port? x)
                        (error "expected an input port" x))
                      x)))

(define current-output-port
  (fluid->parameter %current-output-port-fluid
                    (lambda (x)
                      (unless (output-port? x)
                        (error "expected an output port" x))
                      x)))

(define current-error-port
  (fluid->parameter %current-error-port-fluid
                    (lambda (x)
                      (unless (output-port? x)
                        (error "expected an output port" x))
                      x)))

(define current-warning-port
  (fluid->parameter %current-warning-port-fluid
                    (lambda (x)
                      (unless (output-port? x)
                        (error "expected an output port" x))
                      x)))




;;; {File Descriptors and Ports}
;;;

(define file-position ftell)
(define* (file-set-position port offset #:optional (whence SEEK_SET))
  (seek port offset whence))

(define (move->fdes fd/port fd)
  (cond ((integer? fd/port)
         (dup->fdes fd/port fd)
         (close fd/port)
         fd)
        (else
         (primitive-move->fdes fd/port fd)
         (set-port-revealed! fd/port 1)
         fd/port)))

(define (release-port-handle port)
  (let ((revealed (port-revealed port)))
    (if (> revealed 0)
        (set-port-revealed! port (- revealed 1)))))

(define dup->port
  (case-lambda
    ((port/fd mode)
     (fdopen (dup->fdes port/fd) mode))
    ((port/fd mode new-fd)
     (let ((port (fdopen (dup->fdes port/fd new-fd) mode)))
       (set-port-revealed! port 1)
       port))))

(define dup->inport
  (case-lambda
    ((port/fd)
     (dup->port port/fd "r"))
    ((port/fd new-fd)
     (dup->port port/fd "r" new-fd))))

(define dup->outport
  (case-lambda
    ((port/fd)
     (dup->port port/fd "w"))
    ((port/fd new-fd)
     (dup->port port/fd "w" new-fd))))

(define dup
  (case-lambda
    ((port/fd)
     (if (integer? port/fd)
         (dup->fdes port/fd)
         (dup->port port/fd (port-mode port/fd))))
    ((port/fd new-fd)
     (if (integer? port/fd)
         (dup->fdes port/fd new-fd)
         (dup->port port/fd (port-mode port/fd) new-fd)))))

(define (duplicate-port port modes)
  (dup->port port modes))

(define (fdes->inport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
           (let ((result (fdopen fdes "r")))
             (set-port-revealed! result 1)
             result))
          ((input-port? (car rest-ports))
           (set-port-revealed! (car rest-ports)
                               (+ (port-revealed (car rest-ports)) 1))
           (car rest-ports))
          (else
           (loop (cdr rest-ports))))))

(define (fdes->outport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
           (let ((result (fdopen fdes "w")))
             (set-port-revealed! result 1)
             result))
          ((output-port? (car rest-ports))
           (set-port-revealed! (car rest-ports)
                               (+ (port-revealed (car rest-ports)) 1))
           (car rest-ports))
          (else
           (loop (cdr rest-ports))))))

(define (port->fdes port)
  (set-port-revealed! port (+ (port-revealed port) 1))
  (fileno port))

;; Legacy interfaces.

(define (set-current-input-port port)
  "Set the current default input port to @var{port}."
  (current-input-port port))

(define (set-current-output-port port)
  "Set the current default output port to @var{port}."
  (current-output-port port))

(define (set-current-error-port port)
  "Set the current default error port to @var{port}."
  (current-error-port port))


;;;; high level routines


;;; {High-Level Port Routines}
;;;

;; These are used to request the proper mode to open files in.
;;
(define OPEN_READ "r")
(define OPEN_WRITE "w")
(define OPEN_BOTH "r+")

(define *null-device* "/dev/null")

(define* (open-input-file
          file #:key (binary #f) (encoding #f) (guess-encoding #f))
  "Takes a string naming an existing file and returns an input port
capable of delivering characters from the file.  If the file
cannot be opened, an error is signalled."
  (open-file file (if binary "rb" "r")
             #:encoding encoding
             #:guess-encoding guess-encoding))

(define* (open-output-file file #:key (binary #f) (encoding #f))
  "Takes a string naming an output file to be created and returns an
output port capable of writing characters to a new file by that
name.  If the file cannot be opened, an error is signalled.  If a
file with the given name already exists, the effect is unspecified."
  (open-file file (if binary "wb" "w")
             #:encoding encoding))

(define (open-io-file str) 
  "Open file with name STR for both input and output."
  (open-file str OPEN_BOTH))

(define* (call-with-input-file
          file proc #:key (binary #f) (encoding #f) (guess-encoding #f))
  "PROC should be a procedure of one argument, and FILE should be a
string naming a file.  The file must
already exist. These procedures call PROC
with one argument: the port obtained by opening the named file for
input or output.  If the file cannot be opened, an error is
signalled.  If the procedure returns, then the port is closed
automatically and the values yielded by the procedure are returned.
If the procedure does not return, then the port will not be closed
automatically unless it is possible to prove that the port will
never again be used for a read or write operation."
  (let ((p (open-input-file file
                            #:binary binary
                            #:encoding encoding
                            #:guess-encoding guess-encoding)))
    (call-with-values
      (lambda () (proc p))
      (lambda vals
        (close-input-port p)
        (apply values vals)))))

(define* (call-with-output-file file proc #:key (binary #f) (encoding #f))
  "PROC should be a procedure of one argument, and FILE should be a
string naming a file.  The behaviour is unspecified if the file
already exists. These procedures call PROC
with one argument: the port obtained by opening the named file for
input or output.  If the file cannot be opened, an error is
signalled.  If the procedure returns, then the port is closed
automatically and the values yielded by the procedure are returned.
If the procedure does not return, then the port will not be closed
automatically unless it is possible to prove that the port will
never again be used for a read or write operation."
  (let ((p (open-output-file file #:binary binary #:encoding encoding)))
    (call-with-values
      (lambda () (proc p))
      (lambda vals
        (close-output-port p)
        (apply values vals)))))

(define (with-input-from-port port thunk)
  (parameterize ((current-input-port port))
    (thunk)))

(define (with-output-to-port port thunk)
  (parameterize ((current-output-port port))
    (thunk)))

(define (with-error-to-port port thunk)
  (parameterize ((current-error-port port))
    (thunk)))

(define* (with-input-from-file
          file thunk #:key (binary #f) (encoding #f) (guess-encoding #f))
  "THUNK must be a procedure of no arguments, and FILE must be a
string naming a file.  The file must already exist. The file is opened for
input, an input port connected to it is made
the default value returned by `current-input-port',
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-input-file file
   (lambda (p) (with-input-from-port p thunk))
   #:binary binary
   #:encoding encoding
   #:guess-encoding guess-encoding))

(define* (with-output-to-file file thunk #:key (binary #f) (encoding #f))
  "THUNK must be a procedure of no arguments, and FILE must be a
string naming a file.  The effect is unspecified if the file already exists.
The file is opened for output, an output port connected to it is made
the default value returned by `current-output-port',
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-output-file file
   (lambda (p) (with-output-to-port p thunk))
   #:binary binary
   #:encoding encoding))

(define* (with-error-to-file file thunk #:key (binary #f) (encoding #f))
  "THUNK must be a procedure of no arguments, and FILE must be a
string naming a file.  The effect is unspecified if the file already exists.
The file is opened for output, an output port connected to it is made
the default value returned by `current-error-port',
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-output-file file
   (lambda (p) (with-error-to-port p thunk))
   #:binary binary
   #:encoding encoding))

(define (call-with-input-string string proc)
  "Calls the one-argument procedure @var{proc} with a newly created
input port from which @var{string}'s contents may be read.  The value
yielded by the @var{proc} is returned."
  (proc (open-input-string string)))

(define (with-input-from-string string thunk)
  "THUNK must be a procedure of no arguments.
The test of STRING  is opened for
input, an input port connected to it is made, 
and the THUNK is called with no arguments.
When the THUNK returns, the port is closed.
Returns the values yielded by THUNK.  If an
escape procedure is used to escape from the continuation of these
procedures, their behavior is implementation dependent."
  (call-with-input-string string
   (lambda (p) (with-input-from-port p thunk))))

(define (call-with-output-string proc)
  "Calls the one-argument procedure @var{proc} with a newly created output
port.  When the function returns, the string composed of the characters
written into the port is returned."
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))

(define (with-output-to-string thunk)
  "Calls THUNK and returns its output as a string."
  (call-with-output-string
   (lambda (p) (with-output-to-port p thunk))))

(define (with-error-to-string thunk)
  "Calls THUNK and returns its error output as a string."
  (call-with-output-string
   (lambda (p) (with-error-to-port p thunk))))

(define (inherit-print-state old-port new-port)
  (if (get-print-state old-port)
      (port-with-print-state new-port (get-print-state old-port))
      new-port))
