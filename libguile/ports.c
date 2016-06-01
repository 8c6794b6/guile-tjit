/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2006,
 *   2007, 2008, 2009, 2010, 2011, 2012, 2013,
 *   2014, 2015 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



/* Headers.  */

#define _LARGEFILE64_SOURCE      /* ask for stat64 etc */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>  /* for chsize on mingw */
#include <assert.h>
#include <iconv.h>
#include <poll.h>
#include <uniconv.h>
#include <unistr.h>
#include <striconveh.h>

#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/deprecation.h"
#include "libguile/eval.h"
#include "libguile/fports.h"  /* direct access for seek and truncate */
#include "libguile/goops.h"
#include "libguile/smob.h"
#include "libguile/chars.h"
#include "libguile/dynwind.h"

#include "libguile/keywords.h"
#include "libguile/hashtab.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/mallocs.h"
#include "libguile/validate.h"
#include "libguile/ports.h"
#include "libguile/ports-internal.h"
#include "libguile/vectors.h"
#include "libguile/weak-set.h"
#include "libguile/fluids.h"
#include "libguile/eq.h"
#include "libguile/alist.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

#include <unistd.h>

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

/* Mingw (version 3.4.5, circa 2006) has ftruncate as an alias for chsize
   already, but have this code here in case that wasn't so in past versions,
   or perhaps to help other minimal DOS environments.

   gnulib ftruncate.c has code using fcntl F_CHSIZE and F_FREESP, which
   might be possibilities if we've got other systems without ftruncate.  */

#if defined HAVE_CHSIZE && ! defined HAVE_FTRUNCATE
#define ftruncate(fd, size) chsize (fd, size)
#undef HAVE_FTRUNCATE
#define HAVE_FTRUNCATE 1
#endif



/* We need these symbols early, before (ice-9 ports) loads in the
   snarfed definitions, so we can't use SCM_SYMBOL.  */
static SCM sym_UTF_8;
static SCM sym_ISO_8859_1;
static SCM sym_UTF_16;
static SCM sym_UTF_16LE;
static SCM sym_UTF_16BE;
static SCM sym_UTF_32;
static SCM sym_UTF_32LE;
static SCM sym_UTF_32BE;

/* Port conversion strategies.  */
static SCM sym_error;
static SCM sym_substitute;
static SCM sym_escape;

/* See scm_port_auxiliary_write_buffer and scm_c_write.  */
static const size_t AUXILIARY_WRITE_BUFFER_SIZE = 256;

/* Maximum number of bytes in a UTF-8 sequence.  */
static const size_t UTF8_BUFFER_SIZE = 4;

/* Maximum number of codepoints to write an escape sequence.  */
static const size_t ESCAPE_BUFFER_SIZE = 9;




/* We have to serialize operations on any given iconv descriptor.  */
static scm_i_pthread_mutex_t iconv_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;



/* See Unicode 8.0 section 5.22, "Best Practice for U+FFFD
   Substitution".  */
static const scm_t_wchar UNICODE_REPLACEMENT_CHARACTER = 0xFFFD;



static SCM trampoline_to_c_read_subr;
static SCM trampoline_to_c_write_subr;

static int
default_random_access_p (SCM port)
{
  return SCM_PORT_TYPE (port)->seek != NULL;
}

static int
default_read_wait_fd (SCM port)
{
  scm_misc_error ("read_wait_fd", "unimplemented", SCM_EOL);
}

static int
default_write_wait_fd (SCM port)
{
  scm_misc_error ("write_wait_fd", "unimplemented", SCM_EOL);
}

scm_t_port_type *
scm_make_port_type (char *name,
                    size_t (*read) (SCM port, SCM dst, size_t start,
                                    size_t count),
                    size_t (*write) (SCM port, SCM src, size_t start,
                                     size_t count))
{
  scm_t_port_type *desc;

  desc = scm_gc_malloc_pointerless (sizeof (*desc), "port-type");
  memset (desc, 0, sizeof (*desc));

  desc->name = name;
  desc->print = scm_port_print;
  desc->c_read = read;
  desc->c_write = write;
  desc->scm_read = read ? trampoline_to_c_read_subr : SCM_BOOL_F;
  desc->scm_write = write ? trampoline_to_c_write_subr : SCM_BOOL_F;
  desc->read_wait_fd = default_read_wait_fd;
  desc->write_wait_fd = default_write_wait_fd;
  desc->random_access_p = default_random_access_p;
  scm_make_port_classes (desc);

  return desc;
}

static SCM
trampoline_to_c_read (SCM port, SCM dst, SCM start, SCM count)
#define FUNC_NAME "port-read"
{
  size_t c_start, c_count, ret;

  SCM_VALIDATE_OPPORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, dst);
  c_start = scm_to_size_t (start);
  c_count = scm_to_size_t (count);
  SCM_ASSERT_RANGE (3, start, c_start <= SCM_BYTEVECTOR_LENGTH (dst));
  SCM_ASSERT_RANGE (4, count, c_count <= SCM_BYTEVECTOR_LENGTH (dst) - c_start);

  ret = SCM_PORT_TYPE (port)->c_read (port, dst, c_start, c_count);

  return ret == (size_t) -1 ? SCM_BOOL_F : scm_from_size_t (ret);
}
#undef FUNC_NAME

static size_t
trampoline_to_scm_read (SCM port, SCM dst, size_t start, size_t count)
{
  SCM ret = scm_call_4 (SCM_PORT_TYPE (port)->scm_read, port, dst,
                        scm_from_size_t (start), scm_from_size_t (count));
  return scm_is_true (ret) ? scm_to_size_t (ret) : (size_t) -1;
}

static SCM
trampoline_to_c_write (SCM port, SCM src, SCM start, SCM count)
#define FUNC_NAME "port-write"
{
  size_t c_start, c_count, ret;

  SCM_VALIDATE_OPPORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, src);
  c_start = scm_to_size_t (start);
  c_count = scm_to_size_t (count);
  SCM_ASSERT_RANGE (3, start, c_start <= SCM_BYTEVECTOR_LENGTH (src));
  SCM_ASSERT_RANGE (4, count, c_count <= SCM_BYTEVECTOR_LENGTH (src) - c_start);

  ret = SCM_PORT_TYPE (port)->c_write (port, src, c_start, c_count);

  return ret == (size_t) -1 ? SCM_BOOL_F : scm_from_size_t (ret);
}
#undef FUNC_NAME

static size_t
trampoline_to_scm_write (SCM port, SCM src, size_t start, size_t count)
{
  SCM ret = scm_call_4 (SCM_PORT_TYPE (port)->scm_write, port, src,
                        scm_from_size_t (start), scm_from_size_t (count));
  return scm_is_true (ret) ? scm_to_size_t (ret) : (size_t) -1;
}

void
scm_set_port_scm_read (scm_t_port_type *ptob, SCM read)
{
  ptob->scm_read = read;
  ptob->c_read = trampoline_to_scm_read;
}

void
scm_set_port_scm_write (scm_t_port_type *ptob, SCM write)
{
  ptob->scm_write = write;
  ptob->c_write = trampoline_to_scm_write;
}

void
scm_set_port_read_wait_fd (scm_t_port_type *ptob, int (*get_fd) (SCM))
{
  ptob->read_wait_fd = get_fd;
}

void
scm_set_port_write_wait_fd (scm_t_port_type *ptob, int (*get_fd) (SCM))
{
  ptob->write_wait_fd = get_fd;
}

void
scm_set_port_print (scm_t_port_type *ptob,
                    int (*print) (SCM exp, SCM port, scm_print_state *pstate))
{
  ptob->print = print;
}

void
scm_set_port_close (scm_t_port_type *ptob, void (*close) (SCM))
{
  ptob->close = close;
}

void
scm_set_port_needs_close_on_gc (scm_t_port_type *ptob, int needs_close_p)
{
  if (needs_close_p)
    ptob->flags |= SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC;
  else
    ptob->flags &= ~SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC;
}

void
scm_set_port_seek (scm_t_port_type *ptob,
                   scm_t_off (*seek) (SCM, scm_t_off, int))
{
  ptob->seek = seek;
}

void
scm_set_port_truncate (scm_t_port_type *ptob, void (*truncate) (SCM, scm_t_off))
{
  ptob->truncate = truncate;
}

void
scm_set_port_input_waiting (scm_t_port_type *ptob, int (*input_waiting) (SCM))
{
  ptob->input_waiting = input_waiting;
}

void
scm_set_port_random_access_p (scm_t_port_type *ptob,
                              int (*random_access_p) (SCM))
{
  ptob->random_access_p = random_access_p;
}

void
scm_set_port_get_natural_buffer_sizes
  (scm_t_port_type *ptob,
   void (*get_natural_buffer_sizes) (SCM, size_t *, size_t *))
{
  ptob->get_natural_buffer_sizes = get_natural_buffer_sizes;
}

static void
scm_i_clear_pending_eof (SCM port)
{
  scm_port_buffer_set_has_eof_p (SCM_PORT (port)->read_buf,
                                 SCM_BOOL_F);
}

SCM_DEFINE (scm_i_port_property, "%port-property", 2, 0, 0,
            (SCM port, SCM key),
            "Return the property of @var{port} associated with @var{key}.")
#define FUNC_NAME s_scm_i_port_property
{
  SCM_VALIDATE_OPPORT (1, port);

  return scm_assq_ref (SCM_PORT (port)->alist, key);
}
#undef FUNC_NAME

SCM_DEFINE (scm_i_set_port_property_x, "%set-port-property!", 3, 0, 0,
            (SCM port, SCM key, SCM value),
            "Set the property of @var{port} associated with @var{key} to @var{value}.")
#define FUNC_NAME s_scm_i_set_port_property_x
{
  scm_t_port *pt;

  SCM_VALIDATE_OPPORT (1, port);

  pt = SCM_PORT (port);
  pt->alist = scm_assq_set_x (pt->alist, key, value);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Standard ports --- current input, output, error, and more(!).  */

static SCM cur_inport_fluid = SCM_BOOL_F;
static SCM cur_outport_fluid = SCM_BOOL_F;
static SCM cur_errport_fluid = SCM_BOOL_F;
static SCM cur_warnport_fluid = SCM_BOOL_F;
static SCM cur_loadport_fluid = SCM_BOOL_F;

SCM_DEFINE (scm_current_input_port, "current-input-port", 0, 0, 0,
	    (void),
	    "Return the current input port.  This is the default port used\n"
	    "by many input procedures.  Initially, @code{current-input-port}\n"
	    "returns the @dfn{standard input} in Unix and C terminology.")
#define FUNC_NAME s_scm_current_input_port
{
  if (scm_is_true (cur_inport_fluid))
    return scm_fluid_ref (cur_inport_fluid);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_current_output_port, "current-output-port", 0, 0, 0,
	    (void),
            "Return the current output port.  This is the default port used\n"
	    "by many output procedures.  Initially,\n"
	    "@code{current-output-port} returns the @dfn{standard output} in\n"
	    "Unix and C terminology.")
#define FUNC_NAME s_scm_current_output_port
{
  if (scm_is_true (cur_outport_fluid))
    return scm_fluid_ref (cur_outport_fluid);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_current_error_port, "current-error-port", 0, 0, 0,
            (void),
	    "Return the port to which errors and warnings should be sent (the\n"
	    "@dfn{standard error} in Unix and C terminology).")
#define FUNC_NAME s_scm_current_error_port
{
  if (scm_is_true (cur_errport_fluid))
    return scm_fluid_ref (cur_errport_fluid);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_current_warning_port, "current-warning-port", 0, 0, 0,
            (void),
	    "Return the port to which diagnostic warnings should be sent.")
#define FUNC_NAME s_scm_current_warning_port
{
  if (scm_is_true (cur_warnport_fluid))
    return scm_fluid_ref (cur_warnport_fluid);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_current_load_port, "current-load-port", 0, 0, 0,
	    (),
	    "Return the current-load-port.\n"
            "The load port is used internally by @code{primitive-load}.")
#define FUNC_NAME s_scm_current_load_port
{
  return scm_fluid_ref (cur_loadport_fluid);
}
#undef FUNC_NAME

SCM
scm_set_current_input_port (SCM port)
#define FUNC_NAME "set-current-input-port"
{
  SCM oinp = scm_fluid_ref (cur_inport_fluid);
  SCM_VALIDATE_OPINPORT (1, port);
  scm_fluid_set_x (cur_inport_fluid, port);
  return oinp;
}
#undef FUNC_NAME

SCM
scm_set_current_output_port (SCM port)
#define FUNC_NAME "scm-set-current-output-port"
{
  SCM ooutp = scm_fluid_ref (cur_outport_fluid);
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);
  scm_fluid_set_x (cur_outport_fluid, port);
  return ooutp;
}
#undef FUNC_NAME

SCM
scm_set_current_error_port (SCM port)
#define FUNC_NAME "set-current-error-port"
{
  SCM oerrp = scm_fluid_ref (cur_errport_fluid);
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);
  scm_fluid_set_x (cur_errport_fluid, port);
  return oerrp;
}
#undef FUNC_NAME

SCM
scm_set_current_warning_port (SCM port)
#define FUNC_NAME "set-current-warning-port"
{
  SCM owarnp = scm_fluid_ref (cur_warnport_fluid);
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);
  scm_fluid_set_x (cur_warnport_fluid, port);
  return owarnp;
}
#undef FUNC_NAME

void
scm_dynwind_current_input_port (SCM port)
#define FUNC_NAME NULL
{
  SCM_VALIDATE_OPINPORT (1, port);
  scm_dynwind_fluid (cur_inport_fluid, port);
}
#undef FUNC_NAME

void
scm_dynwind_current_output_port (SCM port)
#define FUNC_NAME NULL
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);
  scm_dynwind_fluid (cur_outport_fluid, port);
}
#undef FUNC_NAME

void
scm_dynwind_current_error_port (SCM port)
#define FUNC_NAME NULL
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);
  scm_dynwind_fluid (cur_errport_fluid, port);
}
#undef FUNC_NAME

void
scm_i_dynwind_current_load_port (SCM port)
{
  scm_dynwind_fluid (cur_loadport_fluid, port);
}




/* Port buffers.  */

static SCM
make_port_buffer (SCM port, size_t size)
{
  SCM ret = scm_c_make_vector (SCM_PORT_BUFFER_FIELD_COUNT, SCM_INUM0);

  SCM_SIMPLE_VECTOR_SET (ret, SCM_PORT_BUFFER_FIELD_BYTEVECTOR,
                         scm_c_make_bytevector (size));
  SCM_SIMPLE_VECTOR_SET (ret, SCM_PORT_BUFFER_FIELD_POSITION,
                         SCM_PORT (port)->position);
  scm_port_buffer_set_has_eof_p (ret, SCM_BOOL_F);

  return ret;
}




/* Retrieving a port's mode.  */

/* Return the flags that characterize a port based on the mode
 * string used to open a file for that port.
 *
 * See PORT FLAGS in scm.h
 */

static long
scm_i_mode_bits_n (SCM modes)
{
  return ((scm_i_string_contains_char (modes, 'r')
	   || scm_i_string_contains_char (modes, '+') ? SCM_RDNG : 0)
	  | (scm_i_string_contains_char (modes, 'w')
	     || scm_i_string_contains_char (modes, 'a')
	     || scm_i_string_contains_char (modes, '+') ? SCM_WRTNG : 0)
	  | (scm_i_string_contains_char (modes, '0') ? SCM_BUF0 : 0)
	  | (scm_i_string_contains_char (modes, 'l') ? SCM_BUFLINE : 0));
}

long
scm_mode_bits (char *modes)
{
  /* Valid characters are rw+a0l.  So, use latin1.  */
  return scm_i_mode_bits (scm_from_latin1_string (modes));
}

long
scm_i_mode_bits (SCM modes)
{
  long bits;

  if (!scm_is_string (modes))
    scm_wrong_type_arg_msg (NULL, 0, modes, "string");

  bits = scm_i_mode_bits_n (modes);
  scm_remember_upto_here_1 (modes);
  return bits;
}

/* Return the mode flags from an open port.
 * Some modes such as "append" are only used when opening
 * a file and are not returned here.  */

SCM_DEFINE (scm_port_mode, "port-mode", 1, 0, 0,
           (SCM port),
	    "Return the port modes associated with the open port @var{port}.\n"
	    "These will not necessarily be identical to the modes used when\n"
	    "the port was opened, since modes such as \"append\" which are\n"
	    "used only during port creation are not retained.")
#define FUNC_NAME s_scm_port_mode
{
  char modes[4];
  modes[0] = '\0';

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPPORT (1, port);
  if (SCM_CELL_WORD_0 (port) & SCM_RDNG) {
    if (SCM_CELL_WORD_0 (port) & SCM_WRTNG)
      strcpy (modes, "r+");
    else
      strcpy (modes, "r");
  }
  else if (SCM_CELL_WORD_0 (port) & SCM_WRTNG)
    strcpy (modes, "w");
  if (SCM_CELL_WORD_0 (port) & SCM_BUF0)
    strcat (modes, "0");

  return scm_from_latin1_string (modes);
}
#undef FUNC_NAME



/* The port table --- a weak set of all ports.

   We need a global registry of ports to flush them all at exit, and to
   get all the ports matching a file descriptor.  */
SCM scm_i_port_weak_set;




/* Port finalization.  */

static SCM
do_close (void *data)
{
  return scm_close_port (SCM_PACK_POINTER (data));
}

/* Finalize the object (a port) pointed to by PTR.  */
static void
finalize_port (void *ptr, void *data)
{
  SCM port = SCM_PACK_POINTER (ptr);

  if (!SCM_PORTP (port))
    abort ();

  if (SCM_OPENP (port))
    {
      scm_internal_catch (SCM_BOOL_T, do_close, ptr,
                          scm_handle_by_message_noexit, NULL);
      scm_gc_ports_collected++;
    }
}




/* Default buffer size.  Used if the port type won't supply a value.  */
static const size_t default_buffer_size = 1024;

static void
initialize_port_buffers (SCM port)
{
  scm_t_port *pt = SCM_PORT (port);
  scm_t_port_type *ptob = SCM_PORT_TYPE (port);
  size_t read_buf_size, write_buf_size;

  if (SCM_CELL_WORD_0 (port) & SCM_BUF0)
    read_buf_size = write_buf_size = 1;
  else
    {
      read_buf_size = write_buf_size = default_buffer_size;
      if (ptob->get_natural_buffer_sizes)
        ptob->get_natural_buffer_sizes (port, &read_buf_size, &write_buf_size);
      if (read_buf_size == 0)
        read_buf_size = 1;
      if (write_buf_size == 0)
        write_buf_size = 1;
    }

  if (!SCM_INPUT_PORT_P (port))
    read_buf_size = 1;
  if (!SCM_OUTPUT_PORT_P (port))
    write_buf_size = 1;

  pt->read_buffering = read_buf_size;
  pt->read_buf = make_port_buffer (port, read_buf_size);
  pt->write_buf = make_port_buffer (port, write_buf_size);
  pt->write_buf_aux = SCM_BOOL_F;
}

SCM
scm_c_make_port_with_encoding (scm_t_port_type *ptob, unsigned long mode_bits,
                               SCM encoding, SCM conversion_strategy,
                               scm_t_bits stream)
{
  SCM ret;
  scm_t_port *pt;

  pt = scm_gc_typed_calloc (scm_t_port);

  ret = scm_words (scm_tc7_port | mode_bits | SCM_OPN, 4);
  SCM_SET_CELL_WORD_1 (ret, stream);
  SCM_SET_CELL_WORD_2 (ret, (scm_t_bits) pt);
  SCM_SET_CELL_WORD_3 (ret, (scm_t_bits) ptob);

  pt->encoding = encoding;
  pt->conversion_strategy = conversion_strategy;
  pt->file_name = SCM_BOOL_F;
  pt->position = scm_cons (SCM_INUM0, SCM_INUM0);

  pt->at_stream_start_for_bom_read  = 1;
  pt->at_stream_start_for_bom_write = 1;

  pt->precise_encoding = SCM_BOOL_F;
  pt->input_cd = (iconv_t) -1;
  pt->output_cd = (iconv_t) -1;

  pt->alist = SCM_EOL;

  if (SCM_PORT_TYPE (ret)->flags & SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC)
    {
      scm_i_set_finalizer (SCM2PTR (ret), finalize_port, NULL);
      scm_weak_set_add_x (scm_i_port_weak_set, ret);
    }

  initialize_port_buffers (ret);

  pt->rw_random = ptob->random_access_p (ret);

  return ret;
}

SCM
scm_c_make_port (scm_t_port_type *ptob,
                 unsigned long mode_bits, scm_t_bits stream)
{
  return scm_c_make_port_with_encoding (ptob, mode_bits,
                                        scm_i_default_port_encoding (),
                                        scm_i_default_port_conversion_strategy (),
                                        stream);
}



/* Predicates.  */

SCM_DEFINE (scm_port_p, "port?", 1, 0, 0,
	    (SCM x),
	    "Return a boolean indicating whether @var{x} is a port.\n"
	    "Equivalent to @code{(or (input-port? @var{x}) (output-port?\n"
	    "@var{x}))}.")
#define FUNC_NAME s_scm_port_p
{
  return scm_from_bool (SCM_PORTP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_input_port_p, "input-port?", 1, 0, 0,
           (SCM x),
	    "Return @code{#t} if @var{x} is an input port, otherwise return\n"
	    "@code{#f}.  Any object satisfying this predicate also satisfies\n"
	    "@code{port?}.")
#define FUNC_NAME s_scm_input_port_p
{
  return scm_from_bool (SCM_INPUT_PORT_P (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_output_port_p, "output-port?", 1, 0, 0,
           (SCM x),
	    "Return @code{#t} if @var{x} is an output port, otherwise return\n"
	    "@code{#f}.  Any object satisfying this predicate also satisfies\n"
	    "@code{port?}.")
#define FUNC_NAME s_scm_output_port_p
{
  x = SCM_COERCE_OUTPORT (x);
  return scm_from_bool (SCM_OUTPUT_PORT_P (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_closed_p, "port-closed?", 1, 0, 0,
           (SCM port),
	    "Return @code{#t} if @var{port} is closed or @code{#f} if it is\n"
	    "open.")
#define FUNC_NAME s_scm_port_closed_p
{
  SCM_VALIDATE_PORT (1, port);
  return scm_from_bool (!SCM_OPPORTP (port));
}
#undef FUNC_NAME

SCM_DEFINE (scm_eof_object_p, "eof-object?", 1, 0, 0,
           (SCM x),
	    "Return @code{#t} if @var{x} is an end-of-file object; otherwise\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_eof_object_p
{
  return scm_from_bool (SCM_EOF_OBJECT_P (x));
}
#undef FUNC_NAME




/* Closing ports.  */

SCM_DEFINE (scm_close_port, "close-port", 1, 0, 0,
           (SCM port),
	    "Close the specified port object.  Return @code{#t} if it\n"
	    "successfully closes a port or @code{#f} if it was already\n"
	    "closed.  An exception may be raised if an error occurs, for\n"
	    "example when flushing buffered output.  See also @ref{Ports and\n"
	    "File Descriptors, close}, for a procedure which can close file\n"
	    "descriptors.")
#define FUNC_NAME s_scm_close_port
{
  scm_t_port *pt;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_PORT (1, port);
  if (SCM_CLOSEDP (port))
    return SCM_BOOL_F;

  /* May throw an exception.  */
  if (SCM_OUTPUT_PORT_P (port))
    scm_flush (port);

  pt = SCM_PORT (port);
  SCM_CLR_PORT_OPEN_FLAG (port);

  if (SCM_PORT_TYPE (port)->flags & SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC)
    scm_weak_set_remove_x (scm_i_port_weak_set, port);

  if (SCM_PORT_TYPE (port)->close)
    /* Note!  This may throw an exception.  Anything after this point
       should be resilient to non-local exits.  */
    SCM_PORT_TYPE (port)->close (port);

  scm_i_pthread_mutex_lock (&iconv_lock);
  if (scm_is_true (pt->precise_encoding))
    {
      if (pt->input_cd != (iconv_t) -1)
        iconv_close (pt->input_cd);
      if (pt->output_cd != (iconv_t) -1)
        iconv_close (pt->output_cd);
      pt->precise_encoding = SCM_BOOL_F;
      pt->input_cd = pt->output_cd = (iconv_t) -1;
    }
  scm_i_pthread_mutex_unlock (&iconv_lock);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_close_input_port, "close-input-port", 1, 0, 0,
           (SCM port),
	    "Close the specified input port object.  The routine has no effect if\n"
	    "the file has already been closed.  An exception may be raised if an\n"
	    "error occurs.  The value returned is unspecified.\n\n"
	    "See also @ref{Ports and File Descriptors, close}, for a procedure\n"
	    "which can close file descriptors.")
#define FUNC_NAME s_scm_close_input_port
{
  SCM_VALIDATE_INPUT_PORT (1, port);
  scm_close_port (port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_close_output_port, "close-output-port", 1, 0, 0,
           (SCM port),
	    "Close the specified output port object.  The routine has no effect if\n"
	    "the file has already been closed.  An exception may be raised if an\n"
	    "error occurs.  The value returned is unspecified.\n\n"
	    "See also @ref{Ports and File Descriptors, close}, for a procedure\n"
	    "which can close file descriptors.")
#define FUNC_NAME s_scm_close_output_port
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OUTPUT_PORT (1, port);
  scm_close_port (port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* Encoding characters to byte streams, and decoding byte streams to
   characters.  */

/* Port encodings are case-insensitive ASCII strings.  */
static char
ascii_toupper (char c)
{
  return (c < 'a' || c > 'z') ? c : ('A' + (c - 'a'));
}

/* It is only necessary to use this function on encodings that come from
   the user and have not been canonicalized yet.  Encodings that are set
   on ports or in the default encoding fluid are in upper-case, and can
   be compared with strcmp.  */
static int
encoding_matches (const char *enc, SCM upper_symbol)
{
  const char *upper = scm_i_symbol_chars (upper_symbol);

  if (!enc)
    enc = "ISO-8859-1";

  while (*enc)
    if (ascii_toupper (*enc++) != *upper++)
      return 0;

  return !*upper;
}

static SCM
canonicalize_encoding (const char *enc)
{
  char *ret;
  int i;

  if (!enc || encoding_matches (enc, sym_ISO_8859_1))
    return sym_ISO_8859_1;
  if (encoding_matches (enc, sym_UTF_8))
    return sym_UTF_8;

  ret = scm_gc_strdup (enc, "port");

  for (i = 0; ret[i]; i++)
    {
      if (ret[i] > 127)
        /* Restrict to ASCII.  */
        scm_misc_error (NULL, "invalid character encoding ~s",
                        scm_list_1 (scm_from_latin1_string (enc)));
      else
        ret[i] = ascii_toupper (ret[i]);
    }

  return scm_from_latin1_symbol (ret);
}

/* A fluid specifying the default encoding for newly created ports.  If it is
   a string, that is the encoding.  If it is #f, it is in the "native"
   (Latin-1) encoding.  */
static SCM default_port_encoding_var;

/* Use ENCODING as the default encoding for future ports.  */
void
scm_i_set_default_port_encoding (const char *encoding)
{
  if (encoding_matches (encoding, sym_ISO_8859_1))
    scm_fluid_set_x (SCM_VARIABLE_REF (default_port_encoding_var), SCM_BOOL_F);
  else
    scm_fluid_set_x (SCM_VARIABLE_REF (default_port_encoding_var),
                     scm_symbol_to_string (canonicalize_encoding (encoding)));
}

/* Return the name of the default encoding for newly created ports.  */
SCM
scm_i_default_port_encoding (void)
{
  SCM encoding;

  encoding = scm_fluid_ref (SCM_VARIABLE_REF (default_port_encoding_var));
  if (!scm_is_string (encoding))
    return sym_ISO_8859_1;
  else
    return canonicalize_encoding (scm_i_string_chars (encoding));
}

/* A fluid specifying the default conversion handler for newly created
   ports.  Its value should be one of the symbols below.  */
static SCM default_conversion_strategy_var;

/* Return the default failed encoding conversion policy for new created
   ports.  */
SCM
scm_i_default_port_conversion_strategy (void)
{
  SCM value;

  value = scm_fluid_ref (SCM_VARIABLE_REF (default_conversion_strategy_var));

  if (scm_is_eq (sym_substitute, value) || scm_is_eq (sym_escape, value))
    return value;

  /* Default to 'error also when the fluid's value is not one of the
     valid symbols.  */
  return sym_error;
}

/* Use HANDLER as the default conversion strategy for future ports.  */
void
scm_i_set_default_port_conversion_strategy (SCM sym)
{
  if (!scm_is_eq (sym, sym_error)
      && !scm_is_eq (sym, sym_substitute)
      && !scm_is_eq (sym, sym_escape))
    /* Internal error.  */
    abort ();

  scm_fluid_set_x (SCM_VARIABLE_REF (default_conversion_strategy_var), sym);
}

static const unsigned char scm_utf8_bom[3]    = {0xEF, 0xBB, 0xBF};
static const unsigned char scm_utf16be_bom[2] = {0xFE, 0xFF};
static const unsigned char scm_utf16le_bom[2] = {0xFF, 0xFE};
static const unsigned char scm_utf32be_bom[4] = {0x00, 0x00, 0xFE, 0xFF};
static const unsigned char scm_utf32le_bom[4] = {0xFF, 0xFE, 0x00, 0x00};

/* Called with the iconv lock.  Will release the lock before throwing
   any error.  */
static void
prepare_iconv_descriptors (SCM port, SCM precise_encoding)
{
  scm_t_port *pt = SCM_PORT (port);
  iconv_t input_cd, output_cd;
  const char *encoding;
  size_t i;

  /* If the specified encoding is UTF-16 or UTF-32, then default to
     big-endian byte order.  This fallback isn't necessary if you read
     on the port before writing to it, as the read will sniff the BOM if
     any and specialize the encoding; see the manual.  */
  if (scm_is_eq (precise_encoding, sym_UTF_16))
    precise_encoding = sym_UTF_16BE;
  else if (scm_is_eq (precise_encoding, sym_UTF_32))
    precise_encoding = sym_UTF_32BE;

  if (scm_is_eq (pt->precise_encoding, precise_encoding))
    return;

  input_cd = output_cd = (iconv_t) -1;

  if (!scm_is_symbol (precise_encoding))
    goto invalid_encoding;

  encoding = scm_i_symbol_chars (precise_encoding);
  for (i = 0; encoding[i]; i++)
    if (encoding[i] > 127)
      goto invalid_encoding;

  /* Open a iconv conversion descriptors between ENCODING and UTF-8.  We
     choose UTF-8, not UTF-32, because iconv implementations can
     typically convert from anything to UTF-8, but not to UTF-32 (see
     http://lists.gnu.org/archive/html/bug-libunistring/2010-09/msg00007.html,
     for more details).  */

  if (SCM_INPUT_PORT_P (port))
    {
      input_cd = iconv_open ("UTF-8", encoding);
      if (input_cd == (iconv_t) -1)
        goto invalid_encoding;
    }

  if (SCM_OUTPUT_PORT_P (port))
    {
      output_cd = iconv_open (encoding, "UTF-8");
      if (output_cd == (iconv_t) -1)
        {
          if (input_cd != (iconv_t) -1)
            iconv_close (input_cd);
          goto invalid_encoding;
        }
    }

  if (pt->input_cd != (iconv_t) -1)
    iconv_close (pt->input_cd);
  if (pt->output_cd != (iconv_t) -1)
    iconv_close (pt->output_cd);

  pt->precise_encoding = precise_encoding;
  pt->input_cd = input_cd;
  pt->output_cd = output_cd;

  /* Make sure this port has a finalizer.  */
  scm_i_set_finalizer (SCM2PTR (port), finalize_port, NULL);

  return;

 invalid_encoding:
  scm_i_pthread_mutex_unlock (&iconv_lock);
  scm_misc_error ("open_iconv_descriptors",
                  "invalid or unknown character encoding ~s",
                  scm_list_1 (precise_encoding));
}

SCM_INTERNAL SCM scm_specialize_port_encoding_x (SCM port, SCM encoding);
SCM_DEFINE (scm_specialize_port_encoding_x,
            "specialize-port-encoding!", 2, 0, 0,
            (SCM port, SCM encoding),
            "")
#define FUNC_NAME s_scm_specialize_port_encoding_x
{
  SCM_VALIDATE_PORT (1, port);
  SCM_VALIDATE_SYMBOL (2, encoding);

  if (scm_is_eq (SCM_PORT (port)->encoding, sym_UTF_16))
    {
      if (!scm_is_eq (encoding, sym_UTF_16LE)
          && !scm_is_eq (encoding, sym_UTF_16BE))
        SCM_OUT_OF_RANGE (2, encoding);
    }
  else if (scm_is_eq (SCM_PORT (port)->encoding, sym_UTF_32))
    {
      if (!scm_is_eq (encoding, sym_UTF_32LE)
          && !scm_is_eq (encoding, sym_UTF_32BE))
        SCM_OUT_OF_RANGE (2, encoding);
    }
  else
    SCM_OUT_OF_RANGE (2, encoding);

  scm_i_pthread_mutex_lock (&iconv_lock);
  prepare_iconv_descriptors (port, encoding);
  scm_i_pthread_mutex_unlock (&iconv_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Acquire the iconv lock and fill in *INPUT_CD and/or *OUTPUT_CD.  */
void
scm_port_acquire_iconv_descriptors (SCM port, iconv_t *input_cd,
                                    iconv_t *output_cd)
{
  scm_t_port *pt = SCM_PORT (port);

  scm_i_pthread_mutex_lock (&iconv_lock);
  if (scm_is_false (pt->precise_encoding))
    prepare_iconv_descriptors (port, pt->encoding);
  if (input_cd)
    *input_cd = pt->input_cd;
  if (output_cd)
    *output_cd = pt->output_cd;
}

void
scm_port_release_iconv_descriptors (SCM port)
{
  scm_i_pthread_mutex_unlock (&iconv_lock);
}

/* The name of the encoding is itself encoded in ASCII.  */
void
scm_i_set_port_encoding_x (SCM port, const char *encoding)
{
  scm_t_port *pt = SCM_PORT (port);

  /* In order to handle cases where the encoding changes mid-stream
     (e.g. within an HTTP stream, or within a file that is composed of
     segments with different encodings), we consider this to be "stream
     start" for purposes of BOM handling, regardless of our actual file
     position. */
  pt->at_stream_start_for_bom_read  = 1;
  pt->at_stream_start_for_bom_write = 1;
  pt->encoding = canonicalize_encoding (encoding);

  scm_i_pthread_mutex_lock (&iconv_lock);
  if (pt->input_cd != (iconv_t) -1)
    iconv_close (pt->input_cd);
  if (pt->output_cd != (iconv_t) -1)
    iconv_close (pt->output_cd);
  pt->precise_encoding = SCM_BOOL_F;
  pt->input_cd = pt->output_cd = (iconv_t) -1;
  scm_i_pthread_mutex_unlock (&iconv_lock);
}

SCM_DEFINE (scm_sys_port_encoding, "%port-encoding", 1, 0, 0,
	    (SCM port),
	    "Returns, as a symbol, the character encoding that @var{port}\n"
	    "uses to interpret its input and output.\n")
#define FUNC_NAME s_scm_sys_port_encoding
{
  SCM_VALIDATE_PORT (1, port);

  return SCM_PORT (port)->encoding;
}
#undef FUNC_NAME

SCM
scm_port_encoding (SCM port)
{
  return scm_symbol_to_string (scm_sys_port_encoding (port));
}

SCM_DEFINE (scm_sys_set_port_encoding_x, "%set-port-encoding!", 2, 0, 0,
	    (SCM port, SCM enc),
	    "Sets the character encoding that will be used to interpret all\n"
	    "port I/O.  New ports are created with the encoding\n"
	    "appropriate for the current locale if @code{setlocale} has \n"
	    "been called or ISO-8859-1 otherwise\n"
	    "and this procedure can be used to modify that encoding.\n")
#define FUNC_NAME s_scm_sys_set_port_encoding_x
{
  SCM_VALIDATE_PORT (1, port);
  SCM_VALIDATE_SYMBOL (2, enc);

  scm_i_set_port_encoding_x (port, scm_i_symbol_chars (enc));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_set_port_encoding_x (SCM port, SCM enc)
{
  return scm_sys_set_port_encoding_x (port, scm_string_to_symbol (enc));
}

scm_t_string_failed_conversion_handler
scm_i_string_failed_conversion_handler (SCM conversion_strategy)
{
  if (scm_is_eq (conversion_strategy, sym_substitute))
    return SCM_FAILED_CONVERSION_QUESTION_MARK;
  if (scm_is_eq (conversion_strategy, sym_escape))
    return SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE;

  /* Default to error.  */
  return SCM_FAILED_CONVERSION_ERROR;
}

SCM_DEFINE (scm_port_conversion_strategy, "port-conversion-strategy",
	    1, 0, 0, (SCM port),
	    "Returns the behavior of the port when handling a character that\n"
	    "is not representable in the port's current encoding.\n"
	    "It returns the symbol @code{error} if unrepresentable characters\n"
	    "should cause exceptions, @code{substitute} if the port should\n"
	    "try to replace unrepresentable characters with question marks or\n"
	    "approximate characters, or @code{escape} if unrepresentable\n"
	    "characters should be converted to string escapes.\n"
	    "\n"
	    "If @var{port} is @code{#f}, then the current default behavior\n"
	    "will be returned.  New ports will have this default behavior\n"
	    "when they are created.\n")
#define FUNC_NAME s_scm_port_conversion_strategy
{
  if (scm_is_false (port))
    return scm_i_default_port_conversion_strategy ();

  SCM_VALIDATE_OPPORT (1, port);
  return SCM_PORT (port)->conversion_strategy;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_port_conversion_strategy_x, "set-port-conversion-strategy!",
	    2, 0, 0, 
	    (SCM port, SCM sym),
	    "Sets the behavior of the interpreter when outputting a character\n"
	    "that is not representable in the port's current encoding.\n"
	    "@var{sym} can be either @code{'error}, @code{'substitute}, or\n"
	    "@code{'escape}.  If it is @code{'error}, an error will be thrown\n"
	    "when an unconvertible character is encountered.  If it is\n"
	    "@code{'substitute}, then unconvertible characters will \n"
	    "be replaced with approximate characters, or with question marks\n"
	    "if no approximately correct character is available.\n"
	    "If it is @code{'escape},\n"
	    "it will appear as a hex escape when output.\n"
	    "\n"
	    "If @var{port} is an open port, the conversion error behavior\n"
	    "is set for that port.  If it is @code{#f}, it is set as the\n"
	    "default behavior for any future ports that get created in\n"
	    "this thread.\n")
#define FUNC_NAME s_scm_set_port_conversion_strategy_x
{
  if (!scm_is_eq (sym, sym_error)
      && !scm_is_eq (sym, sym_substitute)
      && !scm_is_eq (sym, sym_escape))
    SCM_MISC_ERROR ("unknown conversion strategy ~s", scm_list_1 (sym));

  if (scm_is_false (port))
    scm_i_set_default_port_conversion_strategy (sym);
  else
    {
      SCM_VALIDATE_OPPORT (1, port);
      SCM_PORT (port)->conversion_strategy = sym;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* Non-blocking I/O.  */

static int
port_read_wait_fd (SCM port)
{
  scm_t_port_type *ptob = SCM_PORT_TYPE (port);
  return ptob->read_wait_fd (port);
}

static int
port_write_wait_fd (SCM port)
{
  scm_t_port_type *ptob = SCM_PORT_TYPE (port);
  return ptob->write_wait_fd (port);
}

SCM_INTERNAL SCM scm_port_read_wait_fd (SCM);
SCM_DEFINE (scm_port_read_wait_fd, "port-read-wait-fd", 1, 0, 0,
            (SCM port), "")
#define FUNC_NAME s_scm_port_read_wait_fd
{
  int fd;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPINPORT (1, port);

  fd = port_read_wait_fd (port);
  return fd < 0 ? SCM_BOOL_F : scm_from_int (fd);
}
#undef FUNC_NAME

SCM_INTERNAL SCM scm_port_write_wait_fd (SCM);
SCM_DEFINE (scm_port_write_wait_fd, "port-write-wait-fd", 1, 0, 0,
            (SCM port), "")
#define FUNC_NAME s_scm_port_write_wait_fd
{
  int fd;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);

  fd = port_write_wait_fd (port);
  return fd < 0 ? SCM_BOOL_F : scm_from_int (fd);
}
#undef FUNC_NAME

static int
port_poll (SCM port, short events, int timeout)
#define FUNC_NAME "port-poll"
{
  struct pollfd pollfd[2];
  int nfds = 0, rv = 0;

  if (events & POLLIN)
    {
      pollfd[nfds].fd = port_read_wait_fd (port);
      pollfd[nfds].events = events & (POLLIN | POLLPRI);
      pollfd[nfds].revents = 0;
      nfds++;
    }
  if (events & POLLOUT)
    {
      pollfd[nfds].fd = port_write_wait_fd (port);
      pollfd[nfds].events = events & (POLLOUT | POLLPRI);
      pollfd[nfds].revents = 0;
      nfds++;
    }

  if (nfds == 2 && pollfd[0].fd == pollfd[1].fd)
    {
      pollfd[0].events |= pollfd[1].events;
      nfds--;
    }

  SCM_SYSCALL (rv = poll (pollfd, nfds, timeout));
  if (rv < 0)
    SCM_SYSERROR;

  return rv;
}
#undef FUNC_NAME

SCM_INTERNAL SCM scm_port_poll (SCM, SCM, SCM);
SCM_DEFINE (scm_port_poll, "port-poll", 2, 1, 0,
            (SCM port, SCM events, SCM timeout),
            "")
#define FUNC_NAME s_scm_port_poll
{
  short c_events = 0;
  int c_timeout;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_PORT (1, port);
  SCM_VALIDATE_STRING (2, events);
  c_timeout = SCM_UNBNDP (timeout) ? -1 : SCM_NUM2INT (3, timeout);

  if (scm_i_string_contains_char (events, 'r'))
    c_events |= POLLIN;
  if (scm_i_string_contains_char (events, '!'))
    c_events |= POLLPRI;
  if (scm_i_string_contains_char (events, 'w'))
    c_events |= POLLIN;

  return scm_from_int (port_poll (port, c_events, c_timeout));
}
#undef FUNC_NAME




/* Input.  */

static int
get_byte_or_eof (SCM port)
{
  SCM buf = SCM_PORT (port)->read_buf;
  SCM buf_bv, buf_cur, buf_end;
  size_t cur;

  buf_bv = scm_port_buffer_bytevector (buf);
  buf_cur = scm_port_buffer_cur (buf);
  buf_end = scm_port_buffer_end (buf);
  cur = SCM_I_INUM (buf_cur);

  if (SCM_LIKELY (SCM_I_INUMP (buf_cur))
      && SCM_LIKELY (SCM_I_INUMP (buf_end))
      && SCM_LIKELY (cur < SCM_I_INUM (buf_end))
      && SCM_LIKELY (cur < SCM_BYTEVECTOR_LENGTH (buf_bv)))
    {
      scm_t_uint8 ret = SCM_BYTEVECTOR_CONTENTS (buf_bv)[cur];
      scm_port_buffer_set_cur (buf, SCM_I_MAKINUM (cur + 1));
      return ret;
    }

  buf = scm_fill_input (port, 0);
  buf_bv = scm_port_buffer_bytevector (buf);
  buf_cur = scm_port_buffer_cur (buf);
  buf_end = scm_port_buffer_end (buf);
  cur = scm_to_size_t (buf_cur);
  if (cur < scm_to_size_t (buf_end))
    {
      scm_t_uint8 ret = SCM_BYTEVECTOR_CONTENTS (buf_bv)[cur];
      scm_port_buffer_set_cur (buf, SCM_I_MAKINUM (cur + 1));
      return ret;
    }

  /* The next peek or get should cause the read() function to be called
     to see if we still have EOF.  */
  scm_port_buffer_set_has_eof_p (buf, SCM_BOOL_F);
  return EOF;
}

/* Like `scm_get_byte_or_eof' but does not change PORT's `read_pos'.  */
static int
peek_byte_or_eof (SCM port)
{
  SCM buf = SCM_PORT (port)->read_buf;
  SCM buf_bv, buf_cur, buf_end;
  size_t cur;

  buf_bv = scm_port_buffer_bytevector (buf);
  buf_cur = scm_port_buffer_cur (buf);
  buf_end = scm_port_buffer_end (buf);
  cur = scm_to_size_t (buf_cur);
  if (SCM_LIKELY (SCM_I_INUMP (buf_cur))
      && SCM_LIKELY (SCM_I_INUMP (buf_end))
      && SCM_LIKELY (cur < SCM_I_INUM (buf_end))
      && SCM_LIKELY (cur < SCM_BYTEVECTOR_LENGTH (buf_bv)))
    {
      scm_t_uint8 ret = SCM_BYTEVECTOR_CONTENTS (buf_bv)[cur];
      return ret;
    }

  buf = scm_fill_input (port, 0);
  buf_bv = scm_port_buffer_bytevector (buf);
  buf_cur = scm_port_buffer_cur (buf);
  buf_end = scm_port_buffer_end (buf);
  cur = scm_to_size_t (buf_cur);
  if (cur < scm_to_size_t (buf_end))
    {
      scm_t_uint8 ret = SCM_BYTEVECTOR_CONTENTS (buf_bv)[cur];
      return ret;
    }

  return EOF;
}

int
scm_get_byte_or_eof (SCM port)
{
  return get_byte_or_eof (port);
}

int
scm_peek_byte_or_eof (SCM port)
{
  return peek_byte_or_eof (port);
}

static size_t
scm_i_read_bytes (SCM port, SCM dst, size_t start, size_t count)
{
  size_t filled;
  scm_t_port_type *ptob = SCM_PORT_TYPE (port);

  assert (count <= SCM_BYTEVECTOR_LENGTH (dst));
  assert (start + count <= SCM_BYTEVECTOR_LENGTH (dst));

 retry:
  filled = ptob->c_read (port, dst, start, count);

  if (filled == (size_t) -1)
    {
      port_poll (port, POLLIN, -1);
      goto retry;
    }

  assert (filled <= count);

  return filled;
}

/* In text mode, we will slurp a BOM from the beginning of a UTF-8,
   UTF-16, or UTF-32 stream, and write one at the beginning of a UTF-16
   or UTF-32 stream.  In binary mode, we won't.  The mode depends on the
   caller. */
enum bom_io_mode { BOM_IO_TEXT, BOM_IO_BINARY };
static size_t port_clear_stream_start_for_bom_read (SCM, enum bom_io_mode);

/* Used by an application to read arbitrary number of bytes from an SCM
   port.  Same semantics as libc read, except that scm_c_read_bytes only
   returns less than SIZE bytes if at end-of-file.

   Warning: Doesn't update port line and column counts!  */
size_t
scm_c_read_bytes (SCM port, SCM dst, size_t start, size_t count)
#define FUNC_NAME "scm_c_read_bytes"
{
  size_t to_read = count;
  scm_t_port *pt;
  SCM read_buf;
  scm_t_uint8 *dst_ptr = (scm_t_uint8 *) SCM_BYTEVECTOR_CONTENTS (dst) + start;

  SCM_VALIDATE_OPINPORT (1, port);

  pt = SCM_PORT (port);
  read_buf = pt->read_buf;

  if (pt->rw_random)
    scm_flush (port);

  port_clear_stream_start_for_bom_read (port, BOM_IO_BINARY);

  /* Take bytes first from the port's read buffer. */
  {
    size_t did_read = scm_port_buffer_take (read_buf, dst_ptr, to_read);
    dst_ptr += did_read;
    to_read -= did_read;
  }

  while (to_read)
    {
      size_t did_read;

      /* If the read is smaller than the buffering on the read side of
         this port, then go through the buffer.  Otherwise fill our
         buffer directly.  */
      if (to_read < pt->read_buffering)
        {
          read_buf = scm_fill_input (port, 0);
          did_read = scm_port_buffer_take (read_buf, dst_ptr, to_read);
          dst_ptr += did_read;
          to_read -= did_read;
          if (did_read == 0)
            {
              /* Consider that we've read off this EOF.  */
              scm_port_buffer_set_has_eof_p (read_buf, SCM_BOOL_F);
              break;
            }
        }
      else
        {
          did_read = scm_i_read_bytes (port, dst,
                                       start + count - to_read,
                                       to_read);
          to_read -= did_read;
          dst_ptr += did_read;
          if (did_read == 0)
            break;
        }
    }

  return count - to_read;
}
#undef FUNC_NAME

/* Like scm_c_read_bytes, but always proxies reads through the port's
   read buffer.  Used by an application when it wants to read into a
   memory chunk that's not owned by Guile's GC.  */
size_t
scm_c_read (SCM port, void *buffer, size_t size)
#define FUNC_NAME "scm_c_read"
{
  size_t copied = 0;
  scm_t_port *pt;
  SCM read_buf;
  scm_t_uint8 *dst = buffer;

  SCM_VALIDATE_OPINPORT (1, port);

  pt = SCM_PORT (port);
  read_buf = pt->read_buf;

  if (pt->rw_random)
    scm_flush (port);

  while (copied < size)
    {
      size_t count;
      read_buf = scm_fill_input (port, 0);
      count = scm_port_buffer_take (read_buf, dst + copied, size - copied);
      copied += count;
      if (count == 0)
        {
          /* Consider that we've read off this EOF.  */
          scm_port_buffer_set_has_eof_p (read_buf, SCM_BOOL_F);
          break;
        }
    }

  return copied;
}
#undef FUNC_NAME

/* Update the line and column number of PORT after consumption of C.  */
static inline void
update_port_position (SCM position, scm_t_wchar c)
{
  long line = scm_to_long (scm_port_position_line (position));
  int column = scm_to_int (scm_port_position_column (position));

  switch (c)
    {
    case '\a':
    case EOF:
      break;
    case '\b':
      if (column > 0)
        scm_port_position_set_column (position, scm_from_int (column - 1));
      break;
    case '\n':
      scm_port_position_set_line (position, scm_from_long (line + 1));
      scm_port_position_set_column (position, SCM_INUM0);
      break;
    case '\r':
      scm_port_position_set_column (position, SCM_INUM0);
      break;
    case '\t':
      scm_port_position_set_column (position,
                                    scm_from_int (column + 8 - column % 8));
      break;
    default:
      scm_port_position_set_column (position, scm_from_int (column + 1));
      break;
    }
}

/* Convert the SIZE-byte UTF-8 sequence in UTF8_BUF to a codepoint.
   UTF8_BUF is assumed to contain a valid UTF-8 sequence.  */
static scm_t_wchar
utf8_to_codepoint (const scm_t_uint8 *utf8_buf, size_t size)
{
  scm_t_wchar codepoint;

  if (utf8_buf[0] <= 0x7f)
    {
      assert (size >= 1);
      codepoint = utf8_buf[0];
    }
  else if ((utf8_buf[0] & 0xe0) == 0xc0)
    {
      assert (size >= 2);
      codepoint = ((scm_t_wchar) utf8_buf[0] & 0x1f) << 6UL
	| (utf8_buf[1] & 0x3f);
    }
  else if ((utf8_buf[0] & 0xf0) == 0xe0)
    {
      assert (size >= 3);
      codepoint = ((scm_t_wchar) utf8_buf[0] & 0x0f) << 12UL
	| ((scm_t_wchar) utf8_buf[1] & 0x3f) << 6UL
	| (utf8_buf[2] & 0x3f);
    }
  else
    {
      assert (size >= 4);
      codepoint = ((scm_t_wchar) utf8_buf[0] & 0x07) << 18UL
	| ((scm_t_wchar) utf8_buf[1] & 0x3f) << 12UL
	| ((scm_t_wchar) utf8_buf[2] & 0x3f) << 6UL
	| (utf8_buf[3] & 0x3f);
    }

  return codepoint;
}

/* Peek a UTF-8 sequence from PORT.  On success, return the codepoint
   that was read, and set *LEN to the length in bytes.  If there was a
   decoding error and the port conversion strategy was `substitute',
   then return #\? and set *LEN to the length of the shortest prefix
   that cannot begin a valid UTF-8 sequence.  Otherwise signal an
   error.  */
static scm_t_wchar
peek_utf8_codepoint (SCM port, size_t *len)
{
#define DECODING_ERROR(bytes) \
  do { *len = bytes; goto decoding_error; } while (0)
#define RETURN(bytes, codepoint) \
  do { *len = bytes; return codepoint; } while (0)

  int first_byte;

  first_byte = peek_byte_or_eof (port);
  if (first_byte == EOF)
    RETURN (0, EOF);
  else if (first_byte < 0x80)
    RETURN (1, first_byte);
  else if (first_byte >= 0xc2 && first_byte <= 0xdf)
    {
      SCM read_buf = scm_fill_input (port, 2);
      size_t can_take = scm_port_buffer_can_take (read_buf);
      const scm_t_uint8 *ptr = scm_port_buffer_take_pointer (read_buf);

      if (can_take < 2 || (ptr[1] & 0xc0) != 0x80)
        DECODING_ERROR (1);

      RETURN (2, (first_byte & 0x1f) << 6UL | (ptr[1] & 0x3f));
    }
  else if ((first_byte & 0xf0) == 0xe0)
    {
      SCM read_buf = scm_fill_input (port, 3);
      size_t can_take = scm_port_buffer_can_take (read_buf);
      const scm_t_uint8 *ptr = scm_port_buffer_take_pointer (read_buf);

      if (can_take < 2 || (ptr[1] & 0xc0) != 0x80
          || (ptr[0] == 0xe0 && ptr[1] < 0xa0)
          || (ptr[0] == 0xed && ptr[1] > 0x9f))
        DECODING_ERROR (1);

      if (can_take < 3 || (ptr[2] & 0xc0) != 0x80)
        DECODING_ERROR (2);

      RETURN (3,
              ((scm_t_wchar) ptr[0] & 0x0f) << 12UL
              | ((scm_t_wchar) ptr[1] & 0x3f) << 6UL
              | (ptr[2] & 0x3f));
    }
  else if (first_byte >= 0xf0 && first_byte <= 0xf4)
    {
      SCM read_buf = scm_fill_input (port, 4);
      size_t can_take = scm_port_buffer_can_take (read_buf);
      const scm_t_uint8 *ptr = scm_port_buffer_take_pointer (read_buf);

      if (can_take < 2 || (ptr[1] & 0xc0) != 0x80
          || (ptr[0] == 0xf0 && ptr[1] < 0x90)
          || (ptr[0] == 0xf4 && ptr[1] > 0x8f))
        DECODING_ERROR (1);

      if (can_take < 3 || (ptr[2] & 0xc0) != 0x80)
        DECODING_ERROR (2);

      if (can_take < 4 || (ptr[3] & 0xc0) != 0x80)
        DECODING_ERROR (3);

      RETURN (4,
              ((scm_t_wchar) ptr[0] & 0x07) << 18UL
              | ((scm_t_wchar) ptr[1] & 0x3f) << 12UL
              | ((scm_t_wchar) ptr[2] & 0x3f) << 6UL
              | (ptr[3] & 0x3f));
    }
  else
    DECODING_ERROR (1);

 decoding_error:
  if (scm_is_eq (SCM_PORT (port)->conversion_strategy, sym_substitute))
    /* *len already set.  */
    return UNICODE_REPLACEMENT_CHARACTER;

  scm_decoding_error ("peek-char", EILSEQ, "input decoding error", port);
  /* Not reached.  */
  return 0;
#undef DECODING_ERROR
#undef RETURN
}

/* Peek an ISO-8859-1 codepoint (a byte) from PORT.  On success, return
   the codepoint, and set *LEN to 1.  Otherwise on EOF set *LEN to 0.  */
static scm_t_wchar
peek_latin1_codepoint (SCM port, size_t *len)
{
  scm_t_wchar ret = peek_byte_or_eof (port);

  *len = ret == EOF ? 0 : 1;

  return ret;
}

SCM_INTERNAL SCM scm_port_decode_char (SCM, SCM, SCM, SCM);
SCM_DEFINE (scm_port_decode_char, "port-decode-char", 4, 0, 0,
            (SCM port, SCM bv, SCM start, SCM count),
            "")
#define FUNC_NAME s_scm_port_decode_char
{
  char *input, *output;
  scm_t_uint8 utf8_buf[UTF8_BUFFER_SIZE];
  iconv_t input_cd;
  size_t c_start, c_count;
  size_t input_left, output_left, done;

  SCM_VALIDATE_OPINPORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, bv);
  c_start = scm_to_size_t (start);
  c_count = scm_to_size_t (count);
  SCM_ASSERT_RANGE (3, start, c_start <= SCM_BYTEVECTOR_LENGTH (bv));
  SCM_ASSERT_RANGE (4, count, c_count <= SCM_BYTEVECTOR_LENGTH (bv) - c_start);

  input = (char *) SCM_BYTEVECTOR_CONTENTS (bv) + c_start;
  input_left = c_count;
  output = (char *) utf8_buf;
  output_left = sizeof (utf8_buf);

  /* FIXME: locking!  */
  scm_port_acquire_iconv_descriptors (port, &input_cd, NULL);
  done = iconv (input_cd, &input, &input_left, &output, &output_left);
  scm_port_release_iconv_descriptors (port);

  if (done == (size_t) -1)
    {
      int err = errno;
      if (err == EINVAL)
        /* The input byte sequence did not form a complete
           character.  Read another byte and try again. */
        return SCM_BOOL_F;
      else if (scm_is_eq (SCM_PORT (port)->conversion_strategy,
                          sym_substitute))
        return SCM_MAKE_CHAR (UNICODE_REPLACEMENT_CHARACTER);
      else
        scm_decoding_error ("decode-char", err, "input decoding error", port);
    }

  {
    size_t output_size = sizeof (utf8_buf) - output_left;
    if (output_size == 0)
      /* iconv consumed some bytes without producing any output.
         Most likely this means that a Unicode byte-order mark
         (BOM) was consumed.  In any case, keep going until we get
         output.  */
      return SCM_BOOL_F;

    return SCM_MAKE_CHAR (utf8_to_codepoint (utf8_buf, output_size));
  }
}
#undef FUNC_NAME

/* Peek a codepoint from PORT, decoding it through iconv.  On success,
   return the codepoint and set *LEN to the length in bytes.  If there
   was a decoding error and the port conversion strategy was
   `substitute', then return #\? and set *LEN to the length of the
   shortest prefix that cannot begin a valid UTF-8 sequence.  Otherwise
   signal an error.  */
static scm_t_wchar
peek_iconv_codepoint (SCM port, size_t *len)
{
  size_t input_size = 0;
  SCM maybe_char = SCM_BOOL_F;

  while (scm_is_false (maybe_char))
    {
      SCM read_buf = scm_fill_input (port, input_size + 1);

      if (scm_port_buffer_can_take (read_buf) <= input_size)
	{
          *len = input_size;
          if (input_size == 0)
            /* Normal EOF.  */
            {
              /* Make sure iconv descriptors have been opened even if
                 there were no bytes, to be sure that a decoding error
                 is signalled if the encoding itself was invalid.  */
              scm_port_acquire_iconv_descriptors (port, NULL, NULL);
              scm_port_release_iconv_descriptors (port);
              return EOF;
            }

          /* EOF found in the middle of a multibyte character. */
          if (scm_is_eq (SCM_PORT (port)->conversion_strategy,
                         sym_substitute))
            return UNICODE_REPLACEMENT_CHARACTER;

          scm_decoding_error ("peek-char", EILSEQ,
                              "input decoding error", port);
          /* Not reached.  */
          return 0;
	}

      input_size++;
      maybe_char = scm_port_decode_char (port,
                                         scm_port_buffer_bytevector (read_buf),
                                         scm_port_buffer_cur (read_buf),
                                         SCM_I_MAKINUM (input_size));
    }

  *len = input_size;
  return SCM_CHAR (maybe_char);
}

/* Peek a codepoint from PORT and return it in *CODEPOINT.  Set *LEN to
   the length in bytes of that representation.  Return 0 on success and
   an errno value on error.  */
static SCM_C_INLINE scm_t_wchar
peek_codepoint (SCM port, size_t *len)
{
  SCM encoding = SCM_PORT (port)->encoding;

  if (scm_is_eq (encoding, sym_UTF_8))
    return peek_utf8_codepoint (port, len);
  else if (scm_is_eq (encoding, sym_ISO_8859_1))
    return peek_latin1_codepoint (port, len);
  else
    return peek_iconv_codepoint (port, len);
}

/* Read a codepoint from PORT and return it.  */
scm_t_wchar
scm_getc (SCM port)
#define FUNC_NAME "scm_getc"
{
  size_t len = 0;
  scm_t_wchar codepoint;

  codepoint = peek_codepoint (port, &len);
  scm_port_buffer_did_take (SCM_PORT (port)->read_buf, len);
  if (codepoint == EOF)
    scm_i_clear_pending_eof (port);
  update_port_position (SCM_PORT (port)->position, codepoint);

  return codepoint;
}
#undef FUNC_NAME

SCM_DEFINE (scm_read_char, "read-char", 0, 1, 0,
           (SCM port),
	    "Return the next character available from @var{port}, updating\n"
	    "@var{port} to point to the following character.  If no more\n"
	    "characters are available, the end-of-file object is returned.\n"
	    "\n"
	    "When @var{port}'s data cannot be decoded according to its\n"
	    "character encoding, a @code{decoding-error} is raised and\n"
	    "@var{port} points past the erroneous byte sequence.\n")
#define FUNC_NAME s_scm_read_char
{
  scm_t_wchar c;
  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  return SCM_MAKE_CHAR (c);
}
#undef FUNC_NAME




/* Pushback.  */



void
scm_unget_bytes (const scm_t_uint8 *buf, size_t len, SCM port)
#define FUNC_NAME "scm_unget_bytes"
{
  scm_t_port *pt = SCM_PORT (port);
  SCM read_buf = pt->read_buf;

  if (pt->rw_random)
    scm_flush (port);

  if (scm_port_buffer_can_putback (read_buf) < len)
    {
      /* The bytes don't fit directly in the read_buf.  */
      size_t buffered, size;

      buffered = scm_port_buffer_can_take (read_buf);
      size = scm_port_buffer_size (read_buf);

      if (len <= size - buffered)
        {
          /* But they would fit if we shift the not-yet-read bytes from
             the read_buf right.  Let's do that.  */
          const scm_t_uint8 *to_shift = scm_port_buffer_take_pointer (read_buf);
          scm_port_buffer_reset_end (read_buf);
          scm_port_buffer_putback (read_buf, to_shift, buffered);
        }
      else
        {
          /* Bah, have to expand the read_buf for the putback.  */
          while (size < len + buffered)
            size *= 2;
          read_buf = scm_expand_port_read_buffer_x (port,
                                                    scm_from_size_t (size),
                                                    SCM_BOOL_T);
        }
    }

  scm_port_buffer_putback (read_buf, buf, len);
}
#undef FUNC_NAME

void
scm_unget_byte (int c, SCM port)
{
  unsigned char byte = c;
  scm_unget_bytes (&byte, 1, port);
}

void
scm_ungetc (scm_t_wchar c, SCM port)
#define FUNC_NAME "scm_ungetc"
{
  scm_t_port *pt = SCM_PORT (port);
  char *result;
  char result_buf[10];
  size_t len;

  len = sizeof (result_buf);

  if (scm_is_eq (pt->encoding, sym_UTF_8))
    {
      if (c < 0x80)
        {
          result_buf[0] = (char) c;
          result = result_buf;
          len = 1;
        }
      else
        result =
          (char *) u32_to_u8 ((uint32_t *) &c, 1, (uint8_t *) result_buf, &len);
    }
  else if (scm_is_eq (pt->encoding, sym_ISO_8859_1) && c <= 0xff)
    {
      result_buf[0] = (char) c;
      result = result_buf;
      len = 1;
    }
  else
    {
      scm_t_string_failed_conversion_handler handler =
        scm_i_string_failed_conversion_handler (pt->conversion_strategy);

      result = u32_conv_to_encoding (scm_i_symbol_chars (pt->encoding),
                                     (enum iconv_ilseq_handler) handler,
                                     (uint32_t *) &c, 1, NULL,
                                     result_buf, &len);
    }

  if (SCM_UNLIKELY (result == NULL || len == 0))
    scm_encoding_error (FUNC_NAME, errno,
			"conversion to port encoding failed",
			port, SCM_MAKE_CHAR (c));

  scm_unget_bytes ((unsigned char *) result, len, port);

  if (SCM_UNLIKELY (result != result_buf))
    free (result);

  {
    long line;
    int column;

    line = scm_to_long (scm_port_position_line (pt->position));
    column = scm_to_int (scm_port_position_column (pt->position));

    if (c == '\n')
      scm_port_position_set_line (pt->position, scm_from_long (line - 1));
    if (column > 0)
      scm_port_position_set_column (pt->position, scm_from_int (column - 1));
  }
}
#undef FUNC_NAME

void 
scm_ungets (const char *s, int n, SCM port)
{
  /* This is simple minded and inefficient, but unreading strings is
   * probably not a common operation, and remember that line and
   * column numbers have to be handled...
   *
   * Please feel free to write an optimized version!
   */
  while (n--)
    scm_ungetc (s[n], port);
}

SCM_DEFINE (scm_peek_char, "peek-char", 0, 1, 0,
           (SCM port),
	    "Return the next character available from @var{port},\n"
	    "@emph{without} updating @var{port} to point to the following\n"
	    "character.  If no more characters are available, the\n"
	    "end-of-file object is returned.\n"
	    "\n"
	    "The value returned by\n"
	    "a call to @code{peek-char} is the same as the value that would\n"
	    "have been returned by a call to @code{read-char} on the same\n"
	    "port.  The only difference is that the very next call to\n"
	    "@code{read-char} or @code{peek-char} on that @var{port} will\n"
	    "return the value returned by the preceding call to\n"
	    "@code{peek-char}.  In particular, a call to @code{peek-char} on\n"
	    "an interactive port will hang waiting for input whenever a call\n"
	    "to @code{read-char} would have hung.\n"
	    "\n"
	    "As for @code{read-char}, a @code{decoding-error} may be raised\n"
	    "if such a situation occurs.  However, unlike with @code{read-char},\n"
	    "@var{port} still points at the beginning of the erroneous byte\n"
	    "sequence when the error is raised.\n")
#define FUNC_NAME s_scm_peek_char
{
  scm_t_wchar c;
  size_t len = 0;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  c = peek_codepoint (port, &len);

  return c == EOF ? SCM_EOF_VAL : SCM_MAKE_CHAR (c);
}
#undef FUNC_NAME

SCM_DEFINE (scm_unread_char, "unread-char", 1, 1, 0,
            (SCM cobj, SCM port),
	    "Place character @var{cobj} in @var{port} so that it will be\n"
	    "read by the next read operation.  If called multiple times, the\n"
	    "unread characters will be read again in last-in first-out\n"
	    "order.  If @var{port} is not supplied, the current input port\n"
	    "is used.")
#define FUNC_NAME s_scm_unread_char
{
  int c;

  SCM_VALIDATE_CHAR (1, cobj);
  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (2, port);

  c = SCM_CHAR (cobj);

  scm_ungetc (c, port);
  return cobj;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unread_string, "unread-string", 2, 0, 0,
            (SCM str, SCM port),
	    "Place the string @var{str} in @var{port} so that its characters will be\n"
	    "read in subsequent read operations.  If called multiple times, the\n"
	    "unread characters will be read again in last-in first-out order.  If\n"
	    "@var{port} is not supplied, the current-input-port is used.")
#define FUNC_NAME s_scm_unread_string
{
  int n;
  SCM_VALIDATE_STRING (1, str);
  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (2, port);

  n = scm_i_string_length (str);

  while (n--)
    scm_ungetc (scm_i_string_ref (str, n), port);
  
  return str;
}
#undef FUNC_NAME




/* Manipulating the buffers.  */

SCM_SYMBOL (sym_none, "none");
SCM_SYMBOL (sym_line, "line");
SCM_SYMBOL (sym_block, "block");

SCM_DEFINE (scm_setvbuf, "setvbuf", 2, 1, 0,
            (SCM port, SCM mode, SCM size),
	    "Set the buffering mode for @var{port}.  @var{mode} can be one\n"
            "of the following symbols:\n"
	    "@table @code\n"
	    "@item none\n"
	    "no buffering\n"
	    "@item line\n"
	    "line buffering\n"
	    "@item block\n"
	    "block buffering, using a newly allocated buffer of @var{size} bytes.\n"
	    "If @var{size} is omitted, a default size will be used.\n"
	    "@end table\n\n"
	    "Only certain types of ports are supported, most importantly\n"
	    "file ports.")
#define FUNC_NAME s_scm_setvbuf
{
  long csize;
  scm_t_port *pt;
  scm_t_port_type *ptob;
  scm_t_bits tag_word;
  size_t read_buf_size, write_buf_size;
  SCM saved_read_buf;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPENPORT (1, port);
  pt = SCM_PORT (port);
  ptob = SCM_PORT_TYPE (port);
  tag_word = SCM_CELL_WORD_0 (port) & ~(SCM_BUF0 | SCM_BUFLINE);

  if (scm_is_eq (mode, sym_none))
    {
      tag_word |= SCM_BUF0;
      if (!SCM_UNBNDP (size) && !scm_is_eq (size, SCM_INUM0))
	scm_out_of_range (FUNC_NAME, size);
      csize = 0;
    }
  else if (scm_is_eq (mode, sym_line))
    {
      csize = SCM_UNBNDP (size) ? -1 : scm_to_int (size);
      tag_word |= SCM_BUFLINE;
    }
  else if (scm_is_eq (mode, sym_block))
    {
      csize = SCM_UNBNDP (size) ? -1 : scm_to_int (size);
    }
  else
    scm_out_of_range (FUNC_NAME, mode);

  if (!SCM_UNBNDP (size) && csize < 0)
    scm_out_of_range (FUNC_NAME, size);

  if (csize >= 0)
    read_buf_size = write_buf_size = csize;
  else
    {
      read_buf_size = write_buf_size = default_buffer_size;
      if (ptob->get_natural_buffer_sizes)
        ptob->get_natural_buffer_sizes (port, &read_buf_size, &write_buf_size);
    }

  /* Minimum buffer size is one byte.  */
  if (read_buf_size == 0)
    read_buf_size = 1;
  if (write_buf_size == 0)
    write_buf_size = 1;

  if (SCM_OUTPUT_PORT_P (port))
    scm_flush (port);

  saved_read_buf = pt->read_buf;

  SCM_SET_CELL_WORD_0 (port, tag_word);
  pt->read_buffering = read_buf_size;
  pt->read_buf = make_port_buffer (port, read_buf_size);
  pt->write_buf = make_port_buffer (port, write_buf_size);

  if (saved_read_buf)
    scm_unget_bytes (scm_port_buffer_take_pointer (saved_read_buf),
                     scm_port_buffer_can_take (saved_read_buf),
                     port);

  if (saved_read_buf)
    scm_port_buffer_set_has_eof_p (pt->read_buf,
                                   scm_port_buffer_has_eof_p (saved_read_buf));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Move up to READ_LEN bytes from PORT's read buffer into memory
   starting at DEST.  Return the number of bytes moved.  PORT's
   line/column numbers are left unchanged.  */
size_t
scm_take_from_input_buffers (SCM port, char *dest, size_t read_len)
{
  SCM read_buf = SCM_PORT (port)->read_buf;
  return scm_port_buffer_take (read_buf, (scm_t_uint8 *) dest, read_len);
}

/* Clear a port's read buffers, returning the contents.  */
SCM_DEFINE (scm_drain_input, "drain-input", 1, 0, 0, 
            (SCM port),
	    "This procedure clears a port's input buffers, similar\n"
	    "to the way that force-output clears the output buffer.  The\n"
	    "contents of the buffers are returned as a single string, e.g.,\n"
	    "\n"
	    "@lisp\n"
	    "(define p (open-input-file ...))\n"
	    "(drain-input p) => empty string, nothing buffered yet.\n"
	    "(unread-char (read-char p) p)\n"
	    "(drain-input p) => initial chars from p, up to the buffer size.\n"
	    "@end lisp\n\n"
	    "Draining the buffers may be useful for cleanly finishing\n"
	    "buffered I/O so that the file descriptor can be used directly\n"
	    "for further input.")
#define FUNC_NAME s_scm_drain_input
{
  SCM read_buf, result;
  long count;

  SCM_VALIDATE_OPINPORT (1, port);
  read_buf = SCM_PORT (port)->read_buf;
  count = scm_port_buffer_can_take (read_buf);

  if (count)
    {
      const scm_t_uint8 *ptr = scm_port_buffer_take_pointer (read_buf);
      result = scm_from_port_stringn ((const char *) ptr, count, port);
      scm_port_buffer_did_take (read_buf, count);
    }
  else
    result = scm_nullstr;
  
  return result;
}
#undef FUNC_NAME

void
scm_end_input (SCM port)
{
  SCM buf;
  size_t discarded;

  buf = SCM_PORT (port)->read_buf;
  discarded = scm_port_buffer_take (buf, NULL, (size_t) -1);

  if (discarded != 0)
    SCM_PORT_TYPE (port)->seek (port, -discarded, SEEK_CUR);
}

SCM_DEFINE (scm_force_output, "force-output", 0, 1, 0,
           (SCM port),
	    "Flush the specified output port, or the current output port if @var{port}\n"
	    "is omitted.  The current output buffer contents are passed to the\n"
	    "underlying port implementation (e.g., in the case of fports, the\n"
	    "data will be written to the file and the output buffer will be cleared.)\n"
	    "It has no effect on an unbuffered port.\n\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_force_output
{
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();
  else
    {
      port = SCM_COERCE_OUTPORT (port);
      SCM_VALIDATE_OPOUTPORT (1, port);
    }
  scm_flush (port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void scm_i_write (SCM port, SCM buf);

void
scm_flush (SCM port)
{
  SCM buf = SCM_PORT (port)->write_buf;
  if (scm_port_buffer_can_take (buf))
    scm_i_write (port, buf);
}

/* Return number of bytes consumed, or zero if no BOM was consumed.  */
static size_t
maybe_consume_bom (SCM port, const unsigned char *bom, size_t bom_len)
{
  SCM read_buf;
  const scm_t_uint8 *buf;

  if (peek_byte_or_eof (port) != bom[0])
    return 0;

  /* Make sure there's enough space in the buffer for a BOM.  Now that
     we matched the first byte, we know we're going to have to read this
     many bytes anyway.  */
  read_buf = scm_fill_input (port, bom_len);
  buf = scm_port_buffer_take_pointer (read_buf);

  if (scm_port_buffer_can_take (read_buf) < bom_len)
    return 0;

  if (memcmp (buf, bom, bom_len) != 0)
    return 0;

  scm_port_buffer_did_take (read_buf, bom_len);
  return bom_len;
}

static size_t
port_clear_stream_start_for_bom_read (SCM port, enum bom_io_mode io_mode)
{
  scm_t_port *pt = SCM_PORT (port);

  if (!pt->at_stream_start_for_bom_read)
    return 0;

  /* Maybe slurp off a byte-order marker.  */
  pt->at_stream_start_for_bom_read = 0;
  if (pt->rw_random)
    pt->at_stream_start_for_bom_write = 0;

  if (io_mode == BOM_IO_BINARY)
    return 0;

  if (scm_is_eq (pt->encoding, sym_UTF_8))
    return maybe_consume_bom (port, scm_utf8_bom, sizeof (scm_utf8_bom));

  if (scm_is_eq (pt->encoding, sym_UTF_16))
    {
      if (maybe_consume_bom (port, scm_utf16le_bom, sizeof (scm_utf16le_bom)))
        {
          scm_specialize_port_encoding_x (port, sym_UTF_16LE);
          return 2;
        }
      if (maybe_consume_bom (port, scm_utf16be_bom, sizeof (scm_utf16be_bom)))
        {
          scm_specialize_port_encoding_x (port, sym_UTF_16BE);
          return 2;
        }
      /* Big-endian by default.  */
      scm_specialize_port_encoding_x (port, sym_UTF_16BE);
      return 0;
    }

  if (scm_is_eq (pt->encoding, sym_UTF_32))
    {
      if (maybe_consume_bom (port, scm_utf32le_bom, sizeof (scm_utf32le_bom)))
        {
          /* Big-endian by default.  */
          scm_specialize_port_encoding_x (port, sym_UTF_32LE);
          return 4;
        }
      if (maybe_consume_bom (port, scm_utf32be_bom, sizeof (scm_utf32be_bom)))
        {
          scm_specialize_port_encoding_x (port, sym_UTF_32BE);
          return 4;
        }
      /* Big-endian by default.  */
      scm_specialize_port_encoding_x (port, sym_UTF_32BE);
      return 0;
    }

  return 0;
}

SCM_INTERNAL SCM scm_port_clear_stream_start_for_bom_read (SCM port);
SCM_DEFINE (scm_port_clear_stream_start_for_bom_read,
            "port-clear-stream-start-for-bom-read", 1, 0, 0,
            (SCM port),
            "")
#define FUNC_NAME s_scm_port_clear_stream_start_for_bom_read
{
  scm_t_port *pt;

  SCM_VALIDATE_PORT (1, port);

  pt = SCM_PORT (port);
  if (!pt->at_stream_start_for_bom_read)
    return SCM_BOOL_F;

  /* Maybe slurp off a byte-order marker.  */
  pt->at_stream_start_for_bom_read = 0;
  if (pt->rw_random)
    pt->at_stream_start_for_bom_write = 0;

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_INTERNAL SCM scm_port_clear_stream_start_for_bom_write (SCM, SCM);
SCM_DEFINE (scm_port_clear_stream_start_for_bom_write,
            "port-clear-stream-start-for-bom-write", 1, 1, 0,
            (SCM port, SCM buf),
            "")
#define FUNC_NAME s_scm_port_clear_stream_start_for_bom_write
{
  scm_t_port *pt;

  SCM_VALIDATE_PORT (1, port);

  pt = SCM_PORT (port);
  if (!pt->at_stream_start_for_bom_write)
    return SCM_INUM0;

  pt->at_stream_start_for_bom_write = 0;
  if (pt->rw_random)
    pt->at_stream_start_for_bom_read = 0;

  if (SCM_UNBNDP (buf))
    return SCM_INUM0;

  /* Write a BOM if appropriate.  */
  if (scm_is_eq (pt->encoding, sym_UTF_16))
    {
      SCM precise_encoding;
      size_t ret;

      scm_port_acquire_iconv_descriptors (port, NULL, NULL);
      precise_encoding = pt->precise_encoding;
      scm_port_release_iconv_descriptors (port);

      if (scm_is_eq (precise_encoding, sym_UTF_16LE))
        ret = scm_port_buffer_put (buf, scm_utf16le_bom,
                                   sizeof (scm_utf16le_bom));
      else
        ret = scm_port_buffer_put (buf, scm_utf16be_bom,
                                   sizeof (scm_utf16be_bom));

      return scm_from_size_t (ret);
    }
  else if (scm_is_eq (pt->encoding, sym_UTF_32))
    {
      SCM precise_encoding;
      size_t ret;

      scm_port_acquire_iconv_descriptors (port, NULL, NULL);
      precise_encoding = pt->precise_encoding;
      scm_port_release_iconv_descriptors (port);

      if (scm_is_eq (precise_encoding, sym_UTF_32LE))
        ret = scm_port_buffer_put (buf, scm_utf32le_bom,
                                   sizeof (scm_utf32le_bom));
      else
        ret = scm_port_buffer_put (buf, scm_utf32be_bom,
                                   sizeof (scm_utf32be_bom));

      return scm_from_size_t (ret);
    }

  return SCM_INUM0;
}
#undef FUNC_NAME

SCM
scm_fill_input (SCM port, size_t minimum_size)
{
  scm_t_port *pt = SCM_PORT (port);
  SCM read_buf;
  size_t buffered;

  if (minimum_size == 0)
    minimum_size = 1;

  /* The default is BOM_IO_TEXT.  Binary input procedures should
     port_clear_stream_start_for_bom_read with BOM_IO_BINARY before
     filling the input buffers.  */
  port_clear_stream_start_for_bom_read (port, BOM_IO_TEXT);
  read_buf = pt->read_buf;
  buffered = scm_port_buffer_can_take (read_buf);

  if (buffered >= minimum_size
      || scm_is_true (scm_port_buffer_has_eof_p (read_buf)))
    return read_buf;

  if (pt->rw_random)
    scm_flush (port);

  /* Prepare to read.  Make sure there is enough space in the buffer for
     minimum_size, and ensure that cur is zero so that we fill towards
     the end of the buffer.  */
  if (minimum_size > scm_port_buffer_size (read_buf))
    /* Grow the read buffer.  */
    read_buf = scm_expand_port_read_buffer_x (port,
                                              scm_from_size_t (minimum_size),
                                              SCM_BOOL_F);
  else if (buffered == 0)
    scm_port_buffer_reset (read_buf);
  else
    {
      const scm_t_uint8 *to_shift = scm_port_buffer_take_pointer (read_buf);
      scm_port_buffer_reset (read_buf);
      memmove (scm_port_buffer_put_pointer (read_buf), to_shift, buffered);
      scm_port_buffer_did_put (read_buf, buffered);
    }

  while (buffered < minimum_size
         && !scm_is_true (scm_port_buffer_has_eof_p (read_buf)))
    {
      size_t count;
      size_t buffering = pt->read_buffering;
      size_t to_read;

      if (pt->read_buffering < minimum_size)
        buffering = minimum_size;
      to_read = buffering - buffered;

      count = scm_i_read_bytes (port, scm_port_buffer_bytevector (read_buf),
                                buffered, to_read);
      buffered += count;
      scm_port_buffer_did_put (read_buf, count);
      scm_port_buffer_set_has_eof_p (read_buf, scm_from_bool (count == 0));
    }

  return read_buf;
}

SCM_DEFINE (scm_port_random_access_p, "port-random-access?", 1, 0, 0,
            (SCM port),
	    "Return true if the port is random-access, or false otherwise.")
#define FUNC_NAME s_scm_port_random_access_p
{
  SCM_VALIDATE_OPPORT (1, port);
  return scm_from_bool (SCM_PORT (port)->rw_random);
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_read_buffering, "port-read-buffering", 1, 0, 0,
            (SCM port),
	    "Return the amount of read buffering on a port, in bytes.")
#define FUNC_NAME s_scm_port_read_buffering
{
  SCM_VALIDATE_OPINPORT (1, port);
  return scm_from_size_t (SCM_PORT (port)->read_buffering);
}
#undef FUNC_NAME

SCM_DEFINE (scm_expand_port_read_buffer_x, "expand-port-read-buffer!", 2, 1, 0,
            (SCM port, SCM size, SCM putback_p),
	    "Expand the read buffer of @var{port} to @var{size}.  Copy the\n"
            "old buffered data, if, any, to the beginning of the new\n"
            "buffer, unless @var{putback_p} is true, in which case copy it\n"
            "to the end instead.  Return the new buffer.")
#define FUNC_NAME s_scm_expand_port_read_buffer_x
{
  scm_t_port *pt;
  size_t c_size;
  SCM new_buf;

  SCM_VALIDATE_OPINPORT (1, port);
  pt = SCM_PORT (port);
  c_size = scm_to_size_t (size);
  SCM_ASSERT_RANGE (2, size, c_size > scm_port_buffer_size (pt->read_buf));
  if (SCM_UNBNDP (putback_p))
    putback_p = SCM_BOOL_F;

  new_buf = make_port_buffer (port, c_size);
  scm_port_buffer_set_has_eof_p (new_buf,
                                 scm_port_buffer_has_eof_p (pt->read_buf));
  if (scm_is_true (putback_p))
    {
      scm_port_buffer_reset_end (new_buf);
      scm_port_buffer_putback (new_buf,
                               scm_port_buffer_take_pointer (pt->read_buf),
                               scm_port_buffer_can_take (pt->read_buf));
    }
  else
    {
      scm_port_buffer_reset (new_buf);
      scm_port_buffer_put (new_buf,
                           scm_port_buffer_take_pointer (pt->read_buf),
                           scm_port_buffer_can_take (pt->read_buf));
    }
  pt->read_buf = new_buf;

  return new_buf;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_read, "port-read", 1, 0, 0, (SCM port),
	    "Return the read function for an input port.")
#define FUNC_NAME s_scm_port_read
{
  SCM_VALIDATE_OPINPORT (1, port);
  return SCM_PORT_TYPE (port)->scm_read;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_write, "port-write", 1, 0, 0,
            (SCM port),
	    "Return the write function for an output port.")
#define FUNC_NAME s_scm_port_write
{
  SCM_VALIDATE_OPOUTPORT (1, port);
  return SCM_PORT_TYPE (port)->scm_write;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_read_buffer, "port-read-buffer", 1, 0, 0,
            (SCM port),
	    "Return the read buffer for a port.")
#define FUNC_NAME s_scm_port_read_buffer
{
  SCM_VALIDATE_OPPORT (1, port);
  return SCM_PORT (port)->read_buf;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_write_buffer, "port-write-buffer", 1, 0, 0,
            (SCM port),
	    "Return the write buffer for a port.")
#define FUNC_NAME s_scm_port_write_buffer
{
  SCM_VALIDATE_OPPORT (1, port);
  return SCM_PORT (port)->write_buf;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_auxiliary_write_buffer, "port-auxiliary-write-buffer",
            1, 0, 0, (SCM port),
	    "Return the auxiliary write buffer for a port.")
#define FUNC_NAME s_scm_port_auxiliary_write_buffer
{
  scm_t_port *pt;

  SCM_VALIDATE_OPPORT (1, port);

  pt = SCM_PORT (port);
  if (scm_is_false (pt->write_buf_aux))
    pt->write_buf_aux = make_port_buffer (port, AUXILIARY_WRITE_BUFFER_SIZE);

  return pt->write_buf_aux;
}
#undef FUNC_NAME

SCM_INTERNAL SCM scm_port_line_buffered_p (SCM);
SCM_DEFINE (scm_port_line_buffered_p, "port-line-buffered?", 1, 0, 0,
            (SCM port),
	    "Return true if the port is line buffered.")
#define FUNC_NAME s_scm_port_line_buffered_p
{
  SCM_VALIDATE_OPPORT (1, port);
  return scm_from_bool (SCM_CELL_WORD_0 (port) & SCM_BUFLINE);
}
#undef FUNC_NAME




/* Output.  */

static void
scm_i_write_bytes (SCM port, SCM src, size_t start, size_t count)
{
  size_t written = 0;
  scm_t_port_type *ptob = SCM_PORT_TYPE (port);

  assert (count <= SCM_BYTEVECTOR_LENGTH (src));
  assert (start + count <= SCM_BYTEVECTOR_LENGTH (src));

  do
    {
      size_t ret = ptob->c_write (port, src, start + written, count - written);

      if (ret == (size_t) -1)
        port_poll (port, POLLOUT, -1);
      else
        written += ret;
    }
  while (written < count);

  assert (written == count);
}

static void
scm_i_write (SCM port, SCM buf)
{
  size_t start, count;

  scm_port_clear_stream_start_for_bom_write (port, SCM_UNDEFINED);

  /* Update cursors before attempting to write, assuming that I/O errors
     are sticky.  That way if the write throws an error, causing the
     computation to abort, and possibly causing the port to be collected
     by GC when it's open, any subsequent close-port / force-output
     won't signal *another* error.  */

  start = scm_to_size_t (scm_port_buffer_cur (buf));
  count = scm_port_buffer_can_take (buf);
  scm_port_buffer_reset (buf);
  scm_i_write_bytes (port, scm_port_buffer_bytevector (buf), start,
                     count);
}

/* Used by an application to write arbitrary number of bytes to an SCM
   port.  Similar semantics as libc write.  However, unlike libc write,
   scm_c_write writes the requested number of bytes.

   Warning: Doesn't update port line and column counts!  */
void
scm_c_write_bytes (SCM port, SCM src, size_t start, size_t count)
#define FUNC_NAME "scm_c_write_bytes"
{
  scm_t_port *pt;
  SCM write_buf;

  SCM_VALIDATE_OPOUTPORT (1, port);

  pt = SCM_PORT (port);
  write_buf = pt->write_buf;

  if (pt->rw_random)
    scm_end_input (port);

  if (count < scm_port_buffer_size (write_buf))
    {
      /* Make it so that the write_buf "end" cursor is only nonzero if
         there are buffered bytes already.  */
      if (scm_port_buffer_can_take (write_buf) == 0)
        scm_port_buffer_reset (write_buf);

      /* We buffer writes that are smaller in size than the write
         buffer.  If the buffer is too full to hold the new data, we
         flush it beforehand.  Otherwise it could be that the buffer is
         full after filling it with the new data; if that's the case, we
         flush then instead.  */
      if (scm_port_buffer_can_put (write_buf) < count)
        scm_i_write (port, write_buf);

      {
        signed char *src_ptr = SCM_BYTEVECTOR_CONTENTS (src) + start;
        scm_port_buffer_put (write_buf, (scm_t_uint8 *) src_ptr, count);
      }

      if (scm_port_buffer_can_put (write_buf) == 0)
        scm_i_write (port, write_buf);
    }
  else
    {
      /* Our write would overflow the buffer.  Flush buffered bytes (if
         needed), then write our bytes with just one syscall.  */
      if (scm_port_buffer_can_take (write_buf))
        scm_i_write (port, write_buf);

      scm_i_write_bytes (port, src, start, count);
    }
}
#undef FUNC_NAME

/* Like scm_c_write_bytes, but always writes through the write buffer.
   Used when an application wants to write bytes stored in an area not
   managed by GC.  */
void
scm_c_write (SCM port, const void *ptr, size_t size)
#define FUNC_NAME "scm_c_write"
{
  scm_t_port *pt;
  SCM write_buf;
  size_t written = 0;
  int using_aux_buffer = 0;
  const scm_t_uint8 *src = ptr;

  SCM_VALIDATE_OPOUTPORT (1, port);

  pt = SCM_PORT (port);

  if (pt->rw_random)
    scm_end_input (port);

  /* Imagine we are writing 40 bytes on an unbuffered port.  If we were
     writing from a bytevector we could pass that write directly to the
     port.  But since we aren't, we need to go through a bytevector, and
     if we went through the port buffer we'd have to make 40 individual
     calls to the write function.  That would be terrible.  Really we
     need an intermediate bytevector.  But, we shouldn't use a trick
     analogous to what we do with expand-port-read-buffer!, because the
     way we use the cur and end cursors doesn't seem to facilitate that.
     So instead we buffer through an auxiliary write buffer if needed.
     To avoid re-allocating this buffer all the time, we store it on the
     port.  It should never be left with buffered data.

     Use of an auxiliary write buffer is triggered if the buffer is
     smaller than the size we would make for an auxiliary write buffer,
     and the write is bigger than the buffer.  */
  write_buf = pt->write_buf;
  if (scm_port_buffer_size (write_buf) < size &&
      scm_port_buffer_size (write_buf) < AUXILIARY_WRITE_BUFFER_SIZE)
    {
      using_aux_buffer = 1;
      write_buf = scm_port_auxiliary_write_buffer (port);
    }

  while (written < size)
    {
      size_t did_put = scm_port_buffer_put (write_buf, src, size - written);
      written += did_put;
      src += did_put;
      if (using_aux_buffer || scm_port_buffer_can_put (write_buf) == 0)
        scm_i_write (port, write_buf);
    }
}
#undef FUNC_NAME

/* The encoded escape sequence will be written to BUF, and will be valid
   ASCII (so also valid ISO-8859-1 and UTF-8).  Return the number of
   bytes written.  */
static size_t
encode_escape_sequence (scm_t_wchar ch, scm_t_uint8 buf[ESCAPE_BUFFER_SIZE])
{
  /* Represent CH using the in-string escape syntax.  */
  static const char hex[] = "0123456789abcdef";
  static const char escapes[7] = "abtnvfr";
  size_t i = 0;

  buf[i++] = '\\';

  if (ch >= 0x07 && ch <= 0x0D && ch != 0x0A)
    /* Use special escapes for some C0 controls.  */
    buf[i++] = escapes[ch - 0x07];
  else if (!SCM_R6RS_ESCAPES_P)
    {
      if (ch <= 0xFF)
        {
          buf[i++] = 'x';
          buf[i++] = hex[ch / 16];
          buf[i++] = hex[ch % 16];
        }
      else if (ch <= 0xFFFF)
        {
          buf[i++] = 'u';
          buf[i++] = hex[(ch & 0xF000) >> 12];
          buf[i++] = hex[(ch & 0xF00) >> 8];
          buf[i++] = hex[(ch & 0xF0) >> 4];
          buf[i++] = hex[(ch & 0xF)];
        }
      else if (ch > 0xFFFF)
        {
          buf[i++] = 'U';
          buf[i++] = hex[(ch & 0xF00000) >> 20];
          buf[i++] = hex[(ch & 0xF0000) >> 16];
          buf[i++] = hex[(ch & 0xF000) >> 12];
          buf[i++] = hex[(ch & 0xF00) >> 8];
          buf[i++] = hex[(ch & 0xF0) >> 4];
          buf[i++] = hex[(ch & 0xF)];
        }
    }
  else
    {
      buf[i++] = 'x';
      if (ch > 0xfffff) buf[i++] = hex[(ch >> 20) & 0xf];
      if (ch > 0x0ffff) buf[i++] = hex[(ch >> 16) & 0xf];
      if (ch > 0x00fff) buf[i++] = hex[(ch >> 12) & 0xf];
      if (ch > 0x000ff) buf[i++] = hex[(ch >> 8) & 0xf];
      if (ch > 0x0000f) buf[i++] = hex[(ch >> 4) & 0xf];
      buf[i++] = hex[ch & 0xf];
      buf[i++] = ';';
    }

  return i;
}

void
scm_c_put_escaped_char (SCM port, scm_t_wchar ch)
{
  scm_t_uint8 escape[ESCAPE_BUFFER_SIZE];
  size_t len = encode_escape_sequence (ch, escape);
  scm_c_put_latin1_chars (port, escape, len);
}

/* Convert CODEPOINT to UTF-8 and store the result in UTF8.  Return the
   number of bytes of the UTF-8-encoded string.  */
static size_t
codepoint_to_utf8 (scm_t_uint32 codepoint, scm_t_uint8 utf8[UTF8_BUFFER_SIZE])
{
  size_t len;

  if (codepoint <= 0x7f)
    {
      len = 1;
      utf8[0] = codepoint;
    }
  else if (codepoint <= 0x7ffUL)
    {
      len = 2;
      utf8[0] = 0xc0 | (codepoint >> 6);
      utf8[1] = 0x80 | (codepoint & 0x3f);
    }
  else if (codepoint <= 0xffffUL)
    {
      len = 3;
      utf8[0] = 0xe0 | (codepoint >> 12);
      utf8[1] = 0x80 | ((codepoint >> 6) & 0x3f);
      utf8[2] = 0x80 | (codepoint & 0x3f);
    }
  else
    {
      len = 4;
      utf8[0] = 0xf0 | (codepoint >> 18);
      utf8[1] = 0x80 | ((codepoint >> 12) & 0x3f);
      utf8[2] = 0x80 | ((codepoint >> 6) & 0x3f);
      utf8[3] = 0x80 | (codepoint & 0x3f);
    }

  return len;
}

static size_t
try_encode_char_to_iconv_buf (SCM port, SCM buf, scm_t_uint32 ch)
{
  scm_t_uint8 utf8[UTF8_BUFFER_SIZE];
  size_t utf8_len = codepoint_to_utf8 (ch, utf8);
  scm_t_uint8 *aux = scm_port_buffer_put_pointer (buf);
  size_t can_put = scm_port_buffer_can_put (buf);
  iconv_t output_cd;
  int saved_errno;

  char *input = (char *) utf8;
  size_t input_left = utf8_len;
  char *output = (char *) aux;
  size_t output_left = can_put;
  size_t res;

  scm_port_acquire_iconv_descriptors (port, NULL, &output_cd);
  res = iconv (output_cd, &input, &input_left, &output, &output_left);
  saved_errno = errno;
  /* Emit bytes needed to get back to initial state, if needed.  */
  iconv (output_cd, NULL, NULL, &output, &output_left);
  scm_port_release_iconv_descriptors (port);

  if (res != (size_t) -1)
    {
      /* Success.  */
      scm_port_buffer_did_put (buf, can_put - output_left);
      return 1;
    }

  if (saved_errno == E2BIG)
    /* No space to encode the character; try again next time.  */
    return 0;

  /* Otherwise, re-set the output buffer and try to escape or substitute
     the character, as appropriate.  */
  output = (char *) aux;
  output_left = can_put;

  /* The source buffer is valid UTF-8, so we shouldn't get EILSEQ
     because of the input encoding; if we get EILSEQ, that means the
     codepoint is not accessible in the target encoding.  We have whole
     codepoints in the source buffer, so we shouldn't get EINVAL.  We
     already handled E2BIG.  The descriptor should be valid so we
     shouldn't get EBADF.  In summary, we only need to handle EILSEQ.  */

  if (scm_is_eq (SCM_PORT (port)->conversion_strategy, sym_escape))
    {
      scm_t_uint8 escape[ESCAPE_BUFFER_SIZE];
      input = (char *) escape;
      input_left = encode_escape_sequence (ch, escape);
      scm_port_acquire_iconv_descriptors (port, NULL, &output_cd);
      res = iconv (output_cd, &input, &input_left, &output, &output_left);
      saved_errno = errno;
      iconv (output_cd, NULL, NULL, &output, &output_left);
      scm_port_release_iconv_descriptors (port);
    }
  else if (scm_is_eq (SCM_PORT (port)->conversion_strategy, sym_substitute))
    {
      scm_t_uint8 substitute[2] = "?";
      input = (char *) substitute;
      input_left = 1;
      scm_port_acquire_iconv_descriptors (port, NULL, &output_cd);
      res = iconv (output_cd, &input, &input_left, &output, &output_left);
      saved_errno = errno;
      iconv (output_cd, NULL, NULL, &output, &output_left);
      scm_port_release_iconv_descriptors (port);
    }

  if (res != (size_t) -1)
    {
      scm_port_buffer_did_put (buf, can_put - output_left);
      return 1;
    }

  /* No space to write the substitution or escape, or maybe there was an
     error.  If there are buffered bytes, the caller should flush and
     try again; otherwise the caller should raise an error.  */
  return 0;
}

static size_t
encode_latin1_chars_to_latin1_buf (SCM port, SCM buf,
                                   const scm_t_uint8 *chars, size_t count)
{
  return scm_port_buffer_put (buf, chars, count);
}

static size_t
encode_latin1_chars_to_utf8_buf (SCM port, SCM buf, const scm_t_uint8 *chars,
                                 size_t count)
{
  scm_t_uint8 *dst = scm_port_buffer_put_pointer (buf);
  size_t buf_size = scm_port_buffer_can_put (buf);
  size_t read, written;
  for (read = 0, written = 0;
       read < count && written + UTF8_BUFFER_SIZE < buf_size;
       read++)
    written += codepoint_to_utf8 (chars[read], dst + written);
  scm_port_buffer_did_put (buf, written);
  return read;
}

static size_t
encode_latin1_chars_to_iconv_buf (SCM port, SCM buf, const scm_t_uint8 *chars,
                                  size_t count)
{
  size_t read;
  for (read = 0; read < count; read++)
    if (!try_encode_char_to_iconv_buf (port, buf, chars[read]))
      break;
  return read;
}

static size_t
encode_latin1_chars (SCM port, SCM buf, const scm_t_uint8 *chars, size_t count)
{
  scm_t_port *pt = SCM_PORT (port);
  SCM position;
  size_t ret, i;

  if (scm_is_eq (pt->encoding, sym_ISO_8859_1))
    ret = encode_latin1_chars_to_latin1_buf (port, buf, chars, count);
  else if (scm_is_eq (pt->encoding, sym_UTF_8))
    ret = encode_latin1_chars_to_utf8_buf (port, buf, chars, count);
  else
    ret = encode_latin1_chars_to_iconv_buf (port, buf, chars, count);

  if (ret == 0 && count > 0)
    scm_encoding_error ("put-char", EILSEQ,
                        "conversion to port encoding failed",
                        port, SCM_MAKE_CHAR (chars[0]));

  position = pt->position;
  for (i = 0; i < ret; i++)
    update_port_position (position, chars[i]);

  return ret;
}

static size_t
encode_utf32_chars_to_latin1_buf (SCM port, SCM buf,
                                  const scm_t_uint32 *chars, size_t count)
{
  scm_t_port *pt = SCM_PORT (port);
  scm_t_uint8 *dst = scm_port_buffer_put_pointer (buf);
  size_t buf_size = scm_port_buffer_can_put (buf);
  size_t read, written;
  for (read = 0, written = 0; read < count && written < buf_size; read++)
    {
      scm_t_uint32 ch = chars[read];
      if (ch <= 0xff)
        dst[written++] = ch;
      else if (scm_is_eq (pt->conversion_strategy, sym_substitute))
        dst[written++] = '?';
      else if (scm_is_eq (pt->conversion_strategy, sym_escape))
        {
          scm_t_uint8 escape[ESCAPE_BUFFER_SIZE];
          size_t escape_len = encode_escape_sequence (ch, escape);
          if (escape_len > buf_size - written)
            break;
          memcpy (dst + written, escape, escape_len);
          written += escape_len;
        }
      else
        break;
    }
  scm_port_buffer_did_put (buf, written);
  return read;
}

static size_t
encode_utf32_chars_to_utf8_buf (SCM port, SCM buf, const scm_t_uint32 *chars,
                                size_t count)
{
  scm_t_uint8 *dst = scm_port_buffer_put_pointer (buf);
  size_t buf_size = scm_port_buffer_can_put (buf);
  size_t read, written;
  for (read = 0, written = 0;
       read < count && written + UTF8_BUFFER_SIZE < buf_size;
       read++)
    written += codepoint_to_utf8 (chars[read], dst + written);
  scm_port_buffer_did_put (buf, written);
  return read;
}

static size_t
encode_utf32_chars_to_iconv_buf (SCM port, SCM buf, const scm_t_uint32 *chars,
                                 size_t count)
{
  size_t read;
  for (read = 0; read < count; read++)
    if (!try_encode_char_to_iconv_buf (port, buf, chars[read]))
      break;
  return read;
}

static size_t
encode_utf32_chars (SCM port, SCM buf, const scm_t_uint32 *chars, size_t count)
{
  scm_t_port *pt = SCM_PORT (port);
  SCM position;
  size_t ret, i;

  if (scm_is_eq (pt->encoding, sym_ISO_8859_1))
    ret = encode_utf32_chars_to_latin1_buf (port, buf, chars, count);
  else if (scm_is_eq (pt->encoding, sym_UTF_8))
    ret = encode_utf32_chars_to_utf8_buf (port, buf, chars, count);
  else
    ret = encode_utf32_chars_to_iconv_buf (port, buf, chars, count);

  if (ret == 0 && count > 0)
    scm_encoding_error ("put-char", EILSEQ,
                        "conversion to port encoding failed",
                        port, SCM_MAKE_CHAR (chars[0]));

  position = pt->position;
  for (i = 0; i < ret; i++)
    update_port_position (position, chars[i]);

  return ret;
}

static size_t
port_encode_chars (SCM port, SCM buf, SCM str, size_t start, size_t count)
{
  if (count == 0)
    return 0;

  if (scm_i_is_narrow_string (str))
    {
      const char *chars = scm_i_string_chars (str);
      return encode_latin1_chars (port, buf,
                                  ((const scm_t_uint8 *) chars) + start,
                                  count);
    }
  else
    {
      const scm_t_wchar *chars = scm_i_string_wide_chars (str);
      return encode_utf32_chars (port, buf,
                                 ((const scm_t_uint32 *) chars) + start,
                                 count);
    }
}

SCM scm_port_encode_chars (SCM, SCM, SCM, SCM, SCM);
SCM_DEFINE (scm_port_encode_chars, "port-encode-chars", 5, 0, 0,
            (SCM port, SCM buf, SCM str, SCM start, SCM count),
            "")
#define FUNC_NAME s_scm_port_encode_chars
{
  size_t c_start, c_count, c_len, encoded;

  SCM_VALIDATE_OPOUTPORT (1, port);
  SCM_VALIDATE_VECTOR (2, buf);
  SCM_VALIDATE_STRING (3, str);
  c_len = scm_i_string_length (str);
  SCM_VALIDATE_SIZE_COPY (4, start, c_start);
  SCM_ASSERT_RANGE (4, start, c_start <= c_len);
  SCM_VALIDATE_SIZE_COPY (5, count, c_count);
  SCM_ASSERT_RANGE (5, count, c_count <= c_len - c_start);

  encoded = port_encode_chars (port, buf, str, c_start, c_count);

  return scm_from_size_t (encoded);
}
#undef FUNC_NAME

void
scm_c_put_latin1_chars (SCM port, const scm_t_uint8 *chars, size_t len)
{
  SCM aux_buf = scm_port_auxiliary_write_buffer (port);
  SCM aux_bv = scm_port_buffer_bytevector (aux_buf);
  SCM position = SCM_PORT (port)->position;
  SCM saved_line = scm_port_position_line (position);

  scm_port_clear_stream_start_for_bom_write (port, aux_buf);

  while (len)
    {
      size_t encoded = encode_latin1_chars (port, aux_buf, chars, len);
      assert(encoded <= len);
      scm_c_write_bytes (port, aux_bv, 0,
                         scm_to_size_t (scm_port_buffer_end (aux_buf)));
      scm_port_buffer_reset (aux_buf);
      chars += encoded;
      len -= encoded;
    }

  /* Handle line buffering.  */
  if ((SCM_CELL_WORD_0 (port) & SCM_BUFLINE) &&
      !scm_is_eq (saved_line, scm_port_position_line (position)))
    scm_flush (port);
}

void
scm_c_put_utf32_chars (SCM port, const scm_t_uint32 *chars, size_t len)
{
  SCM aux_buf = scm_port_auxiliary_write_buffer (port);
  SCM aux_bv = scm_port_buffer_bytevector (aux_buf);
  SCM position = SCM_PORT (port)->position;
  SCM saved_line = scm_port_position_line (position);

  scm_port_clear_stream_start_for_bom_write (port, aux_buf);

  while (len)
    {
      size_t encoded = encode_utf32_chars (port, aux_buf, chars, len);
      assert(encoded <= len);
      scm_c_write_bytes (port, aux_bv, 0,
                         scm_to_size_t (scm_port_buffer_end (aux_buf)));
      scm_port_buffer_reset (aux_buf);
      chars += encoded;
      len -= encoded;
    }

  /* Handle line buffering.  */
  if ((SCM_CELL_WORD_0 (port) & SCM_BUFLINE) &&
      !scm_is_eq (saved_line, scm_port_position_line (position)))
    scm_flush (port);
}

void
scm_c_put_char (SCM port, scm_t_wchar ch)
{
  if (ch <= 0xff)
    {
      scm_t_uint8 narrow_ch = ch;
      scm_c_put_latin1_chars (port, &narrow_ch, 1);
    }
  else
    {
      scm_t_uint32 wide_ch = ch;
      scm_c_put_utf32_chars (port, &wide_ch, 1);
    }
}

/* Return 0 unless the port can be written out to the port's encoding
   without errors, substitutions, or escapes.  */
int
scm_c_can_put_char (SCM port, scm_t_wchar ch)
{
  SCM encoding = SCM_PORT (port)->encoding;

  if (scm_is_eq (encoding, sym_UTF_8)
      || (scm_is_eq (encoding, sym_ISO_8859_1) && ch <= 0xff)
      || scm_is_eq (encoding, sym_UTF_16)
      || scm_is_eq (encoding, sym_UTF_16LE)
      || scm_is_eq (encoding, sym_UTF_16BE)
      || scm_is_eq (encoding, sym_UTF_32)
      || scm_is_eq (encoding, sym_UTF_32LE)
      || scm_is_eq (encoding, sym_UTF_32BE))
    return 1;

  {
    SCM bv = scm_port_buffer_bytevector (scm_port_auxiliary_write_buffer (port));
    scm_t_uint8 buf[UTF8_BUFFER_SIZE];
    char *input = (char *) buf;
    size_t input_len;
    char *output = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
    size_t output_len = SCM_BYTEVECTOR_LENGTH (bv);
    size_t result;
    iconv_t output_cd;

    input_len = codepoint_to_utf8 (ch, buf);

    scm_port_acquire_iconv_descriptors (port, NULL, &output_cd);
    iconv (output_cd, NULL, NULL, &output, &output_len);
    result = iconv (output_cd, &input, &input_len, &output, &output_len);
    iconv (output_cd, NULL, NULL, &output, &output_len);
    scm_port_release_iconv_descriptors (port);

    return result != (size_t) -1;
  }
}

void
scm_c_put_string (SCM port, SCM string, size_t start, size_t count)
{
  if (scm_i_is_narrow_string (string))
    {
      const char *ptr = scm_i_string_chars (string);
      scm_c_put_latin1_chars (port, ((const scm_t_uint8 *) ptr) + start, count);
    }
  else
    {
      const scm_t_wchar *ptr = scm_i_string_wide_chars (string);
      scm_c_put_utf32_chars (port, ((const scm_t_uint32 *) ptr) + start, count);
    }
}

SCM_DEFINE (scm_put_string, "put-string", 2, 2, 0,
            (SCM port, SCM string, SCM start, SCM count),
            "")
#define FUNC_NAME s_scm_put_string
{
  size_t c_start, c_count, c_len;

  SCM_VALIDATE_OPOUTPORT (1, port);
  SCM_VALIDATE_STRING (2, string);
  c_len = scm_i_string_length (string);
  c_start = SCM_UNBNDP (start) ? 0 : scm_to_size_t (start);
  SCM_ASSERT_RANGE (3, start, c_start <= c_len);
  c_count = SCM_UNBNDP (count) ? c_len - c_start : scm_to_size_t (count);
  SCM_ASSERT_RANGE (4, count, c_count <= c_len - c_start);

  scm_c_put_string (port, string, c_start, c_count);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_putc (char c, SCM port)
{
  SCM_ASSERT_TYPE (SCM_OPOUTPORTP (port), port, 0, NULL, "output port");
  scm_c_put_char (port, (scm_t_uint8) c);
}

void
scm_puts (const char *s, SCM port)
{
  SCM_ASSERT_TYPE (SCM_OPOUTPORTP (port), port, 0, NULL, "output port");
  scm_c_put_latin1_chars (port, (const scm_t_uint8 *) s, strlen (s));
}

/* scm_lfwrite
 *
 * This function differs from scm_c_write; it updates port line and
 * column, flushing line-buffered ports when appropriate. */
void
scm_lfwrite (const char *ptr, size_t size, SCM port)
{
  scm_c_put_latin1_chars (port, (const scm_t_uint8 *) ptr, size);
}

/* Write STR to PORT from START inclusive to END exclusive.  */
void
scm_lfwrite_substr (SCM str, size_t start, size_t end, SCM port)
{
  if (end == (size_t) -1)
    end = scm_i_string_length (str);

  scm_c_put_string (port, str, start, end - start);
}




/* Querying and setting positions, and character availability.  */

SCM_DEFINE (scm_char_ready_p, "char-ready?", 0, 1, 0, 
	    (SCM port),
	    "Return @code{#t} if a character is ready on input @var{port}\n"
	    "and return @code{#f} otherwise.  If @code{char-ready?} returns\n"
	    "@code{#t} then the next @code{read-char} operation on\n"
	    "@var{port} is guaranteed not to hang.  If @var{port} is a file\n"
	    "port at end of file then @code{char-ready?} returns @code{#t}.\n"
	    "\n"
	    "@code{char-ready?} exists to make it possible for a\n"
	    "program to accept characters from interactive ports without\n"
	    "getting stuck waiting for input.  Any input editors associated\n"
	    "with such ports must make sure that characters whose existence\n"
	    "has been asserted by @code{char-ready?} cannot be rubbed out.\n"
	    "If @code{char-ready?} were to return @code{#f} at end of file,\n"
	    "a port at end of file would be indistinguishable from an\n"
	    "interactive port that has no ready characters.")
#define FUNC_NAME s_scm_char_ready_p
{
  SCM read_buf;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  /* It's possible to close the current input port, so validate even in
     this case. */
  SCM_VALIDATE_OPINPORT (1, port);

  read_buf = SCM_PORT (port)->read_buf;

  if (scm_port_buffer_can_take (read_buf) ||
      scm_is_true (scm_port_buffer_has_eof_p (read_buf)))
    /* FIXME: Verify that a whole character is available?  */
    return SCM_BOOL_T;
  else
    {
      scm_t_port_type *ptob = SCM_PORT_TYPE (port);
      
      if (ptob->input_waiting)
	return scm_from_bool (ptob->input_waiting (port));
      else
	return SCM_BOOL_T;
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_seek, "seek", 3, 0, 0,
            (SCM fd_port, SCM offset, SCM whence),
	    "Sets the current position of @var{fd_port} to the integer\n"
	    "@var{offset}, which is interpreted according to the value of\n"
	    "@var{whence}.\n"
	    "\n"
	    "One of the following variables should be supplied for\n"
	    "@var{whence}:\n"
	    "@defvar SEEK_SET\n"
	    "Seek from the beginning of the file.\n"
	    "@end defvar\n"
	    "@defvar SEEK_CUR\n"
	    "Seek from the current position.\n"
	    "@end defvar\n"
	    "@defvar SEEK_END\n"
	    "Seek from the end of the file.\n"
	    "@end defvar\n"
	    "If @var{fd_port} is a file descriptor, the underlying system\n"
	    "call is @code{lseek}.  @var{port} may be a string port.\n"
	    "\n"
	    "The value returned is the new position in the file.  This means\n"
	    "that the current position of a port can be obtained using:\n"
	    "@lisp\n"
	    "(seek port 0 SEEK_CUR)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_seek
{
  int how;

  fd_port = SCM_COERCE_OUTPORT (fd_port);

  how = scm_to_int (whence);
  if (how != SEEK_SET && how != SEEK_CUR && how != SEEK_END)
    SCM_OUT_OF_RANGE (3, whence);

  if (SCM_OPPORTP (fd_port))
    {
      scm_t_port *pt = SCM_PORT (fd_port);
      scm_t_port_type *ptob = SCM_PORT_TYPE (fd_port);
      off_t_or_off64_t off = scm_to_off_t_or_off64_t (offset);
      off_t_or_off64_t rv;

      if (!ptob->seek || !pt->rw_random)
	SCM_MISC_ERROR ("port is not seekable", 
                        scm_cons (fd_port, SCM_EOL));

      /* FIXME: Avoid flushing buffers for SEEK_CUR with an offset of
         0.  */

      scm_end_input (fd_port);
      scm_flush (fd_port);

      rv = ptob->seek (fd_port, off, how);

      /* Set stream-start flags according to new position. */
      pt->at_stream_start_for_bom_read  = (rv == 0);
      pt->at_stream_start_for_bom_write = (rv == 0);

      scm_i_clear_pending_eof (fd_port);

      return scm_from_off_t_or_off64_t (rv);
    }
  else /* file descriptor?.  */
    {
      off_t_or_off64_t off = scm_to_off_t_or_off64_t (offset);
      off_t_or_off64_t rv;
      rv = lseek_or_lseek64 (scm_to_int (fd_port), off, how);
      if (rv == -1)
	SCM_SYSERROR;
      return scm_from_off_t_or_off64_t (rv);
    }
}
#undef FUNC_NAME

#ifndef O_BINARY
#define O_BINARY 0
#endif

/* Mingw has ftruncate(), perhaps implemented above using chsize, but
   doesn't have the filename version truncate(), hence this code.  */
#if HAVE_FTRUNCATE && ! HAVE_TRUNCATE
static int
truncate (const char *file, off_t length)
{
  int ret, fdes;

  fdes = open (file, O_BINARY | O_WRONLY);
  if (fdes == -1)
    return -1;

  ret = ftruncate (fdes, length);
  if (ret == -1)
    {
      int save_errno = errno;
      close (fdes);
      errno = save_errno;
      return -1;
    }

  return close (fdes);
}
#endif /* HAVE_FTRUNCATE && ! HAVE_TRUNCATE */

SCM_DEFINE (scm_truncate_file, "truncate-file", 1, 1, 0,
            (SCM object, SCM length),
	    "Truncate file @var{object} to @var{length} bytes.  @var{object}\n"
	    "can be a filename string, a port object, or an integer file\n"
	    "descriptor.\n"
	    "The return value is unspecified.\n"
	    "\n"
	    "For a port or file descriptor @var{length} can be omitted, in\n"
	    "which case the file is truncated at the current position (per\n"
	    "@code{ftell} above).\n"
	    "\n"
	    "On most systems a file can be extended by giving a length\n"
	    "greater than the current size, but this is not mandatory in the\n"
	    "POSIX standard.")
#define FUNC_NAME s_scm_truncate_file
{
  int rv;

  /* "object" can be a port, fdes or filename.

     Negative "length" makes no sense, but it's left to truncate() or
     ftruncate() to give back an error for that (normally EINVAL).
     */

  if (SCM_UNBNDP (length))
    {
      /* must supply length if object is a filename.  */
      if (scm_is_string (object))
        SCM_MISC_ERROR("must supply length if OBJECT is a filename", SCM_EOL);
      
      length = scm_seek (object, SCM_INUM0, scm_from_int (SEEK_CUR));
    }

  object = SCM_COERCE_OUTPORT (object);
  if (scm_is_integer (object))
    {
      off_t_or_off64_t c_length = scm_to_off_t_or_off64_t (length);
      SCM_SYSCALL (rv = ftruncate_or_ftruncate64 (scm_to_int (object),
                                                  c_length));
    }
  else if (SCM_OPOUTPORTP (object))
    {
      off_t_or_off64_t c_length = scm_to_off_t_or_off64_t (length);
      scm_t_port_type *ptob = SCM_PORT_TYPE (object);

      if (!ptob->truncate)
	SCM_MISC_ERROR ("port is not truncatable", SCM_EOL);

      scm_i_clear_pending_eof (object);

      if (SCM_INPUT_PORT_P (object)
          && SCM_PORT (object)->rw_random)
        scm_end_input (object);
      scm_flush (object);

      ptob->truncate (object, c_length);
      rv = 0;
    }
  else
    {
      off_t_or_off64_t c_length = scm_to_off_t_or_off64_t (length);
      char *str = scm_to_locale_string (object);
      int eno;
      SCM_SYSCALL (rv = truncate_or_truncate64 (str, c_length));
      eno = errno;
      free (str);
      errno = eno;
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_line, "port-line", 1, 0, 0,
            (SCM port),
	    "Return the current line number for @var{port}.\n"
	    "\n"
	    "The first line of a file is 0.  But you might want to add 1\n"
	    "when printing line numbers, since starting from 1 is\n"
	    "traditional in error messages, and likely to be more natural to\n"
	    "non-programmers.")
#define FUNC_NAME s_scm_port_line
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  return scm_port_position_line (SCM_PORT (port)->position);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_port_line_x, "set-port-line!", 2, 0, 0,
            (SCM port, SCM line),
	    "Set the current line number for @var{port} to @var{line}.  The\n"
	    "first line of a file is 0.")
#define FUNC_NAME s_scm_set_port_line_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  scm_to_long (line);
  scm_port_position_set_line (SCM_PORT (port)->position, line);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_column, "port-column", 1, 0, 0,
            (SCM port),
	    "Return the current column number of @var{port}.\n"
	    "If the number is\n"
	    "unknown, the result is #f.  Otherwise, the result is a 0-origin integer\n"
	    "- i.e. the first character of the first line is line 0, column 0.\n"
	    "(However, when you display a file position, for example in an error\n"
	    "message, we recommend you add 1 to get 1-origin integers.  This is\n"
	    "because lines and column numbers traditionally start with 1, and that is\n"
	    "what non-programmers will find most natural.)")
#define FUNC_NAME s_scm_port_column
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  return scm_port_position_column (SCM_PORT (port)->position);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_port_column_x, "set-port-column!", 2, 0, 0,
            (SCM port, SCM column),
	    "Set the current column of @var{port}.  Before reading the first\n"
	    "character on a line the column should be 0.")
#define FUNC_NAME s_scm_set_port_column_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  scm_to_int (column);
  scm_port_position_set_column (SCM_PORT (port)->position, column);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_filename, "port-filename", 1, 0, 0,
            (SCM port),
	    "Return the filename associated with @var{port}, or @code{#f}\n"
	    "if no filename is associated with the port.")
#define FUNC_NAME s_scm_port_filename
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  return SCM_FILENAME (port);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_port_filename_x, "set-port-filename!", 2, 0, 0,
            (SCM port, SCM filename),
	    "Change the filename associated with @var{port}, using the current input\n"
	    "port if none is specified.  Note that this does not change the port's\n"
	    "source of data, but only the value that is returned by\n"
	    "@code{port-filename} and reported in diagnostic output.")
#define FUNC_NAME s_scm_set_port_filename_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  /* We allow the user to set the filename to whatever he likes.  */
  SCM_SET_FILENAME (port, filename);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* Implementation helpers for port printing functions.  */

void
scm_print_port_mode (SCM exp, SCM port)
{
  scm_puts (SCM_CLOSEDP (exp)
	    ? "closed: "
	    : (SCM_RDNG & SCM_CELL_WORD_0 (exp)
	       ? (SCM_WRTNG & SCM_CELL_WORD_0 (exp)
		  ? "input-output: "
		  : "input: ")
	       : (SCM_WRTNG & SCM_CELL_WORD_0 (exp)
		  ? "output: "
		  : "bogus: ")),
	    port);
}

int
scm_port_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  char *type = SCM_PORT_TYPE (port)->name;
  if (!type)
    type = "port";
  scm_puts ("#<", port);
  scm_print_port_mode (exp, port);
  scm_puts (type, port);
  scm_putc (' ', port);
  scm_uintprint ((scm_t_bits) SCM_PORT (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}




/* Iterating over all ports.  */

struct for_each_data 
{
  void (*proc) (void *data, SCM p);
  void *data;
};

static SCM
for_each_trampoline (void *data, SCM port, SCM result)
{
  struct for_each_data *d = data;
  
  d->proc (d->data, port);

  return result;
}

void
scm_c_port_for_each (void (*proc)(void *data, SCM p), void *data)
{
  struct for_each_data d;
  
  d.proc = proc;
  d.data = data;

  scm_c_weak_set_fold (for_each_trampoline, &d, SCM_EOL,
                       scm_i_port_weak_set);
}

static void
scm_for_each_trampoline (void *data, SCM port)
{
  scm_call_1 (SCM_PACK_POINTER (data), port);
}

SCM_DEFINE (scm_port_for_each, "port-for-each", 1, 0, 0,
	    (SCM proc),
	    "Apply @var{proc} to each port in the Guile port table\n"
	    "in turn.  The return value is unspecified.  More specifically,\n"
	    "@var{proc} is applied exactly once to every port that exists\n"
	    "in the system at the time @code{port-for-each} is invoked.\n"
	    "Changes to the port table while @code{port-for-each} is running\n"
	    "have no effect as far as @code{port-for-each} is concerned.") 
#define FUNC_NAME s_scm_port_for_each
{
  SCM_VALIDATE_PROC (1, proc);

  scm_c_port_for_each (scm_for_each_trampoline, SCM_UNPACK_POINTER (proc));
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
flush_output_port (void *closure, SCM port)
{
  if (SCM_OPOUTPORTP (port))
    scm_flush (port);
}

SCM_DEFINE (scm_flush_all_ports, "flush-all-ports", 0, 0, 0,
            (),
	    "Equivalent to calling @code{force-output} on\n"
	    "all open output ports.  The return value is unspecified.")
#define FUNC_NAME s_scm_flush_all_ports
{
  scm_c_port_for_each (&flush_output_port, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* Void ports.   */

scm_t_port_type *scm_void_port_type = 0;

static size_t
void_port_read (SCM port, SCM dst, size_t start, size_t count)
{
  return 0;
}

static size_t
void_port_write (SCM port, SCM src, size_t start, size_t count)
{
  return count;
}

static SCM
scm_i_void_port (long mode_bits)
{
  return scm_c_make_port (scm_void_port_type, mode_bits, 0);
}

SCM
scm_void_port (char *mode_str)
{
  return scm_i_void_port (scm_mode_bits (mode_str));
}

SCM_DEFINE (scm_sys_make_void_port, "%make-void-port", 1, 0, 0,
            (SCM mode),
	    "Create and return a new void port.  A void port acts like\n"
	    "@file{/dev/null}.  The @var{mode} argument\n"
	    "specifies the input/output modes for this port: see the\n"
	    "documentation for @code{open-file} in @ref{File Ports}.")
#define FUNC_NAME s_scm_sys_make_void_port
{
  return scm_i_void_port (scm_i_mode_bits (mode));
}
#undef FUNC_NAME




/* Initialization.  */

static void
scm_init_ice_9_ports (void)
{
#include "libguile/ports.x"

  scm_c_define ("the-eof-object", SCM_EOF_VAL);

  /* lseek() symbols.  */
  scm_c_define ("SEEK_SET", scm_from_int (SEEK_SET));
  scm_c_define ("SEEK_CUR", scm_from_int (SEEK_CUR));
  scm_c_define ("SEEK_END", scm_from_int (SEEK_END));

  scm_c_define ("%current-input-port-fluid", cur_inport_fluid);
  scm_c_define ("%current-output-port-fluid", cur_outport_fluid);
  scm_c_define ("%current-error-port-fluid", cur_errport_fluid);
  scm_c_define ("%current-warning-port-fluid", cur_warnport_fluid);
}

void
scm_init_ports (void)
{
  sym_UTF_8 = scm_from_latin1_symbol ("UTF-8");
  sym_ISO_8859_1 = scm_from_latin1_symbol ("ISO-8859-1");
  sym_UTF_16 = scm_from_latin1_symbol ("UTF-16");
  sym_UTF_16LE = scm_from_latin1_symbol ("UTF-16LE");
  sym_UTF_16BE = scm_from_latin1_symbol ("UTF-16BE");
  sym_UTF_32 = scm_from_latin1_symbol ("UTF-32");
  sym_UTF_32LE = scm_from_latin1_symbol ("UTF-32LE");
  sym_UTF_32BE = scm_from_latin1_symbol ("UTF-32BE");

  sym_substitute = scm_from_latin1_symbol ("substitute");
  sym_escape = scm_from_latin1_symbol ("escape");
  sym_error = scm_from_latin1_symbol ("error");

  trampoline_to_c_read_subr =
    scm_c_make_gsubr ("port-read", 4, 0, 0,
                      (scm_t_subr) trampoline_to_c_read);
  trampoline_to_c_write_subr =
    scm_c_make_gsubr ("port-write", 4, 0, 0,
                      (scm_t_subr) trampoline_to_c_write);

  scm_void_port_type = scm_make_port_type ("void", void_port_read,
					   void_port_write);

  scm_i_port_weak_set = scm_c_make_weak_set (31);

  cur_inport_fluid = scm_make_fluid ();
  cur_outport_fluid = scm_make_fluid ();
  cur_errport_fluid = scm_make_fluid ();
  cur_warnport_fluid = scm_make_fluid ();
  cur_loadport_fluid = scm_make_fluid ();

  /* Use Latin-1 as the default port encoding.  */
  default_port_encoding_var =
    scm_c_define ("%default-port-encoding",
                  scm_make_fluid_with_default (SCM_BOOL_F));
  default_conversion_strategy_var =
    scm_c_define ("%default-port-conversion-strategy",
                  scm_make_fluid_with_default (sym_substitute));

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_ice_9_ports",
			    (scm_t_extension_init_func) scm_init_ice_9_ports,
			    NULL);

  /* The following bindings are used early in boot-9.scm.  */

  /* Used by `include'.  */
  scm_c_define_gsubr ("set-port-encoding!", 2, 0, 0,
                      (scm_t_subr) scm_set_port_encoding_x);
  scm_c_define_gsubr (s_scm_eof_object_p, 1, 0, 0,
                      (scm_t_subr) scm_eof_object_p);

  /* Used by a number of error/warning-printing routines.  */
  scm_c_define_gsubr (s_scm_force_output, 0, 1, 0,
                      (scm_t_subr) scm_force_output);

  /* Used by `file-exists?' and related functions if `stat' is
     unavailable.  */
  scm_c_define_gsubr (s_scm_close_port, 1, 0, 0,
                      (scm_t_subr) scm_close_port);

  /* Used by error routines.  */
  scm_c_define_gsubr (s_scm_current_error_port, 0, 0, 0,
                      (scm_t_subr) scm_current_error_port);
  scm_c_define_gsubr (s_scm_current_warning_port, 0, 0, 0,
                      (scm_t_subr) scm_current_warning_port);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
