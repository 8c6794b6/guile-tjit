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
encoding_matches (const char *enc, const char *upper)
{
  if (!enc)
    enc = "ISO-8859-1";

  while (*enc)
    if (ascii_toupper (*enc++) != *upper++)
      return 0;

  return !*upper;
}

static char*
canonicalize_encoding (const char *enc)
{
  char *ret;
  int i;

  if (!enc)
    return "ISO-8859-1";

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

  return ret;
}



/* The port kind table --- a dynamically resized array of port types.  */


/* scm_ptobs scm_numptob
 * implement a dynamically resized array of ptob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
static scm_t_ptob_descriptor **scm_ptobs = NULL;
static long scm_numptob = 0; /* Number of port types.  */
static long scm_ptobs_size = 0; /* Number of slots in the port type
                                   table.  */
static scm_i_pthread_mutex_t scm_ptobs_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

long
scm_c_num_port_types (void)
{
  long ret;
  
  scm_i_pthread_mutex_lock (&scm_ptobs_lock);
  ret = scm_numptob;
  scm_i_pthread_mutex_unlock (&scm_ptobs_lock);

  return ret;
}

scm_t_ptob_descriptor*
scm_c_port_type_ref (long ptobnum)
{
  scm_t_ptob_descriptor *ret = NULL;

  scm_i_pthread_mutex_lock (&scm_ptobs_lock);

  if (0 <= ptobnum && ptobnum < scm_numptob)
    ret = scm_ptobs[ptobnum];

  scm_i_pthread_mutex_unlock (&scm_ptobs_lock);

  if (!ret)
    scm_out_of_range ("scm_c_port_type_ref", scm_from_long (ptobnum));

  return ret;
}

long
scm_c_port_type_add_x (scm_t_ptob_descriptor *desc)
{
  long ret = -1;

  scm_i_pthread_mutex_lock (&scm_ptobs_lock);
  
  if (scm_numptob + 1 < SCM_I_MAX_PORT_TYPE_COUNT)
    {
      if (scm_numptob == scm_ptobs_size)
        {
          unsigned long old_size = scm_ptobs_size;
          scm_t_ptob_descriptor **old_ptobs = scm_ptobs;
      
          /* Currently there are only 9 predefined port types, so one
             resize will cover it.  */
          scm_ptobs_size = old_size + 10;

          if (scm_ptobs_size >= SCM_I_MAX_PORT_TYPE_COUNT)
            scm_ptobs_size = SCM_I_MAX_PORT_TYPE_COUNT;

          scm_ptobs = scm_gc_malloc (sizeof (*scm_ptobs) * scm_ptobs_size,
                                     "scm_ptobs");

          memcpy (scm_ptobs, old_ptobs, sizeof (*scm_ptobs) * scm_numptob);
        }

      ret = scm_numptob++;
      scm_ptobs[ret] = desc;
    }
  
  scm_i_pthread_mutex_unlock (&scm_ptobs_lock);

  if (ret < 0)
    scm_out_of_range ("scm_c_port_type_add_x", scm_from_long (scm_numptob));

  return ret;
}

/* Default buffer size.  Used if the port type won't supply a value.  */
static const size_t default_buffer_size = 1024;

scm_t_bits
scm_make_port_type (char *name,
                    size_t (*read) (SCM port, SCM dst, size_t start,
                                    size_t count),
                    size_t (*write) (SCM port, SCM src, size_t start,
                                     size_t count))
{
  scm_t_ptob_descriptor *desc;
  long ptobnum;

  desc = scm_gc_malloc_pointerless (sizeof (*desc), "port-type");
  memset (desc, 0, sizeof (*desc));

  desc->name = name;
  desc->print = scm_port_print;
  desc->read = read;
  desc->write = write;

  ptobnum = scm_c_port_type_add_x (desc);

  /* Make a class object if GOOPS is present.  */
  if (SCM_UNPACK (scm_i_port_class[0]) != 0)
    scm_make_port_classes (ptobnum, name);

  return scm_tc7_port + ptobnum * 256;
}

void
scm_set_port_print (scm_t_bits tc, int (*print) (SCM exp, SCM port,
                                                 scm_print_state *pstate))
{
  scm_c_port_type_ref (SCM_TC2PTOBNUM (tc))->print = print;
}

void
scm_set_port_close (scm_t_bits tc, void (*close) (SCM))
{
  scm_c_port_type_ref (SCM_TC2PTOBNUM (tc))->close = close;
}

void
scm_set_port_needs_close_on_gc (scm_t_bits tc, int needs_close_p)
{
  scm_t_ptob_descriptor *ptob = scm_c_port_type_ref (SCM_TC2PTOBNUM (tc));

  if (needs_close_p)
    ptob->flags |= SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC;
  else
    ptob->flags &= ~SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC;
}

void
scm_set_port_seek (scm_t_bits tc, scm_t_off (*seek) (SCM, scm_t_off, int))
{
  scm_c_port_type_ref (SCM_TC2PTOBNUM (tc))->seek = seek;
}

void
scm_set_port_truncate (scm_t_bits tc, void (*truncate) (SCM, scm_t_off))
{
  scm_c_port_type_ref (SCM_TC2PTOBNUM (tc))->truncate = truncate;
}

void
scm_set_port_input_waiting (scm_t_bits tc, int (*input_waiting) (SCM))
{
  scm_c_port_type_ref (SCM_TC2PTOBNUM (tc))->input_waiting = input_waiting;
}

void
scm_set_port_get_natural_buffer_sizes
  (scm_t_bits tc, void (*get_natural_buffer_sizes) (SCM, size_t *, size_t *))
{
  scm_t_ptob_descriptor *ptob = scm_c_port_type_ref (SCM_TC2PTOBNUM (tc));
  ptob->get_natural_buffer_sizes = get_natural_buffer_sizes;
}

static void
scm_i_set_pending_eof (SCM port)
{
  scm_port_buffer_set_has_eof_p (SCM_PTAB_ENTRY (port)->read_buf, SCM_BOOL_T);
}

static void
scm_i_clear_pending_eof (SCM port)
{
  scm_port_buffer_set_has_eof_p (SCM_PTAB_ENTRY (port)->read_buf, SCM_BOOL_F);
}

SCM_DEFINE (scm_i_port_property, "%port-property", 2, 0, 0,
            (SCM port, SCM key),
            "Return the property of @var{port} associated with @var{key}.")
#define FUNC_NAME s_scm_i_port_property
{
  SCM result;
  scm_t_port *pt;

  SCM_VALIDATE_OPPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  scm_i_pthread_mutex_lock (pt->lock);
  result = scm_assq_ref (pt->internal->alist, key);
  scm_i_pthread_mutex_unlock (pt->lock);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_i_set_port_property_x, "%set-port-property!", 3, 0, 0,
            (SCM port, SCM key, SCM value),
            "Set the property of @var{port} associated with @var{key} to @var{value}.")
#define FUNC_NAME s_scm_i_set_port_property_x
{
  scm_t_port *pt;

  SCM_VALIDATE_OPPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  scm_i_pthread_mutex_lock (pt->lock);
  pt->internal->alist = scm_assq_set_x (pt->internal->alist, key, value);
  scm_i_pthread_mutex_unlock (pt->lock);

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

SCM
scm_c_make_port_buffer (size_t size)
{
  SCM ret = scm_c_make_vector (SCM_PORT_BUFFER_FIELD_COUNT, SCM_INUM0);

  SCM_SIMPLE_VECTOR_SET (ret, SCM_PORT_BUFFER_FIELD_BYTEVECTOR,
                         scm_c_make_bytevector (size));
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
  return (SCM_OPN
	  | (scm_i_string_contains_char (modes, 'r') 
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




static void
initialize_port_buffers (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_ptob_descriptor *ptob = SCM_PORT_DESCRIPTOR (port);
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
  pt->read_buf = scm_c_make_port_buffer (read_buf_size);
  pt->write_buf = scm_c_make_port_buffer (write_buf_size);
}

SCM
scm_c_make_port_with_encoding (scm_t_bits tag, unsigned long mode_bits,
                               const char *encoding,
                               scm_t_string_failed_conversion_handler handler,
                               scm_t_bits stream)
{
  SCM ret;
  scm_t_port *entry;
  scm_t_port_internal *pti;
  scm_t_ptob_descriptor *ptob;

  entry = scm_gc_typed_calloc (scm_t_port);
  pti = scm_gc_typed_calloc (scm_t_port_internal);
  ptob = scm_c_port_type_ref (SCM_TC2PTOBNUM (tag));

  ret = scm_words (tag | mode_bits, 3);
  SCM_SET_CELL_WORD_1 (ret, (scm_t_bits) entry);
  SCM_SET_CELL_WORD_2 (ret, (scm_t_bits) ptob);

  entry->lock = scm_gc_malloc_pointerless (sizeof (*entry->lock), "port lock");
  scm_i_pthread_mutex_init (entry->lock, scm_i_pthread_mutexattr_recursive);

  entry->internal = pti;
  entry->file_name = SCM_BOOL_F;
  /* By default, any port type with a seek function has random-access
     ports.  */
  entry->rw_random = ptob->seek != NULL;
  entry->port = ret;
  entry->stream = stream;

  if (encoding_matches (encoding, "UTF-8"))
    {
      pti->encoding_mode = SCM_PORT_ENCODING_MODE_UTF8;
      entry->encoding = "UTF-8";
    }
  else if (encoding_matches (encoding, "ISO-8859-1"))
    {
      pti->encoding_mode = SCM_PORT_ENCODING_MODE_LATIN1;
      entry->encoding = "ISO-8859-1";
    }
  else
    {
      pti->encoding_mode = SCM_PORT_ENCODING_MODE_ICONV;
      entry->encoding = canonicalize_encoding (encoding);
    }

  entry->ilseq_handler = handler;
  pti->iconv_descriptors = NULL;

  pti->at_stream_start_for_bom_read  = 1;
  pti->at_stream_start_for_bom_write = 1;

  pti->alist = SCM_EOL;

  if (SCM_PORT_DESCRIPTOR (ret)->flags & SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC)
    {
      scm_i_set_finalizer (SCM2PTR (ret), finalize_port, NULL);
      scm_weak_set_add_x (scm_i_port_weak_set, ret);
    }

  initialize_port_buffers (ret);

  return ret;
}

SCM
scm_c_make_port (scm_t_bits tag, unsigned long mode_bits, scm_t_bits stream)
{
  return scm_c_make_port_with_encoding (tag, mode_bits,
                                        scm_i_default_port_encoding (),
                                        scm_i_default_port_conversion_handler (),
                                        stream);
}

SCM
scm_new_port_table_entry (scm_t_bits tag)
{
  return scm_c_make_port (tag, 0, 0);
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

static void close_iconv_descriptors (scm_t_iconv_descriptors *id);

/* scm_close_port
 * Call the close operation on a port object. 
 * see also scm_close.
 */
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
  scm_t_port_internal *pti;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_PORT (1, port);
  if (SCM_CLOSEDP (port))
    return SCM_BOOL_F;

  /* May throw an exception.  */
  if (SCM_OUTPUT_PORT_P (port))
    scm_flush (port);

  pti = SCM_PORT_GET_INTERNAL (port);
  SCM_CLR_PORT_OPEN_FLAG (port);

  if (SCM_PORT_DESCRIPTOR (port)->flags & SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC)
    scm_weak_set_remove_x (scm_i_port_weak_set, port);

  if (SCM_PORT_DESCRIPTOR (port)->close)
    /* Note!  This may throw an exception.  Anything after this point
       should be resilient to non-local exits.  */
    SCM_PORT_DESCRIPTOR (port)->close (port);

  if (pti->iconv_descriptors)
    {
      /* If we don't get here, the iconv_descriptors finalizer will
         clean up. */
      close_iconv_descriptors (pti->iconv_descriptors);
      pti->iconv_descriptors = NULL;
    }

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

/* A fluid specifying the default encoding for newly created ports.  If it is
   a string, that is the encoding.  If it is #f, it is in the "native"
   (Latin-1) encoding.  */
static SCM default_port_encoding_var;

/* Use ENCODING as the default encoding for future ports.  */
void
scm_i_set_default_port_encoding (const char *encoding)
{
  if (encoding_matches (encoding, "ISO-8859-1"))
    scm_fluid_set_x (SCM_VARIABLE_REF (default_port_encoding_var), SCM_BOOL_F);
  else
    scm_fluid_set_x (SCM_VARIABLE_REF (default_port_encoding_var),
                     scm_from_latin1_string (canonicalize_encoding (encoding)));
}

/* Return the name of the default encoding for newly created ports.  */
const char *
scm_i_default_port_encoding (void)
{
  SCM encoding;

  encoding = scm_fluid_ref (SCM_VARIABLE_REF (default_port_encoding_var));
  if (!scm_is_string (encoding))
    return "ISO-8859-1";
  else
    return scm_i_string_chars (encoding);
}

/* A fluid specifying the default conversion handler for newly created
   ports.  Its value should be one of the symbols below.  */
static SCM default_conversion_strategy_var;

/* The possible conversion strategies.  */
static SCM sym_error;
static SCM sym_substitute;
static SCM sym_escape;

/* Return the default failed encoding conversion policy for new created
   ports.  */
scm_t_string_failed_conversion_handler
scm_i_default_port_conversion_handler (void)
{
  SCM value;

  value = scm_fluid_ref (SCM_VARIABLE_REF (default_conversion_strategy_var));

  if (scm_is_eq (sym_substitute, value))
    return SCM_FAILED_CONVERSION_QUESTION_MARK;
  else if (scm_is_eq (sym_escape, value))
    return SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE;
  else
    /* Default to 'error also when the fluid's value is not one of
       the valid symbols.  */
    return SCM_FAILED_CONVERSION_ERROR;
}

/* Use HANDLER as the default conversion strategy for future ports.  */
void
scm_i_set_default_port_conversion_handler (scm_t_string_failed_conversion_handler
					   handler)
{
  SCM strategy;

  switch (handler)
    {
    case SCM_FAILED_CONVERSION_ERROR:
      strategy = sym_error;
      break;

    case SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE:
      strategy = sym_escape;
      break;

    case SCM_FAILED_CONVERSION_QUESTION_MARK:
      strategy = sym_substitute;
      break;

    default:
      abort ();
    }

  scm_fluid_set_x (SCM_VARIABLE_REF (default_conversion_strategy_var),
		   strategy);
}

/* If the next LEN bytes from PORT are equal to those in BYTES, then
   return 1, else return 0.  Leave the port position unchanged.  */
static int
looking_at_bytes (SCM port, const unsigned char *bytes, int len)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  int i = 0;

  while (i < len && scm_peek_byte_or_eof (port) == bytes[i])
    {
      scm_port_buffer_did_take (pt->read_buf, 1);
      i++;
    }
  scm_unget_bytes (bytes, i, port);
  return (i == len);
}

static const unsigned char scm_utf8_bom[3]    = {0xEF, 0xBB, 0xBF};
static const unsigned char scm_utf16be_bom[2] = {0xFE, 0xFF};
static const unsigned char scm_utf16le_bom[2] = {0xFF, 0xFE};
static const unsigned char scm_utf32be_bom[4] = {0x00, 0x00, 0xFE, 0xFF};
static const unsigned char scm_utf32le_bom[4] = {0xFF, 0xFE, 0x00, 0x00};

/* Decide what byte order to use for a UTF-16 port.  Return "UTF-16BE"
   or "UTF-16LE".  MODE must be either SCM_PORT_READ or SCM_PORT_WRITE,
   and specifies which operation is about to be done.  The MODE
   determines how we will decide the byte order.  We deliberately avoid
   reading from the port unless the user is about to do so.  If the user
   is about to read, then we look for a BOM, and if present, we use it
   to determine the byte order.  Otherwise we choose big endian, as
   recommended by the Unicode Standard.  Note that the BOM (if any) is
   not consumed here.  */
static const char *
decide_utf16_encoding (SCM port, scm_t_port_rw_active mode)
{
  if (mode == SCM_PORT_READ
      && SCM_PORT_GET_INTERNAL (port)->at_stream_start_for_bom_read
      && looking_at_bytes (port, scm_utf16le_bom, sizeof scm_utf16le_bom))
    return "UTF-16LE";
  else
    return "UTF-16BE";
}

/* Decide what byte order to use for a UTF-32 port.  Return "UTF-32BE"
   or "UTF-32LE".  See the comment above 'decide_utf16_encoding' for
   details.  */
static const char *
decide_utf32_encoding (SCM port, scm_t_port_rw_active mode)
{
  if (mode == SCM_PORT_READ
      && SCM_PORT_GET_INTERNAL (port)->at_stream_start_for_bom_read
      && looking_at_bytes (port, scm_utf32le_bom, sizeof scm_utf32le_bom))
    return "UTF-32LE";
  else
    return "UTF-32BE";
}

static void
finalize_iconv_descriptors (void *ptr, void *data)
{
  close_iconv_descriptors (ptr);
}

static scm_t_iconv_descriptors *
open_iconv_descriptors (const char *encoding, int reading, int writing)
{
  scm_t_iconv_descriptors *id;
  iconv_t input_cd, output_cd;
  size_t i;

  input_cd = (iconv_t) -1;
  output_cd = (iconv_t) -1;

  for (i = 0; encoding[i]; i++)
    if (encoding[i] > 127)
      goto invalid_encoding;

  if (reading)
    {
      /* Open an input iconv conversion descriptor, from ENCODING
         to UTF-8.  We choose UTF-8, not UTF-32, because iconv
         implementations can typically convert from anything to
         UTF-8, but not to UTF-32 (see
         <http://lists.gnu.org/archive/html/bug-libunistring/2010-09/msg00007.html>).  */

      /* Assume opening an iconv descriptor causes about 16 KB of
         allocation.  */
      scm_gc_register_allocation (16 * 1024);

      input_cd = iconv_open ("UTF-8", encoding);
      if (input_cd == (iconv_t) -1)
        goto invalid_encoding;
    }

  if (writing)
    {
      /* Assume opening an iconv descriptor causes about 16 KB of
         allocation.  */
      scm_gc_register_allocation (16 * 1024);

      output_cd = iconv_open (encoding, "UTF-8");
      if (output_cd == (iconv_t) -1)
        {
          if (input_cd != (iconv_t) -1)
            iconv_close (input_cd);
          goto invalid_encoding;
        }
    }

  id = scm_gc_malloc_pointerless (sizeof (*id), "iconv descriptors");
  id->input_cd = input_cd;
  id->output_cd = output_cd;

  /* Register a finalizer to close the descriptors.  */
  scm_i_set_finalizer (id, finalize_iconv_descriptors, NULL);

  return id;

 invalid_encoding:
  {
    SCM err;
    err = scm_from_latin1_string (encoding);
    scm_misc_error ("open_iconv_descriptors",
		    "invalid or unknown character encoding ~s",
		    scm_list_1 (err));
  }
}

static void
close_iconv_descriptors (scm_t_iconv_descriptors *id)
{
  if (id->input_cd != (iconv_t) -1)
    iconv_close (id->input_cd);
  if (id->output_cd != (iconv_t) -1)
    iconv_close (id->output_cd);
  id->input_cd = (void *) -1;
  id->output_cd = (void *) -1;
}

scm_t_iconv_descriptors *
scm_i_port_iconv_descriptors (SCM port, scm_t_port_rw_active mode)
{
  scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (port);

  assert (pti->encoding_mode == SCM_PORT_ENCODING_MODE_ICONV);

  if (!pti->iconv_descriptors)
    {
      scm_t_port *pt = SCM_PTAB_ENTRY (port);
      const char *precise_encoding;

      if (!pt->encoding)
        pt->encoding = "ISO-8859-1";

      /* If the specified encoding is UTF-16 or UTF-32, then make
         that more precise by deciding what byte order to use. */
      if (strcmp (pt->encoding, "UTF-16") == 0)
        precise_encoding = decide_utf16_encoding (port, mode);
      else if (strcmp (pt->encoding, "UTF-32") == 0)
        precise_encoding = decide_utf32_encoding (port, mode);
      else
        precise_encoding = pt->encoding;

      pti->iconv_descriptors =
        open_iconv_descriptors (precise_encoding,
                                SCM_INPUT_PORT_P (port),
                                SCM_OUTPUT_PORT_P (port));
    }

  return pti->iconv_descriptors;
}

/* The name of the encoding is itself encoded in ASCII.  */
void
scm_i_set_port_encoding_x (SCM port, const char *encoding)
{
  scm_t_port *pt;
  scm_t_port_internal *pti;
  scm_t_iconv_descriptors *prev;

  /* Set the character encoding for this port.  */
  pt = SCM_PTAB_ENTRY (port);
  pti = SCM_PORT_GET_INTERNAL (port);
  prev = pti->iconv_descriptors;

  /* In order to handle cases where the encoding changes mid-stream
     (e.g. within an HTTP stream, or within a file that is composed of
     segments with different encodings), we consider this to be "stream
     start" for purposes of BOM handling, regardless of our actual file
     position. */
  pti->at_stream_start_for_bom_read  = 1;
  pti->at_stream_start_for_bom_write = 1;

  if (encoding_matches (encoding, "UTF-8"))
    {
      pt->encoding = "UTF-8";
      pti->encoding_mode = SCM_PORT_ENCODING_MODE_UTF8;
    }
  else if (encoding_matches (encoding, "ISO-8859-1"))
    {
      pt->encoding = "ISO-8859-1";
      pti->encoding_mode = SCM_PORT_ENCODING_MODE_LATIN1;
    }
  else
    {
      pt->encoding = canonicalize_encoding (encoding);
      pti->encoding_mode = SCM_PORT_ENCODING_MODE_ICONV;
    }

  pti->iconv_descriptors = NULL;
  if (prev)
    close_iconv_descriptors (prev);
}

SCM_DEFINE (scm_port_encoding, "port-encoding", 1, 0, 0,
	    (SCM port),
	    "Returns, as a string, the character encoding that @var{port}\n"
	    "uses to interpret its input and output.\n")
#define FUNC_NAME s_scm_port_encoding
{
  SCM_VALIDATE_PORT (1, port);

  return scm_from_latin1_string (SCM_PTAB_ENTRY (port)->encoding);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_port_encoding_x, "set-port-encoding!", 2, 0, 0,
	    (SCM port, SCM enc),
	    "Sets the character encoding that will be used to interpret all\n"
	    "port I/O.  New ports are created with the encoding\n"
	    "appropriate for the current locale if @code{setlocale} has \n"
	    "been called or ISO-8859-1 otherwise\n"
	    "and this procedure can be used to modify that encoding.\n")
#define FUNC_NAME s_scm_set_port_encoding_x
{
  char *enc_str;

  SCM_VALIDATE_PORT (1, port);
  SCM_VALIDATE_STRING (2, enc);

  enc_str = scm_to_latin1_string (enc);
  scm_i_set_port_encoding_x (port, enc_str);
  free (enc_str);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

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
  scm_t_string_failed_conversion_handler h;

  if (scm_is_false (port))
    h = scm_i_default_port_conversion_handler ();
  else
    {
      scm_t_port *pt;

      SCM_VALIDATE_OPPORT (1, port);
      pt = SCM_PTAB_ENTRY (port);

      h = pt->ilseq_handler;
    }

  if (h == SCM_FAILED_CONVERSION_ERROR)
    return scm_from_latin1_symbol ("error");
  else if (h == SCM_FAILED_CONVERSION_QUESTION_MARK)
    return scm_from_latin1_symbol ("substitute");
  else if (h == SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE)
    return scm_from_latin1_symbol ("escape");
  else
    abort ();

  /* Never gets here. */
  return SCM_UNDEFINED;
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
  scm_t_string_failed_conversion_handler handler;

  if (scm_is_eq (sym, sym_error))
    handler = SCM_FAILED_CONVERSION_ERROR;
  else if (scm_is_eq (sym, sym_substitute))
    handler = SCM_FAILED_CONVERSION_QUESTION_MARK;
  else if (scm_is_eq (sym, sym_escape))
    handler = SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE;
  else
    SCM_MISC_ERROR ("unknown conversion strategy ~s", scm_list_1 (sym));

  if (scm_is_false (port))
    scm_i_set_default_port_conversion_handler (handler);
  else
    {
      SCM_VALIDATE_OPPORT (1, port);
      SCM_PTAB_ENTRY (port)->ilseq_handler = handler;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* The port lock.  */

static void
lock_port (void *mutex)
{
  scm_i_pthread_mutex_lock ((scm_i_pthread_mutex_t *) mutex);
}

static void
unlock_port (void *mutex)
{
  scm_i_pthread_mutex_unlock ((scm_i_pthread_mutex_t *) mutex);
}

void
scm_dynwind_lock_port (SCM port)
#define FUNC_NAME "dynwind-lock-port"
{
  scm_i_pthread_mutex_t *lock;
  SCM_VALIDATE_OPPORT (SCM_ARG1, port);
  scm_c_lock_port (port, &lock);
  if (lock)
    {
      scm_dynwind_unwind_handler (unlock_port, lock, SCM_F_WIND_EXPLICITLY);
      scm_dynwind_rewind_handler (lock_port, lock, 0);
    }
}
#undef FUNC_NAME




/* Input.  */

static int
get_byte_or_eof (SCM port)
{
  SCM buf = SCM_PTAB_ENTRY (port)->read_buf;
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

  buf = scm_fill_input (port);
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
  SCM buf = SCM_PTAB_ENTRY (port)->read_buf;
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

  buf = scm_fill_input (port);
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
scm_i_read_bytes_unlocked (SCM port, SCM dst, size_t start, size_t count)
{
  size_t filled;
  scm_t_ptob_descriptor *ptob = SCM_PORT_DESCRIPTOR (port);

  assert (count <= SCM_BYTEVECTOR_LENGTH (dst));
  assert (start + count <= SCM_BYTEVECTOR_LENGTH (dst));

  filled = ptob->read (port, dst, start, count);

  assert (filled <= count);

  return filled;
}

/* scm_i_read_unlocked is used internally to add bytes to the given port
   buffer.  If the number of available bytes in the buffer does not
   increase after a call to scm_i_read_unlocked, that indicates EOF.  */
static void
scm_i_read_unlocked (SCM port, SCM buf)
{
  size_t count;

  count = scm_i_read_bytes_unlocked (port, scm_port_buffer_bytevector (buf),
                                     scm_to_size_t (scm_port_buffer_end (buf)),
                                     scm_port_buffer_can_put (buf));
  scm_port_buffer_did_put (buf, count);
  scm_port_buffer_set_has_eof_p (buf, scm_from_bool (count == 0));
}

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

  pt = SCM_PTAB_ENTRY (port);
  read_buf = pt->read_buf;

  if (pt->rw_random)
    scm_flush (port);

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
          read_buf = scm_fill_input_unlocked (port);
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
          did_read = scm_i_read_bytes_unlocked (port, dst,
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

  pt = SCM_PTAB_ENTRY (port);
  read_buf = pt->read_buf;

  if (pt->rw_random)
    scm_flush (port);

  while (copied < size)
    {
      size_t count;
      read_buf = scm_fill_input (port);
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
update_port_lf (scm_t_wchar c, SCM port)
{
  switch (c)
    {
    case '\a':
    case EOF:
      break;
    case '\b':
      SCM_DECCOL (port);
      break;
    case '\n':
      SCM_INCLINE (port);
      break;
    case '\r':
      SCM_ZEROCOL (port);
      break;
    case '\t':
      SCM_TABCOL (port);
      break;
    default:
      SCM_INCCOL (port);
      break;
    }
}

#define SCM_MBCHAR_BUF_SIZE (4)

/* Convert the SIZE-byte UTF-8 sequence in UTF8_BUF to a codepoint.
   UTF8_BUF is assumed to contain a valid UTF-8 sequence.  */
static scm_t_wchar
utf8_to_codepoint (const scm_t_uint8 *utf8_buf, size_t size)
{
  scm_t_wchar codepoint;

  if (utf8_buf[0] <= 0x7f)
    {
      assert (size == 1);
      codepoint = utf8_buf[0];
    }
  else if ((utf8_buf[0] & 0xe0) == 0xc0)
    {
      assert (size == 2);
      codepoint = ((scm_t_wchar) utf8_buf[0] & 0x1f) << 6UL
	| (utf8_buf[1] & 0x3f);
    }
  else if ((utf8_buf[0] & 0xf0) == 0xe0)
    {
      assert (size == 3);
      codepoint = ((scm_t_wchar) utf8_buf[0] & 0x0f) << 12UL
	| ((scm_t_wchar) utf8_buf[1] & 0x3f) << 6UL
	| (utf8_buf[2] & 0x3f);
    }
  else
    {
      assert (size == 4);
      codepoint = ((scm_t_wchar) utf8_buf[0] & 0x07) << 18UL
	| ((scm_t_wchar) utf8_buf[1] & 0x3f) << 12UL
	| ((scm_t_wchar) utf8_buf[2] & 0x3f) << 6UL
	| (utf8_buf[3] & 0x3f);
    }

  return codepoint;
}

/* Read a UTF-8 sequence from PORT.  On success, return 0 and set
   *CODEPOINT to the codepoint that was read, fill BUF with its UTF-8
   representation, and set *LEN to the length in bytes.  Return
   `EILSEQ' on error.  */
static int
get_utf8_codepoint (SCM port, scm_t_wchar *codepoint,
		    scm_t_uint8 buf[SCM_MBCHAR_BUF_SIZE], size_t *len)
{
#define ASSERT_NOT_EOF(b)			\
  if (SCM_UNLIKELY ((b) == EOF))		\
    goto invalid_seq
#define CONSUME_PEEKED_BYTE()				\
  scm_port_buffer_did_take (pt->read_buf, 1)

  int byte;
  scm_t_port *pt;

  *len = 0;
  pt = SCM_PTAB_ENTRY (port);

  byte = get_byte_or_eof (port);
  if (byte == EOF)
    {
      *codepoint = EOF;
      return 0;
    }

  buf[0] = (scm_t_uint8) byte;
  *len = 1;

  if (buf[0] <= 0x7f)
    /* 1-byte form.  */
    *codepoint = buf[0];
  else if (buf[0] >= 0xc2 && buf[0] <= 0xdf)
    {
      /* 2-byte form.  */
      byte = peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[1] = (scm_t_uint8) byte;
      *len = 2;

      *codepoint = ((scm_t_wchar) buf[0] & 0x1f) << 6UL
	| (buf[1] & 0x3f);
    }
  else if ((buf[0] & 0xf0) == 0xe0)
    {
      /* 3-byte form.  */
      byte = peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80
			|| (buf[0] == 0xe0 && byte < 0xa0)
			|| (buf[0] == 0xed && byte > 0x9f)))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[1] = (scm_t_uint8) byte;
      *len = 2;

      byte = peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[2] = (scm_t_uint8) byte;
      *len = 3;

      *codepoint = ((scm_t_wchar) buf[0] & 0x0f) << 12UL
	| ((scm_t_wchar) buf[1] & 0x3f) << 6UL
	| (buf[2] & 0x3f);
    }
  else if (buf[0] >= 0xf0 && buf[0] <= 0xf4)
    {
      /* 4-byte form.  */
      byte = peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY (((byte & 0xc0) != 0x80)
			|| (buf[0] == 0xf0 && byte < 0x90)
			|| (buf[0] == 0xf4 && byte > 0x8f)))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[1] = (scm_t_uint8) byte;
      *len = 2;

      byte = peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[2] = (scm_t_uint8) byte;
      *len = 3;

      byte = peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[3] = (scm_t_uint8) byte;
      *len = 4;

      *codepoint = ((scm_t_wchar) buf[0] & 0x07) << 18UL
	| ((scm_t_wchar) buf[1] & 0x3f) << 12UL
	| ((scm_t_wchar) buf[2] & 0x3f) << 6UL
	| (buf[3] & 0x3f);
    }
  else
    goto invalid_seq;

  return 0;

 invalid_seq:
  /* Here we could choose the consume the faulty byte when it's not a
     valid starting byte, but it's not a requirement.  What Section 3.9
     of Unicode 6.0.0 mandates, though, is to not consume a byte that
     would otherwise be a valid starting byte.  */

  return EILSEQ;

#undef CONSUME_PEEKED_BYTE
#undef ASSERT_NOT_EOF
}

/* Read an ISO-8859-1 codepoint (a byte) from PORT.  On success, return
   0 and set *CODEPOINT to the codepoint that was read, fill BUF with
   its UTF-8 representation, and set *LEN to the length in bytes.
   Return `EILSEQ' on error.  */
static int
get_latin1_codepoint (SCM port, scm_t_wchar *codepoint,
                      char buf[SCM_MBCHAR_BUF_SIZE], size_t *len)
{
  *codepoint = get_byte_or_eof (port);

  if (*codepoint == EOF)
    *len = 0;
  else
    {
      *len = 1;
      buf[0] = *codepoint;
    }
  return 0;
}

/* Likewise, read a byte sequence from PORT, passing it through its
   input conversion descriptor.  */
static int
get_iconv_codepoint (SCM port, scm_t_wchar *codepoint,
		     char buf[SCM_MBCHAR_BUF_SIZE], size_t *len)
{
  scm_t_iconv_descriptors *id;
  scm_t_uint8 utf8_buf[SCM_MBCHAR_BUF_SIZE];
  size_t input_size = 0;

  id = scm_i_port_iconv_descriptors (port, SCM_PORT_READ);

  for (;;)
    {
      int byte_read;
      char *input, *output;
      size_t input_left, output_left, done;

      byte_read = get_byte_or_eof (port);
      if (SCM_UNLIKELY (byte_read == EOF))
	{
          if (SCM_LIKELY (input_size == 0))
            {
              *codepoint = (scm_t_wchar) EOF;
              *len = input_size;
              return 0;
            }
          else
            {
              /* EOF found in the middle of a multibyte character. */
              scm_i_set_pending_eof (port);
              return EILSEQ;
            }
	}

      buf[input_size++] = byte_read;

      input = buf;
      input_left = input_size;
      output = (char *) utf8_buf;
      output_left = sizeof (utf8_buf);

      done = iconv (id->input_cd, &input, &input_left, &output, &output_left);

      if (done == (size_t) -1)
	{
	  int err = errno;
	  if (SCM_LIKELY (err == EINVAL))
            /* The input byte sequence did not form a complete
               character.  Read another byte and try again. */
            continue;
          else
            return err;
	}
      else
        {
          size_t output_size = sizeof (utf8_buf) - output_left;
          if (SCM_LIKELY (output_size > 0))
            {
              /* iconv generated output.  Convert the UTF8_BUF sequence
                 to a Unicode code point.  */
              *codepoint = utf8_to_codepoint (utf8_buf, output_size);
              *len = input_size;
              return 0;
            }
          else
            {
              /* iconv consumed some bytes without producing any output.
                 Most likely this means that a Unicode byte-order mark
                 (BOM) was consumed, which should not be included in the
                 returned buf.  Shift any remaining bytes to the beginning
                 of buf, and continue the loop. */
              memmove (buf, input, input_left);
              input_size = input_left;
              continue;
            }
        }
    }
}

/* Read a codepoint from PORT and return it in *CODEPOINT.  Fill BUF
   with the byte representation of the codepoint in PORT's encoding, and
   set *LEN to the length in bytes of that representation.  Return 0 on
   success and an errno value on error.  */
static SCM_C_INLINE int
get_codepoint (SCM port, scm_t_wchar *codepoint,
	       char buf[SCM_MBCHAR_BUF_SIZE], size_t *len)
{
  int err;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (port);

  if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_UTF8)
    err = get_utf8_codepoint (port, codepoint, (scm_t_uint8 *) buf, len);
  else if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_LATIN1)
    err = get_latin1_codepoint (port, codepoint, buf, len);
  else
    err = get_iconv_codepoint (port, codepoint, buf, len);

  if (SCM_LIKELY (err == 0))
    {
      if (SCM_UNLIKELY (pti->at_stream_start_for_bom_read))
        {
          /* Record that we're no longer at stream start. */
          pti->at_stream_start_for_bom_read = 0;
          if (pt->rw_random)
            pti->at_stream_start_for_bom_write = 0;

          /* If we just read a BOM in an encoding that recognizes them,
             then silently consume it and read another code point. */
          if (SCM_UNLIKELY
              (*codepoint == SCM_UNICODE_BOM
               && (pti->encoding_mode == SCM_PORT_ENCODING_MODE_UTF8
                   || strcmp (pt->encoding, "UTF-16") == 0
                   || strcmp (pt->encoding, "UTF-32") == 0)))
            return get_codepoint (port, codepoint, buf, len);
        }
      update_port_lf (*codepoint, port);
    }
  else if (pt->ilseq_handler == SCM_ICONVEH_QUESTION_MARK)
    {
      *codepoint = '?';
      err = 0;
      update_port_lf (*codepoint, port);
    }

  return err;
}

/* Read a codepoint from PORT and return it.  */
scm_t_wchar
scm_getc (SCM port)
#define FUNC_NAME "scm_getc"
{
  int err;
  size_t len;
  scm_t_wchar codepoint;
  char buf[SCM_MBCHAR_BUF_SIZE];

  err = get_codepoint (port, &codepoint, buf, &len);
  if (SCM_UNLIKELY (err != 0))
    /* At this point PORT should point past the invalid encoding, as per
       R6RS-lib Section 8.2.4.  */
    scm_decoding_error (FUNC_NAME, err, "input decoding error", port);

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
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
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
          SCM new_buf;

          while (size < len + buffered)
            size *= 2;

          new_buf = scm_c_make_port_buffer (size);
          scm_port_buffer_reset_end (new_buf);
          scm_port_buffer_set_has_eof_p (new_buf,
                                         scm_port_buffer_has_eof_p (read_buf));
          scm_port_buffer_putback (new_buf,
                                   scm_port_buffer_take_pointer (read_buf),
                                   buffered);
          pt->read_buf = read_buf = new_buf;
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
scm_ungetc_unlocked (scm_t_wchar c, SCM port)
#define FUNC_NAME "scm_ungetc"
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (port);
  char *result;
  char result_buf[10];
  size_t len;

  len = sizeof (result_buf);

  if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_UTF8)
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
  else if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_LATIN1 && c <= 0xff)
    {
      result_buf[0] = (char) c;
      result = result_buf;
      len = 1;
    }
  else
    result = u32_conv_to_encoding (pt->encoding,
                                   (enum iconv_ilseq_handler) pt->ilseq_handler,
                                   (uint32_t *) &c, 1, NULL,
                                   result_buf, &len);

  if (SCM_UNLIKELY (result == NULL || len == 0))
    scm_encoding_error (FUNC_NAME, errno,
			"conversion to port encoding failed",
			SCM_BOOL_F, SCM_MAKE_CHAR (c));

  scm_unget_bytes ((unsigned char *) result, len, port);

  if (SCM_UNLIKELY (result != result_buf))
    free (result);

  if (c == '\n')
    SCM_LINUM (port) -= 1;
  SCM_DECCOL (port);
}
#undef FUNC_NAME

void 
scm_ungetc (scm_t_wchar c, SCM port)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_ungetc_unlocked (c, port);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
}

void 
scm_ungets_unlocked (const char *s, int n, SCM port)
{
  /* This is simple minded and inefficient, but unreading strings is
   * probably not a common operation, and remember that line and
   * column numbers have to be handled...
   *
   * Please feel free to write an optimized version!
   */
  while (n--)
    scm_ungetc_unlocked (s[n], port);
}

void
scm_ungets (const char *s, int n, SCM port)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_ungets_unlocked (s, n, port);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
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
  int err;
  SCM result;
  scm_t_wchar c;
  char bytes[SCM_MBCHAR_BUF_SIZE];
  long column, line;
  size_t len = 0;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  column = SCM_COL (port);
  line = SCM_LINUM (port);

  err = get_codepoint (port, &c, bytes, &len);

  scm_unget_bytes ((unsigned char *) bytes, len, port);

  SCM_COL (port) = column;
  SCM_LINUM (port) = line;

  if (SCM_UNLIKELY (err != 0))
    {
      scm_decoding_error (FUNC_NAME, err, "input decoding error", port);

      /* Shouldn't happen since `catch' always aborts to prompt.  */
      result = SCM_BOOL_F;
    }
  else if (c == EOF)
    {
      scm_i_set_pending_eof (port);
      result = SCM_EOF_VAL;
    }
  else
    result = SCM_MAKE_CHAR (c);

  return result;
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

  scm_ungetc_unlocked (c, port);
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
    scm_ungetc_unlocked (scm_i_string_ref (str, n), port);
  
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
  scm_t_ptob_descriptor *ptob;
  scm_t_bits tag_word;
  size_t read_buf_size, write_buf_size;
  SCM saved_read_buf;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPENPORT (1, port);
  pt = SCM_PTAB_ENTRY (port);
  ptob = SCM_PORT_DESCRIPTOR (port);
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
  pt->read_buf = scm_c_make_port_buffer (read_buf_size);
  pt->write_buf = scm_c_make_port_buffer (write_buf_size);

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

SCM
scm_fill_input (SCM port)
{
  scm_i_pthread_mutex_t *lock;
  SCM ret;
  
  scm_c_lock_port (port, &lock);
  ret = scm_fill_input_unlocked (port);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);

  return ret;
}

/* Move up to READ_LEN bytes from PORT's read buffer into memory
   starting at DEST.  Return the number of bytes moved.  PORT's
   line/column numbers are left unchanged.  */
size_t
scm_take_from_input_buffers (SCM port, char *dest, size_t read_len)
{
  SCM read_buf = SCM_PTAB_ENTRY (port)->read_buf;
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
  SCM result;
  scm_t_port *pt;
  SCM read_buf;
  long count;

  SCM_VALIDATE_OPINPORT (1, port);
  pt = SCM_PTAB_ENTRY (port);
  read_buf = pt->read_buf;
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
  scm_t_port *pt;
  SCM buf;
  size_t discarded;

  pt = SCM_PTAB_ENTRY (port);

  scm_i_pthread_mutex_lock (pt->lock);
  buf = SCM_PTAB_ENTRY (port)->read_buf;
  discarded = scm_port_buffer_take (buf, NULL, (size_t) -1);
  scm_i_pthread_mutex_unlock (pt->lock);

  if (discarded != 0)
    SCM_PORT_DESCRIPTOR (port)->seek (port, -discarded, SEEK_CUR);
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

static void scm_i_write_unlocked (SCM port, SCM buf);

void
scm_flush (SCM port)
{
  SCM buf = SCM_PTAB_ENTRY (port)->write_buf;
  if (scm_port_buffer_can_take (buf))
    scm_i_write_unlocked (port, buf);
}

SCM
scm_fill_input_unlocked (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  SCM read_buf = pt->read_buf;

  if (scm_port_buffer_can_take (read_buf) ||
      scm_is_true (scm_port_buffer_has_eof_p (read_buf)))
    return read_buf;

  if (pt->rw_random)
    scm_flush (pt->port);

  /* It could be that putback caused us to enlarge the buffer; now that
     we've read all the bytes we need to shrink it again.  */
  if (scm_port_buffer_size (read_buf) != pt->read_buffering)
    read_buf = pt->read_buf = scm_c_make_port_buffer (pt->read_buffering);
  else
    scm_port_buffer_reset (read_buf);

  scm_i_read_unlocked (port, read_buf);

  return read_buf;
}

SCM_DEFINE (scm_port_read_buffer, "port-read-buffer", 1, 0, 0,
            (SCM port),
	    "Return the read buffer for a port.  If the port is\n"
            "random-access, its write buffer, if any, will be flushed\n"
            "if needed.")
#define FUNC_NAME s_scm_port_read_buffer
{
  scm_t_port *pt;

  SCM_VALIDATE_OPINPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_random)
    scm_flush (pt->port);

  return pt->read_buf;
}
#undef FUNC_NAME

SCM_DEFINE (scm_port_write_buffer, "port-write-buffer", 1, 0, 0,
            (SCM port),
	    "Return the write buffer for a port.  If the port is\n"
            "random-access, its read buffer, if any, will be discarded\n"
            "if needed.")
#define FUNC_NAME s_scm_port_write_buffer
{
  scm_t_port *pt;

  SCM_VALIDATE_OPOUTPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_random)
    scm_end_input (pt->port);

  return pt->write_buf;
}
#undef FUNC_NAME




/* Output.  */

void
scm_putc (char c, SCM port)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_putc_unlocked (c, port);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
}

void
scm_puts (const char *s, SCM port)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_puts_unlocked (s, port);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
}
  
static void
scm_i_write_bytes_unlocked (SCM port, SCM src, size_t start, size_t count)
{
  size_t written = 0;
  scm_t_ptob_descriptor *ptob = SCM_PORT_DESCRIPTOR (port);

  assert (count <= SCM_BYTEVECTOR_LENGTH (src));
  assert (start + count <= SCM_BYTEVECTOR_LENGTH (src));

  do
    written += ptob->write (port, src, start + written, count - written);
  while (written < count);

  assert (written == count);
}

static void
scm_i_write_unlocked (SCM port, SCM buf)
{
  size_t start, count;

  /* Update cursors before attempting to write, assuming that I/O errors
     are sticky.  That way if the write throws an error, causing the
     computation to abort, and possibly causing the port to be collected
     by GC when it's open, any subsequent close-port / force-output
     won't signal *another* error.  */

  start = scm_to_size_t (scm_port_buffer_cur (buf));
  count = scm_port_buffer_can_take (buf);
  scm_port_buffer_reset (buf);
  scm_i_write_bytes_unlocked (port, scm_port_buffer_bytevector (buf), start,
                              count);
}

/* Used by an application to write arbitrary number of bytes to an SCM
   port.  Similar semantics as libc write.  However, unlike libc write,
   scm_c_write writes the requested number of bytes.

   Warning: Doesn't update port line and column counts!  */
static size_t
scm_c_write_bytes_unlocked (SCM port, SCM src, size_t start, size_t count)
#define FUNC_NAME "scm_c_write_bytes"
{
  scm_t_port *pt;
  SCM write_buf;

  SCM_VALIDATE_OPOUTPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
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
        scm_i_write_unlocked (port, write_buf);

      {
        signed char *src_ptr = SCM_BYTEVECTOR_CONTENTS (src) + start;
        scm_port_buffer_put (write_buf, (scm_t_uint8 *) src_ptr, count);
      }

      if (scm_port_buffer_can_put (write_buf) == 0)
        scm_i_write_unlocked (port, write_buf);
    }
  else
    {
      /* Our write would overflow the buffer.  Flush buffered bytes (if
         needed), then write our bytes with just one syscall.  */
      if (scm_port_buffer_can_take (write_buf))
        scm_i_write_unlocked (port, write_buf);

      scm_i_write_bytes_unlocked (port, src, start, count);
    }

  return count;
}
#undef FUNC_NAME

/* Like scm_c_write_bytes, but always writes through the write buffer.
   Used when an application wants to write bytes stored in an area not
   managed by GC.  */
void
scm_c_write_unlocked (SCM port, const void *ptr, size_t size)
#define FUNC_NAME "scm_c_write"
{
  scm_t_port *pt;
  SCM write_buf;
  size_t written = 0;
  const scm_t_uint8 *src = ptr;

  SCM_VALIDATE_OPOUTPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  write_buf = pt->write_buf;

  if (pt->rw_random)
    scm_end_input (port);

  while (written < size)
    {
      size_t did_put = scm_port_buffer_put (write_buf, src, size - written);
      written += did_put;
      src += did_put;
      if (scm_port_buffer_can_put (write_buf) == 0)
        scm_i_write_unlocked (port, write_buf);
    }
}
#undef FUNC_NAME

void
scm_c_write (SCM port, const void *ptr, size_t size)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_c_write_unlocked (port, ptr, size);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
}

void
scm_c_write_bytes (SCM port, SCM src, size_t start, size_t count)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_c_write_bytes_unlocked (port, src, start, count);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
}

/* scm_lfwrite
 *
 * This function differs from scm_c_write; it updates port line and
 * column, flushing line-buffered ports when appropriate. */
void
scm_lfwrite_unlocked (const char *ptr, size_t size, SCM port)
{
  int saved_line;

  scm_c_write_unlocked (port, ptr, size);

  saved_line = SCM_LINUM (port);
  for (; size; ptr++, size--)
    update_port_lf ((scm_t_wchar) (unsigned char) *ptr, port);

  /* Handle line buffering.  */
  if ((SCM_CELL_WORD_0 (port) & SCM_BUFLINE) && saved_line != SCM_LINUM (port))
    scm_flush (port);
}

void
scm_lfwrite (const char *ptr, size_t size, SCM port)
{
  scm_i_pthread_mutex_t *lock;
  scm_c_lock_port (port, &lock);
  scm_lfwrite_unlocked (ptr, size, port);
  if (lock)
    scm_i_pthread_mutex_unlock (lock);
}

/* Write STR to PORT from START inclusive to END exclusive.  */
void
scm_lfwrite_substr (SCM str, size_t start, size_t end, SCM port)
{
  if (end == (size_t) -1)
    end = scm_i_string_length (str);

  scm_i_display_substring (str, start, end, port);
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
  scm_t_port *pt;
  SCM read_buf;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  /* It's possible to close the current input port, so validate even in
     this case. */
  SCM_VALIDATE_OPINPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  read_buf = pt->read_buf;

  if (scm_port_buffer_can_take (read_buf) ||
      scm_is_true (scm_port_buffer_has_eof_p (read_buf)))
    /* FIXME: Verify that a whole character is available?  */
    return SCM_BOOL_T;
  else
    {
      scm_t_ptob_descriptor *ptob = SCM_PORT_DESCRIPTOR (port);
      
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
      scm_t_port *pt = SCM_PTAB_ENTRY (fd_port);
      scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (fd_port);
      scm_t_ptob_descriptor *ptob = SCM_PORT_DESCRIPTOR (fd_port);
      off_t_or_off64_t off = scm_to_off_t_or_off64_t (offset);
      off_t_or_off64_t rv;

      if (!ptob->seek || !pt->rw_random)
	SCM_MISC_ERROR ("port is not seekable", 
                        scm_cons (fd_port, SCM_EOL));

      /* FIXME: Avoid flushing buffers for SEEK_CUR with an offset of
         0.  */

      scm_end_input (pt->port);
      scm_flush (pt->port);

      rv = ptob->seek (fd_port, off, how);

      /* Set stream-start flags according to new position. */
      pti->at_stream_start_for_bom_read  = (rv == 0);
      pti->at_stream_start_for_bom_write = (rv == 0);

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
      scm_t_ptob_descriptor *ptob = SCM_PORT_DESCRIPTOR (object);

      if (!ptob->truncate)
	SCM_MISC_ERROR ("port is not truncatable", SCM_EOL);

      scm_i_clear_pending_eof (object);

      if (SCM_INPUT_PORT_P (object) && SCM_PTAB_ENTRY (object)->rw_random)
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
  return scm_from_long (SCM_LINUM (port));
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
  SCM_PTAB_ENTRY (port)->line_number = scm_to_long (line);
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
  return scm_from_int (SCM_COL (port));
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
  SCM_PTAB_ENTRY (port)->column_number = scm_to_int (column);
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
  scm_puts_unlocked (SCM_CLOSEDP (exp)
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
  char *type = SCM_PTOBNAME (SCM_PTOBNUM (exp));
  if (!type)
    type = "port";
  scm_puts_unlocked ("#<", port);
  scm_print_port_mode (exp, port);
  scm_puts_unlocked (type, port);
  scm_putc_unlocked (' ', port);
  scm_uintprint (SCM_CELL_WORD_1 (exp), 16, port);
  scm_putc_unlocked ('>', port);
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

scm_t_bits scm_tc16_void_port = 0;

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
  return scm_c_make_port (scm_tc16_void_port, mode_bits, 0);
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

  /* lseek() symbols.  */
  scm_c_define ("SEEK_SET", scm_from_int (SEEK_SET));
  scm_c_define ("SEEK_CUR", scm_from_int (SEEK_CUR));
  scm_c_define ("SEEK_END", scm_from_int (SEEK_END));

  /* These bindings are used when boot-9 turns `current-input-port' et
     al into parameters.  They are then removed from the guile module.  */
  scm_c_define ("%current-input-port-fluid", cur_inport_fluid);
  scm_c_define ("%current-output-port-fluid", cur_outport_fluid);
  scm_c_define ("%current-error-port-fluid", cur_errport_fluid);
  scm_c_define ("%current-warning-port-fluid", cur_warnport_fluid);
}

void
scm_init_ports (void)
{
  scm_tc16_void_port = scm_make_port_type ("void", void_port_read,
					   void_port_write);

  scm_i_port_weak_set = scm_c_make_weak_set (31);

  cur_inport_fluid = scm_make_fluid ();
  cur_outport_fluid = scm_make_fluid ();
  cur_errport_fluid = scm_make_fluid ();
  cur_warnport_fluid = scm_make_fluid ();
  cur_loadport_fluid = scm_make_fluid ();

  sym_substitute = scm_from_latin1_symbol ("substitute");
  sym_escape = scm_from_latin1_symbol ("escape");
  sym_error = scm_from_latin1_symbol ("error");

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
  scm_c_define_gsubr (s_scm_set_port_encoding_x, 2, 0, 0,
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
