/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2006,
 *   2007, 2008, 2009, 2010, 2011, 2012, 2013,
 *   2014 Free Software Foundation, Inc.
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
#include <c-strcase.h>

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
#include "libguile/weaks.h"
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


/* The port kind table --- a dynamically resized array of port types.  */


/* scm_ptobs scm_numptob
 * implement a dynamically resized array of ptob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
scm_t_ptob_descriptor *scm_ptobs = NULL;
long scm_numptob = 0;

/* GC marker for a port with stream of SCM type.  */
SCM 
scm_markstream (SCM ptr)
{
  int openp;
  openp = SCM_CELL_WORD_0 (ptr) & SCM_OPN;
  if (openp)
    return SCM_PACK (SCM_STREAM (ptr));
  else
    return SCM_BOOL_F;
}

/*
 * We choose to use an interface similar to the smob interface with
 * fill_input and write as standard fields, passed to the port
 * type constructor, and optional fields set by setters.
 */

static void
flush_port_default (SCM port SCM_UNUSED)
{
}

static void
end_input_default (SCM port SCM_UNUSED, int offset SCM_UNUSED)
{
}

scm_t_bits
scm_make_port_type (char *name,
		    int (*fill_input) (SCM port),
		    void (*write) (SCM port, const void *data, size_t size))
{
  char *tmp;
  if (SCM_I_MAX_PORT_TYPE_COUNT - 1 <= scm_numptob)
    goto ptoberr;
  SCM_CRITICAL_SECTION_START;
  tmp = (char *) scm_gc_realloc ((char *) scm_ptobs,
				 scm_numptob * sizeof (scm_t_ptob_descriptor),
				 (1 + scm_numptob)
				 * sizeof (scm_t_ptob_descriptor),
				 "port-type");
  if (tmp)
    {
      scm_ptobs = (scm_t_ptob_descriptor *) tmp;

      scm_ptobs[scm_numptob].name = name;
      scm_ptobs[scm_numptob].mark = 0;
      scm_ptobs[scm_numptob].free = NULL;
      scm_ptobs[scm_numptob].print = scm_port_print;
      scm_ptobs[scm_numptob].equalp = 0;
      scm_ptobs[scm_numptob].close = 0;

      scm_ptobs[scm_numptob].write = write;
      scm_ptobs[scm_numptob].flush = flush_port_default;

      scm_ptobs[scm_numptob].end_input = end_input_default;
      scm_ptobs[scm_numptob].fill_input = fill_input;
      scm_ptobs[scm_numptob].input_waiting = 0;

      scm_ptobs[scm_numptob].seek = 0;
      scm_ptobs[scm_numptob].truncate = 0;

      scm_numptob++;
    }
  SCM_CRITICAL_SECTION_END;
  if (!tmp)
    {
    ptoberr:
      scm_memory_error ("scm_make_port_type");
    }
  /* Make a class object if Goops is present */
  if (SCM_UNPACK (scm_port_class[0]) != 0)
    scm_make_port_classes (scm_numptob - 1, SCM_PTOBNAME (scm_numptob - 1));
  return scm_tc7_port + (scm_numptob - 1) * 256;
}

void
scm_set_port_mark (scm_t_bits tc, SCM (*mark) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].mark = mark;
}

void
scm_set_port_free (scm_t_bits tc, size_t (*free) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].free = free;
}

void
scm_set_port_print (scm_t_bits tc, int (*print) (SCM exp, SCM port,
					   scm_print_state *pstate))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].print = print;
}

void
scm_set_port_equalp (scm_t_bits tc, SCM (*equalp) (SCM, SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].equalp = equalp;
}

void
scm_set_port_flush (scm_t_bits tc, void (*flush) (SCM port))
{
   scm_ptobs[SCM_TC2PTOBNUM (tc)].flush = flush;
}

void
scm_set_port_end_input (scm_t_bits tc, void (*end_input) (SCM port, int offset))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].end_input = end_input;
}

void
scm_set_port_close (scm_t_bits tc, int (*close) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].close = close;
}

void
scm_set_port_seek (scm_t_bits tc,
		   scm_t_off (*seek) (SCM, scm_t_off, int))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].seek = seek;
}

void
scm_set_port_truncate (scm_t_bits tc, void (*truncate) (SCM, scm_t_off))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].truncate = truncate;
}

void
scm_set_port_input_waiting (scm_t_bits tc, int (*input_waiting) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].input_waiting = input_waiting;
}

static void
scm_i_set_pending_eof (SCM port)
{
  SCM_PORT_GET_INTERNAL (port)->pending_eof = 1;
}

static void
scm_i_clear_pending_eof (SCM port)
{
  SCM_PORT_GET_INTERNAL (port)->pending_eof = 0;
}

SCM_DEFINE (scm_i_port_property, "%port-property", 2, 0, 0,
            (SCM port, SCM key),
            "Return the property of @var{port} associated with @var{key}.")
#define FUNC_NAME s_scm_i_port_property
{
  SCM_VALIDATE_OPPORT (1, port);
  return scm_assq_ref (SCM_PORT_GET_INTERNAL (port)->alist, key);
}
#undef FUNC_NAME

SCM_DEFINE (scm_i_set_port_property_x, "%set-port-property!", 3, 0, 0,
            (SCM port, SCM key, SCM value),
            "Set the property of @var{port} associated with @var{key} to @var{value}.")
#define FUNC_NAME s_scm_i_set_port_property_x
{
  scm_t_port_internal *pti;

  SCM_VALIDATE_OPPORT (1, port);
  pti = SCM_PORT_GET_INTERNAL (port);
  pti->alist = scm_assq_set_x (pti->alist, key, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



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

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  /* It's possible to close the current input port, so validate even in
     this case. */
  SCM_VALIDATE_OPINPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);

  /* if the current read buffer is filled, or the
     last pushed-back char has been read and the saved buffer is
     filled, result is true.  */
  if (pt->read_pos < pt->read_end 
      || (pt->read_buf == pt->putback_buf
	  && pt->saved_read_pos < pt->saved_read_end))
    return SCM_BOOL_T;
  else
    {
      scm_t_ptob_descriptor *ptob = &scm_ptobs[SCM_PTOBNUM (port)];
      
      if (ptob->input_waiting)
	return scm_from_bool(ptob->input_waiting (port));
      else
	return SCM_BOOL_T;
    }
}
#undef FUNC_NAME

/* Move up to READ_LEN bytes from PORT's putback and/or read buffers
   into memory starting at DEST.  Return the number of bytes moved.
   PORT's line/column numbers are left unchanged.  */
size_t
scm_take_from_input_buffers (SCM port, char *dest, size_t read_len)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  size_t bytes_read = 0;
  size_t from_buf = min (pt->read_end - pt->read_pos, read_len);

  if (from_buf > 0)
    {
      memcpy (dest, pt->read_pos, from_buf);
      pt->read_pos += from_buf;
      bytes_read += from_buf;
      read_len -= from_buf;
      dest += from_buf;
    }

  /* if putback was active, try the real input buffer too.  */
  if (pt->read_buf == pt->putback_buf)
    {
      from_buf = min (pt->saved_read_end - pt->saved_read_pos, read_len);
      if (from_buf > 0)
	{
	  memcpy (dest, pt->saved_read_pos, from_buf);
	  pt->saved_read_pos += from_buf;
	  bytes_read += from_buf;
	}
    }

  return bytes_read;
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
  char *data;
  scm_t_port *pt;
  long count;

  SCM_VALIDATE_OPINPORT (1, port);
  pt = SCM_PTAB_ENTRY (port);

  count = pt->read_end - pt->read_pos;
  if (pt->read_buf == pt->putback_buf)
    count += pt->saved_read_end - pt->saved_read_pos;

  if (count)
    {
      result = scm_i_make_string (count, &data, 0);
      scm_take_from_input_buffers (port, data, count);
    }
  else
    result = scm_nullstr;
  
  return result;
}
#undef FUNC_NAME


/* Standard ports --- current input, output, error, and more(!).  */

static SCM cur_inport_fluid = SCM_BOOL_F;
static SCM cur_outport_fluid = SCM_BOOL_F;
static SCM cur_errport_fluid = SCM_BOOL_F;
static SCM cur_loadport_fluid = SCM_BOOL_F;

SCM_DEFINE (scm_current_input_port, "current-input-port", 0, 0, 0,
	    (),
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
	    (),
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
           (),
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

static SCM current_warning_port_var;
static scm_i_pthread_once_t current_warning_port_once = SCM_I_PTHREAD_ONCE_INIT;

static void
init_current_warning_port_var (void)
{
  current_warning_port_var
    = scm_c_private_variable ("guile", "current-warning-port");
}

SCM
scm_current_warning_port (void)
{
  scm_i_pthread_once (&current_warning_port_once,
                      init_current_warning_port_var);
  return scm_call_0 (scm_variable_ref (current_warning_port_var));
}

SCM_DEFINE (scm_current_load_port, "current-load-port", 0, 0, 0,
	    (),
	    "Return the current-load-port.\n"
            "The load port is used internally by @code{primitive-load}.")
#define FUNC_NAME s_scm_current_load_port
{
  return scm_fluid_ref (cur_loadport_fluid);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_current_input_port, "set-current-input-port", 1, 0, 0,
           (SCM port),
	    "@deffnx {Scheme Procedure} set-current-output-port port\n"
	    "@deffnx {Scheme Procedure} set-current-error-port port\n"
	    "Change the ports returned by @code{current-input-port},\n"
	    "@code{current-output-port} and @code{current-error-port}, respectively,\n"
	    "so that they use the supplied @var{port} for input or output.")
#define FUNC_NAME s_scm_set_current_input_port
{
  SCM oinp = scm_fluid_ref (cur_inport_fluid);
  SCM_VALIDATE_OPINPORT (1, port);
  scm_fluid_set_x (cur_inport_fluid, port);
  return oinp;
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_current_output_port, "set-current-output-port", 1, 0, 0,
	    (SCM port),
	    "Set the current default output port to @var{port}.")
#define FUNC_NAME s_scm_set_current_output_port
{
  SCM ooutp = scm_fluid_ref (cur_outport_fluid);
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT (1, port);
  scm_fluid_set_x (cur_outport_fluid, port);
  return ooutp;
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_current_error_port, "set-current-error-port", 1, 0, 0,
	    (SCM port),
	    "Set the current default error port to @var{port}.")
#define FUNC_NAME s_scm_set_current_error_port
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
{
  scm_i_pthread_once (&current_warning_port_once,
                      init_current_warning_port_var);
  return scm_call_1 (scm_variable_ref (current_warning_port_var), port);
}


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


/* The port table --- an array of pointers to ports.  */

/*
  We need a global registry of ports to flush them all at exit, and to
  get all the ports matching a file descriptor.
 */
SCM scm_i_port_weak_hash;

scm_i_pthread_mutex_t scm_i_port_table_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;


/* Port finalization.  */


static void finalize_port (void *, void *);

/* Register a finalizer for PORT.  */
static SCM_C_INLINE_KEYWORD void
register_finalizer_for_port (SCM port)
{
  /* Register a finalizer for PORT so that its
     type's `free' function gets called.  */
  scm_i_set_finalizer (SCM2PTR (port), finalize_port, NULL);
}

/* Finalize the object (a port) pointed to by PTR.  */
static void
finalize_port (void *ptr, void *data)
{
  long port_type;
  SCM port = PTR2SCM (ptr);

  if (!SCM_PORTP (port))
    abort ();

  if (SCM_OPENP (port))
    {
      if (SCM_REVEALED (port) > 0)
	/* Keep "revealed" ports alive and re-register a finalizer.  */
	register_finalizer_for_port (port);
      else
	{
	  port_type = SCM_TC2PTOBNUM (SCM_CELL_TYPE (port));
	  if (port_type >= scm_numptob)
	    abort ();

	  if (scm_ptobs[port_type].free)
	    /* Yes, I really do mean `.free' rather than `.close'.  `.close'
	       is for explicit `close-port' by user.  */
	    scm_ptobs[port_type].free (port);

	  SCM_SETSTREAM (port, 0);
	  SCM_CLR_PORT_OPEN_FLAG (port);

	  scm_gc_ports_collected++;
	}
    }
}





/* This function is not and should not be thread safe. */
SCM
scm_new_port_table_entry (scm_t_bits tag)
#define FUNC_NAME "scm_new_port_table_entry"
{
  /*
    We initialize the cell to empty, this is in case scm_gc_calloc
    triggers GC ; we don't want the GC to scan a half-finished Z.
   */
  
  SCM z = scm_cons (SCM_EOL, SCM_EOL);
  scm_t_port *entry = scm_gc_typed_calloc (scm_t_port);
  scm_t_port_internal *pti = scm_gc_typed_calloc (scm_t_port_internal);
  const char *encoding;

  entry->file_name = SCM_BOOL_F;
  entry->rw_active = SCM_PORT_NEITHER;
  entry->port = z;

  /* Initialize this port with the thread's current default
     encoding.  */
  encoding = scm_i_default_port_encoding ();
  entry->ilseq_handler = scm_i_default_port_conversion_handler ();
  entry->encoding = encoding ? scm_gc_strdup (encoding, "port") : NULL;
  if (encoding && c_strcasecmp (encoding, "UTF-8") == 0)
    pti->encoding_mode = SCM_PORT_ENCODING_MODE_UTF8;
  else
    pti->encoding_mode = SCM_PORT_ENCODING_MODE_ICONV;
  pti->iconv_descriptors = NULL;

  pti->at_stream_start_for_bom_read  = 1;
  pti->at_stream_start_for_bom_write = 1;

  /* XXX These fields are not what they seem.  They have been
     repurposed, but cannot safely be renamed in 2.0 without breaking
     ABI compatibility.  This will be cleaned up in 2.2.  */
  entry->input_cd = pti;   /* XXX pointer to the internal port structure */
  entry->output_cd = NULL; /* XXX unused */

  pti->pending_eof = 0;
  pti->alist = SCM_EOL;

  /* Until Guile 2.0.9 included, 'setvbuf' would only work on file
     ports.  Now all port types can be supported, but it's not clear
     that port types out in wild accept having someone else fiddle with
     their buffer.  Thus, conservatively turn it off by default.  */
  pti->setvbuf = NULL;

  SCM_SET_CELL_TYPE (z, tag);
  SCM_SETPTAB_ENTRY (z, entry);

  scm_hashq_set_x (scm_i_port_weak_hash, z, SCM_BOOL_F);

  /* For each new port, register a finalizer so that it port type's free
     function can be invoked eventually.  */
  register_finalizer_for_port (z);

  return z;
}
#undef FUNC_NAME

#if SCM_ENABLE_DEPRECATED==1
scm_t_port *
scm_add_to_port_table (SCM port)
{
  SCM z;
  scm_t_port * pt;

  scm_c_issue_deprecation_warning ("scm_add_to_port_table is deprecated.");

  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);
  z = scm_new_port_table_entry (scm_tc7_port);
  pt = SCM_PTAB_ENTRY(z);
  pt->port = port;
  SCM_SETCAR (z, SCM_EOL);
  SCM_SETCDR (z, SCM_EOL);
  SCM_SETPTAB_ENTRY (port, pt);
  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  return pt;
}
#endif


/* Remove a port from the table and destroy it.  */

static void close_iconv_descriptors (scm_t_iconv_descriptors *id);

static void
scm_i_remove_port (SCM port)
#define FUNC_NAME "scm_remove_port"
{
  scm_t_port *p;
  scm_t_port_internal *pti;

  scm_i_scm_pthread_mutex_lock (&scm_i_port_table_mutex);

  p = SCM_PTAB_ENTRY (port);
  pti = SCM_PORT_GET_INTERNAL (port);
  scm_port_non_buffer (p);
  p->putback_buf = NULL;
  p->putback_buf_size = 0;

  if (pti->iconv_descriptors)
    {
      close_iconv_descriptors (pti->iconv_descriptors);
      pti->iconv_descriptors = NULL;
    }

  SCM_SETPTAB_ENTRY (port, 0);

  scm_hashq_remove_x (scm_i_port_weak_hash, port);

  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);
}
#undef FUNC_NAME


/* Functions for debugging.  */
#ifdef GUILE_DEBUG
SCM_DEFINE (scm_pt_size, "pt-size", 0, 0, 0,
            (),
	    "Return the number of ports in the port table.  @code{pt-size}\n"
	    "is only included in @code{--enable-guile-debug} builds.")
#define FUNC_NAME s_scm_pt_size
{
  return scm_from_int (SCM_HASHTABLE_N_ITEMS (scm_i_port_weak_hash));
}
#undef FUNC_NAME
#endif

void
scm_port_non_buffer (scm_t_port *pt)
{
  pt->read_pos = pt->read_buf = pt->read_end = &pt->shortbuf;
  pt->write_buf = pt->write_pos = &pt->shortbuf;
  pt->read_buf_size = pt->write_buf_size = 1;
  pt->write_end = pt->write_buf + pt->write_buf_size;
}


/* Revealed counts --- an oddity inherited from SCSH.  */

/* Find a port in the table and return its revealed count.
   Also used by the garbage collector.
 */

int
scm_revealed_count (SCM port)
{
  return SCM_REVEALED(port);
}



/* Return the revealed count for a port.  */

SCM_DEFINE (scm_port_revealed, "port-revealed", 1, 0, 0,
           (SCM port),
	    "Return the revealed count for @var{port}.")
#define FUNC_NAME s_scm_port_revealed
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  return scm_from_int (scm_revealed_count (port));
}
#undef FUNC_NAME

/* Set the revealed count for a port.  */
SCM_DEFINE (scm_set_port_revealed_x, "set-port-revealed!", 2, 0, 0,
           (SCM port, SCM rcount),
	    "Sets the revealed count for a port to a given value.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_set_port_revealed_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT (1, port);
  SCM_REVEALED (port) = scm_to_int (rcount);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



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
  return scm_i_mode_bits (scm_from_locale_string (modes));
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
  return scm_from_locale_string (modes);
}
#undef FUNC_NAME



/* Closing ports.  */

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
  size_t i;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_PORT (1, port);
  if (SCM_CLOSEDP (port))
    return SCM_BOOL_F;
  i = SCM_PTOBNUM (port);
  if (scm_ptobs[i].close)
    rv = (scm_ptobs[i].close) (port);
  else
    rv = 0;
  scm_i_remove_port (port);
  SCM_CLR_PORT_OPEN_FLAG (port);
  return scm_from_bool (rv >= 0);
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

static SCM
collect_keys (void *unused, SCM key, SCM value, SCM result)
{
  return scm_cons (key, result);
}

void
scm_c_port_for_each (void (*proc)(void *data, SCM p), void *data)
{
  SCM ports;

  /* Copy out the port table as a list so that we get strong references
     to all the values.  */
  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);
  ports = scm_internal_hash_fold (collect_keys, NULL,
				  SCM_EOL, scm_i_port_weak_hash);
  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  for (; scm_is_pair (ports); ports = scm_cdr (ports))
    {
      SCM p = scm_car (ports);
      if (SCM_PORTP (p))
        proc (data, p);
    }
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
  SCM ports;

  SCM_VALIDATE_PROC (1, proc);

  /* Copy out the port table as a list so that we get strong references
     to all the values.  */
  scm_i_pthread_mutex_lock (&scm_i_port_table_mutex);
  ports = scm_internal_hash_fold (collect_keys, NULL,
				  SCM_EOL, scm_i_port_weak_hash);
  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  for (; scm_is_pair (ports); ports = scm_cdr (ports))
    if (SCM_PORTP (SCM_CAR (ports)))
      scm_call_1 (proc, SCM_CAR (ports));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Utter miscellany.  Gosh, we should clean this up some time.  */

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
  return scm_from_bool(SCM_EOF_OBJECT_P (x));
}
#undef FUNC_NAME

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
  pt->read_pos++

  int byte;
  scm_t_port *pt;

  *len = 0;
  pt = SCM_PTAB_ENTRY (port);

  byte = scm_get_byte_or_eof (port);
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
      byte = scm_peek_byte_or_eof (port);
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
      byte = scm_peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80
			|| (buf[0] == 0xe0 && byte < 0xa0)
			|| (buf[0] == 0xed && byte > 0x9f)))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[1] = (scm_t_uint8) byte;
      *len = 2;

      byte = scm_peek_byte_or_eof (port);
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
      byte = scm_peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY (((byte & 0xc0) != 0x80)
			|| (buf[0] == 0xf0 && byte < 0x90)
			|| (buf[0] == 0xf4 && byte > 0x8f)))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[1] = (scm_t_uint8) byte;
      *len = 2;

      byte = scm_peek_byte_or_eof (port);
      ASSERT_NOT_EOF (byte);

      if (SCM_UNLIKELY ((byte & 0xc0) != 0x80))
	goto invalid_seq;

      CONSUME_PEEKED_BYTE ();
      buf[2] = (scm_t_uint8) byte;
      *len = 3;

      byte = scm_peek_byte_or_eof (port);
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

      byte_read = scm_get_byte_or_eof (port);
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
static int
get_codepoint (SCM port, scm_t_wchar *codepoint,
	       char buf[SCM_MBCHAR_BUF_SIZE], size_t *len)
{
  int err;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (port);

  if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_UTF8)
    err = get_utf8_codepoint (port, codepoint, (scm_t_uint8 *) buf, len);
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
                   || c_strcasecmp (pt->encoding, "UTF-16") == 0
                   || c_strcasecmp (pt->encoding, "UTF-32") == 0)))
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

/* this should only be called when the read buffer is empty.  it
   tries to refill the read buffer.  it returns the first char from
   the port, which is either EOF or *(pt->read_pos).  */
static int
scm_i_fill_input (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (port);

  assert (pt->read_pos == pt->read_end);

  if (pti->pending_eof)
    {
      pti->pending_eof = 0;
      return EOF;
    }

  if (pt->read_buf == pt->putback_buf)
    {
      /* finished reading put-back chars.  */
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
      if (pt->read_pos < pt->read_end)
	return *(pt->read_pos);
    }
  return scm_ptobs[SCM_PTOBNUM (port)].fill_input (port);
}

int
scm_fill_input (SCM port)
{
  return scm_i_fill_input (port);
}

/* Slow-path fallback for 'scm_get_byte_or_eof' in inline.h */
int
scm_slow_get_byte_or_eof (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    scm_flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (SCM_UNLIKELY (scm_i_fill_input (port) == EOF))
	return EOF;
    }

  return *pt->read_pos++;
}

/* Slow-path fallback for 'scm_peek_byte_or_eof' in inline.h */
int
scm_slow_peek_byte_or_eof (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    scm_flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (SCM_UNLIKELY (scm_i_fill_input (port) == EOF))
        {
          scm_i_set_pending_eof (port);
          return EOF;
        }
    }

  return *pt->read_pos;
}


/* scm_lfwrite
 *
 * This function differs from scm_c_write; it updates port line and
 * column. */

void
scm_lfwrite (const char *ptr, size_t size, SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  scm_t_ptob_descriptor *ptob = &scm_ptobs[SCM_PTOBNUM (port)];

  if (pt->rw_active == SCM_PORT_READ)
    scm_end_input (port);

  ptob->write (port, ptr, size);

  for (; size; ptr++, size--)
    update_port_lf ((scm_t_wchar) (unsigned char) *ptr, port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}

/* Write STR to PORT from START inclusive to END exclusive.  */
void
scm_lfwrite_substr (SCM str, size_t start, size_t end, SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_READ)
    scm_end_input (port);

  if (end == (size_t) -1)
    end = scm_i_string_length (str);

  scm_i_display_substring (str, start, end, port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}

/* scm_c_read
 *
 * Used by an application to read arbitrary number of bytes from an
 * SCM port.  Same semantics as libc read, except that scm_c_read only
 * returns less than SIZE bytes if at end-of-file.
 *
 * Warning: Doesn't update port line and column counts!  */

/* This structure, and the following swap_buffer function, are used
   for temporarily swapping a port's own read buffer, and the buffer
   that the caller of scm_c_read provides. */
struct port_and_swap_buffer
{
  scm_t_port *pt;
  unsigned char *buffer;
  size_t size;
};

static void
swap_buffer (void *data)
{
  struct port_and_swap_buffer *psb = (struct port_and_swap_buffer *) data;
  unsigned char *old_buf = psb->pt->read_buf;
  size_t old_size = psb->pt->read_buf_size;

  /* Make the port use (buffer, size) from the struct. */
  psb->pt->read_pos = psb->pt->read_buf = psb->pt->read_end = psb->buffer;
  psb->pt->read_buf_size = psb->size;

  /* Save the port's old (buffer, size) in the struct. */
  psb->buffer = old_buf;
  psb->size = old_size;
}

size_t
scm_c_read (SCM port, void *buffer, size_t size)
#define FUNC_NAME "scm_c_read"
{
  scm_t_port *pt;
  size_t n_read = 0, n_available;
  struct port_and_swap_buffer psb;

  SCM_VALIDATE_OPINPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  if (pt->rw_active == SCM_PORT_WRITE)
    scm_ptobs[SCM_PTOBNUM (port)].flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  /* Take bytes first from the port's read buffer. */
  if (pt->read_pos < pt->read_end)
    {
      n_available = min (size, pt->read_end - pt->read_pos);
      memcpy (buffer, pt->read_pos, n_available);
      buffer = (char *) buffer + n_available;
      pt->read_pos += n_available;
      n_read += n_available;
      size -= n_available;
    }

  /* Avoid the scm_dynwind_* costs if we now have enough data. */
  if (size == 0)
    return n_read;

  /* Now we will call scm_i_fill_input repeatedly until we have read the
     requested number of bytes.  (Note that a single scm_i_fill_input
     call does not guarantee to fill the whole of the port's read
     buffer.) */
  if (pt->read_buf_size <= 1 &&
      (pt->encoding == NULL
       || c_strcasecmp (pt->encoding, "ISO-8859-1") == 0))
    {
      /* The port that we are reading from is unbuffered - i.e. does
	 not have its own persistent buffer - but we have a buffer,
	 provided by our caller, that is the right size for the data
	 that is wanted.  For the following scm_i_fill_input calls,
	 therefore, we use the buffer in hand as the port's read
	 buffer.

	 We need to make sure that the port's normal (1 byte) buffer
	 is reinstated in case one of the scm_i_fill_input () calls
	 throws an exception; we use the scm_dynwind_* API to achieve
	 that. 

         A consequence of this optimization is that the fill_input
         functions can't unget characters.  That'll push data to the
         pushback buffer instead of this psb buffer.  */
#if SCM_DEBUG == 1
      unsigned char *pback = pt->putback_buf;
#endif      
      psb.pt = pt;
      psb.buffer = buffer;
      psb.size = size;
      scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
      scm_dynwind_rewind_handler (swap_buffer, &psb, SCM_F_WIND_EXPLICITLY);
      scm_dynwind_unwind_handler (swap_buffer, &psb, SCM_F_WIND_EXPLICITLY);

      /* Call scm_i_fill_input until we have all the bytes that we need,
	 or we hit EOF. */
      while (pt->read_buf_size && (scm_i_fill_input (port) != EOF))
	{
	  pt->read_buf_size -= (pt->read_end - pt->read_pos);
	  pt->read_pos = pt->read_buf = pt->read_end;
	}
#if SCM_DEBUG == 1
      if (pback != pt->putback_buf 
          || pt->read_buf - (unsigned char *) buffer < 0)
        scm_misc_error (FUNC_NAME, 
                        "scm_c_read must not call a fill function that pushes "
                        "back characters onto an unbuffered port", SCM_EOL);
#endif      
      n_read += pt->read_buf - (unsigned char *) buffer;
      
      /* Reinstate the port's normal buffer. */
      scm_dynwind_end ();
    }
  else
    {
      /* The port has its own buffer.  It is important that we use it,
	 even if it happens to be smaller than our caller's buffer, so
	 that a custom port implementation's entry points (in
	 particular, fill_input) can rely on the buffer always being
	 the same as they first set up. */
      while (size && (scm_i_fill_input (port) != EOF))
	{
	  n_available = min (size, pt->read_end - pt->read_pos);
	  memcpy (buffer, pt->read_pos, n_available);
	  buffer = (char *) buffer + n_available;
	  pt->read_pos += n_available;
	  n_read += n_available;
	  size -= n_available;
	} 
    }

  return n_read;
}
#undef FUNC_NAME

/* scm_c_write
 *
 * Used by an application to write arbitrary number of bytes to an SCM
 * port.  Similar semantics as libc write.  However, unlike libc
 * write, scm_c_write writes the requested number of bytes and has no
 * return value.
 *
 * Warning: Doesn't update port line and column counts!
 */

void
scm_c_write (SCM port, const void *ptr, size_t size)
#define FUNC_NAME "scm_c_write"
{
  scm_t_port *pt;
  scm_t_ptob_descriptor *ptob;

  SCM_VALIDATE_OPOUTPORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  ptob = &scm_ptobs[SCM_PTOBNUM (port)];

  if (pt->rw_active == SCM_PORT_READ)
    scm_end_input (port);

  ptob->write (port, ptr, size);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}
#undef FUNC_NAME

void
scm_flush (SCM port)
{
  long i = SCM_PTOBNUM (port);
  assert (i >= 0);
  (scm_ptobs[i].flush) (port);
}

void
scm_end_input (SCM port)
{
  long offset;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  scm_i_clear_pending_eof (port);
  if (pt->read_buf == pt->putback_buf)
    {
      offset = pt->read_end - pt->read_pos;
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
    }
  else
    offset = 0;

  scm_ptobs[SCM_PTOBNUM (port)].end_input (port, offset);
}




static void
scm_i_unget_bytes (const unsigned char *buf, size_t len, SCM port)
#define FUNC_NAME "scm_unget_bytes"
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  size_t old_len, new_len;

  scm_i_clear_pending_eof (port);

  if (pt->read_buf != pt->putback_buf)
    /* switch to the put-back buffer.  */
    {
      if (pt->putback_buf == NULL)
	{
          pt->putback_buf_size = (len > SCM_INITIAL_PUTBACK_BUF_SIZE
                                  ? len : SCM_INITIAL_PUTBACK_BUF_SIZE);
	  pt->putback_buf
	    = (unsigned char *) scm_gc_malloc_pointerless
	    (pt->putback_buf_size, "putback buffer");
	}

      pt->saved_read_buf = pt->read_buf;
      pt->saved_read_pos = pt->read_pos;
      pt->saved_read_end = pt->read_end;
      pt->saved_read_buf_size = pt->read_buf_size;

      /* Put read_pos at the end of the buffer, so that ungets will not
         have to shift the buffer contents each time.  */
      pt->read_buf = pt->putback_buf;
      pt->read_pos = pt->read_end = pt->putback_buf + pt->putback_buf_size;
      pt->read_buf_size = pt->putback_buf_size;
    }

  old_len = pt->read_end - pt->read_pos;
  new_len = old_len + len;

  if (new_len > pt->read_buf_size)
    /* The putback buffer needs to be enlarged.  */
    {
      size_t new_buf_size;
      unsigned char *new_buf, *new_end, *new_pos;

      new_buf_size = pt->read_buf_size * 2;
      if (new_buf_size < new_len)
        new_buf_size = new_len;

      new_buf = (unsigned char *)
        scm_gc_malloc_pointerless (new_buf_size, "putback buffer");

      /* Put the bytes at the end of the buffer, so that future
         ungets won't need to shift the buffer.  */
      new_end = new_buf + new_buf_size;
      new_pos = new_end - old_len;
      memcpy (new_pos, pt->read_pos, old_len);

      pt->read_buf = pt->putback_buf = new_buf;
      pt->read_pos = new_pos;
      pt->read_end = new_end;
      pt->read_buf_size = pt->putback_buf_size = new_buf_size;
    }
  else if (pt->read_buf + len < pt->read_pos)
    /* If needed, shift the existing buffer contents up.
       This should not happen unless some external code
       manipulates the putback buffer pointers.  */
    {
      unsigned char *new_end = pt->read_buf + pt->read_buf_size;
      unsigned char *new_pos = new_end - old_len;

      memmove (new_pos, pt->read_pos, old_len);
      pt->read_pos = new_pos;
      pt->read_end = new_end;
    }

  /* Move read_pos back and copy the bytes there.  */
  pt->read_pos -= len;
  memcpy (pt->read_buf + (pt->read_pos - pt->read_buf), buf, len);

  if (pt->rw_active == SCM_PORT_WRITE)
    scm_flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;
}
#undef FUNC_NAME

void
scm_unget_bytes (const unsigned char *buf, size_t len, SCM port)
{
  scm_i_unget_bytes (buf, len, port);
}

void
scm_unget_byte (int c, SCM port)
{
  unsigned char byte;

  byte = c;
  scm_i_unget_bytes (&byte, 1, port);
}

void
scm_ungetc (scm_t_wchar c, SCM port)
#define FUNC_NAME "scm_ungetc"
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  char *result;
  char result_buf[10];
  const char *encoding;
  size_t len;

  if (pt->encoding != NULL)
    encoding = pt->encoding;
  else
    encoding = "ISO-8859-1";

  len = sizeof (result_buf);
  result = u32_conv_to_encoding (encoding,
				 (enum iconv_ilseq_handler) pt->ilseq_handler,
				 (uint32_t *) &c, 1, NULL,
				 result_buf, &len);

  if (SCM_UNLIKELY (result == NULL || len == 0))
    scm_encoding_error (FUNC_NAME, errno,
			"conversion to port encoding failed",
			SCM_BOOL_F, SCM_MAKE_CHAR (c));

  scm_i_unget_bytes ((unsigned char *) result, len, port);

  if (SCM_UNLIKELY (result != result_buf))
    free (result);

  if (c == '\n')
    {
      /* What should col be in this case?
       * We'll leave it at -1.
       */
      SCM_LINUM (port) -= 1;
    }
  else
    SCM_COL(port) -= 1;
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
  int err;
  SCM result;
  scm_t_wchar c;
  char bytes[SCM_MBCHAR_BUF_SIZE];
  long column, line;
  size_t len;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  column = SCM_COL (port);
  line = SCM_LINUM (port);

  err = get_codepoint (port, &c, bytes, &len);

  scm_i_unget_bytes ((unsigned char *) bytes, len, port);

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
      scm_t_port_internal *pti = SCM_PORT_GET_INTERNAL (fd_port);
      scm_t_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (fd_port);
      off_t_or_off64_t off = scm_to_off_t_or_off64_t (offset);
      off_t_or_off64_t rv;

      if (!ptob->seek)
	SCM_MISC_ERROR ("port is not seekable", 
                        scm_cons (fd_port, SCM_EOL));
      else
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
      scm_t_port *pt = SCM_PTAB_ENTRY (object);
      scm_t_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (object);

      if (!ptob->truncate)
	SCM_MISC_ERROR ("port is not truncatable", SCM_EOL);

      scm_i_clear_pending_eof (object);
      if (pt->rw_active == SCM_PORT_READ)
	scm_end_input (object);
      else if (pt->rw_active == SCM_PORT_WRITE)
	ptob->flush (object);

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

/* A fluid specifying the default encoding for newly created ports.  If it is
   a string, that is the encoding.  If it is #f, it is in the "native"
   (Latin-1) encoding.  */
SCM_VARIABLE (default_port_encoding_var, "%default-port-encoding");

static int scm_port_encoding_init = 0;

/* Use ENCODING as the default encoding for future ports.  */
void
scm_i_set_default_port_encoding (const char *encoding)
{
  if (!scm_port_encoding_init
      || !scm_is_fluid (SCM_VARIABLE_REF (default_port_encoding_var)))
    scm_misc_error (NULL, "tried to set port encoding fluid before it is initialized",
		    SCM_EOL);

  if (encoding == NULL
      || c_strcasecmp (encoding, "ASCII") == 0
      || c_strcasecmp (encoding, "ANSI_X3.4-1968") == 0
      || c_strcasecmp (encoding, "ISO-8859-1") == 0)
    scm_fluid_set_x (SCM_VARIABLE_REF (default_port_encoding_var), SCM_BOOL_F);
  else
    scm_fluid_set_x (SCM_VARIABLE_REF (default_port_encoding_var),
		     scm_from_locale_string (encoding));
}

/* Return the name of the default encoding for newly created ports; a
   return value of NULL means "ISO-8859-1".  */
const char *
scm_i_default_port_encoding (void)
{
  if (!scm_port_encoding_init)
    return NULL;
  else if (!scm_is_fluid (SCM_VARIABLE_REF (default_port_encoding_var)))
    return NULL;
  else
    {
      SCM encoding;

      encoding = scm_fluid_ref (SCM_VARIABLE_REF (default_port_encoding_var));
      if (!scm_is_string (encoding))
	return NULL;
      else
	return scm_i_string_chars (encoding);
    }
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
      pt->read_pos++;
      i++;
    }
  scm_i_unget_bytes (bytes, i, port);
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

  input_cd = (iconv_t) -1;
  output_cd = (iconv_t) -1;
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
    err = scm_from_locale_string (encoding);
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

/* Return the iconv_descriptors, initializing them if necessary.  MODE
   must be either SCM_PORT_READ or SCM_PORT_WRITE, and specifies which
   operation is about to be done.  We deliberately avoid reading from
   the port unless the user was about to do so.  */
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
      if (c_strcasecmp (pt->encoding, "UTF-16") == 0)
        precise_encoding = decide_utf16_encoding (port, mode);
      else if (c_strcasecmp (pt->encoding, "UTF-32") == 0)
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

  if (encoding == NULL)
    encoding = "ISO-8859-1";

  /* If ENCODING is UTF-8, then no conversion descriptor is opened
     because we do I/O ourselves.  This saves 100+ KiB for each
     descriptor.  */
  pt->encoding = scm_gc_strdup (encoding, "port");
  if (c_strcasecmp (encoding, "UTF-8") == 0)
    pti->encoding_mode = SCM_PORT_ENCODING_MODE_UTF8;
  else
    pti->encoding_mode = SCM_PORT_ENCODING_MODE_ICONV;

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
  scm_t_port *pt;
  const char *enc;

  SCM_VALIDATE_PORT (1, port);

  pt = SCM_PTAB_ENTRY (port);
  enc = pt->encoding;
  if (enc)
    return scm_from_locale_string (pt->encoding);
  else
    return SCM_BOOL_F;
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

  enc_str = scm_to_locale_string (enc);
  scm_i_set_port_encoding_x (port, enc_str);
  free (enc_str);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* A fluid specifying the default conversion handler for newly created
   ports.  Its value should be one of the symbols below.  */
SCM_VARIABLE (default_conversion_strategy_var,
	      "%default-port-conversion-strategy");

/* Whether the above fluid is initialized.  */
static int scm_conversion_strategy_init = 0;

/* The possible conversion strategies.  */
SCM_SYMBOL (sym_error, "error");
SCM_SYMBOL (sym_substitute, "substitute");
SCM_SYMBOL (sym_escape, "escape");

/* Return the default failed encoding conversion policy for new created
   ports.  */
scm_t_string_failed_conversion_handler
scm_i_default_port_conversion_handler (void)
{
  scm_t_string_failed_conversion_handler handler;

  if (!scm_conversion_strategy_init
      || !scm_is_fluid (SCM_VARIABLE_REF (default_conversion_strategy_var)))
    handler = SCM_FAILED_CONVERSION_QUESTION_MARK;
  else
    {
      SCM fluid, value;

      fluid = SCM_VARIABLE_REF (default_conversion_strategy_var);
      value = scm_fluid_ref (fluid);

      if (scm_is_eq (sym_substitute, value))
	handler = SCM_FAILED_CONVERSION_QUESTION_MARK;
      else if (scm_is_eq (sym_escape, value))
	handler = SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE;
      else
	/* Default to 'error also when the fluid's value is not one of
	   the valid symbols.  */
	handler = SCM_FAILED_CONVERSION_ERROR;
    }

  return handler;
}

/* Use HANDLER as the default conversion strategy for future ports.  */
void
scm_i_set_default_port_conversion_handler (scm_t_string_failed_conversion_handler
					   handler)
{
  SCM strategy;

  if (!scm_conversion_strategy_init
      || !scm_is_fluid (SCM_VARIABLE_REF (default_conversion_strategy_var)))
    scm_misc_error (NULL, "tried to set conversion strategy fluid before it is initialized",
		    SCM_EOL);

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
  char *type = SCM_PTOBNAME (SCM_PTOBNUM (exp));
  if (!type)
    type = "port";
  scm_puts ("#<", port);
  scm_print_port_mode (exp, port);
  scm_puts (type, port);
  scm_putc (' ', port);
  scm_uintprint (SCM_CELL_WORD_1 (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}



/* Void ports.   */

scm_t_bits scm_tc16_void_port = 0;

static int fill_input_void_port (SCM port SCM_UNUSED)
{
  return EOF;
}

static void
write_void_port (SCM port SCM_UNUSED,
		 const void *data SCM_UNUSED,
		 size_t size SCM_UNUSED)
{
}

static SCM
scm_i_void_port (long mode_bits)
{
  scm_i_scm_pthread_mutex_lock (&scm_i_port_table_mutex);
  {
    SCM answer = scm_new_port_table_entry (scm_tc16_void_port);
    scm_t_port * pt = SCM_PTAB_ENTRY(answer);

    scm_port_non_buffer (pt);
  
    SCM_SETSTREAM (answer, 0);
    SCM_SET_CELL_TYPE (answer, scm_tc16_void_port | mode_bits);
    scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);
    return answer;
  }
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

void
scm_init_ports ()
{
  /* lseek() symbols.  */
  scm_c_define ("SEEK_SET", scm_from_int (SEEK_SET));
  scm_c_define ("SEEK_CUR", scm_from_int (SEEK_CUR));
  scm_c_define ("SEEK_END", scm_from_int (SEEK_END));

  scm_tc16_void_port = scm_make_port_type ("void", fill_input_void_port, 
					   write_void_port);

  cur_inport_fluid = scm_make_fluid ();
  cur_outport_fluid = scm_make_fluid ();
  cur_errport_fluid = scm_make_fluid ();
  cur_loadport_fluid = scm_make_fluid ();

  scm_i_port_weak_hash = scm_make_weak_key_hash_table (SCM_I_MAKINUM(31));

#include "libguile/ports.x"

  /* Use Latin-1 as the default port encoding.  */
  SCM_VARIABLE_SET (default_port_encoding_var,
                    scm_make_fluid_with_default (SCM_BOOL_F));
  scm_port_encoding_init = 1;

  SCM_VARIABLE_SET (default_conversion_strategy_var,
                    scm_make_fluid_with_default (sym_substitute));
  scm_conversion_strategy_init = 1;

  /* These bindings are used when boot-9 turns `current-input-port' et
     al into parameters.  They are then removed from the guile module.  */
  scm_c_define ("%current-input-port-fluid", cur_inport_fluid);
  scm_c_define ("%current-output-port-fluid", cur_outport_fluid);
  scm_c_define ("%current-error-port-fluid", cur_errport_fluid);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
