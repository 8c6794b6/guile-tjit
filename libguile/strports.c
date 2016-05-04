/* Copyright (C) 1995, 1996, 1998, 1999, 2000, 2001, 2002, 2003, 2005, 2006,
 *   2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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




#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"

#include <stdio.h>
#include <unistd.h>

#include "libguile/bytevectors.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/modules.h"
#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/srfi-4.h"

#include "libguile/strports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - string ports}
 *
 */

SCM_SYMBOL (sym_UTF_8, "UTF-8");

scm_t_bits scm_tc16_strport;

struct string_port {
  SCM bytevector;
  size_t pos;
  size_t len;
};

static size_t
string_port_read (SCM port, SCM dst, size_t start, size_t count)
{
  struct string_port *stream = (void *) SCM_STREAM (port);

  if (stream->pos >= stream->len)
    return 0;

  if (count > stream->len - stream->pos)
    count = stream->len - stream->pos;

  memcpy (SCM_BYTEVECTOR_CONTENTS (dst) + start,
          SCM_BYTEVECTOR_CONTENTS (stream->bytevector) + stream->pos,
          count);

  stream->pos += count;
  return count;
}

static size_t
string_port_write (SCM port, SCM src, size_t start, size_t count)
{
  struct string_port *stream = (void *) SCM_STREAM (port);

  if (SCM_BYTEVECTOR_LENGTH (stream->bytevector) < stream->pos + count)
    {
      SCM new_bv;
      size_t new_size;

      new_size = max (SCM_BYTEVECTOR_LENGTH (stream->bytevector) * 2,
                      stream->pos + count);
      new_bv = scm_c_make_bytevector (new_size);
      memcpy (SCM_BYTEVECTOR_CONTENTS (new_bv),
              SCM_BYTEVECTOR_CONTENTS (stream->bytevector),
              stream->len);
      stream->bytevector = new_bv;
    }

  memcpy (SCM_BYTEVECTOR_CONTENTS (stream->bytevector) + stream->pos,
          SCM_BYTEVECTOR_CONTENTS (src) + start,
          count);
  stream->pos += count;
  if (stream->pos > stream->len)
    stream->len = stream->pos;

  return count;
}

static scm_t_off
string_port_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "string_port_seek"
{
  struct string_port *stream = (void *) SCM_STREAM (port);
  scm_t_off target;

  if (whence == SEEK_CUR)
    target = offset + stream->pos;
  else if (whence == SEEK_SET)
    target = offset;
  else if (whence == SEEK_END)
    target = offset + stream->len;
  else
    scm_wrong_type_arg_msg (FUNC_NAME, 0, port, "invalid `seek' parameter");

  if (target >= 0 && target <= stream->len)
    stream->pos = target;
  else
    scm_out_of_range (FUNC_NAME, scm_from_long (offset));

  return target;
}
#undef FUNC_NAME



/* The initial size in bytes of a string port's buffer.  */
#define INITIAL_BUFFER_SIZE 128

/* Return a new string port with MODES.  If STR is #f, a new backing
   buffer is allocated; otherwise STR must be a string and a copy of it
   serves as the buffer for the new port.  */
SCM
scm_mkstrport (SCM pos, SCM str, long modes, const char *caller)
{
  SCM buf;
  size_t len, byte_pos;
  struct string_port *stream;

  if (!((modes & SCM_WRTNG) || (modes & SCM_RDNG)))
    scm_misc_error ("scm_mkstrport", "port must read or write", SCM_EOL);

  if (scm_is_false (str))
    {
      /* Allocate a new buffer to write to.  */
      buf = scm_c_make_bytevector (INITIAL_BUFFER_SIZE);
      len = byte_pos = 0;
    }
  else
    {
      SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, caller);

      buf = scm_string_to_utf8 (str);
      len = scm_c_bytevector_length (buf);

      if (scm_is_eq (pos, SCM_INUM0))
        byte_pos = 0;
      else
        /* Inefficient but simple way to convert the character position
           POS into a byte position BYTE_POS.  */
        free (scm_to_utf8_stringn (scm_substring (str, SCM_INUM0, pos),
                                   &byte_pos));
    }

  stream = scm_gc_typed_calloc (struct string_port);
  stream->bytevector = buf;
  stream->pos = byte_pos;
  stream->len = len;

  return
    scm_c_make_port_with_encoding (scm_tc16_strport, modes, sym_UTF_8,
                                   scm_i_default_port_conversion_strategy (),
                                   (scm_t_bits) stream);
}

/* Create a new string from the buffer of PORT, a string port, converting from
   PORT's encoding to the standard string representation.  */
SCM
scm_strport_to_string (SCM port)
{
  signed char *ptr;
  struct string_port *stream = (void *) SCM_STREAM (port);

  scm_flush (port);

  if (stream->len == 0)
    return scm_nullstr;

  ptr = SCM_BYTEVECTOR_CONTENTS (stream->bytevector);
  return scm_from_port_stringn ((char *) ptr, stream->len, port);
}

SCM_DEFINE (scm_object_to_string, "object->string", 1, 1, 0,
	    (SCM obj, SCM printer),
	    "Return a Scheme string obtained by printing @var{obj}.\n"
	    "Printing function can be specified by the optional second\n"
	    "argument @var{printer} (default: @code{write}).")
#define FUNC_NAME s_scm_object_to_string
{
  SCM port, result;

  if (!SCM_UNBNDP (printer))
    SCM_VALIDATE_PROC (2, printer);

  port = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
			SCM_OPN | SCM_WRTNG, FUNC_NAME);

  if (SCM_UNBNDP (printer))
    scm_write (obj, port);
  else
    scm_call_2 (printer, obj, port);

  result = scm_strport_to_string (port);

  /* Explicitly close PORT so that the iconv CDs associated with it are
     deallocated right away.  This is important because CDs use a lot of
     memory that's not visible to the GC, so not freeing them can lead
     to almost large heap usage.  See
     <http://wingolog.org/archives/2011/02/25/ports-weaks-gc-and-dark-matter>
     for details.  */
  scm_close_port (port);

  return result;
}
#undef FUNC_NAME

SCM
scm_call_with_output_string (SCM proc)
{
  static SCM var = SCM_BOOL_F;

  if (scm_is_false (var))
    var = scm_c_private_lookup ("guile", "call-with-output-string");

  return scm_call_1 (scm_variable_ref (var), proc);
}

SCM
scm_call_with_input_string (SCM string, SCM proc)
{
  static SCM var = SCM_BOOL_F;

  if (scm_is_false (var))
    var = scm_c_private_lookup ("guile", "call-with-input-string");

  return scm_call_2 (scm_variable_ref (var), string, proc);
}

SCM_DEFINE (scm_open_input_string, "open-input-string", 1, 0, 0,
	    (SCM str),
	    "Take a string and return an input port that delivers characters\n"
	    "from the string. The port can be closed by\n"
	    "@code{close-input-port}, though its storage will be reclaimed\n"
	    "by the garbage collector if it becomes inaccessible.")
#define FUNC_NAME s_scm_open_input_string
{
  SCM p = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_RDNG, FUNC_NAME);
  return p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_open_output_string, "open-output-string", 0, 0, 0, 
	    (void),
	    "Return an output port that will accumulate characters for\n"
	    "retrieval by @code{get-output-string}. The port can be closed\n"
	    "by the procedure @code{close-output-port}, though its storage\n"
	    "will be reclaimed by the garbage collector if it becomes\n"
	    "inaccessible.")
#define FUNC_NAME s_scm_open_output_string
{
  SCM p;

  p = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
		     SCM_OPN | SCM_WRTNG,
                     FUNC_NAME);
  return p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_output_string, "get-output-string", 1, 0, 0, 
	    (SCM port),
	    "Given an output port created by @code{open-output-string},\n"
	    "return a string consisting of the characters that have been\n"
	    "output to the port so far.")
#define FUNC_NAME s_scm_get_output_string
{
  SCM_VALIDATE_OPOUTSTRPORT (1, port);
  return scm_strport_to_string (port);
}
#undef FUNC_NAME


/* Given a null-terminated string EXPR containing a Scheme expression
   read it, and return it as an SCM value. */
SCM
scm_c_read_string (const char *expr)
{
  SCM port = scm_mkstrport (SCM_INUM0,
			    scm_from_locale_string (expr),
			    SCM_OPN | SCM_RDNG,
			    "scm_c_read_string");
  SCM form;

  form = scm_read (port);

  scm_close_port (port);
  return form;
}

/* Given a null-terminated string EXPR containing Scheme program text,
   evaluate it, and return the result of the last expression evaluated.  */
SCM
scm_c_eval_string (const char *expr)
{
  return scm_eval_string (scm_from_locale_string (expr));
}

SCM
scm_c_eval_string_in_module (const char *expr, SCM module)
{
  return scm_eval_string_in_module (scm_from_locale_string (expr), module);
}


static SCM eval_string_var;
static SCM k_module;

static void
init_eval_string_var_and_k_module (void)
{
  eval_string_var = scm_c_public_variable ("ice-9 eval-string", "eval-string");
  k_module = scm_from_locale_keyword ("module");
}

SCM_DEFINE (scm_eval_string_in_module, "eval-string", 1, 1, 0, 
            (SCM string, SCM module),
	    "Evaluate @var{string} as the text representation of a Scheme\n"
	    "form or forms, and return whatever value they produce.\n"
	    "Evaluation takes place in the given module, or the current\n"
            "module when no module is given.\n"
            "While the code is evaluated, the given module is made the\n"
	    "current one.  The current module is restored when this\n"
            "procedure returns.")
#define FUNC_NAME s_scm_eval_string_in_module
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_eval_string_var_and_k_module);
  
  if (SCM_UNBNDP (module))
    module = scm_current_module ();
  else
    SCM_VALIDATE_MODULE (2, module);

  return scm_call_3 (scm_variable_ref (eval_string_var),
                     string, k_module, module);
}
#undef FUNC_NAME

SCM
scm_eval_string (SCM string)
{
  return scm_eval_string_in_module (string, SCM_UNDEFINED);
}

static scm_t_bits
scm_make_string_port_type ()
{
  scm_t_bits tc = scm_make_port_type ("string",
                                      string_port_read,
                                      string_port_write);
  scm_set_port_seek (tc, string_port_seek);

  return tc;
}

void
scm_init_strports ()
{
  scm_tc16_strport = scm_make_string_port_type ();

#include "libguile/strports.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
