/* Copyright (C) 2009, 2010, 2011, 2013-2015 Free Software Foundation, Inc.
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
# include <config.h>
#endif

#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/bytevectors.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/r6rs-ports.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/vectors.h"
#include "libguile/ports-internal.h"




/* Unimplemented features.  */


/* Transoders are currently not implemented since Guile 1.8 is not
   Unicode-capable.  Thus, most of the code here assumes the use of the
   binary transcoder.  */
static inline void
transcoders_not_implemented (void)
{
  fprintf (stderr, "%s: warning: transcoders not implemented\n",
	   PACKAGE_NAME);
}




/* End-of-file object.  */

SCM_DEFINE (scm_eof_object, "eof-object", 0, 0, 0,
	    (void),
	    "Return the end-of-file object.")
#define FUNC_NAME s_scm_eof_object
{
  return SCM_EOF_VAL;
}
#undef FUNC_NAME




/* Input ports.  */

#ifndef MIN
# define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

/* Bytevector input ports.  */
static scm_t_bits bytevector_input_port_type = 0;

static inline SCM
make_bytevector_input_port (SCM bv)
{
  SCM port;
  char *c_bv;
  unsigned c_len;
  scm_t_port *c_port;
  const unsigned long mode_bits = SCM_OPN | SCM_RDNG;

  port = scm_c_make_port_with_encoding (bytevector_input_port_type,
                                        mode_bits,
                                        NULL, /* encoding */
                                        SCM_FAILED_CONVERSION_ERROR,
                                        SCM_UNPACK (bv));

  c_port = SCM_PTAB_ENTRY (port);

  /* Have the port directly access the bytevector.  */
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  c_len = SCM_BYTEVECTOR_LENGTH (bv);

  c_port->read_pos = c_port->read_buf = (unsigned char *) c_bv;
  c_port->read_end = (unsigned char *) c_bv + c_len;
  c_port->read_buf_size = c_len;

  return port;
}

static int
bytevector_input_port_fill_input (SCM port)
{
  int result;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

  if (c_port->read_pos >= c_port->read_end)
    result = EOF;
  else
    result = (int) *c_port->read_pos;

  return result;
}

static scm_t_off
bytevector_input_port_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "bytevector_input_port_seek"
{
  scm_t_off c_result = 0;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

  switch (whence)
    {
    case SEEK_CUR:
      offset += c_port->read_pos - c_port->read_buf;
      /* Fall through.  */

    case SEEK_SET:
      if (c_port->read_buf + offset <= c_port->read_end)
	{
	  c_port->read_pos = c_port->read_buf + offset;
	  c_result = offset;
	}
      else
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      break;

    case SEEK_END:
      if (c_port->read_end - offset >= c_port->read_buf)
	{
	  c_port->read_pos = c_port->read_end - offset;
	  c_result = c_port->read_pos - c_port->read_buf;
	}
      else
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      break;

    default:
      scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
			      "invalid `seek' parameter");
    }

  return c_result;
}
#undef FUNC_NAME


/* Instantiate the bytevector input port type.  */
static inline void
initialize_bytevector_input_ports (void)
{
  bytevector_input_port_type =
    scm_make_port_type ("r6rs-bytevector-input-port",
                        bytevector_input_port_fill_input,
			NULL);

  scm_set_port_seek (bytevector_input_port_type, bytevector_input_port_seek);
}


SCM_DEFINE (scm_open_bytevector_input_port,
	    "open-bytevector-input-port", 1, 1, 0,
	    (SCM bv, SCM transcoder),
	    "Return an input port whose contents are drawn from "
	    "bytevector @var{bv}.")
#define FUNC_NAME s_scm_open_bytevector_input_port
{
  SCM_VALIDATE_BYTEVECTOR (1, bv);
  if (!SCM_UNBNDP (transcoder) && !scm_is_false (transcoder))
    transcoders_not_implemented ();

  return make_bytevector_input_port (bv);
}
#undef FUNC_NAME




/* Custom binary ports.  The following routines are shared by input and
   output custom binary ports.  */

struct custom_binary_port {
  SCM read_buffer;
  SCM read;
  SCM write;
  SCM get_position;
  SCM set_position_x;
  SCM close;
};

static scm_t_off
custom_binary_port_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "custom_binary_port_seek"
{
  SCM result;
  struct custom_binary_port *stream = (void *) SCM_STREAM (port);
  scm_t_off c_result = 0;

  switch (whence)
    {
    case SEEK_CUR:
      {
	if (SCM_LIKELY (scm_is_true (stream->get_position)))
	  result = scm_call_0 (stream->get_position);
	else
	  scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
				  "R6RS custom binary port with "
				  "`port-position' support");
	c_result = scm_to_int (result);
	if (offset == 0)
	  /* We just want to know the current position.  */
	  break;

	offset += c_result;
	/* Fall through.  */
      }

    case SEEK_SET:
      {
	if (SCM_LIKELY (scm_is_true (stream->set_position_x)))
	  result = scm_call_1 (stream->set_position_x, scm_from_int (offset));
	else
	  scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
				  "seekable R6RS custom binary port");

	/* Assuming setting the position succeeded.  */
	c_result = offset;
	break;
      }

    default:
      /* `SEEK_END' cannot be supported.  */
      scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
			      "R6RS custom binary ports do not "
			      "support `SEEK_END'");
    }

  return c_result;
}
#undef FUNC_NAME

static void
custom_binary_port_close (SCM port)
{
  struct custom_binary_port *stream = (void *) SCM_STREAM (port);

  if (scm_is_true (stream->close))
    /* Invoke the `close' thunk.  */
    scm_call_0 (stream->close);
}




/* Custom binary input ports.  */

static scm_t_bits custom_binary_input_port_type = 0;

/* Initial size of the buffer embedded in custom binary input ports.  */
#define CUSTOM_BINARY_INPUT_PORT_BUFFER_SIZE  8192


/* Set PORT's internal buffer according to READ_SIZE.  */
static void
custom_binary_input_port_setvbuf (SCM port, long read_size, long write_size)
{
  SCM bv;
  scm_t_port *pt;
  struct custom_binary_port *stream = (void *) SCM_STREAM (port);

  pt = SCM_PTAB_ENTRY (port);
  bv = stream->read_buffer;

  switch (read_size)
    {
    case 0:
      /* Unbuffered: keep using PORT's bytevector as the underlying
	 buffer (it will also be used by future 'scm_c_read' calls.)  */
      assert (SCM_BYTEVECTOR_LENGTH (bv) >= 1);
      pt->read_buf = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);
      pt->read_buf_size = 1;
      break;

    case -1:
      /* Preferred size: keep the current bytevector and use it as the
	 backing store.  */
      pt->read_buf = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);
      pt->read_buf_size = SCM_BYTEVECTOR_LENGTH (bv);
      break;

    default:
      /* Fully buffered: allocate a buffer of READ_SIZE bytes.  */
      bv = scm_c_make_bytevector (read_size);
      stream->read_buffer = bv;
      pt->read_buf = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);
      pt->read_buf_size = read_size;
    }

  pt->read_pos = pt->read_end = pt->read_buf;
}

static inline SCM
make_custom_binary_input_port (SCM read_proc, SCM get_position_proc,
                               SCM set_position_proc, SCM close_proc)
{
  SCM port, bv;
  char *c_bv;
  unsigned c_len;
  scm_t_port *c_port;
  struct custom_binary_port *stream;
  const unsigned long mode_bits = SCM_OPN | SCM_RDNG;

  /* Use a bytevector as the underlying buffer.  */
  c_len = CUSTOM_BINARY_INPUT_PORT_BUFFER_SIZE;
  bv = scm_c_make_bytevector (c_len);
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);

  stream = scm_gc_typed_calloc (struct custom_binary_port);
  stream->read_buffer = bv;
  stream->read = read_proc;
  stream->write = SCM_BOOL_F;
  stream->get_position = get_position_proc;
  stream->set_position_x = set_position_proc;
  stream->close = close_proc;

  port = scm_c_make_port_with_encoding (custom_binary_input_port_type,
                                        mode_bits,
                                        NULL, /* encoding */
                                        SCM_FAILED_CONVERSION_ERROR,
                                        (scm_t_bits) stream);

  c_port = SCM_PTAB_ENTRY (port);

  /* Have the port directly access the buffer (bytevector).  */
  c_port->read_pos = c_port->read_buf = (unsigned char *) c_bv;
  c_port->read_end = (unsigned char *) c_bv;
  c_port->read_buf_size = c_len;

  return port;
}

static int
custom_binary_input_port_fill_input (SCM port)
#define FUNC_NAME "custom_binary_input_port_fill_input"
{
  int result;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);
  struct custom_binary_port *stream = (void *) SCM_STREAM (port);

  if (c_port->read_pos >= c_port->read_end)
    {
      /* Invoke the user's `read!' procedure.  */
      int buffered;
      size_t c_octets, c_requested;
      SCM bv, octets;

      c_requested = c_port->read_buf_size;

      bv = stream->read_buffer;
      buffered =
	(c_port->read_buf == (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv));

      if (buffered)
	{
	  /* Make sure the buffer isn't corrupt.  Its size can be 1 when
	     someone called 'setvbuf' with 'none.  BV can be passed
	     directly to READ_PROC.  */
	  assert (c_port->read_buf_size == SCM_BYTEVECTOR_LENGTH (bv)
		  || c_port->read_buf_size == 1);
	  c_port->read_pos = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);
	}
      else
	{
	  /* This is an unbuffered port.  When called via the
	     'get-bytevector-*' procedures, and thus via 'scm_c_read', we
	     are passed the caller-provided buffer, so we need to check its
	     size.  */
	  if (SCM_BYTEVECTOR_LENGTH (bv) < c_requested)
            /* Bad luck: we have to make another allocation.  Save that
               nbytevector for later reuse, in the hope that the application
               has regular access patterns.  */
            stream->read_buffer = bv = scm_c_make_bytevector (c_requested);
	}

      octets = scm_call_3 (stream->read, bv, SCM_INUM0,
			   scm_from_size_t (c_requested));
      c_octets = scm_to_size_t (octets);
      if (SCM_UNLIKELY (c_octets > c_requested))
	scm_out_of_range (FUNC_NAME, octets);

      if (!buffered)
	/* Copy the data back to the internal buffer.  */
	memcpy ((char *) c_port->read_pos, SCM_BYTEVECTOR_CONTENTS (bv),
		c_octets);

      c_port->read_end = (unsigned char *) c_port->read_pos + c_octets;

      if (c_octets != 0 || c_requested == 0)
	result = (int) *c_port->read_pos;
      else
	result = EOF;
    }
  else
    result = (int) *c_port->read_pos;

  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_custom_binary_input_port,
	    "make-custom-binary-input-port", 5, 0, 0,
	    (SCM id, SCM read_proc, SCM get_position_proc,
	     SCM set_position_proc, SCM close_proc),
	    "Return a new custom binary input port whose input is drained "
	    "by invoking @var{read_proc} and passing it a bytevector, an "
	    "index where octets should be written, and an octet count.")
#define FUNC_NAME s_scm_make_custom_binary_input_port
{
  SCM_VALIDATE_STRING (1, id);
  SCM_VALIDATE_PROC (2, read_proc);

  if (!scm_is_false (get_position_proc))
    SCM_VALIDATE_PROC (3, get_position_proc);

  if (!scm_is_false (set_position_proc))
    SCM_VALIDATE_PROC (4, set_position_proc);

  if (!scm_is_false (close_proc))
    SCM_VALIDATE_PROC (5, close_proc);

  return make_custom_binary_input_port (read_proc, get_position_proc,
                                        set_position_proc, close_proc);
}
#undef FUNC_NAME


/* Instantiate the custom binary input port type.  */
static inline void
initialize_custom_binary_input_ports (void)
{
  custom_binary_input_port_type =
    scm_make_port_type ("r6rs-custom-binary-input-port",
			custom_binary_input_port_fill_input, NULL);

  scm_set_port_seek (custom_binary_input_port_type, custom_binary_port_seek);
  scm_set_port_close (custom_binary_input_port_type, custom_binary_port_close);
  scm_set_port_setvbuf (custom_binary_input_port_type,
                        custom_binary_input_port_setvbuf);
}




/* Binary input.  */

/* We currently don't support specific binary input ports.  */
#define SCM_VALIDATE_BINARY_INPUT_PORT SCM_VALIDATE_OPINPORT

SCM_DEFINE (scm_get_u8, "get-u8", 1, 0, 0,
	    (SCM port),
	    "Read an octet from @var{port}, a binary input port, "
	    "blocking as necessary.")
#define FUNC_NAME s_scm_get_u8
{
  SCM result;
  int c_result;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  c_result = scm_get_byte_or_eof (port);
  if (c_result == EOF)
    result = SCM_EOF_VAL;
  else
    result = SCM_I_MAKINUM ((unsigned char) c_result);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_lookahead_u8, "lookahead-u8", 1, 0, 0,
	    (SCM port),
	    "Like @code{get-u8} but does not update @var{port} to "
	    "point past the octet.")
#define FUNC_NAME s_scm_lookahead_u8
{
  int u8;
  SCM result;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  u8 = scm_peek_byte_or_eof (port);
  if (u8 == EOF)
    result = SCM_EOF_VAL;
  else
    result = SCM_I_MAKINUM ((scm_t_uint8) u8);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_n, "get-bytevector-n", 2, 0, 0,
	    (SCM port, SCM count),
	    "Read @var{count} octets from @var{port}, blocking as "
	    "necessary and return a bytevector containing the octets "
	    "read.  If fewer bytes are available, a bytevector smaller "
	    "than @var{count} is returned.")
#define FUNC_NAME s_scm_get_bytevector_n
{
  SCM result;
  char *c_bv;
  unsigned c_count;
  size_t c_read;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);
  c_count = scm_to_uint (count);

  result = scm_c_make_bytevector (c_count);
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (result);

  if (SCM_LIKELY (c_count > 0))
    /* XXX: `scm_c_read ()' does not update the port position.  */
    c_read = scm_c_read_unlocked (port, c_bv, c_count);
  else
    /* Don't invoke `scm_c_read ()' since it may block.  */
    c_read = 0;

  if (c_read < c_count)
    {
      if (c_read == 0)
        result = SCM_EOF_VAL;
      else
	result = scm_c_shrink_bytevector (result, c_read);
    }

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_n_x, "get-bytevector-n!", 4, 0, 0,
	    (SCM port, SCM bv, SCM start, SCM count),
	    "Read @var{count} bytes from @var{port} and store them "
	    "in @var{bv} starting at index @var{start}.  Return either "
	    "the number of bytes actually read or the end-of-file "
	    "object.")
#define FUNC_NAME s_scm_get_bytevector_n_x
{
  SCM result;
  char *c_bv;
  unsigned c_start, c_count, c_len;
  size_t c_read;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, bv);
  c_start = scm_to_uint (start);
  c_count = scm_to_uint (count);

  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  c_len = SCM_BYTEVECTOR_LENGTH (bv);

  if (SCM_UNLIKELY (c_start + c_count > c_len))
    scm_out_of_range (FUNC_NAME, count);

  if (SCM_LIKELY (c_count > 0))
    c_read = scm_c_read_unlocked (port, c_bv + c_start, c_count);
  else
    /* Don't invoke `scm_c_read ()' since it may block.  */
    c_read = 0;

  if (c_read == 0 && c_count > 0)
    result = SCM_EOF_VAL;
  else
    result = scm_from_size_t (c_read);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_some, "get-bytevector-some", 1, 0, 0,
	    (SCM port),
            "Read from @var{port}, blocking as necessary, until bytes "
            "are available or an end-of-file is reached.  Return either "
            "the end-of-file object or a new bytevector containing some "
            "of the available bytes (at least one), and update the port "
            "position to point just past these bytes.")
#define FUNC_NAME s_scm_get_bytevector_some
{
  scm_t_port *pt;
  size_t size;
  SCM bv;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);
  pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    scm_flush_unlocked (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (scm_fill_input_unlocked (port) == EOF)
	return SCM_EOF_VAL;
    }

  size = pt->read_end - pt->read_pos;
  if (pt->read_buf == pt->putback_buf)
    size += pt->saved_read_end - pt->saved_read_pos;

  bv = scm_c_make_bytevector (size);
  scm_take_from_input_buffers
    (port, (char *) SCM_BYTEVECTOR_CONTENTS (bv), size);

  return bv;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_bytevector_all, "get-bytevector-all", 1, 0, 0,
	    (SCM port),
	    "Read from @var{port}, blocking as necessary, until "
	    "the end-of-file is reached.  Return either "
	    "a new bytevector containing the data read or the "
	    "end-of-file object (if no data were available).")
#define FUNC_NAME s_scm_get_bytevector_all
{
  SCM result;
  char *c_bv;
  unsigned c_len, c_count;
  size_t c_read, c_total;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);

  c_len = c_count = 4096;
  c_bv = (char *) scm_gc_malloc_pointerless (c_len, SCM_GC_BYTEVECTOR);
  c_total = c_read = 0;

  do
    {
      if (c_total + c_read > c_len)
	{
	  /* Grow the bytevector.  */
	  c_bv = (char *) scm_gc_realloc (c_bv, c_len, c_len * 2,
					  SCM_GC_BYTEVECTOR);
	  c_count = c_len;
	  c_len *= 2;
	}

      /* `scm_c_read ()' blocks until C_COUNT bytes are available or EOF is
	 reached.  */
      c_read = scm_c_read_unlocked (port, c_bv + c_total, c_count);
      c_total += c_read, c_count -= c_read;
    }
  while (c_count == 0);

  if (c_total == 0)
    {
      result = SCM_EOF_VAL;
      scm_gc_free (c_bv, c_len, SCM_GC_BYTEVECTOR);
    }
  else
    {
      if (c_len > c_total)
	{
	  /* Shrink the bytevector.  */
	  c_bv = (char *) scm_gc_realloc (c_bv, c_len, c_total,
					  SCM_GC_BYTEVECTOR);
	  c_len = (unsigned) c_total;
	}

      result = scm_c_take_gc_bytevector ((signed char *) c_bv, c_len,
                                         SCM_BOOL_F);
    }

  return result;
}
#undef FUNC_NAME




/* Binary output.  */

/* We currently don't support specific binary input ports.  */
#define SCM_VALIDATE_BINARY_OUTPUT_PORT SCM_VALIDATE_OPOUTPORT


SCM_DEFINE (scm_put_u8, "put-u8", 2, 0, 0,
	    (SCM port, SCM octet),
	    "Write @var{octet} to binary port @var{port}.")
#define FUNC_NAME s_scm_put_u8
{
  scm_t_uint8 c_octet;

  SCM_VALIDATE_BINARY_OUTPUT_PORT (1, port);
  c_octet = scm_to_uint8 (octet);

  scm_putc_unlocked ((char) c_octet, port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_put_bytevector, "put-bytevector", 2, 2, 0,
	    (SCM port, SCM bv, SCM start, SCM count),
	    "Write the contents of @var{bv} to @var{port}, optionally "
	    "starting at index @var{start} and limiting to @var{count} "
	    "octets.")
#define FUNC_NAME s_scm_put_bytevector
{
  char *c_bv;
  unsigned c_start, c_count, c_len;

  SCM_VALIDATE_BINARY_OUTPUT_PORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (!scm_is_eq (start, SCM_UNDEFINED))
    {
      c_start = scm_to_uint (start);

      if (!scm_is_eq (count, SCM_UNDEFINED))
	{
	  c_count = scm_to_uint (count);
	  if (SCM_UNLIKELY (c_start + c_count > c_len))
	    scm_out_of_range (FUNC_NAME, count);
	}
      else
	{
	  if (SCM_UNLIKELY (c_start >= c_len))
	    scm_out_of_range (FUNC_NAME, start);
	  else
	    c_count = c_len - c_start;
	}
    }
  else
    c_start = 0, c_count = c_len;

  scm_c_write_unlocked (port, c_bv + c_start, c_count);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unget_bytevector, "unget-bytevector", 2, 2, 0,
	    (SCM port, SCM bv, SCM start, SCM count),
	    "Unget the contents of @var{bv} to @var{port}, optionally "
	    "starting at index @var{start} and limiting to @var{count} "
	    "octets.")
#define FUNC_NAME s_scm_unget_bytevector
{
  unsigned char *c_bv;
  size_t c_start, c_count, c_len;

  SCM_VALIDATE_BINARY_INPUT_PORT (1, port);
  SCM_VALIDATE_BYTEVECTOR (2, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (!scm_is_eq (start, SCM_UNDEFINED))
    {
      c_start = scm_to_size_t (start);

      if (!scm_is_eq (count, SCM_UNDEFINED))
	{
	  c_count = scm_to_size_t (count);
	  if (SCM_UNLIKELY (c_start + c_count > c_len))
	    scm_out_of_range (FUNC_NAME, count);
	}
      else
	{
	  if (SCM_UNLIKELY (c_start >= c_len))
	    scm_out_of_range (FUNC_NAME, start);
	  else
	    c_count = c_len - c_start;
	}
    }
  else
    c_start = 0, c_count = c_len;

  scm_unget_bytes (c_bv + c_start, c_count, port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




/* Bytevector output port.  */

/* Implementation of "bytevector output ports".

   Each bytevector output port has an internal buffer, of type
   `scm_t_bytevector_output_port_buffer', attached to it.  The procedure
   returned along with the output port is actually an applicable SMOB.
   The SMOB holds a reference to the port.  When applied, the SMOB
   swallows the port's internal buffer, turning it into a bytevector,
   and resets it.

   XXX: Access to a bytevector output port's internal buffer is not
   thread-safe.  */

static scm_t_bits bytevector_output_port_type = 0;

SCM_SMOB (bytevector_output_port_procedure,
	  "r6rs-bytevector-output-port-procedure",
	  0);

#define SCM_GC_BYTEVECTOR_OUTPUT_PORT "r6rs-bytevector-output-port"
#define SCM_BYTEVECTOR_OUTPUT_PORT_BUFFER_INITIAL_SIZE 4096

/* Representation of a bytevector output port's internal buffer.  */
typedef struct
{
  size_t total_len;
  size_t len;
  size_t pos;
  char  *buffer;
} scm_t_bytevector_output_port_buffer;


/* Accessing a bytevector output port's buffer.  */
#define SCM_BYTEVECTOR_OUTPUT_PORT_BUFFER(_port) \
  ((scm_t_bytevector_output_port_buffer *) SCM_STREAM (_port))
#define SCM_SET_BYTEVECTOR_OUTPUT_PORT_BUFFER(_port, _buf) \
  (SCM_SETSTREAM ((_port), (scm_t_bits) (_buf)))


static inline void
bytevector_output_port_buffer_init (scm_t_bytevector_output_port_buffer *buf)
{
  buf->total_len = buf->len = buf->pos = 0;
  buf->buffer = NULL;
}

static inline void
bytevector_output_port_buffer_grow (scm_t_bytevector_output_port_buffer *buf,
                                    size_t min_size)
{
  char *new_buf;
  size_t new_size;

  for (new_size = buf->total_len
	 ? buf->total_len : SCM_BYTEVECTOR_OUTPUT_PORT_BUFFER_INITIAL_SIZE;
       new_size < min_size;
       new_size *= 2);

  if (buf->buffer)
    new_buf = scm_gc_realloc ((void *) buf->buffer, buf->total_len,
			      new_size, SCM_GC_BYTEVECTOR_OUTPUT_PORT);
  else
    new_buf = scm_gc_malloc_pointerless (new_size,
                                         SCM_GC_BYTEVECTOR_OUTPUT_PORT);

  buf->buffer = new_buf;
  buf->total_len = new_size;
}

static inline SCM
make_bytevector_output_port (void)
{
  SCM port, proc;
  scm_t_port *c_port;
  scm_t_bytevector_output_port_buffer *buf;
  const unsigned long mode_bits = SCM_OPN | SCM_WRTNG;

  buf = (scm_t_bytevector_output_port_buffer *)
    scm_gc_malloc (sizeof (* buf), SCM_GC_BYTEVECTOR_OUTPUT_PORT);
  bytevector_output_port_buffer_init (buf);

  port = scm_c_make_port_with_encoding (bytevector_output_port_type,
                                        mode_bits,
                                        NULL, /* encoding */
                                        SCM_FAILED_CONVERSION_ERROR,
                                        (scm_t_bits)buf);

  c_port = SCM_PTAB_ENTRY (port);

  c_port->write_buf = c_port->write_pos = c_port->write_end = NULL;
  c_port->write_buf_size = 0;

  /* Make the bytevector output port procedure.  */
  SCM_NEWSMOB (proc, bytevector_output_port_procedure, buf);

  return (scm_values (scm_list_2 (port, proc)));
}

/* Write SIZE octets from DATA to PORT.  */
static void
bytevector_output_port_write (SCM port, const void *data, size_t size)
{
  scm_t_bytevector_output_port_buffer *buf;

  buf = SCM_BYTEVECTOR_OUTPUT_PORT_BUFFER (port);

  if (buf->pos + size > buf->total_len)
    bytevector_output_port_buffer_grow (buf, buf->pos + size);

  memcpy (buf->buffer + buf->pos, data, size);
  buf->pos += size;
  buf->len = (buf->len > buf->pos) ? buf->len : buf->pos;
}

static scm_t_off
bytevector_output_port_seek (SCM port, scm_t_off offset, int whence)
#define FUNC_NAME "bytevector_output_port_seek"
{
  scm_t_bytevector_output_port_buffer *buf;

  buf = SCM_BYTEVECTOR_OUTPUT_PORT_BUFFER (port);
  switch (whence)
    {
    case SEEK_CUR:
      offset += (scm_t_off) buf->pos;
      /* Fall through.  */

    case SEEK_SET:
      if (offset < 0 || (unsigned) offset > buf->len)
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      else
	buf->pos = offset;
      break;

    case SEEK_END:
      if (offset < 0 || (unsigned) offset >= buf->len)
	scm_out_of_range (FUNC_NAME, scm_from_int (offset));
      else
	buf->pos = buf->len - (offset + 1);
      break;

    default:
      scm_wrong_type_arg_msg (FUNC_NAME, 0, port,
			      "invalid `seek' parameter");
    }

  return buf->pos;
}
#undef FUNC_NAME

/* Fetch data from a bytevector output port.  */
SCM_SMOB_APPLY (bytevector_output_port_procedure,
		bytevector_output_port_proc_apply, 0, 0, 0, (SCM proc))
{
  SCM bv;
  scm_t_bytevector_output_port_buffer *buf, result_buf;

  buf = (scm_t_bytevector_output_port_buffer *) SCM_SMOB_DATA (proc);

  result_buf = *buf;
  bytevector_output_port_buffer_init (buf);

  if (result_buf.len == 0)
    bv = scm_c_take_gc_bytevector (NULL, 0, SCM_BOOL_F);
  else
    {
      if (result_buf.total_len > result_buf.len)
	/* Shrink the buffer.  */
	result_buf.buffer = scm_gc_realloc ((void *) result_buf.buffer,
					    result_buf.total_len,
					    result_buf.len,
					    SCM_GC_BYTEVECTOR_OUTPUT_PORT);

      bv = scm_c_take_gc_bytevector ((signed char *) result_buf.buffer,
                                     result_buf.len, SCM_BOOL_F);
    }

  return bv;
}

SCM_DEFINE (scm_open_bytevector_output_port,
	    "open-bytevector-output-port", 0, 1, 0,
	    (SCM transcoder),
	    "Return two values: an output port and a procedure.  The latter "
	    "should be called with zero arguments to obtain a bytevector "
	    "containing the data accumulated by the port.")
#define FUNC_NAME s_scm_open_bytevector_output_port
{
  if (!SCM_UNBNDP (transcoder) && !scm_is_false (transcoder))
    transcoders_not_implemented ();

  return make_bytevector_output_port ();
}
#undef FUNC_NAME

static inline void
initialize_bytevector_output_ports (void)
{
  bytevector_output_port_type =
    scm_make_port_type ("r6rs-bytevector-output-port",
			NULL, bytevector_output_port_write);

  scm_set_port_seek (bytevector_output_port_type, bytevector_output_port_seek);
}




/* Custom binary output ports.  */

static scm_t_bits custom_binary_output_port_type;


static inline SCM
make_custom_binary_output_port (SCM write_proc, SCM get_position_proc,
                                SCM set_position_proc, SCM close_proc)
{
  SCM port;
  scm_t_port *c_port;
  struct custom_binary_port *stream;
  const unsigned long mode_bits = SCM_OPN | SCM_WRTNG;

  /* Store the various methods and bytevector in a vector.  */
  stream = scm_gc_typed_calloc (struct custom_binary_port);

  stream->read_buffer = SCM_BOOL_F;
  stream->read = SCM_BOOL_F;
  stream->write = write_proc;
  stream->get_position = get_position_proc;
  stream->set_position_x = set_position_proc;
  stream->close = close_proc;

  port = scm_c_make_port_with_encoding (custom_binary_output_port_type,
                                        mode_bits,
                                        NULL, /* encoding */
                                        SCM_FAILED_CONVERSION_ERROR,
                                        (scm_t_bits) stream);

  c_port = SCM_PTAB_ENTRY (port);

  /* Have the port directly access the buffer (bytevector).  */
  c_port->write_buf = c_port->write_pos = c_port->write_end = NULL;
  c_port->write_buf_size = c_port->read_buf_size = 0;

  return port;
}

/* Write SIZE octets from DATA to PORT.  */
static void
custom_binary_output_port_write (SCM port, const void *data, size_t size)
#define FUNC_NAME "custom_binary_output_port_write"
{
  long int c_result;
  size_t c_written;
  struct custom_binary_port *stream = (void *) SCM_STREAM (port);
  SCM bv, result;

  /* XXX: Allocating a new bytevector at each `write' call is inefficient,
     but necessary since (1) we don't control the lifetime of the buffer
     pointed to by DATA, and (2) the `write!' procedure could capture the
     bytevector it is passed.  */
  bv = scm_c_make_bytevector (size);
  memcpy (SCM_BYTEVECTOR_CONTENTS (bv), data, size);

  /* Since the `write' procedure of Guile's ports has type `void', it must
     try hard to write exactly SIZE bytes, regardless of how many bytes the
     sink can handle.  */
  for (c_written = 0;
       c_written < size;
       c_written += c_result)
    {
      result = scm_call_3 (stream->write, bv,
			   scm_from_size_t (c_written),
			   scm_from_size_t (size - c_written));

      c_result = scm_to_long (result);
      if (SCM_UNLIKELY (c_result < 0
			|| (size_t) c_result > (size - c_written)))
	scm_wrong_type_arg_msg (FUNC_NAME, 0, result,
				"R6RS custom binary output port `write!' "
				"returned a incorrect integer");
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_custom_binary_output_port,
	    "make-custom-binary-output-port", 5, 0, 0,
	    (SCM id, SCM write_proc, SCM get_position_proc,
	     SCM set_position_proc, SCM close_proc),
	    "Return a new custom binary output port whose output is drained "
	    "by invoking @var{write_proc} and passing it a bytevector, an "
	    "index where octets should be written, and an octet count.")
#define FUNC_NAME s_scm_make_custom_binary_output_port
{
  SCM_VALIDATE_STRING (1, id);
  SCM_VALIDATE_PROC (2, write_proc);

  if (!scm_is_false (get_position_proc))
    SCM_VALIDATE_PROC (3, get_position_proc);

  if (!scm_is_false (set_position_proc))
    SCM_VALIDATE_PROC (4, set_position_proc);

  if (!scm_is_false (close_proc))
    SCM_VALIDATE_PROC (5, close_proc);

  return make_custom_binary_output_port (write_proc, get_position_proc,
                                         set_position_proc, close_proc);
}
#undef FUNC_NAME


/* Instantiate the custom binary output port type.  */
static inline void
initialize_custom_binary_output_ports (void)
{
  custom_binary_output_port_type =
    scm_make_port_type ("r6rs-custom-binary-output-port",
			NULL, custom_binary_output_port_write);

  scm_set_port_seek (custom_binary_output_port_type, custom_binary_port_seek);
  scm_set_port_close (custom_binary_output_port_type, custom_binary_port_close);
}




/* Transcoded ports.  */

static scm_t_bits transcoded_port_type = 0;

#define TRANSCODED_PORT_INPUT_BUFFER_SIZE 4096

#define SCM_TRANSCODED_PORT_BINARY_PORT(_port) SCM_PACK (SCM_STREAM (_port))

static inline SCM
make_transcoded_port (SCM binary_port, unsigned long mode)
{
  SCM port;
  scm_t_port *c_port;
  const unsigned long mode_bits = SCM_OPN | mode;
  
  port = scm_c_make_port (transcoded_port_type, mode_bits,
                          SCM_UNPACK (binary_port));

  if (SCM_INPUT_PORT_P (port))
    {
      c_port = SCM_PTAB_ENTRY (port);
      c_port->read_buf =
        scm_gc_malloc_pointerless (TRANSCODED_PORT_INPUT_BUFFER_SIZE,
                                   "port buffer");
      c_port->read_pos = c_port->read_end = c_port->read_buf;
      c_port->read_buf_size = TRANSCODED_PORT_INPUT_BUFFER_SIZE;
      
      SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) & ~SCM_BUF0);
    }
  
  return port;
}

static void
transcoded_port_write (SCM port, const void *data, size_t size)
{
  scm_c_write_unlocked (SCM_TRANSCODED_PORT_BINARY_PORT (port), data, size);
}

static int
transcoded_port_fill_input (SCM port)
{
  size_t count;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);
  SCM bport = SCM_TRANSCODED_PORT_BINARY_PORT (port);
  scm_t_port *c_bport = SCM_PTAB_ENTRY (bport);

  /* We can't use `scm_c_read' here, since it blocks until the whole
     block has been read or EOF. */
  
  if (c_bport->rw_active == SCM_PORT_WRITE)
    scm_force_output (bport);

  if (c_bport->read_pos >= c_bport->read_end)
    scm_fill_input_unlocked (bport);
  
  count = c_bport->read_end - c_bport->read_pos;
  if (count > c_port->read_buf_size)
    count = c_port->read_buf_size;

  memcpy (c_port->read_buf, c_bport->read_pos, count);
  c_bport->read_pos += count;

  if (c_bport->rw_random)
    c_bport->rw_active = SCM_PORT_READ;

  if (count == 0)
    return EOF;
  else
    {
      c_port->read_pos = c_port->read_buf;
      c_port->read_end = c_port->read_buf + count;
      return *c_port->read_buf;
    }
}

static void
transcoded_port_flush (SCM port)
{
  SCM binary_port = SCM_TRANSCODED_PORT_BINARY_PORT (port);
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);
  size_t count = c_port->write_pos - c_port->write_buf;

  /* As the runtime will try to flush all ports upon exit, we test for
     the underlying port still being open here. Otherwise, when you
     would explicitly close the underlying port and the transcoded port
     still had data outstanding, you'd get an exception on Guile exit.
     
     We just throw away the data when the underlying port is closed.  */
  
  if (SCM_OPOUTPORTP (binary_port))
      scm_c_write_unlocked (binary_port, c_port->write_buf, count);

  c_port->write_pos = c_port->write_buf;

  if (SCM_OPOUTPORTP (binary_port))
    scm_force_output (binary_port);
}

static void
transcoded_port_close (SCM port)
{
  SCM bport = SCM_TRANSCODED_PORT_BINARY_PORT (port);
  if (SCM_OUTPUT_PORT_P (port))
    transcoded_port_flush (port);
  scm_close_port (bport);
}

static inline void
initialize_transcoded_ports (void)
{
  transcoded_port_type =
    scm_make_port_type ("r6rs-transcoded-port", transcoded_port_fill_input,
                        transcoded_port_write);
  
  scm_set_port_flush (transcoded_port_type, transcoded_port_flush);
  scm_set_port_close (transcoded_port_type, transcoded_port_close);
  scm_set_port_needs_close_on_gc (transcoded_port_type, 1);
}

SCM_INTERNAL SCM scm_i_make_transcoded_port (SCM);

SCM_DEFINE (scm_i_make_transcoded_port,
	    "%make-transcoded-port", 1, 0, 0,
	    (SCM port),
	    "Return a new port which reads and writes to @var{port}")
#define FUNC_NAME s_scm_i_make_transcoded_port
{
  SCM result;
  unsigned long mode = 0;
  
  SCM_VALIDATE_PORT (SCM_ARG1, port);

  if (scm_is_true (scm_output_port_p (port)))
    mode |= SCM_WRTNG;
  else if (scm_is_true (scm_input_port_p (port)))
    mode |=  SCM_RDNG;
  
  result = make_transcoded_port (port, mode);

  /* FIXME: We should actually close `port' "in a special way" here,
     according to R6RS.  As there is no way to do that in Guile without
     rendering the underlying port unusable for our purposes as well, we
     just leave it open. */
  
  return result;
}
#undef FUNC_NAME


/* Textual I/O */

SCM_DEFINE (scm_get_string_n_x,
            "get-string-n!", 4, 0, 0,
            (SCM port, SCM str, SCM start, SCM count),
            "Read up to @var{count} characters from @var{port} into "
            "@var{str}, starting at @var{start}.  If no characters "
            "can be read before the end of file is encountered, the end "
            "of file object is returned.  Otherwise, the number of "
            "characters read is returned.")
#define FUNC_NAME s_scm_get_string_n_x
{
  size_t c_start, c_count, c_len, c_end, j;
  scm_t_wchar c;

  SCM_VALIDATE_OPINPORT (1, port);
  SCM_VALIDATE_STRING (2, str);
  c_len = scm_c_string_length (str);
  c_start = scm_to_size_t (start);
  c_count = scm_to_size_t (count);
  c_end = c_start + c_count;

  if (SCM_UNLIKELY (c_end > c_len))
    scm_out_of_range (FUNC_NAME, count);

  for (j = c_start; j < c_end; j++)
    {
      c = scm_getc_unlocked (port);
      if (c == EOF)
        {
          size_t chars_read = j - c_start;
          return chars_read == 0 ? SCM_EOF_VAL : scm_from_size_t (chars_read);
        }
      scm_c_string_set_x (str, j, SCM_MAKE_CHAR (c));
    }
  return count;
}
#undef FUNC_NAME


/* Initialization.  */

void
scm_register_r6rs_ports (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_r6rs_ports",
			    (scm_t_extension_init_func) scm_init_r6rs_ports,
			    NULL);
}

void
scm_init_r6rs_ports (void)
{
#include "libguile/r6rs-ports.x"

  initialize_bytevector_input_ports ();
  initialize_custom_binary_input_ports ();
  initialize_bytevector_output_ports ();
  initialize_custom_binary_output_ports ();
  initialize_transcoded_ports ();
}
