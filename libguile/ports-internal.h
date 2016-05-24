/*
 * ports-internal.h - internal-only declarations for ports.
 *
 * Copyright (C) 2013 Free Software Foundation, Inc.
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

#ifndef SCM_PORTS_INTERNAL
#define SCM_PORTS_INTERNAL

#include <assert.h>
#include <iconv.h>

#include "libguile/_scm.h"
#include "libguile/ports.h"

typedef enum scm_t_port_type_flags {
  /* Indicates that the port should be closed if it is garbage collected
     while it is open.  */
  SCM_PORT_TYPE_NEEDS_CLOSE_ON_GC = 1 << 0
} scm_t_port_type_flags;

/* port-type description.  */
struct scm_t_port_type
{
  char *name;
  int (*print) (SCM exp, SCM port, scm_print_state *pstate);

  size_t (*c_read) (SCM port, SCM dst, size_t start, size_t count);
  size_t (*c_write) (SCM port, SCM src, size_t start, size_t count);
  SCM scm_read;
  SCM scm_write;

  int (*read_wait_fd) (SCM port);
  int (*write_wait_fd) (SCM port);

  scm_t_off (*seek) (SCM port, scm_t_off OFFSET, int WHENCE);
  void (*close) (SCM port);

  void (*get_natural_buffer_sizes) (SCM port, size_t *read_size,
                                    size_t *write_size);
  int (*random_access_p) (SCM port);

  int (*input_waiting) (SCM port);

  void (*truncate) (SCM port, scm_t_off length);

  unsigned flags;

  /* GOOPS tomfoolery.  */
  SCM input_class, output_class, input_output_class;
};

/* Port buffers.

   It's important to avoid calling into the kernel too many times.  For
   that reason we buffer the input and output, using "port buffer"
   objects.  Port buffers are represented as vectors containing the
   buffer, two cursors, and a flag.  The bytes in a read buffer are laid
   out like this:

                    |already read | not yet | invalid
                    |    data     |  read   |  data
      readbuf: #vu8(|r r r r r r r|u u u u u|x x x x x|)
               ^buf               ^cur      ^end      ^size(buf)

   Similarly for a write buffer:

                     |already written | not yet | invalid
                     |    data        | written |  data
      writebuf: #vu8(|w w w w w w w w |u u u u u|x x x x x|)
                ^buf                  ^cur      ^end      ^size(buf)

   We use the same port buffer data structure for both purposes.  Port
   buffers are implemented as their own object so that they can be
   atomically swapped in or out of ports, and as Scheme vectors so they
   can be manipulated from Scheme.  */

enum scm_port_buffer_field {
  SCM_PORT_BUFFER_FIELD_BYTEVECTOR,
  SCM_PORT_BUFFER_FIELD_CUR,
  SCM_PORT_BUFFER_FIELD_END,
  SCM_PORT_BUFFER_FIELD_HAS_EOF_P,
  SCM_PORT_BUFFER_FIELD_POSITION,
  SCM_PORT_BUFFER_FIELD_COUNT
};

/* The port buffers are exposed to Scheme, which can mutate their
   fields.  We have to do dynamic checks to ensure that
   potentially-malicious Scheme doesn't invalidate our invariants.
   However these dynamic checks are slow, so we need to avoid them where
   they are unnecessary.  An unnecessary check is a check which has
   already been performed, or one which would already be performed by
   the time that memory is accessed.  Given that the "can_take",
   "can_put", or "can_putback" functions are eventually called before
   any access to the buffer, we hoist the necessary type checks the
   can_foo and size functions, and otherwise assume that the cur and end
   values are inums within the right ranges.  */

static inline SCM
scm_port_buffer_bytevector (SCM buf)
{
  return SCM_SIMPLE_VECTOR_REF (buf, SCM_PORT_BUFFER_FIELD_BYTEVECTOR);
}

static inline SCM
scm_port_buffer_cur (SCM buf)
{
  return SCM_SIMPLE_VECTOR_REF (buf, SCM_PORT_BUFFER_FIELD_CUR);
}

static inline void
scm_port_buffer_set_cur (SCM buf, SCM cur)
{
  SCM_SIMPLE_VECTOR_SET (buf, SCM_PORT_BUFFER_FIELD_CUR, cur);
}

static inline SCM
scm_port_buffer_end (SCM buf)
{
  return SCM_SIMPLE_VECTOR_REF (buf, SCM_PORT_BUFFER_FIELD_END);
}

static inline void
scm_port_buffer_set_end (SCM buf, SCM end)
{
  SCM_SIMPLE_VECTOR_SET (buf, SCM_PORT_BUFFER_FIELD_END, end);
}

static inline SCM
scm_port_buffer_has_eof_p (SCM buf)
{
  return SCM_SIMPLE_VECTOR_REF (buf, SCM_PORT_BUFFER_FIELD_HAS_EOF_P);
}

static inline void
scm_port_buffer_set_has_eof_p (SCM buf, SCM has_eof_p)
{
  SCM_SIMPLE_VECTOR_SET (buf, SCM_PORT_BUFFER_FIELD_HAS_EOF_P,
                         has_eof_p);
}

/* The port position object is a pair that is referenced by the port.
   To make things easier for Scheme port code, it is also referenced by
   port buffers.  */
static inline SCM
scm_port_buffer_position (SCM buf)
{
  return SCM_SIMPLE_VECTOR_REF (buf, SCM_PORT_BUFFER_FIELD_POSITION);
}

static inline SCM
scm_port_position_line (SCM position)
{
  return scm_car (position);
}

static inline void
scm_port_position_set_line (SCM position, SCM line)
{
  scm_set_car_x (position, line);
}

static inline SCM
scm_port_position_column (SCM position)
{
  return scm_cdr (position);
}

static inline void
scm_port_position_set_column (SCM position, SCM column)
{
  scm_set_cdr_x (position, column);
}

static inline size_t
scm_port_buffer_size (SCM buf)
{
  SCM bv = scm_port_buffer_bytevector (buf);
  if (SCM_LIKELY (SCM_BYTEVECTOR_P (bv)))
    return SCM_BYTEVECTOR_LENGTH (bv);
  scm_misc_error (NULL, "invalid port buffer ~a", scm_list_1 (bv));
  return -1;
}

static inline void
scm_port_buffer_reset (SCM buf)
{
  scm_port_buffer_set_cur (buf, SCM_INUM0);
  scm_port_buffer_set_end (buf, SCM_INUM0);
}

static inline void
scm_port_buffer_reset_end (SCM buf)
{
  scm_port_buffer_set_cur (buf, scm_from_size_t (scm_port_buffer_size (buf)));
  scm_port_buffer_set_end (buf, scm_from_size_t (scm_port_buffer_size (buf)));
}

static inline size_t
scm_port_buffer_can_take (SCM buf)
{
  size_t cur, end;
  cur = scm_to_size_t (scm_port_buffer_cur (buf));
  end = scm_to_size_t (scm_port_buffer_end (buf));
  if (cur > end || end > scm_port_buffer_size (buf))
    scm_misc_error (NULL, "invalid port buffer ~a", scm_list_1 (buf));
  return end - cur;
}

static inline size_t
scm_port_buffer_can_put (SCM buf)
{
  size_t end = scm_to_size_t (scm_port_buffer_end (buf));
  if (end > scm_port_buffer_size (buf))
    scm_misc_error (NULL, "invalid port buffer ~a", scm_list_1 (buf));
  return scm_port_buffer_size (buf) - end;
}

static inline size_t
scm_port_buffer_can_putback (SCM buf)
{
  size_t cur = scm_to_size_t (scm_port_buffer_cur (buf));
  if (cur > scm_port_buffer_size (buf))
    scm_misc_error (NULL, "invalid port buffer ~a", scm_list_1 (buf));
  return cur;
}

static inline void
scm_port_buffer_did_take (SCM buf, size_t count)
{
  scm_port_buffer_set_cur
    (buf, SCM_I_MAKINUM (SCM_I_INUM (scm_port_buffer_cur (buf)) + count));
}

static inline void
scm_port_buffer_did_put (SCM buf, size_t count)
{
  scm_port_buffer_set_end
    (buf, SCM_I_MAKINUM (SCM_I_INUM (scm_port_buffer_end (buf)) + count));
}

static inline const scm_t_uint8 *
scm_port_buffer_take_pointer (SCM buf)
{
  signed char *ret = SCM_BYTEVECTOR_CONTENTS (scm_port_buffer_bytevector (buf));
  return ((scm_t_uint8 *) ret) + scm_to_size_t (scm_port_buffer_cur (buf));
}

static inline scm_t_uint8 *
scm_port_buffer_put_pointer (SCM buf)
{
  signed char *ret = SCM_BYTEVECTOR_CONTENTS (scm_port_buffer_bytevector (buf));
  return ((scm_t_uint8 *) ret) + scm_to_size_t (scm_port_buffer_end (buf));
}

static inline size_t
scm_port_buffer_take (SCM buf, scm_t_uint8 *dst, size_t count)
{
  count = min (count, scm_port_buffer_can_take (buf));
  if (dst)
    memcpy (dst, scm_port_buffer_take_pointer (buf), count);
  scm_port_buffer_did_take (buf, count);
  return count;
}

static inline size_t
scm_port_buffer_put (SCM buf, const scm_t_uint8 *src, size_t count)
{
  count = min (count, scm_port_buffer_can_put (buf));
  if (src)
    memcpy (scm_port_buffer_put_pointer (buf), src, count);
  scm_port_buffer_did_put (buf, count);
  return count;
}

static inline void
scm_port_buffer_putback (SCM buf, const scm_t_uint8 *src, size_t count)
{
  size_t cur = scm_to_size_t (scm_port_buffer_cur (buf));

  assert (count <= cur);

  /* Sometimes used to move around data within a buffer, so we must use
     memmove.  */
  cur -= count;
  scm_port_buffer_set_cur (buf, scm_from_size_t (cur));
  memmove (SCM_BYTEVECTOR_CONTENTS (scm_port_buffer_bytevector (buf)) + cur,
           src, count);
}

struct scm_t_port
{
  /* Source location information.  */
  SCM file_name;
  SCM position;

  /* Port buffers.  */
  SCM read_buf;
  SCM write_buf;
  SCM write_buf_aux;

  /* All ports have read and write buffers; an unbuffered port simply
     has a one-byte buffer.  However unreading bytes can expand the read
     buffer, but that doesn't mean that we want to increase the input
     buffering.  For that reason `read_buffering' is a separate
     indication of how many characters to buffer on the read side.
     There isn't a write_buf_size because there isn't an
     `unwrite-byte'.  */
  size_t read_buffering;

  /* True if the port is random access.  Implies that the buffers must
     be flushed before switching between reading and writing, seeking,
     and so on.  */
  unsigned rw_random : 1;
  unsigned at_stream_start_for_bom_read  : 1;
  unsigned at_stream_start_for_bom_write : 1;

  /* Character encoding support.  */
  SCM encoding;  /* A symbol of upper-case ASCII.  */
  SCM conversion_strategy; /* A symbol; either substitute, error, or escape.  */

  /* This is the same as pt->encoding, except if `encoding' is UTF-16 or
     UTF-32, in which case this is UTF-16LE or a similar
     byte-order-specialed version of UTF-16 or UTF-32.  This is a
     separate field from `encoding' because being just plain UTF-16 or
     UTF-32 has an additional meaning, being that we should consume and
     produce byte order marker codepoints as appropriate.  Set to #f
     before the iconv descriptors have been opened.  */
  SCM precise_encoding;  /* with iconv_lock */
  iconv_t input_cd;      /* with iconv_lock */
  iconv_t output_cd;     /* with iconv_lock */

  /* Port properties.  */
  SCM alist;
};

#define SCM_UNICODE_BOM  0xFEFFUL  /* Unicode byte-order mark */

#define SCM_FILENAME(x)           (SCM_PORT (x)->file_name)
#define SCM_SET_FILENAME(x, n)    (SCM_PORT (x)->file_name = (n))

SCM_INTERNAL void scm_port_acquire_iconv_descriptors (SCM port,
                                                      iconv_t *input_cd,
                                                      iconv_t *output_cd);
SCM_INTERNAL void scm_port_release_iconv_descriptors (SCM port);

#endif
