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

#include "libguile/_scm.h"
#include "libguile/ports.h"

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

enum scm_port_encoding_mode {
  SCM_PORT_ENCODING_MODE_UTF8,
  SCM_PORT_ENCODING_MODE_LATIN1,
  SCM_PORT_ENCODING_MODE_ICONV
};

typedef enum scm_port_encoding_mode scm_t_port_encoding_mode;

/* This is a separate object so that only those ports that use iconv
   cause finalizers to be registered.  */
struct scm_iconv_descriptors
{
  /* input/output iconv conversion descriptors */
  void *input_cd;
  void *output_cd;
};

typedef struct scm_iconv_descriptors scm_t_iconv_descriptors;

struct scm_port_internal
{
  unsigned at_stream_start_for_bom_read  : 1;
  unsigned at_stream_start_for_bom_write : 1;
  scm_t_iconv_descriptors *iconv_descriptors;
  SCM alist;
};

typedef struct scm_port_internal scm_t_port_internal;

#define SCM_UNICODE_BOM  0xFEFFUL  /* Unicode byte-order mark */

#define SCM_PORT_GET_INTERNAL(x)  (SCM_PTAB_ENTRY(x)->internal)

typedef enum scm_t_port_rw_active {
  SCM_PORT_NEITHER = 0,
  SCM_PORT_READ = 1,
  SCM_PORT_WRITE = 2
} scm_t_port_rw_active;

SCM_INTERNAL scm_t_iconv_descriptors *
scm_i_port_iconv_descriptors (SCM port, scm_t_port_rw_active mode);

#endif
