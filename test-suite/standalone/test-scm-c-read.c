/* Copyright (C) 2008, 2014 Free Software Foundation, Inc.
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

/* Exercise `scm_c_read ()' and the port type API.  Verify assumptions that
   can be made by port type implementations.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#undef NDEBUG

#include <libguile.h>
#include <assert.h>



/* Size of our port's internal buffer.  */
#define PORT_BUFFER_SIZE 1024

struct custom_port
{
  size_t pos;
  size_t len;
  char *buf;
};


/* Return a new port of type PORT_TYPE.  */
static inline SCM
make_port (scm_t_port_type *port_type)
{
  struct custom_port *stream = scm_gc_typed_calloc (struct custom_port);

  stream->pos = 0;
  stream->len = PORT_BUFFER_SIZE;
  stream->buf = scm_gc_calloc (stream->len, "custom-port-buffer");

  return scm_c_make_port (port_type, SCM_RDNG, (scm_t_bits) stream);
}

static size_t
custom_port_read (SCM port, SCM dst, size_t start, size_t count)
{
  size_t to_copy = count;
  struct custom_port *stream = (void *) SCM_STREAM (port);

  if (stream->pos + to_copy > stream->len)
    to_copy = stream->len - stream->pos;

  memcpy (SCM_BYTEVECTOR_CONTENTS (dst) + start,
          stream->buf + stream->pos, to_copy);
  stream->pos += to_copy;

  return to_copy;
}

/* Return true (non-zero) if BUF contains only zeros.  */
static inline int
zeroed_buffer_p (const char *buf, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    if (buf[i] != 0)
      return 0;

  return 1;
}

/* Run the test.  */
static void *
do_start (void *arg)
{
  SCM port;
  scm_t_port_type *port_type;
  char buffer[PORT_BUFFER_SIZE + (PORT_BUFFER_SIZE / 2)];
  size_t read, last_read;

  port_type = scm_make_port_type ("custom-input-port", custom_port_read, NULL);
  port = make_port (port_type);

  read = 0;
  do
    {
      last_read = scm_c_read (port, &buffer[read], 123);
      assert (last_read <= 123);
      assert (zeroed_buffer_p (&buffer[read], last_read));

      read += last_read;
    }
  while (last_read > 0 && read < sizeof (buffer));

  /* We shouldn't be able to read more than what's in PORT's buffer.  */
  assert (read == PORT_BUFFER_SIZE);

  return NULL;
}


int
main (int argc, char *argv[])
{
  scm_with_guile (do_start, NULL);

  return 0;
}
