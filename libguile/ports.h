/* classes: h_files */

#ifndef SCM_PORTS_H
#define SCM_PORTS_H

/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004,
 *   2006, 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "libguile/gc.h"
#include "libguile/tags.h"
#include "libguile/error.h"
#include "libguile/print.h"
#include "libguile/struct.h"
#include "libguile/threads.h"
#include "libguile/strings.h"



#define SCM_INITIAL_PUTBACK_BUF_SIZE 4

/* values for the rw_active flag.  */
typedef enum scm_t_port_rw_active {
  SCM_PORT_NEITHER = 0,
  SCM_PORT_READ = 1,
  SCM_PORT_WRITE = 2
} scm_t_port_rw_active;

typedef enum scm_t_port_encoding_mode {
  SCM_PORT_ENCODING_MODE_UTF8,
  SCM_PORT_ENCODING_MODE_ICONV
} scm_t_port_encoding_mode;

/* This is a separate object so that only those ports that use iconv
   cause finalizers to be registered.  */
typedef struct scm_t_iconv_descriptors
{
  /* input/output iconv conversion descriptors */
  void *input_cd;
  void *output_cd;
} scm_t_iconv_descriptors;

/* C representation of a Scheme port.  */

typedef struct 
{
  SCM port;			/* Link back to the port object.  */
  scm_i_pthread_mutex_t *lock;  /* A recursive lock for this port.  */

  /* data for the underlying port implementation as a raw C value. */
  scm_t_bits stream;

  SCM file_name;		/* debugging support.  */
  long line_number;		/* debugging support.  */
  int column_number;		/* debugging support.  */

  /* port buffers.  the buffer(s) are set up for all ports.  
     in the case of string ports, the buffer is the string itself.
     in the case of unbuffered file ports, the buffer is a
     single char: shortbuf.  */

  /* this buffer is filled from read_buf to read_end using the ptob 
     buffer_fill.  then input requests are taken from read_pos until
     it reaches read_end.  */

  unsigned char *read_buf;	/* buffer start.  */
  const unsigned char *read_pos;/* the next unread char.  */
  unsigned char *read_end;      /* pointer to last buffered char + 1.  */
  scm_t_off read_buf_size;		/* size of the buffer.  */

  /* when chars are put back into the buffer, e.g., using peek-char or
     unread-string, the read-buffer pointers are switched to cbuf.
     the original pointers are saved here and restored when the put-back
     chars have been consumed.  */
  unsigned char *saved_read_buf;
  const unsigned char *saved_read_pos;
  unsigned char *saved_read_end;
  scm_t_off saved_read_buf_size;

  /* write requests are saved into this buffer at write_pos until it
     reaches write_buf + write_buf_size, then the ptob flush is
     called.  */

  unsigned char *write_buf;     /* buffer start.  */
  unsigned char *write_pos;     /* pointer to last buffered char + 1.  */
  unsigned char *write_end;     /* pointer to end of buffer + 1.  */
  scm_t_off write_buf_size;		/* size of the buffer.  */

  unsigned char shortbuf;       /* buffer for "unbuffered" streams.  */

  int rw_random;                /* true if the port is random access.
				   implies that the buffers must be
				   flushed before switching between
				   reading and writing, seeking, etc.  */

  scm_t_port_rw_active rw_active; /* for random access ports,
                                     indicates which of the buffers
                                     is currently in use.  can be
                                     SCM_PORT_WRITE, SCM_PORT_READ,
                                     or SCM_PORT_NEITHER.  */


  /* a buffer for un-read chars and strings.  */
  unsigned char *putback_buf;
  size_t putback_buf_size;        /* allocated size of putback_buf.  */

  /* Character encoding support  */
  char *encoding;
  scm_t_port_encoding_mode encoding_mode;
  scm_t_string_failed_conversion_handler ilseq_handler;
  scm_t_iconv_descriptors *iconv_descriptors;
} scm_t_port;


SCM_INTERNAL SCM scm_i_port_weak_set;


#define SCM_READ_BUFFER_EMPTY_P(c_port) (c_port->read_pos >= c_port->read_end)



#define SCM_EOF_OBJECT_P(x) (scm_is_eq ((x), SCM_EOF_VAL))

/* PORT FLAGS
 * A set of flags characterizes a port.
 * Note that we reserve the bits 1 << 24 and above for use by the
 * routines in the port's scm_ptobfuns structure.
 */
#define SCM_OPN		(1L<<16) /* Is the port open? */
#define SCM_RDNG	(2L<<16) /* Is it a readable port? */
#define SCM_WRTNG	(4L<<16) /* Is it writable? */
#define SCM_BUF0	(8L<<16) /* Is it unbuffered? */
#define SCM_BUFLINE     (64L<<16) /* Is it line-buffered? */

#define SCM_PORTP(x) (SCM_HAS_TYP7 (x, scm_tc7_port))
#define SCM_OPPORTP(x) (SCM_PORTP (x) && (SCM_CELL_WORD_0 (x) & SCM_OPN))
#define SCM_INPUT_PORT_P(x) (SCM_PORTP (x) && (SCM_CELL_WORD_0 (x) & SCM_RDNG))
#define SCM_OUTPUT_PORT_P(x) (SCM_PORTP (x) && (SCM_CELL_WORD_0 (x) & SCM_WRTNG))
#define SCM_OPINPORTP(x) (SCM_OPPORTP (x) && SCM_INPUT_PORT_P (x))
#define SCM_OPOUTPORTP(x) (SCM_OPPORTP (x) && SCM_OUTPUT_PORT_P (x))
#define SCM_OPENP(x) (SCM_OPPORTP (x))
#define SCM_CLOSEDP(x) (!SCM_OPENP (x))
#define SCM_CLR_PORT_OPEN_FLAG(p) \
  SCM_SET_CELL_WORD_0 ((p), SCM_CELL_WORD_0 (p) & ~SCM_OPN)

#define SCM_PTAB_ENTRY(x)         ((scm_t_port *) SCM_CELL_WORD_1 (x))
#define SCM_PORT_DESCRIPTOR(port) ((scm_t_ptob_descriptor *) SCM_CELL_WORD_2 (port))
#define SCM_SETPTAB_ENTRY(x, ent)  (SCM_SET_CELL_WORD_1 ((x), (scm_t_bits) (ent)))
#define SCM_STREAM(x)             (SCM_PTAB_ENTRY(x)->stream)
#define SCM_SETSTREAM(x, s)        (SCM_PTAB_ENTRY(x)->stream = (scm_t_bits) (s))
#define SCM_FILENAME(x)           (SCM_PTAB_ENTRY(x)->file_name)
#define SCM_SET_FILENAME(x, n)    (SCM_PTAB_ENTRY(x)->file_name = (n))
#define SCM_LINUM(x)              (SCM_PTAB_ENTRY(x)->line_number)
#define SCM_COL(x)                (SCM_PTAB_ENTRY(x)->column_number)

#define SCM_INCLINE(port)  	do {SCM_LINUM (port) += 1; SCM_COL (port) = 0;} while (0)
#define SCM_ZEROCOL(port)  	do {SCM_COL (port) = 0;} while (0)
#define SCM_INCCOL(port)  	do {SCM_COL (port) += 1;} while (0)
#define SCM_DECCOL(port)  	do {if (SCM_COL (port) > 0) SCM_COL (port) -= 1;} while (0)
#define SCM_TABCOL(port)  	do {SCM_COL (port) += 8 - SCM_COL (port) % 8;} while (0)

/* Maximum number of port types.  */
#define SCM_I_MAX_PORT_TYPE_COUNT  256



typedef enum scm_t_port_type_flags {
  SCM_PORT_TYPE_HAS_FLUSH = 1 << 0
} scm_t_port_type_flags;

/* port-type description.  */
typedef struct scm_t_ptob_descriptor
{
  char *name;
  SCM (*mark) (SCM);
  size_t (*free) (SCM);
  int (*print) (SCM exp, SCM port, scm_print_state *pstate);
  SCM (*equalp) (SCM, SCM);
  int (*close) (SCM port);

  void (*write) (SCM port, const void *data, size_t size);
  void (*flush) (SCM port);

  void (*end_input) (SCM port, int offset);
  int (*fill_input) (SCM port);
  int (*input_waiting) (SCM port);

  scm_t_off (*seek) (SCM port, scm_t_off OFFSET, int WHENCE);
  void (*truncate) (SCM port, scm_t_off length);

  unsigned flags;
} scm_t_ptob_descriptor;

#define SCM_TC2PTOBNUM(x) (0x0ff & ((x) >> 8))
#define SCM_PTOBNUM(x) (SCM_TC2PTOBNUM (SCM_CELL_TYPE (x)))
/* SCM_PTOBNAME can be 0 if name is missing */
#define SCM_PTOBNAME(ptobnum) (scm_c_port_type_ref (ptobnum)->name)

/* Port types, and their vtables.  */
SCM_INTERNAL long scm_c_num_port_types (void);
SCM_API scm_t_ptob_descriptor* scm_c_port_type_ref (long ptobnum);
SCM_API long scm_c_port_type_add_x (scm_t_ptob_descriptor *desc);
SCM_API scm_t_bits scm_make_port_type (char *name,
				       int (*fill_input) (SCM port),
				       void (*write) (SCM port, 
						      const void *data,
						      size_t size));
SCM_API void scm_set_port_mark (scm_t_bits tc, SCM (*mark) (SCM));
SCM_API void scm_set_port_free (scm_t_bits tc, size_t (*free) (SCM));
SCM_API void scm_set_port_print (scm_t_bits tc,
				 int (*print) (SCM exp,
					       SCM port,
					       scm_print_state *pstate));
SCM_API void scm_set_port_equalp (scm_t_bits tc, SCM (*equalp) (SCM, SCM));
SCM_API void scm_set_port_close (scm_t_bits tc, int (*close) (SCM));

SCM_API void scm_set_port_flush (scm_t_bits tc, void (*flush) (SCM port));
SCM_API void scm_set_port_end_input (scm_t_bits tc,
                                     void (*end_input) (SCM port,
                                                        int offset));
SCM_API void scm_set_port_seek (scm_t_bits tc,
				scm_t_off (*seek) (SCM port,
						   scm_t_off OFFSET,
						   int WHENCE));
SCM_API void scm_set_port_truncate (scm_t_bits tc,
				    void (*truncate) (SCM port,
						      scm_t_off length));
SCM_API void scm_set_port_input_waiting (scm_t_bits tc, int (*input_waiting) (SCM));

/* The input, output, error, and load ports.  */
SCM_API SCM scm_current_input_port (void);
SCM_API SCM scm_current_output_port (void);
SCM_API SCM scm_current_error_port (void);
SCM_API SCM scm_current_warning_port (void);
SCM_API SCM scm_current_load_port (void);
SCM_API SCM scm_set_current_input_port (SCM port);
SCM_API SCM scm_set_current_output_port (SCM port);
SCM_API SCM scm_set_current_error_port (SCM port);
SCM_API SCM scm_set_current_warning_port (SCM port);
SCM_API void scm_dynwind_current_input_port (SCM port);
SCM_API void scm_dynwind_current_output_port (SCM port);
SCM_API void scm_dynwind_current_error_port (SCM port);
SCM_INTERNAL void scm_i_dynwind_current_load_port (SCM port);

/* Mode bits.  */
SCM_INTERNAL long scm_i_mode_bits (SCM modes);
SCM_API long scm_mode_bits (char *modes);
SCM_API SCM scm_port_mode (SCM port);

/* Low-level constructors.  */
SCM_API SCM
scm_c_make_port_with_encoding (scm_t_bits tag,
                               unsigned long mode_bits,
                               const char *encoding,
                               scm_t_string_failed_conversion_handler handler,
                               scm_t_bits stream);
SCM_API SCM scm_c_make_port (scm_t_bits tag, unsigned long mode_bits,
                             scm_t_bits stream);
SCM_API SCM scm_new_port_table_entry (scm_t_bits tag);

/* Predicates.  */
SCM_API SCM scm_port_p (SCM x);
SCM_API SCM scm_input_port_p (SCM x);
SCM_API SCM scm_output_port_p (SCM x);
SCM_API SCM scm_port_closed_p (SCM port);
SCM_API SCM scm_eof_object_p (SCM x);

/* Closing ports.  */
SCM_API SCM scm_close_port (SCM port);
SCM_API SCM scm_close_input_port (SCM port);
SCM_API SCM scm_close_output_port (SCM port);

/* Encoding characters to byte streams, and decoding byte streams to
   characters.  */
SCM_INTERNAL const char *scm_i_default_port_encoding (void);
SCM_INTERNAL void scm_i_set_default_port_encoding (const char *);
SCM_INTERNAL scm_t_iconv_descriptors *scm_i_port_iconv_descriptors (SCM port);
SCM_INTERNAL void scm_i_set_port_encoding_x (SCM port, const char *str);
SCM_API SCM scm_port_encoding (SCM port);
SCM_API SCM scm_set_port_encoding_x (SCM port, SCM encoding);
SCM_INTERNAL scm_t_string_failed_conversion_handler scm_i_get_conversion_strategy (SCM port);
SCM_INTERNAL void scm_i_set_conversion_strategy_x (SCM port, 
						   scm_t_string_failed_conversion_handler h);
SCM_API SCM scm_port_conversion_strategy (SCM port);
SCM_API SCM scm_set_port_conversion_strategy_x (SCM port, SCM behavior);

/* Acquiring and releasing the port lock.  */
SCM_API void scm_dynwind_lock_port (SCM port);
SCM_INLINE int scm_c_lock_port (SCM port, scm_i_pthread_mutex_t **lock);
SCM_INLINE int scm_c_try_lock_port (SCM port, scm_i_pthread_mutex_t **lock);

/* Input.  */
SCM_API int scm_get_byte_or_eof (SCM port);
SCM_INLINE int scm_get_byte_or_eof_unlocked (SCM port);
SCM_API int scm_peek_byte_or_eof (SCM port);
SCM_INLINE int scm_peek_byte_or_eof_unlocked (SCM port);
SCM_API size_t scm_c_read (SCM port, void *buffer, size_t size);
SCM_API size_t scm_c_read_unlocked (SCM port, void *buffer, size_t size);
SCM_API scm_t_wchar scm_getc (SCM port);
SCM_API scm_t_wchar scm_getc_unlocked (SCM port);
SCM_API SCM scm_read_char (SCM port);

/* Pushback.  */
SCM_INTERNAL void scm_unget_byte (int c, SCM port); 
SCM_INTERNAL void scm_unget_byte_unlocked (int c, SCM port); 
SCM_API void scm_ungetc (scm_t_wchar c, SCM port);
SCM_API void scm_ungetc_unlocked (scm_t_wchar c, SCM port);
SCM_API void scm_ungets (const char *s, int n, SCM port);
SCM_API void scm_ungets_unlocked (const char *s, int n, SCM port);
SCM_API SCM scm_peek_char (SCM port);
SCM_API SCM scm_unread_char (SCM cobj, SCM port);
SCM_API SCM scm_unread_string (SCM str, SCM port);

/* Manipulating the buffers.  */
SCM_API void scm_port_non_buffer (scm_t_port *pt);
SCM_API int scm_fill_input (SCM port);
SCM_API int scm_fill_input_unlocked (SCM port);
SCM_INTERNAL size_t scm_take_from_input_buffers (SCM port, char *dest, size_t read_len);
SCM_API SCM scm_drain_input (SCM port);
SCM_API void scm_end_input (SCM port);
SCM_API void scm_end_input_unlocked (SCM port);
SCM_API SCM scm_force_output (SCM port);
SCM_API void scm_flush (SCM port);
SCM_API void scm_flush_unlocked (SCM port);

/* Output.  */
SCM_API void scm_putc (char c, SCM port);
SCM_INLINE void scm_putc_unlocked (char c, SCM port);
SCM_API void scm_puts (const char *str_data, SCM port);
SCM_INLINE void scm_puts_unlocked (const char *str_data, SCM port);
SCM_API void scm_c_write (SCM port, const void *buffer, size_t size);
SCM_API void scm_c_write_unlocked (SCM port, const void *buffer, size_t size);
SCM_API void scm_lfwrite (const char *ptr, size_t size, SCM port);
SCM_API void scm_lfwrite_unlocked (const char *ptr, size_t size, SCM port);
SCM_INTERNAL void scm_lfwrite_substr (SCM str, size_t start, size_t end,
				      SCM port);

/* Querying and setting positions, and character availability.  */
SCM_API SCM scm_char_ready_p (SCM port);
SCM_API SCM scm_seek (SCM object, SCM offset, SCM whence);
SCM_API SCM scm_truncate_file (SCM object, SCM length);
SCM_API SCM scm_port_line (SCM port);
SCM_API SCM scm_set_port_line_x (SCM port, SCM line);
SCM_API SCM scm_port_column (SCM port);
SCM_API SCM scm_set_port_column_x (SCM port, SCM line);
SCM_API SCM scm_port_filename (SCM port);
SCM_API SCM scm_set_port_filename_x (SCM port, SCM filename);

/* Implementation helpers for port printing functions.  */
SCM_API int scm_port_print (SCM exp, SCM port, scm_print_state *);
SCM_API void scm_print_port_mode (SCM exp, SCM port);

/* Iterating over all ports.  */
SCM_API SCM scm_port_for_each (SCM proc);
SCM_API void scm_c_port_for_each (void (*proc)(void *data, SCM p), void *data);
SCM_API SCM scm_flush_all_ports (void);

/* Void ports.  */
SCM_API SCM scm_void_port (char * mode_str);
SCM_API SCM scm_sys_make_void_port (SCM mode);

/* Initialization.  */
SCM_INTERNAL void scm_init_ports (void);


/* Inline function implementations.  */

#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
SCM_INLINE_IMPLEMENTATION int
scm_c_lock_port (SCM port, scm_i_pthread_mutex_t **lock)
{
  *lock = SCM_PTAB_ENTRY (port)->lock;

  if (*lock)
    return scm_i_pthread_mutex_lock (*lock);
  else
    return 0;
}

SCM_INLINE_IMPLEMENTATION int
scm_c_try_lock_port (SCM port, scm_i_pthread_mutex_t **lock)
{
  *lock = SCM_PTAB_ENTRY (port)->lock;
  if (*lock)
    {
      int ret = scm_i_pthread_mutex_trylock (*lock);
      if (ret != 0)
        *lock = NULL;
      return ret;
    }
  else
    return 0;
}

SCM_INLINE_IMPLEMENTATION int
scm_get_byte_or_eof_unlocked (SCM port)
{
  int c;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    /* may be marginally faster than calling scm_flush.  */
    SCM_PORT_DESCRIPTOR (port)->flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (SCM_UNLIKELY (scm_fill_input_unlocked (port) == EOF))
	return EOF;
    }

  c = *(pt->read_pos++);

  return c;
}

/* Like `scm_get_byte_or_eof' but does not change PORT's `read_pos'.  */
SCM_INLINE_IMPLEMENTATION int
scm_peek_byte_or_eof_unlocked (SCM port)
{
  int c;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    /* may be marginally faster than calling scm_flush.  */
    SCM_PORT_DESCRIPTOR (port)->flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (SCM_UNLIKELY (scm_fill_input_unlocked (port) == EOF))
	return EOF;
    }

  c = *pt->read_pos;

  return c;
}

SCM_INLINE_IMPLEMENTATION void
scm_putc_unlocked (char c, SCM port)
{
  SCM_ASSERT_TYPE (SCM_OPOUTPORTP (port), port, 0, NULL, "output port");
  scm_lfwrite_unlocked (&c, 1, port);
}

SCM_INLINE_IMPLEMENTATION void
scm_puts_unlocked (const char *s, SCM port)
{
  SCM_ASSERT_TYPE (SCM_OPOUTPORTP (port), port, 0, NULL, "output port");
  scm_lfwrite_unlocked (s, strlen (s), port);
}
#endif  /* SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES */

#endif  /* SCM_PORTS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
