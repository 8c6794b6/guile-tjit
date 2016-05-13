/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
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



#define _LARGEFILE64_SOURCE      /* ask for stat64 etc */
#define _GNU_SOURCE              /* ask for LONG_LONG_MAX/LONG_LONG_MIN */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <fcntl.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <unistd.h>
#ifdef HAVE_IO_H
#include <io.h>
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
#include <sys/stat.h>
#endif
#include <poll.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>

#include <full-write.h>

#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/gc.h"
#include "libguile/posix.h"
#include "libguile/dynwind.h"
#include "libguile/hashtab.h"

#include "libguile/fports.h"
#include "libguile/ports-internal.h"

#if SIZEOF_OFF_T == SIZEOF_INT
#define OFF_T_MAX  INT_MAX
#define OFF_T_MIN  INT_MIN
#elif SIZEOF_OFF_T == SIZEOF_LONG
#define OFF_T_MAX  LONG_MAX
#define OFF_T_MIN  LONG_MIN
#elif SIZEOF_OFF_T == SIZEOF_LONG_LONG
#define OFF_T_MAX  LONG_LONG_MAX
#define OFF_T_MIN  LONG_LONG_MIN
#else
#error Oops, unknown OFF_T size
#endif

scm_t_port_type *scm_file_port_type;


/* Move ports with the specified file descriptor to new descriptors,
 * resetting the revealed count to 0.
 */
static void
scm_i_evict_port (void *closure, SCM port)
{
  int fd = * (int*) closure;

  if (SCM_OPFPORTP (port))
    {
      scm_t_fport *fp = SCM_FSTREAM (port);
      if ((fp != NULL) && (fp->fdes == fd))
	{
	  fp->fdes = dup (fd);
	  if (fp->fdes == -1)
	    scm_syserror ("scm_evict_ports");
	  scm_set_port_revealed_x (port, scm_from_int (0));
	}
    }
}

void
scm_evict_ports (int fd)
{
  scm_c_port_for_each (scm_i_evict_port, (void *) &fd);
}


SCM_DEFINE (scm_file_port_p, "file-port?", 1, 0, 0,
	    (SCM obj),
	    "Determine whether @var{obj} is a port that is related to a file.")
#define FUNC_NAME s_scm_file_port_p
{
  return scm_from_bool (SCM_FPORTP (obj));
}
#undef FUNC_NAME


static SCM sys_file_port_name_canonicalization;
static SCM sym_relative;
static SCM sym_absolute;

static SCM
fport_canonicalize_filename (SCM filename)
{
  SCM mode = scm_fluid_ref (sys_file_port_name_canonicalization);

  if (!scm_is_string (filename))
    {
      return filename;
    }
  else if (scm_is_eq (mode, sym_relative))
    {
      SCM path, rel;

      path = scm_variable_ref (scm_c_module_lookup (scm_the_root_module (),
                                                    "%load-path"));
      rel = scm_i_relativize_path (filename, path);

      return scm_is_true (rel) ? rel : filename;
    }
  else if (scm_is_eq (mode, sym_absolute))
    {
      char *str, *canon;
  
      str = scm_to_locale_string (filename);
      canon = canonicalize_file_name (str);
      free (str);
  
      return canon ? scm_take_locale_string (canon) : filename;
    }
  else
    {
      return filename;
    }
}

/* scm_open_file_with_encoding
   Return a new port open on a given file.

   The mode string must match the pattern: [rwa+]** which
   is interpreted in the usual unix way.

   Unless binary mode is requested, the character encoding of the new
   port is determined as follows: First, if GUESS_ENCODING is true,
   'file-encoding' is used to guess the encoding of the file.  If
   GUESS_ENCODING is false or if 'file-encoding' fails, ENCODING is used
   unless it is also false.  As a last resort, the default port encoding
   is used.  It is an error to pass a non-false GUESS_ENCODING or
   ENCODING if binary mode is requested.

   Return the new port. */
SCM
scm_open_file_with_encoding (SCM filename, SCM mode,
                             SCM guess_encoding, SCM encoding)
#define FUNC_NAME "open-file"
{
  SCM port;
  int fdes, flags = 0, binary = 0;
  unsigned int retries;
  char *file;
  const char *md, *ptr;

  if (SCM_UNLIKELY (!(scm_is_false (encoding) || scm_is_string (encoding))))
    scm_wrong_type_arg_msg (FUNC_NAME, 0, encoding,
                            "encoding to be string or false");

  scm_dynwind_begin (0);

  file = scm_to_locale_string (filename);
  scm_dynwind_free (file);

  if (SCM_UNLIKELY (!scm_i_try_narrow_string (mode)))
    scm_out_of_range (FUNC_NAME, mode);

  md = scm_i_string_chars (mode);

  switch (*md)
    {
    case 'r':
      flags |= O_RDONLY;
      break;
    case 'w':
      flags |= O_WRONLY | O_CREAT | O_TRUNC;
      break;
    case 'a':
      flags |= O_WRONLY | O_CREAT | O_APPEND;
      break;
    default:
      scm_out_of_range (FUNC_NAME, mode);
    }
  ptr = md + 1;
  while (*ptr != '\0')
    {
      switch (*ptr)
	{
	case '+':
	  flags = (flags & ~(O_RDONLY | O_WRONLY)) | O_RDWR;
	  break;
	case 'b':
	  binary = 1;
#if defined (O_BINARY)
	  flags |= O_BINARY;
#endif
	  break;
	case '0':  /* unbuffered: handled later.  */
	case 'l':  /* line buffered: handled during output.  */
	  break;
	default:
	  scm_out_of_range (FUNC_NAME, mode);
	}
      ptr++;
    }

  for (retries = 0, fdes = -1;
       fdes < 0 && retries < 2;
       retries++)
    {
      SCM_SYSCALL (fdes = open_or_open64 (file, flags, 0666));
      if (fdes == -1)
	{
	  int en = errno;

	  if (en == EMFILE && retries == 0)
	    /* Run the GC in case it collects open file ports that are no
	       longer referenced.  */
	    scm_i_gc (FUNC_NAME);
	  else
	    SCM_SYSERROR_MSG ("~A: ~S",
			      scm_cons (scm_strerror (scm_from_int (en)),
					scm_cons (filename, SCM_EOL)), en);
	}
    }

  /* Create a port from this file descriptor.  The port's encoding is initially
     %default-port-encoding.  */
  port = scm_i_fdes_to_port (fdes, scm_i_mode_bits (mode),
                             fport_canonicalize_filename (filename));

  if (binary)
    {
      if (scm_is_true (encoding))
        scm_misc_error (FUNC_NAME,
                        "Encoding specified on a binary port",
                        scm_list_1 (encoding));
      if (scm_is_true (guess_encoding))
        scm_misc_error (FUNC_NAME,
                        "Request to guess encoding on a binary port",
                        SCM_EOL);

      /* Use the binary-friendly ISO-8859-1 encoding. */
      scm_i_set_port_encoding_x (port, NULL);
    }
  else
    {
      char *enc = NULL;

      if (scm_is_true (guess_encoding))
        {
          if (SCM_INPUT_PORT_P (port))
            enc = scm_i_scan_for_encoding (port);
          else
            scm_misc_error (FUNC_NAME,
                            "Request to guess encoding on an output-only port",
                            SCM_EOL);
        }

      if (!enc && scm_is_true (encoding))
        {
          char *buf = scm_to_latin1_string (encoding);
          enc = scm_gc_strdup (buf, "encoding");
          free (buf);
        }

      if (enc)
        scm_i_set_port_encoding_x (port, enc);
    }

  scm_dynwind_end ();

  return port;
}
#undef FUNC_NAME

SCM
scm_open_file (SCM filename, SCM mode)
{
  return scm_open_file_with_encoding (filename, mode, SCM_BOOL_F, SCM_BOOL_F);
}

/* We can't define these using SCM_KEYWORD, because keywords have not
   yet been initialized when scm_init_fports is called.  */
static SCM k_guess_encoding = SCM_UNDEFINED;
static SCM k_encoding       = SCM_UNDEFINED;

SCM_INTERNAL SCM scm_i_open_file (SCM, SCM, SCM);

SCM_DEFINE (scm_i_open_file, "open-file", 2, 0, 1,
	    (SCM filename, SCM mode, SCM keyword_args),
	    "Open the file whose name is @var{filename}, and return a port\n"
	    "representing that file.  The attributes of the port are\n"
	    "determined by the @var{mode} string.  The way in which this is\n"
	    "interpreted is similar to C stdio.  The first character must be\n"
	    "one of the following:\n"
	    "@table @samp\n"
	    "@item r\n"
	    "Open an existing file for input.\n"
	    "@item w\n"
	    "Open a file for output, creating it if it doesn't already exist\n"
	    "or removing its contents if it does.\n"
	    "@item a\n"
	    "Open a file for output, creating it if it doesn't already\n"
	    "exist.  All writes to the port will go to the end of the file.\n"
	    "The \"append mode\" can be turned off while the port is in use\n"
	    "@pxref{Ports and File Descriptors, fcntl}\n"
	    "@end table\n"
	    "The following additional characters can be appended:\n"
	    "@table @samp\n"
	    "@item b\n"
	    "Open the underlying file in binary mode, if supported by the system.\n"
	    "Also, open the file using the binary-compatible character encoding\n"
	    "\"ISO-8859-1\", ignoring the default port encoding.\n"
	    "@item +\n"
	    "Open the port for both input and output.  E.g., @code{r+}: open\n"
	    "an existing file for both input and output.\n"
	    "@item 0\n"
	    "Create an \"unbuffered\" port.  In this case input and output\n"
	    "operations are passed directly to the underlying port\n"
	    "implementation without additional buffering.  This is likely to\n"
	    "slow down I/O operations.  The buffering mode can be changed\n"
	    "while a port is in use @pxref{Ports and File Descriptors,\n"
	    "setvbuf}\n"
	    "@item l\n"
	    "Add line-buffering to the port.  The port output buffer will be\n"
	    "automatically flushed whenever a newline character is written.\n"
	    "@end table\n"
	    "In theory we could create read/write ports which were buffered\n"
	    "in one direction only.  However this isn't included in the\n"
	    "current interfaces.  If a file cannot be opened with the access\n"
	    "requested, @code{open-file} throws an exception.")
#define FUNC_NAME s_scm_i_open_file
{
  SCM encoding = SCM_BOOL_F;
  SCM guess_encoding = SCM_BOOL_F;

  scm_c_bind_keyword_arguments (FUNC_NAME, keyword_args, 0,
                                k_guess_encoding, &guess_encoding,
                                k_encoding, &encoding,
                                SCM_UNDEFINED);

  return scm_open_file_with_encoding (filename, mode,
                                      guess_encoding, encoding);
}
#undef FUNC_NAME


/* Building Guile ports from a file descriptor.  */

/* Build a Scheme port from an open file descriptor `fdes'.
   MODE indicates whether FILE is open for reading or writing; it uses
      the same notation as open-file's second argument.
   NAME is a string to be used as the port's filename.
*/
SCM
scm_i_fdes_to_port (int fdes, long mode_bits, SCM name)
#define FUNC_NAME "scm_fdes_to_port"
{
  SCM port;
  scm_t_fport *fp;

  /* Test that fdes is valid.  */
#ifdef F_GETFL
  int flags = fcntl (fdes, F_GETFL, 0);
  if (flags == -1)
    SCM_SYSERROR;
  flags &= O_ACCMODE;
  if (flags != O_RDWR
      && ((flags != O_WRONLY && (mode_bits & SCM_WRTNG))
	  || (flags != O_RDONLY && (mode_bits & SCM_RDNG))))
    {
      SCM_MISC_ERROR ("requested file mode not available on fdes", SCM_EOL);
    }
#else
  /* If we don't have F_GETFL, as on mingw, at least we can test that
     it is a valid file descriptor.  */
  struct stat st;
  if (fstat (fdes, &st) != 0)
    SCM_SYSERROR;
#endif

  fp = (scm_t_fport *) scm_gc_malloc_pointerless (sizeof (scm_t_fport),
                                                  "file port");
  fp->fdes = fdes;

  port = scm_c_make_port (scm_file_port_type, mode_bits, (scm_t_bits)fp);
  
  SCM_SET_FILENAME (port, name);

  return port;
}
#undef FUNC_NAME

SCM
scm_fdes_to_port (int fdes, char *mode, SCM name)
{
  return scm_i_fdes_to_port (fdes, scm_mode_bits (mode), name);
}

/* Return a lower bound on the number of bytes available for input.  */
static int
fport_input_waiting (SCM port)
{
  int fdes = SCM_FSTREAM (port)->fdes;

  struct pollfd pollfd = { fdes, POLLIN, 0 };

  if (poll (&pollfd, 1, 0) < 0)
    scm_syserror ("fport_input_waiting");

  return pollfd.revents & POLLIN ? 1 : 0;
}




/* Revealed counts --- an oddity inherited from SCSH.  */

#define SCM_REVEALED(x) (SCM_FSTREAM(x)->revealed)

static SCM revealed_ports = SCM_EOL;
static scm_i_pthread_mutex_t revealed_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Find a port in the table and return its revealed count.
   Also used by the garbage collector.
 */
int
scm_revealed_count (SCM port)
{
  int ret;

  scm_i_pthread_mutex_lock (&revealed_lock);
  ret = SCM_REVEALED (port);
  scm_i_pthread_mutex_unlock (&revealed_lock);

  return ret;
}

SCM_DEFINE (scm_port_revealed, "port-revealed", 1, 0, 0,
           (SCM port),
	    "Return the revealed count for @var{port}.")
#define FUNC_NAME s_scm_port_revealed
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPFPORT (1, port);
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
  int r, prev;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPFPORT (1, port);

  r = scm_to_int (rcount);

  scm_i_pthread_mutex_lock (&revealed_lock);

  prev = SCM_REVEALED (port);
  SCM_REVEALED (port) = r;

  if (r && !prev)
    revealed_ports = scm_cons (port, revealed_ports);
  else if (prev && !r)
    revealed_ports = scm_delq_x (port, revealed_ports);

  scm_i_pthread_mutex_unlock (&revealed_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Set the revealed count for a port.  */
SCM_DEFINE (scm_adjust_port_revealed_x, "adjust-port-revealed!", 2, 0, 0,
           (SCM port, SCM addend),
	    "Add @var{addend} to the revealed count of @var{port}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_adjust_port_revealed_x
{
  int a;

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPFPORT (1, port);

  a = scm_to_int (addend);
  if (!a)
    return SCM_UNSPECIFIED;

  scm_i_pthread_mutex_lock (&revealed_lock);

  SCM_REVEALED (port) += a;
  if (SCM_REVEALED (port) == a)
    revealed_ports = scm_cons (port, revealed_ports);
  else if (!SCM_REVEALED (port))
    revealed_ports = scm_delq_x (port, revealed_ports);

  scm_i_pthread_mutex_unlock (&revealed_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



static int 
fport_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<", port);
  scm_print_port_mode (exp, port);    
  if (SCM_OPFPORTP (exp))
    {
      int fdes;
      SCM name = SCM_FILENAME (exp);
      if (scm_is_string (name) || scm_is_symbol (name))
	scm_display (name, port);
      else
	scm_puts (SCM_PORT_TYPE (exp)->name, port);
      scm_putc (' ', port);
      fdes = (SCM_FSTREAM (exp))->fdes;

#if (defined HAVE_TTYNAME) && (defined HAVE_POSIX)
      if (isatty (fdes))
	scm_display (scm_ttyname (exp), port);
      else
#endif /* HAVE_TTYNAME */
	scm_intprint (fdes, 10, port);
    }
  else
    {
      scm_puts (SCM_PORT_TYPE (exp)->name, port);
      scm_putc (' ', port);
      scm_uintprint ((scm_t_bits) SCM_PORT (exp), 16, port);
    }
  scm_putc ('>', port);
  return 1;
}

/* fill a port's read-buffer with a single read.  returns the first
   char or EOF if end of file.  */
static size_t
fport_read (SCM port, SCM dst, size_t start, size_t count)
{
  long res;
  scm_t_fport *fp = SCM_FSTREAM (port);
  signed char *ptr = SCM_BYTEVECTOR_CONTENTS (dst) + start;

  SCM_SYSCALL (res = read (fp->fdes, ptr, count));
  if (res == -1)
    scm_syserror ("fport_read");
  return res;
}

static size_t
fport_write (SCM port, SCM src, size_t start, size_t count)
{
  int fd = SCM_FPORT_FDES (port);
  signed char *ptr = SCM_BYTEVECTOR_CONTENTS (src) + start;

  if (full_write (fd, ptr, count) < count)
    scm_syserror ("fport_write");

  return count;
}

static scm_t_off
fport_seek (SCM port, scm_t_off offset, int whence)
{
  scm_t_fport *fp = SCM_FSTREAM (port);
  off_t_or_off64_t result;

  result = lseek_or_lseek64 (fp->fdes, offset, whence);

  if (result == -1)
    scm_syserror ("fport_seek");

  return result;
}

static void
fport_truncate (SCM port, scm_t_off length)
{
  scm_t_fport *fp = SCM_FSTREAM (port);

  if (ftruncate (fp->fdes, length) == -1)
    scm_syserror ("ftruncate");
}

static void
fport_close (SCM port)
{
  scm_t_fport *fp = SCM_FSTREAM (port);

  if (close (fp->fdes) != 0)
    /* It's not useful to retry after EINTR, as the file descriptor is
       in an undefined state.  See http://lwn.net/Articles/365294/.
       Instead just throw an error if close fails, trusting that the fd
       was cleaned up.  */
    scm_syserror ("fport_close");
}

static int
fport_random_access_p (SCM port)
{
  return SCM_FDES_RANDOM_P (SCM_FSTREAM (port)->fdes);
}

/* Query the OS to get the natural buffering for FPORT, if available.  */
static void
fport_get_natural_buffer_sizes (SCM port, size_t *read_size, size_t *write_size)
{
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
  scm_t_fport *fp = SCM_FSTREAM (port);
  struct stat st;

  if (fstat (fp->fdes, &st) == 0)
    *read_size = *write_size = st.st_blksize;
#endif
}

static scm_t_port_type *
scm_make_fptob ()
{
  scm_t_port_type *ptob = scm_make_port_type ("file", fport_read, fport_write);

  scm_set_port_print                    (ptob, fport_print);
  scm_set_port_needs_close_on_gc        (ptob, 1);
  scm_set_port_close                    (ptob, fport_close);
  scm_set_port_seek                     (ptob, fport_seek);
  scm_set_port_truncate                 (ptob, fport_truncate);
  scm_set_port_input_waiting            (ptob, fport_input_waiting);
  scm_set_port_random_access_p          (ptob, fport_random_access_p);
  scm_set_port_get_natural_buffer_sizes (ptob, fport_get_natural_buffer_sizes);

  return ptob;
}

/* We can't initialize the keywords from 'scm_init_fports', because
   keywords haven't yet been initialized at that point.  */
void
scm_init_fports_keywords ()
{
  k_guess_encoding = scm_from_latin1_keyword ("guess-encoding");
  k_encoding       = scm_from_latin1_keyword ("encoding");
}

static void
scm_init_ice_9_fports (void)
{
#include "libguile/fports.x"
}

void
scm_init_fports ()
{
  scm_file_port_type = scm_make_fptob ();

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_ice_9_fports",
			    (scm_t_extension_init_func) scm_init_ice_9_fports,
			    NULL);

  /* The following bindings are used early in boot-9.scm.  */

  /* Used by `include' and also by `file-exists?' if `stat' is
     unavailable.  */
  scm_c_define_gsubr (s_scm_i_open_file, 2, 0, 1, (scm_t_subr) scm_i_open_file);

  /* Used by `open-file.', also via C.  */
  sym_relative = scm_from_latin1_symbol ("relative");
  sym_absolute = scm_from_latin1_symbol ("absolute");
  sys_file_port_name_canonicalization = scm_make_fluid ();
  scm_c_define ("%file-port-name-canonicalization",
                sys_file_port_name_canonicalization);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
