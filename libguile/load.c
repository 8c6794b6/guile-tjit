/* Copyright (C) 1995, 1996, 1998, 1999, 2000, 2001, 2004, 2006, 2008,
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

#include <string.h>
#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/private-gc.h" /* scm_getenv_int */
#include "libguile/libpath.h"
#include "libguile/fports.h"
#include "libguile/read.h"
#include "libguile/eval.h"
#include "libguile/throw.h"
#include "libguile/alist.h"
#include "libguile/dynwind.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/modules.h"
#include "libguile/chars.h"
#include "libguile/srfi-13.h"

#include "libguile/validate.h"
#include "libguile/load.h"
#include "libguile/fluids.h"

#include "libguile/vm.h" /* for load-compiled/vm */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif /* HAVE_PWD_H */

#ifndef R_OK
#define R_OK 4
#endif

#include <stat-time.h>


/* Loading a file, given an absolute filename.  */

/* Hook to run when we load a file, perhaps to announce the fact somewhere.
   Applied to the full name of the file.  */
static SCM *scm_loc_load_hook;

/* The current reader (a fluid).  */
static SCM the_reader = SCM_BOOL_F;


SCM_DEFINE (scm_primitive_load, "primitive-load", 1, 0, 0, 
           (SCM filename),
	    "Load the file named @var{filename} and evaluate its contents in\n"
	    "the top-level environment. The load paths are not searched;\n"
	    "@var{filename} must either be a full pathname or be a pathname\n"
	    "relative to the current directory.  If the  variable\n"
	    "@code{%load-hook} is defined, it should be bound to a procedure\n"
	    "that will be called before any code is loaded.  See the\n"
	    "documentation for @code{%load-hook} later in this section.")
#define FUNC_NAME s_scm_primitive_load
{
  SCM hook = *scm_loc_load_hook;
  SCM ret = SCM_UNSPECIFIED;

  SCM_VALIDATE_STRING (1, filename);
  if (scm_is_true (hook) && scm_is_false (scm_procedure_p (hook)))
    SCM_MISC_ERROR ("value of %load-hook is neither a procedure nor #f",
		    SCM_EOL);

  if (!scm_is_false (hook))
    scm_call_1 (hook, filename);

  {
    SCM port;

    port = scm_open_file_with_encoding (filename,
                                        scm_from_latin1_string ("r"),
                                        SCM_BOOL_T, /* guess_encoding */
                                        scm_from_latin1_string ("UTF-8"));

    scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
    scm_i_dynwind_current_load_port (port);

    while (1)
      {
	SCM reader, form;

	/* Lookup and use the current reader to read the next
	   expression. */
	reader = scm_fluid_ref (the_reader);
	if (scm_is_false (reader))
	  form = scm_read (port);
	else
	  form = scm_call_1 (reader, port);

	if (SCM_EOF_OBJECT_P (form))
	  break;

	ret = scm_primitive_eval_x (form);
      }

    scm_dynwind_end ();
    scm_close_port (port);
  }
  return ret;
}
#undef FUNC_NAME

SCM
scm_c_primitive_load (const char *filename)
{
  return scm_primitive_load (scm_from_locale_string (filename));
}


/* Builtin path to scheme library files. */
#ifdef SCM_PKGDATA_DIR
SCM_DEFINE (scm_sys_package_data_dir, "%package-data-dir", 0, 0, 0, 
            (),
	    "Return the name of the directory where Scheme packages, modules and\n"
	    "libraries are kept.  On most Unix systems, this will be\n"
	    "@samp{/usr/local/share/guile}.")
#define FUNC_NAME s_scm_sys_package_data_dir
{
  return scm_from_locale_string (SCM_PKGDATA_DIR);
}
#undef FUNC_NAME
#endif /* SCM_PKGDATA_DIR */

#ifdef SCM_LIBRARY_DIR
SCM_DEFINE (scm_sys_library_dir, "%library-dir", 0,0,0,
            (),
	    "Return the directory where the Guile Scheme library files are installed.\n"
	    "E.g., may return \"/usr/share/guile/1.3.5\".")
#define FUNC_NAME s_scm_sys_library_dir
{
  return scm_from_locale_string (SCM_LIBRARY_DIR);
}
#undef FUNC_NAME
#endif /* SCM_LIBRARY_DIR */

#ifdef SCM_SITE_DIR
SCM_DEFINE (scm_sys_site_dir, "%site-dir", 0,0,0,
            (),
	    "Return the directory where users should install Scheme code for use\n"
            "with this version of Guile.\n\n"
	    "E.g., may return \"/usr/share/guile/site/" SCM_EFFECTIVE_VERSION "\".")
#define FUNC_NAME s_scm_sys_site_dir
{
  return scm_from_locale_string (SCM_SITE_DIR);
}
#undef FUNC_NAME
#endif /* SCM_SITE_DIR */

#ifdef SCM_GLOBAL_SITE_DIR
SCM_DEFINE (scm_sys_global_site_dir, "%global-site-dir", 0,0,0,
            (),
	    "Return the directory where users should install Scheme code for use\n"
            "with all versions of Guile.\n\n"
	    "E.g., may return \"/usr/share/guile/site\".")
#define FUNC_NAME s_scm_sys_global_site_dir
{
  return scm_from_locale_string (SCM_GLOBAL_SITE_DIR);
}
#undef FUNC_NAME
#endif /* SCM_GLOBAL_SITE_DIR */

#ifdef SCM_SITE_CCACHE_DIR
SCM_DEFINE (scm_sys_site_ccache_dir, "%site-ccache-dir", 0,0,0,
            (),
	    "Return the directory where users should install compiled\n"
            "@code{.go} files for use with this version of Guile.\n\n"
	    "E.g., may return \"/usr/lib/guile/" SCM_EFFECTIVE_VERSION "/site-ccache\".")
#define FUNC_NAME s_scm_sys_site_ccache_dir
{
  return scm_from_locale_string (SCM_SITE_CCACHE_DIR);
}
#undef FUNC_NAME
#endif /* SCM_SITE_CCACHE_DIR */



/* Initializing the load path, and searching it.  */

/* List of names of directories we search for files to load.  */
static SCM *scm_loc_load_path;

/* List of extensions we try adding to the filenames.  */
static SCM *scm_loc_load_extensions;

/* Like %load-path and %load-extensions, but for compiled files. */
static SCM *scm_loc_load_compiled_path;
static SCM *scm_loc_load_compiled_extensions;

/* Whether we should try to auto-compile. */
static SCM *scm_loc_load_should_auto_compile;

/* Whether to treat all auto-compiled files as stale. */
static SCM *scm_loc_fresh_auto_compile;

/* The fallback path for auto-compilation */
static SCM *scm_loc_compile_fallback_path;

/* Ellipsis: "..." */
static SCM scm_ellipsis;

SCM_DEFINE (scm_parse_path, "parse-path", 1, 1, 0, 
            (SCM path, SCM tail),
	    "Parse @var{path}, which is expected to be a colon-separated\n"
	    "string, into a list and return the resulting list with\n"
	    "@var{tail} appended. If @var{path} is @code{#f}, @var{tail}\n"
	    "is returned.")
#define FUNC_NAME s_scm_parse_path
{
#ifdef __MINGW32__
  SCM sep = SCM_MAKE_CHAR (';');
#else
  SCM sep = SCM_MAKE_CHAR (':');
#endif
  
  if (SCM_UNBNDP (tail))
    tail = SCM_EOL;
  return (scm_is_false (path)
	  ? tail
	  : scm_append_x (scm_list_2 (scm_string_split (path, sep), tail)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_parse_path_with_ellipsis, "parse-path-with-ellipsis", 2, 0, 0,
            (SCM path, SCM base),
	    "Parse @var{path}, which is expected to be a colon-separated\n"
	    "string, into a list and return the resulting list with\n"
	    "@var{base} (a list) spliced in place of the @code{...} path\n"
            "component, if present, or else @var{base} is added to the end.\n"
            "If @var{path} is @code{#f}, @var{base} is returned.")
#define FUNC_NAME s_scm_parse_path_with_ellipsis
{
  SCM lst = scm_parse_path (path, SCM_EOL);
  SCM walk = lst;
  SCM *prev = &lst;

  while (!scm_is_null (walk) &&
         scm_is_false (scm_equal_p (scm_car (walk), scm_ellipsis)))
    {
      prev = SCM_CDRLOC (walk);
      walk = *prev;
    }
  *prev = scm_is_null (walk)
    ? base
    : scm_append (scm_list_2 (base, scm_cdr (walk)));
  return lst;
}
#undef FUNC_NAME


/* Initialize the global variable %load-path, given the value of the
   SCM_SITE_DIR and SCM_LIBRARY_DIR preprocessor symbols and the
   GUILE_LOAD_PATH environment variable.  */
void
scm_init_load_path ()
{
  char *env;
  SCM path = SCM_EOL;
  SCM cpath = SCM_EOL;

#ifdef SCM_LIBRARY_DIR
  env = getenv ("GUILE_SYSTEM_PATH");
  if (env && strcmp (env, "") == 0)
    /* special-case interpret system-path=="" as meaning no system path instead
       of '("") */
    ; 
  else if (env)
    path = scm_parse_path (scm_from_locale_string (env), path);
  else
    path = scm_list_4 (scm_from_locale_string (SCM_LIBRARY_DIR),
                       scm_from_locale_string (SCM_SITE_DIR),
                       scm_from_locale_string (SCM_GLOBAL_SITE_DIR),
                       scm_from_locale_string (SCM_PKGDATA_DIR));

  env = getenv ("GUILE_SYSTEM_COMPILED_PATH");
  if (env && strcmp (env, "") == 0)
    /* like above */
    ; 
  else if (env)
    cpath = scm_parse_path (scm_from_locale_string (env), cpath);
  else
    {
      cpath = scm_list_2 (scm_from_locale_string (SCM_CCACHE_DIR),
                          scm_from_locale_string (SCM_SITE_CCACHE_DIR));
    }

#endif /* SCM_LIBRARY_DIR */

  {
    char cachedir[1024];
    char *e;
#ifdef HAVE_GETPWENT
    struct passwd *pwd;
#endif

#define FALLBACK_DIR \
    "guile/ccache/" SCM_EFFECTIVE_VERSION "-" SCM_OBJCODE_MACHINE_VERSION_STRING

    if ((e = getenv ("XDG_CACHE_HOME")))
      snprintf (cachedir, sizeof(cachedir), "%s/" FALLBACK_DIR, e);
    else if ((e = getenv ("HOME")))
      snprintf (cachedir, sizeof(cachedir), "%s/.cache/" FALLBACK_DIR, e);
#ifdef HAVE_GETPWENT
    else if ((pwd = getpwuid (getuid ())) && pwd->pw_dir)
      snprintf (cachedir, sizeof(cachedir), "%s/.cache/" FALLBACK_DIR,
                pwd->pw_dir);
#endif /* HAVE_GETPWENT */
#ifdef __MINGW32__
    else if ((e = getenv ("LOCALAPPDATA")))
      snprintf (cachedir, sizeof (cachedir), "%s/.cache/" FALLBACK_DIR, e);
    else if ((e = getenv ("APPDATA")))
      snprintf (cachedir, sizeof (cachedir), "%s/.cache/" FALLBACK_DIR, e);
#endif /* __MINGW32__ */
    else
      cachedir[0] = 0;

    if (cachedir[0])
      *scm_loc_compile_fallback_path = scm_from_locale_string (cachedir);
  }

  env = getenv ("GUILE_LOAD_PATH");
  if (env)
    path = scm_parse_path_with_ellipsis (scm_from_locale_string (env), path);

  env = getenv ("GUILE_LOAD_COMPILED_PATH");
  if (env)
    cpath = scm_parse_path_with_ellipsis (scm_from_locale_string (env), cpath);

  *scm_loc_load_path = path;
  *scm_loc_load_compiled_path = cpath;
}

SCM scm_listofnullstr;

/* Utility functions for assembling C strings in a buffer.
 */

struct stringbuf {
  char *buf, *ptr;
  size_t buf_len;
};

static void
stringbuf_free (void *data)
{
  struct stringbuf *buf = (struct stringbuf *)data;
  free (buf->buf);
}

static void
stringbuf_grow (struct stringbuf *buf)
{
  size_t ptroff = buf->ptr - buf->buf;
  buf->buf_len *= 2; 
  buf->buf = scm_realloc (buf->buf, buf->buf_len);
  buf->ptr = buf->buf + ptroff;
}

static void
stringbuf_cat_locale_string (struct stringbuf *buf, SCM str)
{
  size_t max_len = buf->buf_len - (buf->ptr - buf->buf) - 1;
  size_t len = scm_to_locale_stringbuf (str, buf->ptr, max_len);
  if (len > max_len)
    {
      /* buffer is too small, double its size and try again. 
       */
      stringbuf_grow (buf);
      stringbuf_cat_locale_string (buf, str);
    }
  else
    {
      /* string fits, terminate it and check for embedded '\0'.
       */
      buf->ptr[len] = '\0';
      if (strlen (buf->ptr) != len)
	scm_misc_error (NULL,
			"string contains #\\nul character: ~S",
			scm_list_1 (str));
      buf->ptr += len;
    }
}

static void
stringbuf_cat (struct stringbuf *buf, char *str)
{
  size_t max_len = buf->buf_len - (buf->ptr - buf->buf) - 1;
  size_t len = strlen (str);
  if (len > max_len)
    {
      /* buffer is too small, double its size and try again. 
       */
      stringbuf_grow (buf);
      stringbuf_cat (buf, str);
    }
  else
    {
      /* string fits, copy it into buffer.
       */
      strcpy (buf->ptr, str);
      buf->ptr += len;
    }
}

  
static int
scm_c_string_has_an_ext (char *str, size_t len, SCM extensions)
{
  for (; !scm_is_null (extensions); extensions = SCM_CDR (extensions))
    {
      char *ext;
      size_t extlen;
      int match;
      ext = scm_to_locale_string (SCM_CAR (extensions));
      extlen = strlen (ext);
      match = (len > extlen && str[len - extlen - 1] == '.'
               && strncmp (str + (len - extlen), ext, extlen) == 0);
      free (ext);
      if (match)
        return 1;
    }
  return 0;
}

#ifdef __MINGW32__
#define FILE_NAME_SEPARATOR_STRING "\\"
#else
#define FILE_NAME_SEPARATOR_STRING "/"
#endif

static int
is_file_name_separator (SCM c)
{
  if (scm_is_eq (c, SCM_MAKE_CHAR ('/')))
    return 1;
#ifdef __MINGW32__
  if (scm_is_eq (c, SCM_MAKE_CHAR ('\\')))
    return 1;
#endif
  return 0;
}

static int
is_drive_letter (SCM c)
{
#ifdef __MINGW32__
  if (SCM_CHAR (c) >= 'a' && SCM_CHAR (c) <= 'z')
    return 1;
  else if (SCM_CHAR (c) >= 'A' && SCM_CHAR (c) <= 'Z')
    return 1;
#endif
  return 0;
}

static int
is_absolute_file_name (SCM filename)
{
  size_t filename_len = scm_c_string_length (filename);

  if (filename_len >= 1
      && is_file_name_separator (scm_c_string_ref (filename, 0))
#ifdef __MINGW32__
      /* On Windows, one initial separator indicates a drive-relative
         path.  Two separators indicate a Universal Naming Convention
         (UNC) path.  UNC paths are always absolute.  */
      && filename_len >= 2
      && is_file_name_separator (scm_c_string_ref (filename, 1))
#endif
      )
    return 1;
  if (filename_len >= 3
      && is_drive_letter (scm_c_string_ref (filename, 0))
      && scm_is_eq (scm_c_string_ref (filename, 1), SCM_MAKE_CHAR (':'))
      && is_file_name_separator (scm_c_string_ref (filename, 2)))
    return 1;
  return 0;
}

/* Search PATH for a directory containing a file named FILENAME.
   The file must be readable, and not a directory.
   If we find one, return its full pathname; otherwise, return #f.
   If FILENAME is absolute, return it unchanged.
   We also fill *stat_buf corresponding to the returned pathname.
   If given, EXTENSIONS is a list of strings; for each directory 
   in PATH, we search for FILENAME concatenated with each EXTENSION.  */
static SCM
search_path (SCM path, SCM filename, SCM extensions, SCM require_exts,
             struct stat *stat_buf)
{
  struct stringbuf buf;
  char *filename_chars;
  size_t filename_len;
  SCM result = SCM_BOOL_F;

  if (scm_ilength (path) < 0)
    scm_misc_error ("%search-path", "path is not a proper list: ~a",
                    scm_list_1 (path));
  if (scm_ilength (extensions) < 0)
    scm_misc_error ("%search-path", "bad extensions list: ~a",
                    scm_list_1 (extensions));

  scm_dynwind_begin (0);

  filename_chars = scm_to_locale_string (filename);
  filename_len = strlen (filename_chars);
  scm_dynwind_free (filename_chars);

  /* If FILENAME is absolute and is still valid, return it unchanged.  */
  if (is_absolute_file_name (filename))
    {
      if ((scm_is_false (require_exts) ||
           scm_c_string_has_an_ext (filename_chars, filename_len,
                                    extensions))
          && stat (filename_chars, stat_buf) == 0
          && !(stat_buf->st_mode & S_IFDIR))
        result = filename;
      goto end;
    }

  /* If FILENAME has an extension, don't try to add EXTENSIONS to it.  */
  {
    char *endp;

    for (endp = filename_chars + filename_len - 1;
	 endp >= filename_chars;
	 endp--)
      {
	if (*endp == '.')
	  {
            if (scm_is_true (require_exts) &&
                !scm_c_string_has_an_ext (filename_chars, filename_len,
                                          extensions))
              {
                /* This filename has an extension, but not one of the right
                   ones... */
                goto end;
              }
	    /* This filename already has an extension, so cancel the
               list of extensions.  */
	    extensions = SCM_EOL;
	    break;
	  }
	else if (is_file_name_separator (SCM_MAKE_CHAR (*endp)))
	  /* This filename has no extension, so keep the current list
             of extensions.  */
	  break;
      }
  }

  /* This simplifies the loop below a bit.
   */
  if (scm_is_null (extensions))
    extensions = scm_listofnullstr;

  buf.buf_len = 512;
  buf.buf = scm_malloc (buf.buf_len);
  scm_dynwind_unwind_handler (stringbuf_free, &buf, SCM_F_WIND_EXPLICITLY);

  /* Try every path element.
   */
  for (; scm_is_pair (path); path = SCM_CDR (path))
    {
      SCM dir = SCM_CAR (path);
      SCM exts;
      size_t sans_ext_len;

      buf.ptr = buf.buf;
      stringbuf_cat_locale_string (&buf, dir);
	
      /* Concatenate the path name and the filename. */
      
      if (buf.ptr > buf.buf
          && !is_file_name_separator (SCM_MAKE_CHAR (buf.ptr[-1])))
	stringbuf_cat (&buf, FILE_NAME_SEPARATOR_STRING);

      stringbuf_cat (&buf, filename_chars);
      sans_ext_len = buf.ptr - buf.buf;

      /* Try every extension. */
      for (exts = extensions; scm_is_pair (exts); exts = SCM_CDR (exts))
	{
	  SCM ext = SCM_CAR (exts);
	  
	  buf.ptr = buf.buf + sans_ext_len;
	  stringbuf_cat_locale_string (&buf, ext);
	  
	  /* If the file exists at all, we should return it.  If the
	     file is inaccessible, then that's an error.  */

	  if (stat (buf.buf, stat_buf) == 0
	      && ! (stat_buf->st_mode & S_IFDIR))
	    {
	      result = scm_from_locale_string (buf.buf);
	      goto end;
	    }
	}
      
      if (!SCM_NULL_OR_NIL_P (exts))
	scm_wrong_type_arg_msg (NULL, 0, extensions, "proper list");
    }

  if (!SCM_NULL_OR_NIL_P (path))
    scm_wrong_type_arg_msg (NULL, 0, path, "proper list");

 end:
  scm_dynwind_end ();
  return result;
}

SCM_DEFINE (scm_search_path, "search-path", 2, 0, 1,
            (SCM path, SCM filename, SCM rest),
	    "Search @var{path} for a directory containing a file named\n"
	    "@var{filename}. The file must be readable, and not a directory.\n"
	    "If we find one, return its full filename; otherwise, return\n"
	    "@code{#f}.  If @var{filename} is absolute, return it unchanged.\n"
	    "If given, @var{rest} is a list of extension strings; for each\n"
	    "directory in @var{path}, we search for @var{filename}\n"
	    "concatenated with each extension.")
#define FUNC_NAME s_scm_search_path
{
  SCM extensions, require_exts;
  struct stat stat_buf;

  if (SCM_UNBNDP (rest) || scm_is_null (rest))
    {
      /* Called either by Scheme code that didn't provide the optional
         arguments, or C code that used the Guile 1.8 signature (2 required,
         1 optional arg) and passed '() or nothing as the EXTENSIONS
	 argument.  */
      extensions = SCM_EOL;
      require_exts = SCM_UNDEFINED;
    }
  else
    {
      if (scm_is_null (SCM_CAR (rest)) || scm_is_pair (SCM_CAR (rest)))
	{
	  /* Called by Scheme code written for 1.9.  */
	  extensions = SCM_CAR (rest);
	  if (scm_is_null (SCM_CDR (rest)))
	    require_exts = SCM_UNDEFINED;
	  else
	    {
	      require_exts = SCM_CADR (rest);
	      if (SCM_UNLIKELY (!scm_is_null (SCM_CDDR (rest))))
		scm_wrong_num_args (scm_from_locale_string (FUNC_NAME));
	    }
	}
      else
	{
	  /* Called by C code that uses the 1.8 signature, i.e., which
	     expects the 3rd argument to be EXTENSIONS.  */
	  extensions = rest;
	  require_exts = SCM_UNDEFINED;
	}
    }

  if (SCM_UNBNDP (extensions))
    extensions = SCM_EOL;

  if (SCM_UNBNDP (require_exts))
    require_exts = SCM_BOOL_F;

  return search_path (path, filename, extensions, require_exts, &stat_buf);
}
#undef FUNC_NAME


/* Search %load-path for a directory containing a file named FILENAME.
   The file must be readable, and not a directory.
   If we find one, return its full filename; otherwise, return #f.
   If FILENAME is absolute, return it unchanged.  */
SCM_DEFINE (scm_sys_search_load_path, "%search-load-path", 1, 0, 0, 
	    (SCM filename),
	    "Search @var{%load-path} for the file named @var{filename},\n"
	    "which must be readable by the current user.  If @var{filename}\n"
	    "is found in the list of paths to search or is an absolute\n"
	    "pathname, return its full pathname.  Otherwise, return\n"
	    "@code{#f}.  Filenames may have any of the optional extensions\n"
	    "in the @code{%load-extensions} list; @code{%search-load-path}\n"
	    "will try each extension automatically.")
#define FUNC_NAME s_scm_sys_search_load_path
{
  struct stat stat_buf;
  
  SCM_VALIDATE_STRING (1, filename);

  return search_path (*scm_loc_load_path, filename, *scm_loc_load_extensions,
                      SCM_BOOL_F, &stat_buf);
}
#undef FUNC_NAME


/* Return true if COMPILED_FILENAME is newer than source file
   FULL_FILENAME, false otherwise.  */
static int
compiled_is_fresh (SCM full_filename, SCM compiled_filename,
                   struct stat *stat_source, struct stat *stat_compiled)
{
  int compiled_is_newer;
  struct timespec source_mtime, compiled_mtime;

  source_mtime = get_stat_mtime (stat_source);
  compiled_mtime = get_stat_mtime (stat_compiled);

  if (source_mtime.tv_sec < compiled_mtime.tv_sec
      || (source_mtime.tv_sec == compiled_mtime.tv_sec
          && source_mtime.tv_nsec <= compiled_mtime.tv_nsec))
    compiled_is_newer = 1;
  else
    {
      compiled_is_newer = 0;
      scm_puts (";;; note: source file ", scm_current_error_port ());
      scm_display (full_filename, scm_current_error_port ());
      scm_puts ("\n;;;       newer than compiled ", scm_current_error_port ());
      scm_display (compiled_filename, scm_current_error_port ());
      scm_puts ("\n", scm_current_error_port ());
    }

  return compiled_is_newer;
}

SCM_KEYWORD (kw_env, "env");
SCM_KEYWORD (kw_opts, "opts");

SCM_SYMBOL (sym_compile_file, "compile-file");
SCM_SYMBOL (sym_auto_compilation_options, "%auto-compilation-options");

static SCM
do_try_auto_compile (void *data)
{
  SCM source = PTR2SCM (data);
  SCM comp_mod, compile_file;

  scm_puts (";;; compiling ", scm_current_error_port ());
  scm_display (source, scm_current_error_port ());
  scm_newline (scm_current_error_port ());

  comp_mod = scm_c_resolve_module ("system base compile");
  compile_file = scm_module_variable (comp_mod, sym_compile_file);

  if (scm_is_true (compile_file))
    {
      /* Auto-compile in the context of the current module.  */
      SCM res, opts;
      SCM args[5];

      opts = scm_module_variable (scm_the_root_module (),
				  sym_auto_compilation_options);
      if (SCM_VARIABLEP (opts))
	opts = SCM_VARIABLE_REF (opts);
      else
	opts = SCM_EOL;

      args[0] = source;
      args[1] = kw_opts;
      args[2] = opts;
      args[3] = kw_env;
      args[4] = scm_current_module ();

      /* Assume `*current-warning-prefix*' has an appropriate value.  */
      res = scm_call_n (scm_variable_ref (compile_file), args, 5);

      scm_puts (";;; compiled ", scm_current_error_port ());
      scm_display (res, scm_current_error_port ());
      scm_newline (scm_current_error_port ());
      return res;
    }
  else
    {
      scm_puts (";;; it seems ", scm_current_error_port ());
      scm_display (source, scm_current_error_port ());
      scm_puts ("\n;;; is part of the compiler; skipping auto-compilation\n",
                scm_current_error_port ());
      return SCM_BOOL_F;
    }
}

static SCM
auto_compile_catch_handler (void *data, SCM tag, SCM throw_args)
{
  SCM source = PTR2SCM (data);
  SCM oport, lines;

  oport = scm_open_output_string ();
  scm_print_exception (oport, SCM_BOOL_F, tag, throw_args);

  scm_puts (";;; WARNING: compilation of ", scm_current_warning_port ());
  scm_display (source, scm_current_warning_port ());
  scm_puts (" failed:\n", scm_current_warning_port ());

  lines = scm_string_split (scm_get_output_string (oport),
                            SCM_MAKE_CHAR ('\n'));
  for (; scm_is_pair (lines); lines = scm_cdr (lines))
    if (scm_c_string_length (scm_car (lines)))
      {
        scm_puts (";;; ", scm_current_warning_port ());
        scm_display (scm_car (lines), scm_current_warning_port ());
        scm_newline (scm_current_warning_port ());
      }

  scm_close_port (oport);

  return SCM_BOOL_F;
}

SCM_DEFINE (scm_sys_warn_auto_compilation_enabled, "%warn-auto-compilation-enabled", 0, 0, 0,
	    (void), "")
#define FUNC_NAME s_scm_sys_warn_auto_compilation_enabled
{
  static int message_shown = 0;

  if (!message_shown)
    {
      scm_puts (";;; note: auto-compilation is enabled, set GUILE_AUTO_COMPILE=0\n"
                ";;;       or pass the --no-auto-compile argument to disable.\n",
                scm_current_warning_port ());
      message_shown = 1;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
scm_try_auto_compile (SCM source)
{
  if (scm_is_false (*scm_loc_load_should_auto_compile))
    return SCM_BOOL_F;

  scm_sys_warn_auto_compilation_enabled ();
  return scm_c_catch (SCM_BOOL_T,
                      do_try_auto_compile,
                      SCM2PTR (source),
                      auto_compile_catch_handler,
                      SCM2PTR (source),
                      NULL, NULL);
}

/* The auto-compilation code will residualize a .go file in the cache
   dir: by default, $HOME/.cache/guile/2.0/ccache/PATH.go.  This
   function determines the PATH to use as a key into the compilation
   cache.  See also (system base compile):compiled-file-name. */
static SCM
canonical_suffix (SCM fname)
{
  SCM canon;

  /* CANON should be absolute.  */
  canon = scm_canonicalize_path (fname);
  
#ifdef __MINGW32__
  {
    size_t len = scm_c_string_length (canon);

    /* On Windows, an absolute file name that doesn't start with a
       separator starts with a drive component.  Transform the drive
       component to a file name element: c:\foo -> \c\foo. */
    if (len >= 2
        && is_absolute_file_name (canon)
        && !is_file_name_separator (scm_c_string_ref (canon, 0)))
      return scm_string_append
        (scm_list_3 (scm_from_latin1_string (FILE_NAME_SEPARATOR_STRING),
                     scm_c_substring (canon, 0, 1),
                     scm_c_substring (canon, 2, len)));
  }
#endif

  return canon;
}

SCM_DEFINE (scm_primitive_load_path, "primitive-load-path", 0, 0, 1,
	    (SCM args),
	    "Search @var{%load-path} for the file named @var{filename} and\n"
	    "load it into the top-level environment.\n\n"
            "If @var{filename} is a relative pathname and is not found in\n"
            "the list of search paths, one of three things may happen,\n"
            "depending on the optional second argument,\n"
            "@var{exception_on_not_found}.  If it is @code{#f}, @code{#f}\n"
            "will be returned.  If it is a procedure, it will be called\n"
            "with no arguments.  Otherwise an error is signalled.")
#define FUNC_NAME s_scm_primitive_load_path
{
  SCM filename, exception_on_not_found;
  SCM full_filename, compiled_filename;
  int compiled_is_fallback = 0;
  SCM hook = *scm_loc_load_hook;
  struct stat stat_source, stat_compiled;

  if (scm_is_true (hook) && scm_is_false (scm_procedure_p (hook)))
    SCM_MISC_ERROR ("value of %load-hook is neither a procedure nor #f",
		    SCM_EOL);

  if (scm_is_string (args))
    {
      /* C code written for 1.8 and earlier expects this function to take a
	 single argument (the file name).  */
      filename = args;
      exception_on_not_found = SCM_UNDEFINED;
    }
  else
    {
      /* Starting from 1.9, this function takes 1 required and 1 optional
	 argument.  */
      long len;

      SCM_VALIDATE_LIST_COPYLEN (SCM_ARG1, args, len);
      if (len < 1 || len > 2)
	scm_error_num_args_subr (FUNC_NAME);

      filename = SCM_CAR (args);
      SCM_VALIDATE_STRING (SCM_ARG1, filename);

      exception_on_not_found = len > 1 ? SCM_CADR (args) : SCM_UNDEFINED;
    }

  if (SCM_UNBNDP (exception_on_not_found))
    exception_on_not_found = SCM_BOOL_T;

  full_filename = search_path (*scm_loc_load_path, filename,
                               *scm_loc_load_extensions, SCM_BOOL_F,
                               &stat_source);

  compiled_filename =
    search_path (*scm_loc_load_compiled_path, filename,
                 *scm_loc_load_compiled_extensions, SCM_BOOL_T,
                 &stat_compiled);

  if (scm_is_false (compiled_filename)
      && scm_is_true (full_filename)
      && scm_is_true (*scm_loc_compile_fallback_path)
      && scm_is_false (*scm_loc_fresh_auto_compile)
      && scm_is_pair (*scm_loc_load_compiled_extensions)
      && scm_is_string (scm_car (*scm_loc_load_compiled_extensions)))
    {
      SCM fallback;
      char *fallback_chars;

      fallback = scm_string_append
        (scm_list_3 (*scm_loc_compile_fallback_path,
                     canonical_suffix (full_filename),
                     scm_car (*scm_loc_load_compiled_extensions)));

      fallback_chars = scm_to_locale_string (fallback);
      if (stat (fallback_chars, &stat_compiled) == 0)
        {
          compiled_filename = fallback;
          compiled_is_fallback = 1;
        }
      free (fallback_chars);
    }
  
  if (scm_is_false (full_filename) && scm_is_false (compiled_filename))
    {
      if (scm_is_true (scm_procedure_p (exception_on_not_found)))
        return scm_call_0 (exception_on_not_found);
      else if (scm_is_false (exception_on_not_found))
        return SCM_BOOL_F;
      else
        SCM_MISC_ERROR ("Unable to find file ~S in load path",
                        scm_list_1 (filename));
    }

  if (!scm_is_false (hook))
    scm_call_1 (hook, (scm_is_true (full_filename)
                       ? full_filename : compiled_filename));

  if (scm_is_false (full_filename)
      || (scm_is_true (compiled_filename)
          && compiled_is_fresh (full_filename, compiled_filename,
                                &stat_source, &stat_compiled)))
    return scm_load_compiled_with_vm (compiled_filename);

  /* Perhaps there was the installed .go that was stale, but our fallback is
     fresh. Let's try that. Duplicating code, but perhaps that's OK. */

  if (!compiled_is_fallback
      && scm_is_true (*scm_loc_compile_fallback_path)
      && scm_is_false (*scm_loc_fresh_auto_compile)
      && scm_is_pair (*scm_loc_load_compiled_extensions)
      && scm_is_string (scm_car (*scm_loc_load_compiled_extensions)))
    {
      SCM fallback;
      char *fallback_chars;
      int stat_ret;
      
      fallback = scm_string_append
        (scm_list_3 (*scm_loc_compile_fallback_path,
                     canonical_suffix (full_filename),
                     scm_car (*scm_loc_load_compiled_extensions)));

      fallback_chars = scm_to_locale_string (fallback);
      stat_ret = stat (fallback_chars, &stat_compiled);
      free (fallback_chars);

      if (stat_ret == 0 && compiled_is_fresh (full_filename, fallback,
                                              &stat_source, &stat_compiled))
        {
          scm_puts (";;; found fresh local cache at ", scm_current_warning_port ());
          scm_display (fallback, scm_current_warning_port ());
          scm_newline (scm_current_warning_port ());
          return scm_load_compiled_with_vm (fallback);
        }
    }

  /* Otherwise, we bottom out here. */
  {
    SCM freshly_compiled = scm_try_auto_compile (full_filename);

    if (scm_is_true (freshly_compiled))
      return scm_load_compiled_with_vm (freshly_compiled);
    else
      return scm_primitive_load (full_filename);
  }
}
#undef FUNC_NAME

SCM
scm_c_primitive_load_path (const char *filename)
{
  return scm_primitive_load_path (scm_from_locale_string (filename));
}

void
scm_init_eval_in_scheme (void)
{
  SCM eval_scm, eval_go;
  struct stat stat_source, stat_compiled;

  eval_scm = search_path (*scm_loc_load_path,
                          scm_from_locale_string ("ice-9/eval.scm"),
                          SCM_EOL, SCM_BOOL_F, &stat_source);
  eval_go = search_path (*scm_loc_load_compiled_path,
                         scm_from_locale_string ("ice-9/eval.go"),
                         SCM_EOL, SCM_BOOL_F, &stat_compiled);
  
  if (scm_is_true (eval_scm) && scm_is_true (eval_go)
      && compiled_is_fresh (eval_scm, eval_go,
                            &stat_source, &stat_compiled))
    scm_load_compiled_with_vm (eval_go);
  else
    /* if we have no eval.go, we shouldn't load any compiled code at all */
    *scm_loc_load_compiled_path = SCM_EOL;
}


/* Information about the build environment.  */

SCM_VARIABLE_INIT (sys_host_type, "%host-type",
		   scm_from_locale_string (HOST_TYPE));


/* Initialize the scheme variable %guile-build-info, based on data
   provided by the Makefile, via libpath.h.  */
static void
init_build_info ()
{
  static struct { char *name; char *value; } info[] = SCM_BUILD_INFO;
  SCM *loc = SCM_VARIABLE_LOC (scm_c_define ("%guile-build-info", SCM_EOL));
  unsigned long i;

  for (i = 0; i < (sizeof (info) / sizeof (info[0])); i++)
    {
      SCM key = scm_from_locale_symbol (info[i].name);
      SCM val = scm_from_locale_string (info[i].value);
      *loc = scm_acons (key, val, *loc);
    }
#ifdef PACKAGE_PACKAGER
  *loc = scm_acons (scm_from_latin1_symbol ("packager"),
                    scm_from_latin1_string (PACKAGE_PACKAGER),
                    *loc);
#endif
#ifdef PACKAGE_PACKAGER_VERSION
  *loc = scm_acons (scm_from_latin1_symbol ("packager-version"),
                    scm_from_latin1_string (PACKAGE_PACKAGER_VERSION),
                    *loc);
#endif
#ifdef PACKAGE_PACKAGER_BUG_REPORTS
  *loc = scm_acons (scm_from_latin1_symbol ("packager-bug-reports"),
                    scm_from_latin1_string (PACKAGE_PACKAGER_BUG_REPORTS),
                    *loc);
#endif
}


void
scm_init_load ()
{
  scm_listofnullstr = scm_list_1 (scm_nullstr);
  scm_loc_load_path = SCM_VARIABLE_LOC (scm_c_define ("%load-path", SCM_EOL));
  scm_loc_load_extensions
    = SCM_VARIABLE_LOC (scm_c_define ("%load-extensions",
				      scm_list_2 (scm_from_locale_string (".scm"),
						  scm_nullstr)));
  scm_loc_load_compiled_path
    = SCM_VARIABLE_LOC (scm_c_define ("%load-compiled-path", SCM_EOL));
  scm_loc_load_compiled_extensions
    = SCM_VARIABLE_LOC (scm_c_define ("%load-compiled-extensions",
				      scm_list_1 (scm_from_locale_string (".go"))));
  scm_loc_load_hook = SCM_VARIABLE_LOC (scm_c_define ("%load-hook", SCM_BOOL_F));

  scm_loc_compile_fallback_path
    = SCM_VARIABLE_LOC (scm_c_define ("%compile-fallback-path", SCM_BOOL_F));
  scm_loc_load_should_auto_compile
    = SCM_VARIABLE_LOC (scm_c_define ("%load-should-auto-compile", SCM_BOOL_F));
  scm_loc_fresh_auto_compile
    = SCM_VARIABLE_LOC (scm_c_define ("%fresh-auto-compile", SCM_BOOL_F));

  scm_ellipsis = scm_from_latin1_string ("...");

  the_reader = scm_make_fluid_with_default (SCM_BOOL_F);
  scm_c_define("current-reader", the_reader);

  scm_c_define ("load-compiled",
                scm_c_make_gsubr ("load-compiled/vm", 1, 0, 0,
                                  scm_load_compiled_with_vm));

  init_build_info ();

#include "libguile/load.x"
}

void
scm_init_load_should_auto_compile ()
{
  char *auto_compile = getenv ("GUILE_AUTO_COMPILE");

  if (auto_compile && strcmp (auto_compile, "0") == 0)
    {
      *scm_loc_load_should_auto_compile = SCM_BOOL_F;
      *scm_loc_fresh_auto_compile = SCM_BOOL_F;
    }
  /* Allow "freshen" also.  */
  else if (auto_compile && strncmp (auto_compile, "fresh", 5) == 0)
    {
      *scm_loc_load_should_auto_compile = SCM_BOOL_T;
      *scm_loc_fresh_auto_compile = SCM_BOOL_T;
    }
  else
    {
      *scm_loc_load_should_auto_compile = SCM_BOOL_T;
      *scm_loc_fresh_auto_compile = SCM_BOOL_F;
    }
}
  
  

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
