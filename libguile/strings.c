/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#include <string.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"



/* {Strings}
 */

SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a string, else @code{#f}.")
#define FUNC_NAME s_scm_string_p
{
  return scm_from_bool (SCM_STRINGP (obj));
}
#undef FUNC_NAME


SCM_REGISTER_PROC (s_scm_list_to_string, "list->string", 1, 0, 0, scm_string);

SCM_DEFINE (scm_string, "string", 0, 0, 1, 
            (SCM chrs),
	    "@deffnx {Scheme Procedure} list->string chrs\n"
	    "Return a newly allocated string composed of the arguments,\n"
	    "@var{chrs}.")
#define FUNC_NAME s_scm_string
{
  SCM result;

  {
    long i = scm_ilength (chrs);

    SCM_ASSERT (i >= 0, chrs, SCM_ARG1, FUNC_NAME);
    result = scm_allocate_string (i);
  }

  {
    unsigned char *data = SCM_STRING_UCHARS (result);

    while (!SCM_NULLP (chrs))
      {
	SCM elt = SCM_CAR (chrs);

	SCM_VALIDATE_CHAR (SCM_ARGn, elt);
	*data++ = SCM_CHAR (elt);
	chrs = SCM_CDR (chrs);
      }
  }
  return result;
}
#undef FUNC_NAME


/* converts C scm_array of strings to SCM scm_list of strings. */
/* If argc < 0, a null terminated scm_array is assumed. */
SCM 
scm_makfromstrs (int argc, char **argv)
{
  int i = argc;
  SCM lst = SCM_EOL;
  if (0 > i)
    for (i = 0; argv[i]; i++);
  while (i--)
    lst = scm_cons (scm_mem2string (argv[i], strlen (argv[i])), lst);
  return lst;
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, size_t len)
#define FUNC_NAME "scm_take_str"
{
  SCM answer;

  SCM_ASSERT_RANGE (2, scm_ulong2num (len), len <= SCM_STRING_MAX_LENGTH);

  answer = scm_cell (SCM_MAKE_STRING_TAG (len), (scm_t_bits) s);
  scm_gc_register_collectable_memory (s, len+1, "string");

  return answer;
}
#undef FUNC_NAME


/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_str (s, strlen (s));
}


SCM 
scm_mem2string (const char *src, size_t len)
{
  SCM s = scm_allocate_string (len);
  char *dst = SCM_STRING_CHARS (s);
  memcpy (dst, src, len);
  return s;
}


SCM
scm_str2string (const char *src)
{
  return scm_mem2string (src, strlen (src));
}


SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_mem2string (src, strlen (src));
}


SCM 
scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}


SCM
scm_allocate_string (size_t len)
#define FUNC_NAME "scm_allocate_string"
{
  char *mem;
  SCM s;

  SCM_ASSERT_RANGE (1, scm_long2num (len), len <= SCM_STRING_MAX_LENGTH);

  mem = (char *) scm_gc_malloc (len + 1, "string");
  mem[len] = 0;

  s = scm_cell (SCM_MAKE_STRING_TAG (len), (scm_t_bits) mem);

  return s;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_string, "make-string", 1, 1, 0,
            (SCM k, SCM chr),
	    "Return a newly allocated string of\n"
            "length @var{k}.  If @var{chr} is given, then all elements of\n"
	    "the string are initialized to @var{chr}, otherwise the contents\n"
	    "of the @var{string} are unspecified.")
#define FUNC_NAME s_scm_make_string
{
  if (SCM_INUMP (k))
    {
      long int i = SCM_INUM (k);
      SCM res;

      SCM_ASSERT_RANGE (1, k, i >= 0);

      res = scm_allocate_string (i);
      if (!SCM_UNBNDP (chr))
	{
	  unsigned char *dst;

	  SCM_VALIDATE_CHAR (2, chr);

	  dst = SCM_STRING_UCHARS (res);
	  memset (dst, SCM_CHAR (chr), i);
	}

      return res;
    }
  else if (SCM_BIGP (k))
    SCM_OUT_OF_RANGE (1, k);
  else
    SCM_WRONG_TYPE_ARG (1, k);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
	    (SCM string),
	    "Return the number of characters in @var{string}.")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_STRING (1, string);
  return SCM_MAKINUM (SCM_STRING_LENGTH (string));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ref, "string-ref", 2, 0, 0,
            (SCM str, SCM k),
	    "Return character @var{k} of @var{str} using zero-origin\n"
	    "indexing. @var{k} must be a valid index of @var{str}.")
#define FUNC_NAME s_scm_string_ref
{
  long idx;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM_COPY (2, k, idx);
  SCM_ASSERT_RANGE (2, k, idx >= 0 && idx < SCM_STRING_LENGTH (str));
  return SCM_MAKE_CHAR (SCM_STRING_UCHARS (str)[idx]);
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
            (SCM str, SCM k, SCM chr),
	    "Store @var{chr} in element @var{k} of @var{str} and return\n"
	    "an unspecified value. @var{k} must be a valid index of\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_set_x
{
  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM_RANGE (2, k,0, SCM_STRING_LENGTH(str));
  SCM_VALIDATE_CHAR (3, chr);
  SCM_STRING_UCHARS (str)[SCM_INUM (k)] = SCM_CHAR (chr);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_substring, "substring", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring
{
  long int from;
  long int to;
  SCM substr;

  SCM_VALIDATE_STRING (1, str);
  SCM_VALIDATE_INUM (2, start);
  SCM_VALIDATE_INUM_DEF (3, end, SCM_STRING_LENGTH (str));

  from = SCM_INUM (start);
  SCM_ASSERT_RANGE (2, start, 0 <= from && from <= SCM_STRING_LENGTH (str));
  to = SCM_INUM (end);
  SCM_ASSERT_RANGE (3, end, from <= to && to <= SCM_STRING_LENGTH (str));

  substr = scm_mem2string (&SCM_STRING_CHARS (str)[from], to - from);
  scm_remember_upto_here_1 (str);
  return substr;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_append, "string-append", 0, 0, 1, 
            (SCM args),
	    "Return a newly allocated string whose characters form the\n"
            "concatenation of the given strings, @var{args}.")
#define FUNC_NAME s_scm_string_append
{
  SCM res;
  size_t i = 0;
  register SCM l, s;
  register unsigned char *data;

  SCM_VALIDATE_REST_ARGUMENT (args);
  for (l = args; !SCM_NULLP (l); l = SCM_CDR (l)) {
    s = SCM_CAR (l);
    SCM_VALIDATE_STRING (SCM_ARGn, s);
    i += SCM_STRING_LENGTH (s);
  }
  res = scm_allocate_string (i);
  data = SCM_STRING_UCHARS (res);
  for (l = args; !SCM_NULLP (l);l = SCM_CDR (l)) {
    s = SCM_CAR (l);
    for (i = 0;i<SCM_STRING_LENGTH (s);i++) *data++ = SCM_STRING_UCHARS (s)[i];
  }
  return res;
}
#undef FUNC_NAME


/* Converts the given Scheme string OBJ into a C string, containing a copy
   of OBJ's content with a trailing null byte.  If LENP is non-NULL, set
   *LENP to the string's length.

   When STR is non-NULL it receives the copy and is returned by the function,
   otherwise new memory is allocated and the caller is responsible for 
   freeing it via free().  If out of memory, NULL is returned.

   Note that Scheme strings may contain arbitrary data, including null
   characters.  This means that null termination is not a reliable way to 
   determine the length of the returned value.  However, the function always 
   copies the complete contents of OBJ, and sets *LENP to the length of the
   scheme string (if LENP is non-null).  */
#define FUNC_NAME "scm_c_string2str"
char *
scm_c_string2str (SCM obj, char *str, size_t *lenp)
{
  size_t len;

  SCM_ASSERT (SCM_STRINGP (obj), obj, SCM_ARG1, FUNC_NAME);
  len = SCM_STRING_LENGTH (obj);

  if (str == NULL)
    {
      /* FIXME: Should we use exported wrappers for malloc (and free), which
       * allow windows DLLs to call the correct freeing function? */
      str = (char *) scm_malloc ((len + 1) * sizeof (char));
      if (str == NULL)
	return NULL;
    }

  memcpy (str, SCM_STRING_CHARS (obj), len);
  scm_remember_upto_here_1 (obj);
  str[len] = '\0';

  if (lenp != NULL)
    *lenp = len;

  return str;
}
#undef FUNC_NAME


/* Copy LEN characters at START from the Scheme string OBJ to memory
   at STR.  START is an index into OBJ; zero means the beginning of
   the string.  STR has already been allocated by the caller.

   If START + LEN is off the end of OBJ, silently truncate the source
   region to fit the string.  If truncation occurs, the corresponding
   area of STR is left unchanged.  */
#define FUNC_NAME "scm_c_substring2str"
char *
scm_c_substring2str (SCM obj, char *str, size_t start, size_t len)
{
  size_t src_length, effective_length;

  SCM_ASSERT (SCM_STRINGP (obj), obj, SCM_ARG2, FUNC_NAME);
  src_length = SCM_STRING_LENGTH (obj);
  effective_length = (len + start <= src_length) ? len : src_length - start;
  memcpy (str, SCM_STRING_CHARS (obj) + start, effective_length);
  scm_remember_upto_here_1 (obj);
  return str;
}
#undef FUNC_NAME


void
scm_init_strings ()
{
  scm_nullstr = scm_allocate_string (0);

#include "libguile/strings.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
