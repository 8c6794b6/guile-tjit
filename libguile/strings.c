/* Copyright (C) 1995,1996,1998,2000,2001, 2004, 2006, 2008, 2009 Free Software Foundation, Inc.
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

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <unistr.h>
#include <uniconv.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/generalized-vectors.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/dynwind.h"



/* {Strings}
 */


/* Stringbufs 
 *
 * XXX - keeping an accurate refcount during GC seems to be quite
 * tricky, so we just keep score of whether a stringbuf might be
 * shared, not whether it definitely is.  
 *
 * The scheme I (mvo) tried to keep an accurate reference count would
 * recount all strings that point to a stringbuf during the mark-phase
 * of the GC.  This was done since one cannot access the stringbuf of
 * a string when that string is freed (in order to decrease the
 * reference count).  The memory of the stringbuf might have been
 * reused already for something completely different.
 *
 * This recounted worked for a small number of threads beating on
 * cow-strings, but it failed randomly with more than 10 threads, say.
 * I couldn't figure out what went wrong, so I used the conservative
 * approach implemented below.
 * 
 * A stringbuf needs to know its length, but only so that it can be
 * reported when the stringbuf is freed.
 *
 * There are 3 storage strategies for stringbufs: inline, outline, and
 * wide.
 *
 * Inline strings are small 8-bit strings stored within the double
 * cell itself.  Outline strings are larger 8-bit strings with GC
 * allocated storage.  Wide strings are 32-bit strings with allocated
 * storage.
 *
 * There was little value in making wide string inlineable, since
 * there is only room for three inlined 32-bit characters.  Thus wide
 * stringbufs are never inlined.
 */

#define STRINGBUF_F_SHARED      0x100
#define STRINGBUF_F_INLINE      0x200
#define STRINGBUF_F_WIDE        0x400 /* If true, strings have UCS-4
                                         encoding.  Otherwise, strings
                                         are Latin-1.  */

#define STRINGBUF_TAG           scm_tc7_stringbuf
#define STRINGBUF_SHARED(buf)   (SCM_CELL_WORD_0(buf) & STRINGBUF_F_SHARED)
#define STRINGBUF_INLINE(buf)   (SCM_CELL_WORD_0(buf) & STRINGBUF_F_INLINE)
#define STRINGBUF_WIDE(buf)     (SCM_CELL_WORD_0(buf) & STRINGBUF_F_WIDE)

#define STRINGBUF_OUTLINE_CHARS(buf)   ((unsigned char *) SCM_CELL_WORD_1(buf))
#define STRINGBUF_OUTLINE_LENGTH(buf)  (SCM_CELL_WORD_2(buf))
#define STRINGBUF_INLINE_CHARS(buf)    ((unsigned char *) SCM_CELL_OBJECT_LOC(buf,1))
#define STRINGBUF_INLINE_LENGTH(buf)   (((size_t)SCM_CELL_WORD_0(buf))>>16)

#define STRINGBUF_CHARS(buf)  (STRINGBUF_INLINE (buf) \
                               ? STRINGBUF_INLINE_CHARS (buf) \
                               : STRINGBUF_OUTLINE_CHARS (buf))

#define STRINGBUF_WIDE_CHARS(buf) ((scm_t_wchar *) SCM_CELL_WORD_1(buf))
#define STRINGBUF_LENGTH(buf) (STRINGBUF_INLINE (buf) \
                               ? STRINGBUF_INLINE_LENGTH (buf) \
                               : STRINGBUF_OUTLINE_LENGTH (buf))

#define STRINGBUF_MAX_INLINE_LEN (3*sizeof(scm_t_bits))

#define SET_STRINGBUF_SHARED(buf) \
  (SCM_SET_CELL_WORD_0 ((buf), SCM_CELL_WORD_0 (buf) | STRINGBUF_F_SHARED))

#if SCM_STRING_LENGTH_HISTOGRAM
static size_t lenhist[1001];
#endif

/* Make a stringbuf with space for LEN 8-bit Latin-1-encoded
   characters. */
static SCM
make_stringbuf (size_t len)
{
  /* XXX - for the benefit of SCM_STRING_CHARS, SCM_SYMBOL_CHARS and
     scm_i_symbol_chars, all stringbufs are null-terminated.  Once
     SCM_STRING_CHARS and SCM_SYMBOL_CHARS are removed and the code
     has been changed for scm_i_symbol_chars, this null-termination
     can be dropped.
  */

#if SCM_STRING_LENGTH_HISTOGRAM
  if (len < 1000)
    lenhist[len]++;
  else
    lenhist[1000]++;
#endif

  if (len <= STRINGBUF_MAX_INLINE_LEN-1)
    {
      return scm_double_cell (STRINGBUF_TAG | STRINGBUF_F_INLINE | (len << 16),
			      0, 0, 0);
    }
  else
    {
      char *mem = scm_gc_malloc (len+1, "string");
      mem[len] = '\0';
      return scm_double_cell (STRINGBUF_TAG, (scm_t_bits) mem,
			      (scm_t_bits) len, (scm_t_bits) 0);
    }
}

/* Make a stringbuf with space for LEN 32-bit UCS-4-encoded
   characters.  */
static SCM
make_wide_stringbuf (size_t len)
{
  scm_t_wchar *mem;
#if SCM_STRING_LENGTH_HISTOGRAM
  if (len < 1000)
    lenhist[len]++;
  else
    lenhist[1000]++;
#endif

  mem = scm_gc_malloc (sizeof (scm_t_wchar) * (len + 1), "string");
  mem[len] = 0;
  return scm_double_cell (STRINGBUF_TAG | STRINGBUF_F_WIDE, (scm_t_bits) mem,
                          (scm_t_bits) len, (scm_t_bits) 0);
}

/* Return a new stringbuf whose underlying storage consists of the LEN+1
   octets pointed to by STR (the last octet is zero).  */
SCM
scm_i_take_stringbufn (char *str, size_t len)
{
  scm_gc_register_collectable_memory (str, len + 1, "stringbuf");

  return scm_double_cell (STRINGBUF_TAG, (scm_t_bits) str,
			  (scm_t_bits) len, (scm_t_bits) 0);
}

SCM
scm_i_stringbuf_mark (SCM buf)
{
  return SCM_BOOL_F;
}

void
scm_i_stringbuf_free (SCM buf)
{
  if (!STRINGBUF_INLINE (buf))
    {
      if (!STRINGBUF_WIDE (buf))
        scm_gc_free (STRINGBUF_OUTLINE_CHARS (buf),
                     STRINGBUF_OUTLINE_LENGTH (buf) + 1, "string");
      else
        scm_gc_free (STRINGBUF_OUTLINE_CHARS (buf),
                     sizeof (scm_t_wchar) * (STRINGBUF_OUTLINE_LENGTH (buf) 
                                             + 1), "string");
    }

}

/* Convert a stringbuf containing 8-bit Latin-1-encoded characters to
   one containing 32-bit UCS-4-encoded characters.  */
static void
widen_stringbuf (SCM buf)
{
  size_t i, len;
  scm_t_wchar *mem;

  if (STRINGBUF_WIDE (buf))
    return;

  if (STRINGBUF_INLINE (buf))
    {
      len = STRINGBUF_INLINE_LENGTH (buf);

      mem = scm_gc_malloc (sizeof (scm_t_wchar) * (len + 1), "string");
      for (i = 0; i < len; i++)
        mem[i] =
          (scm_t_wchar) STRINGBUF_INLINE_CHARS (buf)[i];
      mem[len] = 0;

      SCM_SET_CELL_WORD_0 (buf, SCM_CELL_WORD_0 (buf) ^ STRINGBUF_F_INLINE);
      SCM_SET_CELL_WORD_0 (buf, SCM_CELL_WORD_0 (buf) | STRINGBUF_F_WIDE);
      SCM_SET_CELL_WORD_1 (buf, mem);
      SCM_SET_CELL_WORD_2 (buf, len);
    }
  else
    {
      len = STRINGBUF_OUTLINE_LENGTH (buf);

      mem = scm_gc_malloc (sizeof (scm_t_wchar) * (len + 1), "string");
      for (i = 0; i < len; i++)
        mem[i] =
          (scm_t_wchar) STRINGBUF_OUTLINE_CHARS (buf)[i];
      mem[len] = 0;

      scm_gc_free (STRINGBUF_OUTLINE_CHARS (buf), len + 1, "string");

      SCM_SET_CELL_WORD_0 (buf, SCM_CELL_WORD_0 (buf) | STRINGBUF_F_WIDE);
      SCM_SET_CELL_WORD_1 (buf, mem);
      SCM_SET_CELL_WORD_2 (buf, len);
    }
}

scm_i_pthread_mutex_t stringbuf_write_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Copy-on-write strings.
 */

#define STRING_TAG            scm_tc7_string

#define STRING_STRINGBUF(str) (SCM_CELL_OBJECT_1(str))
#define STRING_START(str)     ((size_t)SCM_CELL_WORD_2(str))
#define STRING_LENGTH(str)    ((size_t)SCM_CELL_WORD_3(str))

#define SET_STRING_STRINGBUF(str,buf) (SCM_SET_CELL_OBJECT_1(str,buf))
#define SET_STRING_START(str,start) (SCM_SET_CELL_WORD_2(str,start))

#define IS_STRING(str)        (SCM_NIMP(str) && SCM_TYP7(str) == STRING_TAG)

/* Read-only strings.
 */

#define RO_STRING_TAG         (scm_tc7_string + 0x200)
#define IS_RO_STRING(str)     (SCM_CELL_TYPE(str)==RO_STRING_TAG)

/* Mutation-sharing substrings
 */

#define SH_STRING_TAG       (scm_tc7_string + 0x100)

#define SH_STRING_STRING(sh) (SCM_CELL_OBJECT_1(sh))
/* START and LENGTH as for STRINGs. */

#define IS_SH_STRING(str)   (SCM_CELL_TYPE(str)==SH_STRING_TAG)

/* Create a scheme string with space for LEN 8-bit Latin-1-encoded
   characters.  CHARSP, if not NULL, will be set to location of the
   char array.  */
SCM
scm_i_make_string (size_t len, char **charsp)
{
  SCM buf = make_stringbuf (len);
  SCM res;
  if (charsp)
    *charsp = (char *) STRINGBUF_CHARS (buf);
  res = scm_double_cell (STRING_TAG, SCM_UNPACK(buf),
			 (scm_t_bits)0, (scm_t_bits) len);
  return res;
}

/* Create a scheme string with space for LEN 32-bit UCS-4-encoded
   characters.  CHARSP, if not NULL, will be set to location of the
   character array.  */
SCM
scm_i_make_wide_string (size_t len, scm_t_wchar **charsp)
{
  SCM buf = make_wide_stringbuf (len);
  SCM res;
  if (charsp)
    *charsp = STRINGBUF_WIDE_CHARS (buf);
  res = scm_double_cell (STRING_TAG, SCM_UNPACK (buf),
                         (scm_t_bits) 0, (scm_t_bits) len);
  return res;
}

static void
validate_substring_args (SCM str, size_t start, size_t end)
{
  if (!IS_STRING (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  if (start > STRING_LENGTH (str))
    scm_out_of_range (NULL, scm_from_size_t (start));
  if (end > STRING_LENGTH (str) || end < start)
    scm_out_of_range (NULL, scm_from_size_t (end));
}

static inline void
get_str_buf_start (SCM *str, SCM *buf, size_t *start)
{
  *start = STRING_START (*str);
  if (IS_SH_STRING (*str))
    {
      *str = SH_STRING_STRING (*str);
      *start += STRING_START (*str);
    }
  *buf = STRING_STRINGBUF (*str);
}

SCM
scm_i_substring (SCM str, size_t start, size_t end)
{
  SCM buf;
  size_t str_start;
  get_str_buf_start (&str, &buf, &str_start);
  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  SET_STRINGBUF_SHARED (buf);
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
  return scm_double_cell (STRING_TAG, SCM_UNPACK(buf),
			  (scm_t_bits)str_start + start,
			  (scm_t_bits) end - start);
}

SCM
scm_i_substring_read_only (SCM str, size_t start, size_t end)
{
  SCM buf;
  size_t str_start;
  get_str_buf_start (&str, &buf, &str_start);
  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  SET_STRINGBUF_SHARED (buf);
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
  return scm_double_cell (RO_STRING_TAG, SCM_UNPACK(buf),
			  (scm_t_bits)str_start + start,
			  (scm_t_bits) end - start);
}

SCM
scm_i_substring_copy (SCM str, size_t start, size_t end)
{
  size_t len = end - start;
  SCM buf, my_buf;
  size_t str_start;
  get_str_buf_start (&str, &buf, &str_start);
  if (scm_i_is_narrow_string (str))
    {
      my_buf = make_stringbuf (len);
      memcpy (STRINGBUF_CHARS (my_buf),
              STRINGBUF_CHARS (buf) + str_start + start, len);
    }
  else
    {
      my_buf = make_wide_stringbuf (len);
      u32_cpy ((scm_t_uint32 *) STRINGBUF_WIDE_CHARS (my_buf),
               (scm_t_uint32 *) (STRINGBUF_WIDE_CHARS (buf) + str_start 
                                 + start), len);
      /* Even though this string is wide, the substring may be narrow.
         Consider adding code to narrow the string.  */
    }
  scm_remember_upto_here_1 (buf);
  return scm_double_cell (STRING_TAG, SCM_UNPACK (my_buf),
                          (scm_t_bits) 0, (scm_t_bits) len);
}

SCM
scm_i_substring_shared (SCM str, size_t start, size_t end)
{
  if (start == 0 && end == STRING_LENGTH (str))
    return str;
  else 
    {
      size_t len = end - start;
      if (IS_SH_STRING (str))
	{
	  start += STRING_START (str);
	  str = SH_STRING_STRING (str);
	}
      return scm_double_cell (SH_STRING_TAG, SCM_UNPACK(str),
			      (scm_t_bits)start, (scm_t_bits) len);
    }
}

SCM
scm_c_substring (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring (str, start, end);
}

SCM
scm_c_substring_read_only (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring_read_only (str, start, end);
}

SCM
scm_c_substring_copy (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring_copy (str, start, end);
}

SCM
scm_c_substring_shared (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring_shared (str, start, end);
}

SCM
scm_i_string_mark (SCM str)
{
  if (IS_SH_STRING (str))
    return SH_STRING_STRING (str);
  else
    return STRING_STRINGBUF (str);
}

void
scm_i_string_free (SCM str)
{
}

/* Internal accessors
 */

/* Returns the number of characters in STR.  This may be different
   than the memory size of the string storage.  */
size_t
scm_i_string_length (SCM str)
{
  return STRING_LENGTH (str);
}

/* True if the string is 'narrow', meaning it has a 8-bit Latin-1
   encoding.  False if it is 'wide', having a 32-bit UCS-4
   encoding.  */
int
scm_i_is_narrow_string (SCM str)
{
  return !STRINGBUF_WIDE (STRING_STRINGBUF (str));
}

/* Returns a pointer to the 8-bit Latin-1 encoded character array of
   STR.  */
const char *
scm_i_string_chars (SCM str)
{
  SCM buf;
  size_t start;
  get_str_buf_start (&str, &buf, &start);
  if (scm_i_is_narrow_string (str))
    return (const char *) STRINGBUF_CHARS (buf) + start;
  else
    scm_misc_error (NULL, "Invalid read access of chars of wide string: ~s",
                    scm_list_1 (str));
  return NULL;
}

/* Returns a pointer to the 32-bit UCS-4 encoded character array of
   STR.  */
const scm_t_wchar *
scm_i_string_wide_chars (SCM str)
{
  SCM buf;
  size_t start;

  get_str_buf_start (&str, &buf, &start);
  if (!scm_i_is_narrow_string (str))
    return (const scm_t_wchar *) STRINGBUF_WIDE_CHARS (buf) + start;
  else
    scm_misc_error (NULL, "Invalid read access of chars of narrow string: ~s",
                    scm_list_1 (str));
}

/* If the buffer in ORIG_STR is shared, copy ORIG_STR's characters to
   a new string buffer, so that it can be modified without modifying
   other strings.  Also, lock the string mutex.  Later, one must call
   scm_i_string_stop_writing to unlock the mutex.  */
SCM
scm_i_string_start_writing (SCM orig_str)
{
  SCM buf, str = orig_str;
  size_t start;

  get_str_buf_start (&str, &buf, &start);
  if (IS_RO_STRING (str))
    scm_misc_error (NULL, "string is read-only: ~s", scm_list_1 (orig_str));

  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  if (STRINGBUF_SHARED (buf))
    {
      /* Clone the stringbuf.  */
      size_t len = STRING_LENGTH (str);
      SCM new_buf;

      scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);

      if (scm_i_is_narrow_string (str))
        {
          new_buf = make_stringbuf (len);
          memcpy (STRINGBUF_CHARS (new_buf),
                  STRINGBUF_CHARS (buf) + STRING_START (str), len);

        }
      else
        {
          new_buf = make_wide_stringbuf (len);
          u32_cpy ((scm_t_uint32 *) STRINGBUF_WIDE_CHARS (new_buf),
                   (scm_t_uint32 *) (STRINGBUF_WIDE_CHARS (buf) 
                                     + STRING_START (str)), len);
        }
      scm_i_thread_put_to_sleep ();
      SET_STRING_STRINGBUF (str, new_buf);
      start -= STRING_START (str);
      SET_STRING_START (str, 0);
      scm_i_thread_wake_up ();

      buf = new_buf;

      scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
    }
  return orig_str;
}

/* Return a pointer to the 8-bit Latin-1 chars of a string.  */
char *
scm_i_string_writable_chars (SCM str)
{
  SCM buf;
  size_t start;

  get_str_buf_start (&str, &buf, &start);
  if (scm_i_is_narrow_string (str))
    return (char *) STRINGBUF_CHARS (buf) + start;
  else
    scm_misc_error (NULL, "Invalid write access of chars of wide string: ~s",
                    scm_list_1 (str));
  return NULL;
}

/* Return a pointer to the UCS-4 codepoints of a string.  */
static scm_t_wchar *
scm_i_string_writable_wide_chars (SCM str)
{
  SCM buf;
  size_t start;

  get_str_buf_start (&str, &buf, &start);
  if (!scm_i_is_narrow_string (str))
    return STRINGBUF_WIDE_CHARS (buf) + start;
  else
    scm_misc_error (NULL, "Invalid write access of chars of narrow string: ~s",
                    scm_list_1 (str));
}

/* Unlock the string mutex that was locked when
   scm_i_string_start_writing was called.  */
void
scm_i_string_stop_writing (void)
{
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
}

/* Return the Xth character of STR as a UCS-4 codepoint.  */
scm_t_wchar
scm_i_string_ref (SCM str, size_t x)
{
  if (scm_i_is_narrow_string (str))
    return (scm_t_wchar) (unsigned char) (scm_i_string_chars (str)[x]);
  else
    return scm_i_string_wide_chars (str)[x];
}

int 
scm_i_string_strcmp (SCM sstr, size_t start_x, const char *cstr)
{
  if (scm_i_is_narrow_string (sstr))
    {
      const char *a = scm_i_string_chars (sstr) + start_x;
      const char *b = cstr;
      return strncmp (a, b, strlen(b));
    }
  else
    {
      size_t i;
      const scm_t_wchar *a = scm_i_string_wide_chars (sstr) + start_x;
      const char *b = cstr;
      for (i = 0; i < strlen (b); i++)
        {
          if (a[i] != (unsigned char) b[i])
            return 1;
        }
    }
  return 0;
}

/* Set the Pth character of STR to UCS-4 codepoint CHR. */
void
scm_i_string_set_x (SCM str, size_t p, scm_t_wchar chr)
{
  if (chr > 0xFF && scm_i_is_narrow_string (str))
    widen_stringbuf (STRING_STRINGBUF (str));

  if (scm_i_is_narrow_string (str))
    {
      char *dst = scm_i_string_writable_chars (str);
      dst[p] = (char) (unsigned char) chr;
    }
  else
    {
      scm_t_wchar *dst = scm_i_string_writable_wide_chars (str);
      dst[p] = chr;
    }
}

/* Symbols.
 
   Basic symbol creation and accessing is done here, the rest is in
   symbols.[hc].  This has been done to keep stringbufs and the
   internals of strings and string-like objects confined to this file.
*/

#define SYMBOL_STRINGBUF SCM_CELL_OBJECT_1

SCM
scm_i_make_symbol (SCM name, scm_t_bits flags,
		   unsigned long hash, SCM props)
{
  SCM buf;
  size_t start = STRING_START (name);
  size_t length = STRING_LENGTH (name);

  if (IS_SH_STRING (name))
    {
      name = SH_STRING_STRING (name);
      start += STRING_START (name);
    }
  buf = SYMBOL_STRINGBUF (name);

  if (start == 0 && length == STRINGBUF_LENGTH (buf))
    {
      /* reuse buf. */
      scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
      SET_STRINGBUF_SHARED (buf);
      scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
    }
  else
    {
      /* make new buf. */
      if (scm_i_is_narrow_string (name))
        {
          SCM new_buf = make_stringbuf (length);
          memcpy (STRINGBUF_CHARS (new_buf),
                  STRINGBUF_CHARS (buf) + start, length);
          buf = new_buf;
        }
      else
        {
          SCM new_buf = make_wide_stringbuf (length);
          u32_cpy ((scm_t_uint32 *) STRINGBUF_WIDE_CHARS (new_buf),
                   (scm_t_uint32 *) STRINGBUF_WIDE_CHARS (buf) + start,
                   length);
          buf = new_buf;
        }
    }
  return scm_double_cell (scm_tc7_symbol | flags, SCM_UNPACK (buf),
			  (scm_t_bits) hash, SCM_UNPACK (props));
}

SCM
scm_i_c_make_symbol (const char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props)
{
  SCM buf = make_stringbuf (len);
  memcpy (STRINGBUF_CHARS (buf), name, len);

  return scm_double_cell (scm_tc7_symbol | flags, SCM_UNPACK (buf),
			  (scm_t_bits) hash, SCM_UNPACK (props));
}

/* Return a new symbol that uses the LEN bytes pointed to by NAME as its
   underlying storage.  */
SCM
scm_i_c_take_symbol (char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props)
{
  SCM buf = scm_i_take_stringbufn (name, len);

  return scm_double_cell (scm_tc7_symbol | flags, SCM_UNPACK (buf),
			  (scm_t_bits) hash, SCM_UNPACK (props));
}

/* Returns the number of characters in SYM.  This may be different
   from the memory size of SYM.  */
size_t
scm_i_symbol_length (SCM sym)
{
  return STRINGBUF_LENGTH (SYMBOL_STRINGBUF (sym));
}

size_t
scm_c_symbol_length (SCM sym)
#define FUNC_NAME "scm_c_symbol_length"
{
  SCM_VALIDATE_SYMBOL (1, sym);

  return STRINGBUF_LENGTH (SYMBOL_STRINGBUF (sym));
}
#undef FUNC_NAME

/* True if the name of SYM is stored as a Latin-1 encoded string.
   False if it is stored as a 32-bit UCS-4-encoded string.  */
int
scm_i_is_narrow_symbol (SCM sym)
{
  SCM buf;

  buf = SYMBOL_STRINGBUF (sym);
  return !STRINGBUF_WIDE (buf);
}

/* Returns a pointer to the 8-bit Latin-1 encoded character array that
   contains the name of SYM.  */
const char *
scm_i_symbol_chars (SCM sym)
{
  SCM buf;

  buf = SYMBOL_STRINGBUF (sym);
  if (!STRINGBUF_WIDE (buf))
    return (const char *) STRINGBUF_CHARS (buf);
  else
    scm_misc_error (NULL, "Invalid access of chars of a wide symbol ~S",
                    scm_list_1 (sym));
}

/* Return a pointer to the 32-bit UCS-4-encoded character array of a
   symbol's name.  */
const scm_t_wchar *
scm_i_symbol_wide_chars (SCM sym)
{
  SCM buf;

  buf = SYMBOL_STRINGBUF (sym);
  if (STRINGBUF_WIDE (buf))
    return (const scm_t_wchar *) STRINGBUF_WIDE_CHARS (buf);
  else
    scm_misc_error (NULL, "Invalid access of chars of a narrow symbol ~S",
                    scm_list_1 (sym));
}

SCM
scm_i_symbol_mark (SCM sym)
{
  scm_gc_mark (SYMBOL_STRINGBUF (sym));
  return SCM_CELL_OBJECT_3 (sym);
}

void
scm_i_symbol_free (SCM sym)
{
}

SCM
scm_i_symbol_substring (SCM sym, size_t start, size_t end)
{
  SCM buf = SYMBOL_STRINGBUF (sym);
  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  SET_STRINGBUF_SHARED (buf);
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
  return scm_double_cell (RO_STRING_TAG, SCM_UNPACK (buf),
			  (scm_t_bits)start, (scm_t_bits) end - start);
}

/* Returns the Xth character of symbol SYM as a UCS-4 codepoint.  */
scm_t_wchar
scm_i_symbol_ref (SCM sym, size_t x)
{
  if (scm_i_is_narrow_symbol (sym))
    return (scm_t_wchar) (unsigned char) (scm_i_symbol_chars (sym)[x]);
  else
    return scm_i_symbol_wide_chars (sym)[x];
}

/* Debugging
 */

SCM_DEFINE (scm_sys_string_dump, "%string-dump", 1, 0, 0, (SCM str), 
            "Returns an association list containing debugging information\n"
            "for @var{str}. The association list has the following entries."
            "@table @code\n"
            "@item string\n"
            "The string itself.\n"
            "@item start\n"
            "The start index of the string into its stringbuf\n"
            "@item length\n"
            "The length of the string\n"
            "@item shared\n"
            "If this string is a substring, it returns its parent string.\n"
            "Otherwise, it returns @code{#f}\n"
            "@item read-only\n"
            "@code{#t} if the string is read-only\n"
            "@item stringbuf-chars\n"
            "A new string containing this string's stringbuf's characters\n"
            "@item stringbuf-length\n"
            "The number of characters in this stringbuf\n"
            "@item stringbuf-shared\n"
            "@code{#t} if this stringbuf is shared\n"
            "@item stringbuf-inline\n"
            "@code{#t} if this stringbuf's characters are stored in the\n"
            "cell itself, or @code{#f} if they were allocated in memory\n"
            "@item stringbuf-wide\n"
            "@code{#t} if this stringbuf's characters are stored in a\n"
            "32-bit buffer, or @code{#f} if they are stored in an 8-bit\n"
            "buffer\n"
            "@end table")
#define FUNC_NAME s_scm_sys_string_dump
{
  SCM e1, e2, e3, e4, e5, e6, e7, e8, e9, e10;
  SCM buf;
  SCM_VALIDATE_STRING (1, str);

  /* String info */
  e1 = scm_cons (scm_from_locale_symbol ("string"),
                 str);
  e2 = scm_cons (scm_from_locale_symbol ("start"),
                 scm_from_size_t (STRING_START (str)));
  e3 = scm_cons (scm_from_locale_symbol ("length"),
                 scm_from_size_t (STRING_LENGTH (str)));

  if (IS_SH_STRING (str))
    {
      e4 = scm_cons (scm_from_locale_symbol ("shared"),
                     SH_STRING_STRING (str));
      buf = STRING_STRINGBUF (SH_STRING_STRING (str));
    }
  else
    {
      e4 = scm_cons (scm_from_locale_symbol ("shared"),
                     SCM_BOOL_F);
      buf = STRING_STRINGBUF (str);
    }

  if (IS_RO_STRING (str))
    e5 = scm_cons (scm_from_locale_symbol ("read-only"),
                   SCM_BOOL_T);
  else
    e5 = scm_cons (scm_from_locale_symbol ("read-only"),
                   SCM_BOOL_F);
      
  /* Stringbuf info */
  if (!STRINGBUF_WIDE (buf))
    {
      size_t len = STRINGBUF_LENGTH (buf);
      char *cbuf;
      SCM sbc = scm_i_make_string (len, &cbuf);
      memcpy (cbuf, STRINGBUF_CHARS (buf), len);
      e6 = scm_cons (scm_from_locale_symbol ("stringbuf-chars"),
                     sbc);
    }
  else
    {
      size_t len = STRINGBUF_LENGTH (buf);
      scm_t_wchar *cbuf;
      SCM sbc = scm_i_make_wide_string (len, &cbuf);
      u32_cpy ((scm_t_uint32 *) cbuf, 
               (scm_t_uint32 *) STRINGBUF_WIDE_CHARS (buf), len);
      e6 = scm_cons (scm_from_locale_symbol ("stringbuf-chars"),
                     sbc);
    }
  e7 = scm_cons (scm_from_locale_symbol ("stringbuf-length"), 
                 scm_from_size_t (STRINGBUF_LENGTH (buf)));
  if (STRINGBUF_SHARED (buf))
    e8 = scm_cons (scm_from_locale_symbol ("stringbuf-shared"), 
                   SCM_BOOL_T);
  else
    e8 = scm_cons (scm_from_locale_symbol ("stringbuf-shared"), 
                   SCM_BOOL_F);
  if (STRINGBUF_INLINE (buf))
    e9 = scm_cons (scm_from_locale_symbol ("stringbuf-inline"), 
                   SCM_BOOL_T);
  else
    e9 = scm_cons (scm_from_locale_symbol ("stringbuf-inline"), 
                   SCM_BOOL_F);
  if (STRINGBUF_WIDE (buf))
    e10 = scm_cons (scm_from_locale_symbol ("stringbuf-wide"),
                    SCM_BOOL_T);
  else
    e10 = scm_cons (scm_from_locale_symbol ("stringbuf-wide"),
                    SCM_BOOL_F);

  return scm_list_n (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_symbol_dump, "%symbol-dump", 1, 0, 0, (SCM sym),
            "Returns an association list containing debugging information\n"
            "for @var{sym}. The association list has the following entries."
            "@table @code\n"
            "@item symbol\n"
            "The symbol itself\n"
            "@item hash\n"
            "Its hash value\n"
            "@item interned\n"
            "@code{#t} if it is an interned symbol\n"
            "@item stringbuf-chars\n"
            "A new string containing this symbols's stringbuf's characters\n"
            "@item stringbuf-length\n"
            "The number of characters in this stringbuf\n"
            "@item stringbuf-shared\n"
            "@code{#t} if this stringbuf is shared\n"
            "@item stringbuf-inline\n"
            "@code{#t} if this stringbuf's characters are stored in the\n"
            "cell itself, or @code{#f} if they were allocated in memory\n"
            "@item stringbuf-wide\n"
            "@code{#t} if this stringbuf's characters are stored in a\n"
            "32-bit buffer, or @code{#f} if they are stored in an 8-bit\n"
            "buffer\n"
            "@end table")
#define FUNC_NAME s_scm_sys_symbol_dump
{
  SCM e1, e2, e3, e4, e5, e6, e7, e8;
  SCM buf;
  SCM_VALIDATE_SYMBOL (1, sym);
  e1 = scm_cons (scm_from_locale_symbol ("symbol"),
                 sym);
  e2 = scm_cons (scm_from_locale_symbol ("hash"),
                 scm_from_ulong (scm_i_symbol_hash (sym)));
  e3 = scm_cons (scm_from_locale_symbol ("interned"),
                 scm_symbol_interned_p (sym));
  buf = SYMBOL_STRINGBUF (sym);

  /* Stringbuf info */
  if (!STRINGBUF_WIDE (buf))
    {
      size_t len = STRINGBUF_LENGTH (buf);
      char *cbuf;
      SCM sbc = scm_i_make_string (len, &cbuf);
      memcpy (cbuf, STRINGBUF_CHARS (buf), len);
      e4 = scm_cons (scm_from_locale_symbol ("stringbuf-chars"),
                     sbc);
    }
  else
    {
      size_t len = STRINGBUF_LENGTH (buf);
      scm_t_wchar *cbuf;
      SCM sbc = scm_i_make_wide_string (len, &cbuf);
      u32_cpy ((scm_t_uint32 *) cbuf, 
               (scm_t_uint32 *) STRINGBUF_WIDE_CHARS (buf), len);
      e4 = scm_cons (scm_from_locale_symbol ("stringbuf-chars"),
                     sbc);
    }
  e5 = scm_cons (scm_from_locale_symbol ("stringbuf-length"), 
                 scm_from_size_t (STRINGBUF_LENGTH (buf)));
  if (STRINGBUF_SHARED (buf))
    e6 = scm_cons (scm_from_locale_symbol ("stringbuf-shared"), 
                   SCM_BOOL_T);
  else
    e6 = scm_cons (scm_from_locale_symbol ("stringbuf-shared"), 
                   SCM_BOOL_F);
  if (STRINGBUF_INLINE (buf))
    e7 = scm_cons (scm_from_locale_symbol ("stringbuf-inline"), 
                   SCM_BOOL_T);
  else
    e7 = scm_cons (scm_from_locale_symbol ("stringbuf-inline"), 
                   SCM_BOOL_F);
  if (STRINGBUF_WIDE (buf))
    e8 = scm_cons (scm_from_locale_symbol ("stringbuf-wide"),
                    SCM_BOOL_T);
  else
    e8 = scm_cons (scm_from_locale_symbol ("stringbuf-wide"),
                    SCM_BOOL_F);
  return scm_list_n (e1, e2, e3, e4, e5, e6, e7, e8, SCM_UNDEFINED);

}
#undef FUNC_NAME

#if SCM_STRING_LENGTH_HISTOGRAM

SCM_DEFINE (scm_sys_stringbuf_hist, "%stringbuf-hist", 0, 0, 0, (void), "")
#define FUNC_NAME s_scm_sys_stringbuf_hist
{
  int i;
  for (i = 0; i < 1000; i++)
    if (lenhist[i])
      fprintf (stderr, " %3d: %u\n", i, lenhist[i]);
  fprintf (stderr, ">999: %u\n", lenhist[1000]);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif



SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a string, else @code{#f}.")
#define FUNC_NAME s_scm_string_p
{
  return scm_from_bool (IS_STRING (obj));
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
  SCM result = SCM_BOOL_F;
  SCM rest;
  size_t len;
  size_t p = 0;
  long i;
  int wide = 0;

  /* Verify that this is a list of chars.  */
  i = scm_ilength (chrs);
  SCM_ASSERT (i >= 0, chrs, SCM_ARG1, FUNC_NAME);

  len = (size_t) i;
  rest = chrs;

  while (len > 0 && scm_is_pair (rest))
    {
      SCM elt = SCM_CAR (rest);
      SCM_VALIDATE_CHAR (SCM_ARGn, elt);
      if (SCM_CHAR (elt) > 0xFF)
        wide = 1;
      rest = SCM_CDR (rest);
      len--;
      scm_remember_upto_here_1 (elt);
    }

  /* Construct a string containing this list of chars.  */
  len = (size_t) i;
  rest = chrs;

  if (wide == 0)
    {
      result = scm_i_make_string (len, NULL);
      result = scm_i_string_start_writing (result);
      char *buf = scm_i_string_writable_chars (result);
      while (len > 0 && scm_is_pair (rest))
        {
          SCM elt = SCM_CAR (rest);
          buf[p] = (unsigned char) SCM_CHAR (elt);
          p++;
          rest = SCM_CDR (rest);
          len--;
          scm_remember_upto_here_1 (elt);
        }
    }
  else
    {
      result = scm_i_make_wide_string (len, NULL);
      result = scm_i_string_start_writing (result);
      scm_t_wchar *buf = scm_i_string_writable_wide_chars (result);
      while (len > 0 && scm_is_pair (rest))
        {
          SCM elt = SCM_CAR (rest);
          buf[p] = SCM_CHAR (elt);
          p++;
          rest = SCM_CDR (rest);
          len--;
          scm_remember_upto_here_1 (elt);
        }
    }
  scm_i_string_stop_writing ();

  if (len > 0)
    scm_misc_error (NULL, "list changed while constructing string", SCM_EOL);
  if (!scm_is_null (rest))
    scm_wrong_type_arg_msg (NULL, 0, chrs, "proper list");

  return result;
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
  return scm_c_make_string (scm_to_size_t (k), chr);
}
#undef FUNC_NAME

SCM
scm_c_make_string (size_t len, SCM chr)
#define FUNC_NAME NULL
{
  size_t p;
  SCM res = scm_i_make_string (len, NULL);

  if (!SCM_UNBNDP (chr))
    {
      SCM_VALIDATE_CHAR (0, chr);
      res = scm_i_string_start_writing (res);
      for (p = 0; p < len; p++)
        scm_i_string_set_x (res, p, SCM_CHAR (chr));
      scm_i_string_stop_writing ();
    }

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
	    (SCM string),
	    "Return the number of characters in @var{string}.")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_STRING (1, string);
  return scm_from_size_t (STRING_LENGTH (string));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_bytes_per_char, "string-bytes-per-char", 1, 0, 0,
            (SCM string),
            "Return the bytes used to represent a character in @var{string}."
            "This will return 1 or 4.")
#define FUNC_NAME s_scm_string_bytes_per_char
{
  SCM_VALIDATE_STRING (1, string);
  if (!scm_i_is_narrow_string (string))
    return scm_from_int (4);

  return scm_from_int (1);
}
#undef FUNC_NAME

size_t
scm_c_string_length (SCM string)
{
  if (!IS_STRING (string))
    scm_wrong_type_arg_msg (NULL, 0, string, "string");
  return STRING_LENGTH (string);
}

SCM_DEFINE (scm_string_ref, "string-ref", 2, 0, 0,
            (SCM str, SCM k),
            "Return character @var{k} of @var{str} using zero-origin\n"
            "indexing. @var{k} must be a valid index of @var{str}.")
#define FUNC_NAME s_scm_string_ref
{
  size_t len;
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);

  len = scm_i_string_length (str);
  if (SCM_LIKELY (len > 0))
    idx = scm_to_unsigned_integer (k, 0, len - 1);
  else
    scm_out_of_range (NULL, k);

  if (scm_i_is_narrow_string (str))
    return SCM_MAKE_CHAR (scm_i_string_chars (str)[idx]);
  else
    return SCM_MAKE_CHAR (scm_i_string_wide_chars (str)[idx]);
}
#undef FUNC_NAME

SCM
scm_c_string_ref (SCM str, size_t p)
{
  if (p >= scm_i_string_length (str))
    scm_out_of_range (NULL, scm_from_size_t (p));
  if (scm_i_is_narrow_string (str))
    return SCM_MAKE_CHAR (scm_i_string_chars (str)[p]);
  else
    return SCM_MAKE_CHAR (scm_i_string_wide_chars (str)[p]);

}

SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
            (SCM str, SCM k, SCM chr),
            "Store @var{chr} in element @var{k} of @var{str} and return\n"
            "an unspecified value. @var{k} must be a valid index of\n"
            "@var{str}.")
#define FUNC_NAME s_scm_string_set_x
{
  size_t len;
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);

  len = scm_i_string_length (str);
  if (SCM_LIKELY (len > 0))
    idx = scm_to_unsigned_integer (k, 0, len - 1);
  else
    scm_out_of_range (NULL, k);

  SCM_VALIDATE_CHAR (3, chr);
  str = scm_i_string_start_writing (str);
  scm_i_string_set_x (str, idx, SCM_CHAR (chr));
  scm_i_string_stop_writing ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_string_set_x (SCM str, size_t p, SCM chr)
{
  if (p >= scm_i_string_length (str))
    scm_out_of_range (NULL, scm_from_size_t (p));
  str = scm_i_string_start_writing (str);
  scm_i_string_set_x (str, p, SCM_CHAR (chr));
  scm_i_string_stop_writing ();
}

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
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_i_substring (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_read_only, "substring/read-only", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n"
	    "\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).\n"
	    "\n"
	    "The returned string is read-only.\n")
#define FUNC_NAME s_scm_substring_read_only
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_i_substring_read_only (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_copy, "substring/copy", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring_copy
{
  /* For the Scheme version, START is mandatory, but for the C
     version, it is optional.  See scm_string_copy in srfi-13.c for a
     rationale.
  */

  size_t from, to;

  SCM_VALIDATE_STRING (1, str);
  scm_i_get_substring_spec (scm_i_string_length (str),
			    start, &from, end, &to);
  return scm_i_substring_copy (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_shared, "substring/shared", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return string that indirectly refers to the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring_shared
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_i_substring_shared (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_append, "string-append", 0, 0, 1, 
            (SCM args),
            "Return a newly allocated string whose characters form the\n"
            "concatenation of the given strings, @var{args}.")
#define FUNC_NAME s_scm_string_append
{
  SCM res;
  size_t len = 0;
  int wide = 0;
  SCM l, s;
  size_t i;
  union
  {
    char *narrow;
    scm_t_wchar *wide;
  } data;

  SCM_VALIDATE_REST_ARGUMENT (args);
  for (l = args; !scm_is_null (l); l = SCM_CDR (l))
    {
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      len += scm_i_string_length (s);
      if (!scm_i_is_narrow_string (s))
        wide = 1;
    }
  data.narrow = NULL;
  if (!wide)
    res = scm_i_make_string (len, &data.narrow);
  else
    res = scm_i_make_wide_string (len, &data.wide);

  for (l = args; !scm_is_null (l); l = SCM_CDR (l))
    {
      size_t len;
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      len = scm_i_string_length (s);
      if (!wide)
        {
          memcpy (data.narrow, scm_i_string_chars (s), len);
          data.narrow += len;
        }
      else
        {
          if (scm_i_is_narrow_string (s))
            {
              for (i = 0; i < scm_i_string_length (s); i++)
                data.wide[i] = (unsigned char) scm_i_string_chars (s)[i];
            }
          else
            u32_cpy ((scm_t_uint32 *) data.wide,
                     (scm_t_uint32 *) scm_i_string_wide_chars (s), len);
          data.wide += len;
        }
      scm_remember_upto_here_1 (s);
    }
  return res;
}
#undef FUNC_NAME

int
scm_is_string (SCM obj)
{
  return IS_STRING (obj);
}

SCM
scm_from_locale_stringn (const char *str, size_t len)
{
  SCM res;
  char *dst;

  if (len == (size_t) -1)
    len = strlen (str);
  if (len == 0)
    return scm_nullstr;

  res = scm_i_make_string (len, &dst);
  memcpy (dst, str, len);
  return res;
}

SCM
scm_from_locale_string (const char *str)
{
  if (str == NULL)
    return scm_nullstr;

  return scm_from_locale_stringn (str, -1);
}

/* Create a new scheme string from the C string STR.  The memory of
   STR may be used directly as storage for the new string.  */
SCM
scm_take_locale_stringn (char *str, size_t len)
{
  SCM buf, res;

  if (len == (size_t) -1)
    len = strlen (str);
  else
    {
      /* Ensure STR is null terminated.  A realloc for 1 extra byte should
         often be satisfied from the alignment padding after the block, with
         no actual data movement.  */
      str = scm_realloc (str, len + 1);
      str[len] = '\0';
    }

  buf = scm_i_take_stringbufn (str, len);
  res = scm_double_cell (STRING_TAG,
                         SCM_UNPACK (buf), (scm_t_bits) 0, (scm_t_bits) len);
  return res;
}

SCM
scm_take_locale_string (char *str)
{
  return scm_take_locale_stringn (str, -1);
}

/* Change libunistring escapes (\uXXXX and \UXXXXXXXX) to \xXX \uXXXX
   and \UXXXXXX.  */
static void
unistring_escapes_to_guile_escapes (char **bufp, size_t *lenp)
{
  char *before, *after;
  size_t i, j;

  before = *bufp;
  after = *bufp;
  i = 0;
  j = 0;
  while (i < *lenp)
    {
      if ((i <= *lenp - 6)
          && before[i] == '\\'
          && before[i + 1] == 'u'
          && before[i + 2] == '0' && before[i + 3] == '0')
        {
          /* Convert \u00NN to \xNN */
          after[j] = '\\';
          after[j + 1] = 'x';
          after[j + 2] = tolower ((int) before[i + 4]);
          after[j + 3] = tolower ((int) before[i + 5]);
          i += 6;
          j += 4;
        }
      else if ((i <= *lenp - 10)
               && before[i] == '\\'
               && before[i + 1] == 'U'
               && before[i + 2] == '0' && before[i + 3] == '0')
        {
          /* Convert \U00NNNNNN to \UNNNNNN */
          after[j] = '\\';
          after[j + 1] = 'U';
          after[j + 2] = tolower ((int) before[i + 4]);
          after[j + 3] = tolower ((int) before[i + 5]);
          after[j + 4] = tolower ((int) before[i + 6]);
          after[j + 5] = tolower ((int) before[i + 7]);
          after[j + 6] = tolower ((int) before[i + 8]);
          after[j + 7] = tolower ((int) before[i + 9]);
          i += 10;
          j += 8;
        }
      else
        {
          after[j] = before[i];
          i++;
          j++;
        }
    }
  *lenp = j;
  after = scm_realloc (after, j);
}

char *
scm_to_locale_stringn (SCM str, size_t * lenp)
{
  const char *enc;

  /* In the future, enc will hold the port's encoding.  */
  enc = NULL;

  return scm_to_stringn (str, lenp, enc, 
                         SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
}

/* Low-level scheme to C string conversion function.  */
char *
scm_to_stringn (SCM str, size_t * lenp, const char *encoding,
                scm_t_string_failed_conversion_handler handler)
{
  static const char iso[11] = "ISO-8859-1";
  char *buf;
  size_t ilen, len, i;

  if (!scm_is_string (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  ilen = scm_i_string_length (str);

  if (ilen == 0)
    {
      buf = scm_malloc (1);
      buf[0] = '\0';
      if (lenp)
        *lenp = 0;
      return buf;
    }
	
  if (lenp == NULL)
    for (i = 0; i < ilen; i++)
      if (scm_i_string_ref (str, i) == '\0')
        scm_misc_error (NULL,
                        "string contains #\\nul character: ~S",
                        scm_list_1 (str));

  if (scm_i_is_narrow_string (str))
    {
      if (lenp)
        {
          buf = scm_malloc (ilen);
          memcpy (buf, scm_i_string_chars (str), ilen);
          *lenp = ilen;
          return buf;
        }
      else
        {
          buf = scm_malloc (ilen + 1);
          memcpy (buf, scm_i_string_chars (str), ilen);
          buf[ilen] = '\0';
          return buf;
        }
    }

  
  buf = NULL;
  len = 0;
  buf = u32_conv_to_encoding (iso,
                              (enum iconv_ilseq_handler) handler,
                              (scm_t_uint32 *) scm_i_string_wide_chars (str),
                              ilen, NULL, NULL, &len);
  if (buf == NULL)
    scm_misc_error (NULL, "cannot convert to output locale ~s: \"~s\"",
                    scm_list_2 (scm_from_locale_string (iso), str));

  if (handler == SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE)
    unistring_escapes_to_guile_escapes (&buf, &len);

  if (lenp)
    *lenp = len;
  else
    {
      buf = scm_realloc (buf, len + 1);
      buf[len] = '\0';
    }

  scm_remember_upto_here_1 (str);
  return buf;
}

char *
scm_to_locale_string (SCM str)
{
  return scm_to_locale_stringn (str, NULL);
}

size_t
scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len)
{
  size_t len;
  char *result = NULL;
  if (!scm_is_string (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  result = scm_to_locale_stringn (str, &len);

  memcpy (buf, result, (len > max_len) ? max_len : len);
  free (result);

  scm_remember_upto_here_1 (str);
  return len;
}

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
    lst = scm_cons (scm_from_locale_string (argv[i]), lst);
  return lst;
}

/* Return a newly allocated array of char pointers to each of the strings
   in args, with a terminating NULL pointer.  */

char **
scm_i_allocate_string_pointers (SCM list)
{
  char **result;
  int len = scm_ilength (list);
  int i;

  if (len < 0)
    scm_wrong_type_arg_msg (NULL, 0, list, "proper list");

  scm_dynwind_begin (0);

  result = (char **) scm_malloc ((len + 1) * sizeof (char *));
  result[len] = NULL;
  scm_dynwind_unwind_handler (free, result, 0);

  /* The list might be have been modified in another thread, so
     we check LIST before each access.
   */
  for (i = 0; i < len && scm_is_pair (list); i++)
    {
      result[i] = scm_to_locale_string (SCM_CAR (list));
      list = SCM_CDR (list);
    }

  scm_dynwind_end ();
  return result;
}

void
scm_i_free_string_pointers (char **pointers)
{
  int i;
  
  for (i = 0; pointers[i]; i++)
    free (pointers[i]);
  free (pointers);
}

void
scm_i_get_substring_spec (size_t len,
			  SCM start, size_t *cstart,
			  SCM end, size_t *cend)
{
  if (SCM_UNBNDP (start))
    *cstart = 0;
  else
    *cstart = scm_to_unsigned_integer (start, 0, len);

  if (SCM_UNBNDP (end))
    *cend = len;
  else
    *cend = scm_to_unsigned_integer (end, *cstart, len);
}
		  
#if SCM_ENABLE_DEPRECATED

/* When these definitions are removed, it becomes reasonable to use
   read-only strings for string literals.  For that, change the reader
   to create string literals with scm_c_substring_read_only instead of
   with scm_c_substring_copy.
*/

int
scm_i_deprecated_stringp (SCM str)
{
  scm_c_issue_deprecation_warning
    ("SCM_STRINGP is deprecated.  Use scm_is_string instead.");
  
  return scm_is_string (str);
}

char *
scm_i_deprecated_string_chars (SCM str)
{
  char *chars;

  scm_c_issue_deprecation_warning
    ("SCM_STRING_CHARS is deprecated.  See the manual for alternatives.");

  /* We don't accept shared substrings here since they are not
     null-terminated.
  */
  if (IS_SH_STRING (str))
    scm_misc_error (NULL, 
		    "SCM_STRING_CHARS does not work with shared substrings.",
		    SCM_EOL);

  /* We explicitly test for read-only strings to produce a better
     error message.
  */

  if (IS_RO_STRING (str))
    scm_misc_error (NULL, 
		    "SCM_STRING_CHARS does not work with read-only strings.",
		    SCM_EOL);
    
  /* The following is still wrong, of course...
   */
  str = scm_i_string_start_writing (str);
  chars = scm_i_string_writable_chars (str);
  scm_i_string_stop_writing ();
  return chars;
}

size_t
scm_i_deprecated_string_length (SCM str)
{
  scm_c_issue_deprecation_warning
    ("SCM_STRING_LENGTH is deprecated.  Use scm_c_string_length instead.");
  return scm_c_string_length (str);
}

#endif

static SCM
string_handle_ref (scm_t_array_handle *h, size_t index)
{
  return scm_c_string_ref (h->array, index);
}

static void
string_handle_set (scm_t_array_handle *h, size_t index, SCM val)
{
  scm_c_string_set_x (h->array, index, val);
}

static void
string_get_handle (SCM v, scm_t_array_handle *h)
{
  h->array = v;
  h->ndims = 1;
  h->dims = &h->dim0;
  h->dim0.lbnd = 0;
  h->dim0.ubnd = scm_c_string_length (v) - 1;
  h->dim0.inc = 1;
  h->element_type = SCM_ARRAY_ELEMENT_TYPE_CHAR;
  h->elements = h->writable_elements = NULL;
}

SCM_ARRAY_IMPLEMENTATION (scm_tc7_string, 0x7f & ~2,
                          string_handle_ref, string_handle_set,
                          string_get_handle);
SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_CHAR, scm_make_string);

void
scm_init_strings ()
{
  scm_nullstr = scm_i_make_string (0, NULL);

#include "libguile/strings.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
