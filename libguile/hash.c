/*	Copyright (C) 1995,1996,1997, 2000, 2001, 2003, 2004, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif

#include <math.h>
#include <unistr.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/symbols.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/hash.h"


#ifndef floor
extern double floor();
#endif


/* This hash function is originally from
   http://burtleburtle.net/bob/c/lookup3.c by Bob Jenkins, May 2006,
   Public Domain.  No warranty.  */

#define rot(x,k) (((x)<<(k)) | ((x)>>(32-(k))))
#define mix(a,b,c) \
{ \
  a -= c;  a ^= rot(c, 4);  c += b; \
  b -= a;  b ^= rot(a, 6);  a += c; \
  c -= b;  c ^= rot(b, 8);  b += a; \
  a -= c;  a ^= rot(c,16);  c += b; \
  b -= a;  b ^= rot(a,19);  a += c; \
  c -= b;  c ^= rot(b, 4);  b += a; \
}

#define final(a,b,c) \
{ \
  c ^= b; c -= rot(b,14); \
  a ^= c; a -= rot(c,11); \
  b ^= a; b -= rot(a,25); \
  c ^= b; c -= rot(b,16); \
  a ^= c; a -= rot(c,4);  \
  b ^= a; b -= rot(a,14); \
  c ^= b; c -= rot(b,24); \
}

#define JENKINS_LOOKUP3_HASHWORD2(k, length, ret)                       \
  do {                                                                  \
    scm_t_uint32 a, b, c;                                               \
                                                                        \
    /* Set up the internal state.  */                                   \
    a = b = c = 0xdeadbeef + ((scm_t_uint32)(length<<2)) + 47;          \
                                                                        \
    /* Handle most of the key.  */                                      \
    while (length > 3)                                                  \
      {                                                                 \
        a += k[0];                                                      \
        b += k[1];                                                      \
        c += k[2];                                                      \
        mix (a, b, c);                                                  \
        length -= 3;                                                    \
        k += 3;                                                         \
      }                                                                 \
                                                                        \
    /* Handle the last 3 elements.  */                                  \
    switch(length) /* All the case statements fall through.  */         \
      {                                                                 \
      case 3 : c += k[2];                                               \
      case 2 : b += k[1];                                               \
      case 1 : a += k[0];                                               \
        final (a, b, c);                                                \
      case 0:     /* case 0: nothing left to add */                     \
        break;                                                          \
      }                                                                 \
                                                                        \
    if (sizeof (ret) == 8)                                              \
      ret = (((unsigned long) c) << 32) | b;                            \
    else                                                                \
      ret = c;                                                          \
  } while (0)


static unsigned long
narrow_string_hash (const scm_t_uint8 *str, size_t len)
{
  unsigned long ret;
  JENKINS_LOOKUP3_HASHWORD2 (str, len, ret);
  ret >>= 2; /* Ensure that it fits in a fixnum.  */
  return ret;
}

static unsigned long
wide_string_hash (const scm_t_wchar *str, size_t len)
{
  unsigned long ret;
  JENKINS_LOOKUP3_HASHWORD2 (str, len, ret);
  ret >>= 2; /* Ensure that it fits in a fixnum.  */
  return ret;
}

unsigned long 
scm_string_hash (const unsigned char *str, size_t len)
{
  return narrow_string_hash (str, len);
}

unsigned long 
scm_i_string_hash (SCM str)
{
  size_t len = scm_i_string_length (str);

  if (scm_i_is_narrow_string (str))
    return narrow_string_hash ((const scm_t_uint8 *) scm_i_string_chars (str),
                               len);
  else
    return wide_string_hash (scm_i_string_wide_chars (str), len);
}

unsigned long 
scm_i_locale_string_hash (const char *str, size_t len)
{
  return scm_i_string_hash (scm_from_locale_stringn (str, len));
}

unsigned long 
scm_i_latin1_string_hash (const char *str, size_t len)
{
  if (len == (size_t) -1)
    len = strlen (str);

  return narrow_string_hash ((const scm_t_uint8 *) str, len);
}

/* A tricky optimization, but probably worth it.  */
unsigned long 
scm_i_utf8_string_hash (const char *str, size_t len)
{
  const scm_t_uint8 *end, *ustr = (const scm_t_uint8 *) str;
  unsigned long ret;

  /* The length of the string in characters.  This name corresponds to
     Jenkins' original name.  */
  size_t length;

  scm_t_uint32 a, b, c, u32;

  if (len == (size_t) -1)
    len = strlen (str);

  end = ustr + len;

  if (u8_check (ustr, len) != NULL)
    /* Invalid UTF-8; punt.  */
    return scm_i_string_hash (scm_from_utf8_stringn (str, len));

  length = u8_strnlen (ustr, len);

  /* Set up the internal state.  */
  a = b = c = 0xdeadbeef + ((scm_t_uint32)(length<<2)) + 47;

  /* Handle most of the key.  */
  while (length > 3)
    {
      ustr += u8_mbtouc_unsafe (&u32, ustr, end - ustr);
      a += u32;
      ustr += u8_mbtouc_unsafe (&u32, ustr, end - ustr);
      b += u32;
      ustr += u8_mbtouc_unsafe (&u32, ustr, end - ustr);
      c += u32;
      mix (a, b, c);
      length -= 3;
    }

  /* Handle the last 3 elements's.  */
  ustr += u8_mbtouc_unsafe (&u32, ustr, end - ustr);
  a += u32;
  if (--length)
    {
      ustr += u8_mbtouc_unsafe (&u32, ustr, end - ustr);
      b += u32;
      if (--length)
        {
          ustr += u8_mbtouc_unsafe (&u32, ustr, end - ustr);
          c += u32;
        }
    }

  final (a, b, c);

  if (sizeof (unsigned long) == 8)
    ret = (((unsigned long) c) << 32) | b;
  else
    ret = c;

  ret >>= 2; /* Ensure that it fits in a fixnum.  */
  return ret;
}


/* Thomas Wang's integer hasher, from
   http://www.cris.com/~Ttwang/tech/inthash.htm.  */
static unsigned long
scm_raw_ihashq (scm_t_bits key)
{
  if (sizeof (key) < 8)
    {
      key = (key ^ 61) ^ (key >> 16);
      key = key + (key << 3);
      key = key ^ (key >> 4);
      key = key * 0x27d4eb2d;
      key = key ^ (key >> 15);
    }
  else
    {
      key = (~key) + (key << 21); // key = (key << 21) - key - 1;
      key = key ^ (key >> 24);
      key = (key + (key << 3)) + (key << 8); // key * 265
      key = key ^ (key >> 14);
      key = (key + (key << 2)) + (key << 4); // key * 21
      key = key ^ (key >> 28);
      key = key + (key << 31);
    }
  key >>= 2; /* Ensure that it fits in a fixnum.  */
  return key;
}


/* Dirk:FIXME:: why downcase for characters? (2x: scm_hasher, scm_ihashv) */
/* Dirk:FIXME:: scm_hasher could be made static. */


unsigned long
scm_hasher(SCM obj, unsigned long n, size_t d)
{
  switch (SCM_ITAG3 (obj)) {
  case scm_tc3_int_1: 
  case scm_tc3_int_2:
    return SCM_I_INUM(obj) % n;   /* SCM_INUMP(obj) */
  case scm_tc3_imm24:
    if (SCM_CHARP(obj))
      return (unsigned)(scm_c_downcase(SCM_CHAR(obj))) % n;
    switch (SCM_UNPACK (obj)) {
    case SCM_EOL_BITS:
      d = 256; 
      break;
    case SCM_BOOL_T_BITS:
      d = 257; 
      break;
    case SCM_BOOL_F_BITS:
      d = 258; 
      break;
    case SCM_EOF_VAL_BITS:
      d = 259; 
      break;
    default: 
      d = 263;		/* perhaps should be error */
    }
    return d % n;
  default: 
    return 263 % n;	/* perhaps should be error */
  case scm_tc3_cons:
    switch SCM_TYP7(obj) {
    default: 
      return 263 % n;
    case scm_tc7_smob:
      return 263 % n;
    case scm_tc7_number:
      switch SCM_TYP16 (obj) {
      case scm_tc16_big:
        return scm_to_ulong (scm_modulo (obj, scm_from_ulong (n)));
      case scm_tc16_real:
	{
	  double r = SCM_REAL_VALUE (obj);
	  if (floor (r) == r && !isinf (r) && !isnan (r))
	    {
	      obj = scm_inexact_to_exact (obj);
	      return scm_to_ulong (scm_modulo (obj, scm_from_ulong (n)));
	    }
	}
        /* Fall through */
      case scm_tc16_complex:
      case scm_tc16_fraction:
	obj = scm_number_to_string (obj, scm_from_int (10));
        /* Fall through */
      }
      /* Fall through */
    case scm_tc7_string:
      {
	unsigned long hash =
	  scm_i_string_hash (obj) % n;
	return hash;
      }
    case scm_tc7_symbol:
      return scm_i_symbol_hash (obj) % n;
    case scm_tc7_pointer:
      {
	/* Pointer objects are typically used to store addresses of heap
	   objects.  On most platforms, these are at least 3-byte
	   aligned (on x86_64-*-gnu, `malloc' returns 4-byte aligned
	   addresses), so get rid of the least significant bits.  */
	scm_t_uintptr significant_bits;

	significant_bits = (scm_t_uintptr) SCM_POINTER_VALUE (obj) >> 4UL;
	return (size_t) significant_bits  % n;
      }
    case scm_tc7_wvect:
    case scm_tc7_vector:
      {
	size_t len = SCM_SIMPLE_VECTOR_LENGTH (obj);
	if (len > 5)
	  {
	    size_t i = d/2;
	    unsigned long h = 1;
	    while (i--)
	      {
		SCM elt = SCM_SIMPLE_VECTOR_REF (obj, h % len);
		h = ((h << 8) + (scm_hasher (elt, n, 2))) % n;
	      }
	    return h;
	  }
	else
	  {
	    size_t i = len;
	    unsigned long h = (n)-1;
	    while (i--)
	      {
		SCM elt = SCM_SIMPLE_VECTOR_REF (obj, h % len);
		h = ((h << 8) + (scm_hasher (elt, n, d/len))) % n;
	      }
	    return h;
	  }
      }
    case scm_tcs_cons_imcar: 
    case scm_tcs_cons_nimcar:
      if (d) return (scm_hasher (SCM_CAR (obj), n, d/2)
                     + scm_hasher (SCM_CDR (obj), n, d/2)) % n;
      else return 1;
    case scm_tc7_port:
      return ((SCM_RDNG & SCM_CELL_WORD_0 (obj)) ? 260 : 261) % n;
    case scm_tc7_program:
      return 262 % n;
    }
  }
}




unsigned long
scm_ihashq (SCM obj, unsigned long n)
{
  return scm_raw_ihashq (SCM_UNPACK (obj)) % n;
}


SCM_DEFINE (scm_hashq, "hashq", 2, 0, 0,
           (SCM key, SCM size),
	    "Determine a hash value for @var{key} that is suitable for\n"
	    "lookups in a hashtable of size @var{size}, where @code{eq?} is\n"
	    "used as the equality predicate.  The function returns an\n"
	    "integer in the range 0 to @var{size} - 1.  Note that\n"
	    "@code{hashq} may use internal addresses.  Thus two calls to\n"
	    "hashq where the keys are @code{eq?} are not guaranteed to\n"
	    "deliver the same value if the key object gets garbage collected\n"
	    "in between.  This can happen, for example with symbols:\n"
	    "@code{(hashq 'foo n) (gc) (hashq 'foo n)} may produce two\n"
	    "different values, since @code{foo} will be garbage collected.")
#define FUNC_NAME s_scm_hashq
{
  unsigned long sz = scm_to_unsigned_integer (size, 1, ULONG_MAX);
  return scm_from_ulong (scm_ihashq (key, sz));
}
#undef FUNC_NAME





unsigned long
scm_ihashv (SCM obj, unsigned long n)
{
  if (SCM_CHARP(obj))
    return ((unsigned long) (scm_c_downcase (SCM_CHAR (obj)))) % n; /* downcase!?!! */

  if (SCM_NUMP(obj))
    return (unsigned long) scm_hasher(obj, n, 10);
  else
    return scm_raw_ihashq (SCM_UNPACK (obj)) % n;
}


SCM_DEFINE (scm_hashv, "hashv", 2, 0, 0,
           (SCM key, SCM size),
	    "Determine a hash value for @var{key} that is suitable for\n"
	    "lookups in a hashtable of size @var{size}, where @code{eqv?} is\n"
	    "used as the equality predicate.  The function returns an\n"
	    "integer in the range 0 to @var{size} - 1.  Note that\n"
	    "@code{(hashv key)} may use internal addresses.  Thus two calls\n"
	    "to hashv where the keys are @code{eqv?} are not guaranteed to\n"
	    "deliver the same value if the key object gets garbage collected\n"
	    "in between.  This can happen, for example with symbols:\n"
	    "@code{(hashv 'foo n) (gc) (hashv 'foo n)} may produce two\n"
	    "different values, since @code{foo} will be garbage collected.")
#define FUNC_NAME s_scm_hashv
{
  unsigned long sz = scm_to_unsigned_integer (size, 1, ULONG_MAX);
  return scm_from_ulong (scm_ihashv (key, sz));
}
#undef FUNC_NAME





unsigned long
scm_ihash (SCM obj, unsigned long n)
{
  return (unsigned long) scm_hasher (obj, n, 10);
}

SCM_DEFINE (scm_hash, "hash", 2, 0, 0,
           (SCM key, SCM size),
	    "Determine a hash value for @var{key} that is suitable for\n"
	    "lookups in a hashtable of size @var{size}, where @code{equal?}\n"
	    "is used as the equality predicate.  The function returns an\n"
	    "integer in the range 0 to @var{size} - 1.")
#define FUNC_NAME s_scm_hash
{
  unsigned long sz = scm_to_unsigned_integer (size, 1, ULONG_MAX);
  return scm_from_ulong (scm_ihash (key, sz));
}
#undef FUNC_NAME





void
scm_init_hash ()
{
#include "libguile/hash.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
