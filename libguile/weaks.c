/*	Copyright (C) 1995,1996,1998, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, Free Software Foundation gives permission
 * for additional uses of the text contained in its release of this library.
 *
 * The exception is that, if you link this library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking this library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by 
 * Free Software Foundation as part of this library.  If you copy
 * code from other releases distributed under the terms of the GPL into a copy of
 * this library, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from such code.
 *
 * If you write modifications of your own for this library, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/weaks.h"



/* {Weak Vectors}
 */


SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
           (SCM k, SCM fill),
	    "Return a weak vector with @var{size} elements. If the optional\n"
	    "argument @var{fill} is given, all entries in the vector will be set to\n"
	    "@var{fill}. The default value for @var{fill} is the empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  SCM v;
  v = scm_make_vector (scm_sum (k, SCM_MAKINUM (2)), fill);
  SCM_DEFER_INTS;
  SCM_SETLENGTH(v, SCM_INUM (k), scm_tc7_wvect);
  SCM_SETVELTS(v, SCM_VELTS(v) + 2);
  SCM_VELTS(v)[-2] = SCM_EOL;
  SCM_UNPACK (SCM_VELTS (v)[-1]) = 0;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);

SCM_DEFINE (scm_weak_vector, "weak-vector", 0, 0, 1, 
           (SCM l),
	    "@deffnx primitive list->weak-vector l\n"
	    "Construct a weak vector from a list: @code{weak-vector} uses the list of\n"
	    "its arguments while @code{list->weak-vector} uses its only argument\n"
	    "@var{l} (a list) to construct a weak vector the same way\n"
	    "@code{vector->list} would.")
#define FUNC_NAME s_scm_weak_vector
{
  SCM res;
  register SCM *data;
  long i;

  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, SCM_ARG1, FUNC_NAME);
  res = scm_make_weak_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED);
  data = SCM_VELTS (res);
  for (;
       i && SCM_CONSP (l);
       --i, l = SCM_CDR (l))
    *data++ = SCM_CAR (l);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
           (SCM x),
	    "Return @var{#t} if @var{obj} is a weak vector. Note that all weak\n"
	    "hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return SCM_BOOL(SCM_WVECTP (x) && !SCM_IS_WHVEC (x));
}
#undef FUNC_NAME







SCM_DEFINE (scm_make_weak_key_hash_table, "make-weak-key-hash-table", 1, 0, 0, 
           (SCM k),
	    "@deffnx primitive make-weak-value-hash-table size\n"
	    "@deffnx primitive make-doubly-weak-hash-table size\n"
	    "Return a weak hash table with @var{size} buckets. As with any hash\n"
	    "table, choosing a good size for the table requires some caution.\n\n"
	    "You can modify weak hash tables in exactly the same way you would modify\n"
	    "regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_hash_table
{
  SCM v;
  SCM_VALIDATE_INUM (1,k);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_DEFER_INTS;
  SCM_UNPACK (SCM_VELTS (v)[-1]) = 1;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_hash_table, "make-weak-value-hash-table", 1, 0, 0, 
            (SCM k),
	    "")
#define FUNC_NAME s_scm_make_weak_value_hash_table
{
  SCM v;
  SCM_VALIDATE_INUM (1,k);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_DEFER_INTS;
  SCM_UNPACK (SCM_VELTS (v)[-1]) = 2;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 1, 0, 0, 
            (SCM k),
	    "")
#define FUNC_NAME s_scm_make_doubly_weak_hash_table
{
  SCM v;
  SCM_VALIDATE_INUM (1,k);
  v = scm_make_weak_vector (k, SCM_EOL);
  SCM_DEFER_INTS;
  SCM_UNPACK (SCM_VELTS (v)[-1]) = 3;
  SCM_ALLOW_INTS;
  return v;
}
#undef FUNC_NAME

SCM_DEFINE (scm_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, 
           (SCM x),
	    "@deffnx primitive weak-value-hash-table? obj\n"
	    "@deffnx primitive doubly-weak-hash-table? obj\n"
	    "Return @var{#t} if @var{obj} is the specified weak hash table. Note\n"
	    "that a doubly weak hash table is neither a weak key nor a weak value\n"
	    "hash table.")
#define FUNC_NAME s_scm_weak_key_hash_table_p
{
  return SCM_BOOL(SCM_WVECTP (x) && SCM_IS_WHVEC(x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, 
            (SCM x),
	    "")
#define FUNC_NAME s_scm_weak_value_hash_table_p
{
  return SCM_BOOL(SCM_WVECTP (x) && SCM_IS_WHVEC_V(x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, 
            (SCM x),
	    "")
#define FUNC_NAME s_scm_doubly_weak_hash_table_p
{
  return SCM_BOOL(SCM_WVECTP (x) && SCM_IS_WHVEC_B (x));
}
#undef FUNC_NAME

static void *
scm_weak_vector_gc_init (void *dummy1, void *dummy2, void *dummy3)
{
  scm_weak_vectors = SCM_EOL;

  return 0;
}

static void *
scm_mark_weak_vector_spines (void *dummy1, void *dummy2, void *dummy3)
{
  SCM w;

  for (w = scm_weak_vectors; !SCM_NULLP (w); w = SCM_WVECT_GC_CHAIN (w))
    {
      if (SCM_IS_WHVEC_ANY (w))
	{
	  SCM *ptr;
	  SCM obj;
	  int j;
	  int n;

	  obj = w;
	  ptr = SCM_VELTS (w);
	  n = SCM_LENGTH (w);
	  for (j = 0; j < n; ++j)
	    {
	      SCM alist;

	      alist = ptr[j];
	      while (   SCM_CONSP (alist)
		     && !SCM_GCMARKP (alist)
		     && SCM_CONSP  (SCM_CAR (alist)))
		{
		  SCM_SETGCMARK (alist);
		  SCM_SETGCMARK (SCM_CAR (alist));
		  alist = SCM_GCCDR (alist);
		}
	    }
	}
    }

  return 0;
}

static void *
scm_scan_weak_vectors (void *dummy1, void *dummy2, void *dummy3)
{
  SCM *ptr, w;
  for (w = scm_weak_vectors; !SCM_NULLP (w); w = SCM_WVECT_GC_CHAIN (w))
    {
      if (!SCM_IS_WHVEC_ANY (w))
	{
	  register long j, n;

	  ptr = SCM_VELTS (w);
	  n = SCM_LENGTH (w);
	  for (j = 0; j < n; ++j)
	    if (SCM_FREEP (ptr[j]))
	      ptr[j] = SCM_BOOL_F;
	}
      else /* if (SCM_IS_WHVEC_ANY (scm_weak_vectors[i])) */
	{
	  SCM obj = w;
	  register long n = SCM_LENGTH (w);
	  register long j;

	  ptr = SCM_VELTS (w);

	  for (j = 0; j < n; ++j)
	    {
	      SCM * fixup;
	      SCM alist;
	      int weak_keys;
	      int weak_values;

	      weak_keys = SCM_IS_WHVEC (obj) || SCM_IS_WHVEC_B (obj);
	      weak_values = SCM_IS_WHVEC_V (obj) || SCM_IS_WHVEC_B (obj);

	      fixup = ptr + j;
	      alist = *fixup;

	      while (   SCM_CONSP (alist)
			&& SCM_CONSP (SCM_CAR (alist)))
		{
		  SCM key;
		  SCM value;

		  key = SCM_CAAR (alist);
		  value = SCM_CDAR (alist);
		  if (   (weak_keys && SCM_FREEP (key))
			 || (weak_values && SCM_FREEP (value)))
		    {
		      *fixup = SCM_CDR (alist);
		    }
		  else
		    fixup = SCM_CDRLOC (alist);
		  alist = SCM_CDR (alist);
		}
	    }
	}
    }

  return 0;
}





void
scm_weaks_prehistory ()
{
  scm_c_hook_add (&scm_before_mark_c_hook, scm_weak_vector_gc_init, 0, 0);
  scm_c_hook_add (&scm_before_sweep_c_hook, scm_mark_weak_vector_spines, 0, 0);
  scm_c_hook_add (&scm_after_sweep_c_hook, scm_scan_weak_vectors, 0, 0);
}

void
scm_init_weaks ()
{
#include "libguile/weaks.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
