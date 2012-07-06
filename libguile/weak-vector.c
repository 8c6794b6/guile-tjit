/* Copyright (C) 1995,1996,1998,2000,2001, 2003, 2006, 2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"



/* {Weak Vectors}
 */

#define VECTOR_MAX_LENGTH (SCM_T_BITS_MAX >> 8)

static SCM
make_weak_vector (size_t len, SCM fill)
#define FUNC_NAME "make-weak-vector"
{
  SCM wv;
  size_t j;

  SCM_ASSERT_RANGE (1, scm_from_size_t (len), len <= VECTOR_MAX_LENGTH);

  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;

  wv = SCM_PACK_POINTER (scm_gc_malloc_pointerless ((len + 1) * sizeof (SCM),
                                           "weak vector"));

  SCM_SET_CELL_WORD_0 (wv, (len << 8) | scm_tc7_wvect);

  if (SCM_HEAP_OBJECT_P (fill))
    {
      memset (SCM_I_VECTOR_WELTS (wv), 0, len * sizeof (SCM));
      for (j = 0; j < len; j++)
        scm_c_weak_vector_set_x (wv, j, fill);
    }
  else
    for (j = 0; j < len; j++)
      SCM_SIMPLE_VECTOR_SET (wv, j, fill);

  return wv;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
	    (SCM size, SCM fill),
	    "Return a weak vector with @var{size} elements. If the optional\n"
	    "argument @var{fill} is given, all entries in the vector will be\n"
	    "set to @var{fill}. The default value for @var{fill} is the\n"
	    "empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  return make_weak_vector (scm_to_size_t (size), fill);
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);

SCM_DEFINE (scm_weak_vector, "weak-vector", 0, 0, 1, 
           (SCM lst),
	    "@deffnx {Scheme Procedure} list->weak-vector lst\n"
	    "Construct a weak vector from a list: @code{weak-vector} uses\n"
	    "the list of its arguments while @code{list->weak-vector} uses\n"
	    "its only argument @var{l} (a list) to construct a weak vector\n"
	    "the same way @code{list->vector} would.")
#define FUNC_NAME s_scm_weak_vector
{
  SCM wv;
  size_t i;
  long c_size;

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG1, lst, c_size);

  wv = make_weak_vector ((size_t) c_size, SCM_BOOL_F);

  for (i = 0; scm_is_pair (lst); lst = SCM_CDR (lst), i++)
    scm_c_weak_vector_set_x (wv, i, SCM_CAR (lst));

  return wv;
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak vector. Note that all\n"
	    "weak hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj));
}
#undef FUNC_NAME


struct weak_vector_ref_data
{
  SCM wv;
  size_t k;
};

static void*
weak_vector_ref (void *data)
{
  struct weak_vector_ref_data *d = data;

  return SCM_SIMPLE_VECTOR_REF (d->wv, d->k);
}

SCM
scm_c_weak_vector_ref (SCM wv, size_t k)
{
  struct weak_vector_ref_data d;
  void *ret;

  d.wv = wv;
  d.k = k;
  
  if (k >= SCM_I_VECTOR_LENGTH (wv))
    scm_out_of_range (NULL, scm_from_size_t (k)); 

  ret = GC_call_with_alloc_lock (weak_vector_ref, &d);
  
  if (ret)
    return SCM_PACK_POINTER (ret);
  else
    return SCM_BOOL_F;
}


void
scm_c_weak_vector_set_x (SCM wv, size_t k, SCM x)
{
  SCM *elts;
  struct weak_vector_ref_data d;
  void *prev;

  d.wv = wv;
  d.k = k;

  if (k >= SCM_I_VECTOR_LENGTH (wv))
    scm_out_of_range (NULL, scm_from_size_t (k)); 
  
  prev = GC_call_with_alloc_lock (weak_vector_ref, &d);

  elts = SCM_I_VECTOR_WELTS (wv);

  if (prev && SCM_HEAP_OBJECT_P (SCM_PACK_POINTER (prev)))
    GC_unregister_disappearing_link ((void **) &elts[k]);
  
  elts[k] = x;

  if (SCM_HEAP_OBJECT_P (x))
    SCM_I_REGISTER_DISAPPEARING_LINK ((void **) &elts[k],
                                      SCM2PTR (x));
}



static void
scm_init_weak_vector_builtins (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/weak-vector.x"
#endif
}

void
scm_init_weak_vectors ()
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_weak_vector_builtins",
                            (scm_t_extension_init_func)scm_init_weak_vector_builtins,
                            NULL);
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
