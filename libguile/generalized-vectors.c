/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005, 2006, 2009 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/__scm.h"

#include "libguile/array-handle.h"
#include "libguile/generalized-arrays.h"
#include "libguile/generalized-vectors.h"


struct scm_t_vector_ctor
{
  SCM tag;
  SCM (*ctor)(SCM, SCM);
};

#define VECTOR_CTORS_N_STATIC_ALLOC 20
static struct scm_t_vector_ctor vector_ctors[VECTOR_CTORS_N_STATIC_ALLOC];
static int num_vector_ctors_registered = 0;

void
scm_i_register_vector_constructor (SCM type, SCM (*ctor)(SCM, SCM))
{
  if (num_vector_ctors_registered >= VECTOR_CTORS_N_STATIC_ALLOC)
    /* need to increase VECTOR_CTORS_N_STATIC_ALLOC, buster */
    abort ();
  else
    { 
      vector_ctors[num_vector_ctors_registered].tag = type;
      vector_ctors[num_vector_ctors_registered].ctor = ctor;
      num_vector_ctors_registered++;
    }
}

SCM_DEFINE (scm_make_generalized_vector, "make-generalized-vector", 2, 1, 0,
            (SCM type, SCM len, SCM fill),
            "Make a generalized vector")
#define FUNC_NAME s_scm_make_generalized_vector
{
  int i;
  for (i = 0; i < num_vector_ctors_registered; i++)
    if (vector_ctors[i].tag == type)
      return vector_ctors[i].ctor(len, fill);
  scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, type, "array type");
}
#undef FUNC_NAME

int
scm_is_generalized_vector (SCM obj)
{
  int ret = 0;
  if (scm_is_array (obj))
    {
      scm_t_array_handle h;
      scm_array_get_handle (obj, &h);
      ret = scm_array_handle_rank (&h) == 1;
      scm_array_handle_release (&h);
    }
  return ret;
}

SCM_DEFINE (scm_generalized_vector_p, "generalized-vector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, string,\n"
	    "bitvector, or uniform numeric vector.")
#define FUNC_NAME s_scm_generalized_vector_p
{
  return scm_from_bool (scm_is_generalized_vector (obj));
}
#undef FUNC_NAME

#define SCM_VALIDATE_VECTOR_WITH_HANDLE(pos, val, handle)   \
  scm_generalized_vector_get_handle (val, handle)
   

void
scm_generalized_vector_get_handle (SCM vec, scm_t_array_handle *h)
{
  scm_array_get_handle (vec, h);
  if (scm_array_handle_rank (h) != 1)
    {
      scm_array_handle_release (h);
      scm_wrong_type_arg_msg (NULL, 0, vec, "vector");
    }
}

size_t
scm_c_generalized_vector_length (SCM v)
{
  scm_t_array_handle h;
  size_t ret;
  scm_generalized_vector_get_handle (v, &h);
  ret = h.dims[0].ubnd - h.dims[0].lbnd + 1;
  scm_array_handle_release (&h);
  return ret;
}

SCM_DEFINE (scm_generalized_vector_length, "generalized-vector-length", 1, 0, 0,
	    (SCM v),
	    "Return the length of the generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_length
{
  return scm_from_size_t (scm_c_generalized_vector_length (v));
}
#undef FUNC_NAME

SCM
scm_c_generalized_vector_ref (SCM v, size_t idx)
{
  scm_t_array_handle h;
  SCM ret;
  scm_generalized_vector_get_handle (v, &h);
  ret = h.impl->vref (&h, idx);
  scm_array_handle_release (&h);
  return ret;
}

SCM_DEFINE (scm_generalized_vector_ref, "generalized-vector-ref", 2, 0, 0,
	    (SCM v, SCM idx),
	    "Return the element at index @var{idx} of the\n"
	    "generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_ref
{
  return scm_c_generalized_vector_ref (v, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_generalized_vector_set_x (SCM v, size_t idx, SCM val)
{
  scm_t_array_handle h;
  scm_generalized_vector_get_handle (v, &h);
  h.impl->vset (&h, idx, val);
  scm_array_handle_release (&h);
}

SCM_DEFINE (scm_generalized_vector_set_x, "generalized-vector-set!", 3, 0, 0,
	    (SCM v, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the\n"
	    "generalized vector @var{v} to @var{val}.")
#define FUNC_NAME s_scm_generalized_vector_set_x
{
  scm_c_generalized_vector_set_x (v, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_generalized_vector_to_list, "generalized-vector->list", 1, 0, 0,
	    (SCM v),
	    "Return a new list whose elements are the elements of the\n"
	    "generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_to_list
{
  SCM ret = SCM_EOL;
  ssize_t pos, i = 0;
  scm_t_array_handle h;
  scm_generalized_vector_get_handle (v, &h);
  // FIXME CHECKME
  for (pos = h.dims[0].ubnd, i = (h.dims[0].ubnd - h.dims[0].lbnd + 1);
       i >= 0;
       pos += h.dims[0].inc)
    ret = scm_cons (h.impl->vref (&h, pos), ret);
  scm_array_handle_release (&h);
  return ret;
}
#undef FUNC_NAME

void
scm_init_generalized_vectors ()
{
#include "libguile/generalized-vectors.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
