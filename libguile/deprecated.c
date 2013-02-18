/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

/* Copyright (C) 2003, 2004, 2006, 2008, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

#define SCM_BUILDING_DEPRECATED_CODE

#include "libguile/_scm.h"
#include "libguile/deprecation.h"

#if (SCM_ENABLE_DEPRECATED == 1)



SCM
scm_internal_dynamic_wind (scm_t_guard before,
			   scm_t_inner inner,
			   scm_t_guard after,
			   void *inner_data,
			   void *guard_data)
{
  SCM ans;

  scm_c_issue_deprecation_warning
    ("`scm_internal_dynamic_wind' is deprecated.  "
     "Use the `scm_dynwind_begin' / `scm_dynwind_end' API instead.");

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_rewind_handler (before, guard_data, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (after, guard_data, SCM_F_WIND_EXPLICITLY);
  ans = inner (inner_data);
  scm_dynwind_end ();
  return ans;
}



SCM
scm_immutable_cell (scm_t_bits car, scm_t_bits cdr)
{
  scm_c_issue_deprecation_warning
    ("scm_immutable_cell is deprecated.  Use scm_cell instead.");

  return scm_cell (car, cdr);
}

SCM
scm_immutable_double_cell (scm_t_bits car, scm_t_bits cbr,
			   scm_t_bits ccr, scm_t_bits cdr)
{
  scm_c_issue_deprecation_warning
    ("scm_immutable_double_cell is deprecated.  Use scm_double_cell instead.");

  return scm_double_cell (car, cbr, ccr, cdr);
}





SCM_DEFINE (scm_generalized_vector_p, "generalized-vector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, string,\n"
	    "bitvector, or uniform numeric vector.")
#define FUNC_NAME s_scm_generalized_vector_p
{
  scm_c_issue_deprecation_warning
    ("generalized-vector? is deprecated.  Use array? and check the "
     "array-rank instead.");
  return scm_from_bool (scm_is_generalized_vector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_generalized_vector_length, "generalized-vector-length", 1, 0, 0,
	    (SCM v),
	    "Return the length of the generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_length
{
  scm_c_issue_deprecation_warning
    ("generalized-vector-length is deprecated.  Use array-length instead.");
  return scm_from_size_t (scm_c_generalized_vector_length (v));
}
#undef FUNC_NAME

SCM_DEFINE (scm_generalized_vector_ref, "generalized-vector-ref", 2, 0, 0,
	    (SCM v, SCM idx),
	    "Return the element at index @var{idx} of the\n"
	    "generalized vector @var{v}.")
#define FUNC_NAME s_scm_generalized_vector_ref
{
  scm_c_issue_deprecation_warning
    ("generalized-vector-ref is deprecated.  Use array-ref instead.");
  return scm_c_generalized_vector_ref (v, scm_to_size_t (idx));
}
#undef FUNC_NAME

SCM_DEFINE (scm_generalized_vector_set_x, "generalized-vector-set!", 3, 0, 0,
	    (SCM v, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the\n"
	    "generalized vector @var{v} to @var{val}.")
#define FUNC_NAME s_scm_generalized_vector_set_x
{
  scm_c_issue_deprecation_warning
    ("generalized-vector-set! is deprecated.  Use array-set! instead.  "
     "Note the change in argument order!");
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
  /* FIXME: This duplicates `array_to_list'.  */
  SCM ret = SCM_EOL;
  long inc;
  ssize_t pos, i;
  scm_t_array_handle h;

  scm_c_issue_deprecation_warning
    ("generalized-vector->list is deprecated.  Use array->list instead.");

  scm_generalized_vector_get_handle (v, &h);

  i = h.dims[0].ubnd - h.dims[0].lbnd + 1;
  inc = h.dims[0].inc;
  pos = (i - 1) * inc;

  for (; i > 0; i--, pos -= inc)
    ret = scm_cons (h.impl->vref (&h, h.base + pos), ret);

  scm_array_handle_release (&h);
  return ret;
}
#undef FUNC_NAME




void
scm_i_init_deprecated ()
{
#include "libguile/deprecated.x"
}

#endif
