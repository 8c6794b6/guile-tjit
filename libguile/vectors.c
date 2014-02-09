/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2006, 2008, 2009, 2010,
 *   2011, 2012, 2014 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/eq.h"
#include "libguile/root.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/vectors.h"
#include "libguile/arrays.h" /* Hit me with the ugly stick */
#include "libguile/generalized-vectors.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/dynwind.h"

#include "libguile/bdw-gc.h"




#define VECTOR_MAX_LENGTH (SCM_T_BITS_MAX >> 8)

int
scm_is_vector (SCM obj)
{
  return SCM_I_IS_VECTOR (obj);
}

int
scm_is_simple_vector (SCM obj)
{
  return SCM_I_IS_VECTOR (obj);
}

const SCM *
scm_vector_elements (SCM vec, scm_t_array_handle *h,
		     size_t *lenp, ssize_t *incp)
{
  if (SCM_I_WVECTP (vec))
    scm_wrong_type_arg_msg (NULL, 0, vec, "non-weak vector");

  scm_generalized_vector_get_handle (vec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_elements (h);
}

SCM *
scm_vector_writable_elements (SCM vec, scm_t_array_handle *h,
			      size_t *lenp, ssize_t *incp)
{
  if (SCM_I_WVECTP (vec))
    scm_wrong_type_arg_msg (NULL, 0, vec, "non-weak vector");

  scm_generalized_vector_get_handle (vec, h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return scm_array_handle_writable_elements (h);
}

SCM_DEFINE (scm_vector_p, "vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_vector_p
{
  return scm_from_bool (scm_is_vector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_length, "vector-length", 1, 0, 0, 
	    (SCM v),
            "Returns the number of elements in @var{vector} as an exact integer.")
#define FUNC_NAME s_scm_vector_length
{
  return scm_from_size_t (scm_c_vector_length (v));
}
#undef FUNC_NAME

size_t
scm_c_vector_length (SCM v)
#define FUNC_NAME s_scm_vector_length
{
  SCM_VALIDATE_VECTOR (1, v);

  return SCM_I_VECTOR_LENGTH (v);
}
#undef FUNC_NAME

SCM_REGISTER_PROC (s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
/*
	    "Return a newly created vector initialized to the elements of"
	    "the list @var{list}.\n\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{} (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}   #(dididit dah)\n"
	    "@end lisp")
*/
SCM_DEFINE (scm_vector, "vector", 0, 0, 1, 
	    (SCM l),
	    "@deffnx {Scheme Procedure} list->vector l\n"
	    "Return a newly allocated vector composed of the\n"
	    "given arguments.  Analogous to @code{list}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector 'a 'b 'c) @result{} #(a b c)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector
{
  SCM res;
  SCM *data;
  long i, len;
  scm_t_array_handle handle;

  SCM_VALIDATE_LIST_COPYLEN (1, l, len);

  res = scm_c_make_vector (len, SCM_UNSPECIFIED);
  data = scm_vector_writable_elements (res, &handle, NULL, NULL);
  i = 0;
  while (scm_is_pair (l) && i < len) 
    {
      data[i] = SCM_CAR (l);
      l = SCM_CDR (l);
      i += 1;
    }

  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_ref, "vector-ref", 2, 0, 0, 
	    (SCM vector, SCM k),
            "@var{k} must be a valid index of @var{vector}.\n"
            "@samp{Vector-ref} returns the contents of element @var{k} of\n"
            "@var{vector}.\n\n"
            "@lisp\n"
            "(vector-ref '#(1 1 2 3 5 8 13 21) 5) @result{} 8\n"
            "(vector-ref '#(1 1 2 3 5 8 13 21)\n"
            "    (let ((i (round (* 2 (acos -1)))))\n"
            "      (if (inexact? i)\n"
            "        (inexact->exact i)\n"
            "           i))) @result{} 13\n"
            "@end lisp")
#define FUNC_NAME s_scm_vector_ref
{
  return scm_c_vector_ref (vector, scm_to_size_t (k));
}
#undef FUNC_NAME

SCM
scm_c_vector_ref (SCM v, size_t k)
#define FUNC_NAME s_scm_vector_ref
{
  SCM_VALIDATE_VECTOR (1, v);

  if (k >= SCM_I_VECTOR_LENGTH (v))
    scm_out_of_range (NULL, scm_from_size_t (k));

  return SCM_SIMPLE_VECTOR_REF (v, k);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_set_x, "vector-set!", 3, 0, 0, 
	    (SCM vector, SCM k, SCM obj),
            "@var{k} must be a valid index of @var{vector}.\n"
            "@code{Vector-set!} stores @var{obj} in element @var{k} of @var{vector}.\n"
            "The value returned by @samp{vector-set!} is unspecified.\n"
            "@lisp\n"
            "(let ((vec (vector 0 '(2 2 2 2) \"Anna\")))\n"
            "  (vector-set! vec 1 '(\"Sue\" \"Sue\"))\n"
            "  vec) @result{}  #(0 (\"Sue\" \"Sue\") \"Anna\")\n"
            "(vector-set! '#(0 1 2) 1 \"doe\") @result{} @emph{error} ; constant vector\n"
            "@end lisp")
#define FUNC_NAME s_scm_vector_set_x
{
  scm_c_vector_set_x (vector, scm_to_size_t (k), obj);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_vector_set_x (SCM v, size_t k, SCM obj)
#define FUNC_NAME s_scm_vector_set_x
{
  SCM_VALIDATE_VECTOR (1, v);

  if (k >= SCM_I_VECTOR_LENGTH (v))
    scm_out_of_range (NULL, scm_from_size_t (k)); 

  SCM_SIMPLE_VECTOR_SET (v, k, obj);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_vector, "make-vector", 1, 1, 0,
            (SCM k, SCM fill),
	    "Return a newly allocated vector of @var{k} elements.  If a\n"
	    "second argument is given, then each position is initialized to\n"
	    "@var{fill}.  Otherwise the initial contents of each position is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_make_vector
{
  size_t l = scm_to_unsigned_integer (k, 0, VECTOR_MAX_LENGTH);

  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;
  
  return scm_c_make_vector (l, fill);
}
#undef FUNC_NAME


SCM
scm_c_make_vector (size_t k, SCM fill)
#define FUNC_NAME s_scm_make_vector
{
  SCM vector;
  unsigned long int j;

  SCM_ASSERT_RANGE (1, scm_from_size_t (k), k <= VECTOR_MAX_LENGTH);

  vector = scm_words ((k << 8) | scm_tc7_vector, k + 1);

  for (j = 0; j < k; ++j)
    SCM_SIMPLE_VECTOR_SET (vector, j, fill);

  return vector;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_copy, "vector-copy", 1, 0, 0,
	    (SCM vec),
	    "Return a copy of @var{vec}.")
#define FUNC_NAME s_scm_vector_copy
{
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const SCM *src;
  SCM result, *dst;

  src = scm_vector_elements (vec, &handle, &len, &inc);

  result = scm_c_make_vector (len, SCM_UNDEFINED);
  dst = SCM_I_VECTOR_WELTS (result);
  for (i = 0; i < len; i++, src += inc)
    dst[i] = *src;

  scm_array_handle_release (&handle);

  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_to_list, "vector->list", 1, 0, 0, 
	    (SCM v),
	    "Return a newly allocated list composed of the elements of @var{v}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{}  (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}  #(dididit dah)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector_to_list
{
  SCM res = SCM_EOL;
  const SCM *data;
  scm_t_array_handle handle;
  size_t i, count, len;
  ssize_t inc;

  data = scm_vector_elements (v, &handle, &len, &inc);
  for (i = (len - 1) * inc, count = 0;
       count < len;
       i -= inc, count++)
    res = scm_cons (data[i], res);

  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_fill_x, "vector-fill!", 2, 0, 0,
            (SCM v, SCM fill),
	    "Store @var{fill} in every position of @var{vector}.  The value\n"
	    "returned by @code{vector-fill!} is unspecified.")
#define FUNC_NAME s_scm_vector_fill_x
{
  scm_t_array_handle handle;
  SCM *data;
  size_t i, len;
  ssize_t inc;

  data = scm_vector_writable_elements (v, &handle, &len, &inc);
  for (i = 0; i < len; i += inc)
    data[i] = fill;
  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_i_vector_equal_p (SCM x, SCM y)
{
  long i;
  for (i = SCM_I_VECTOR_LENGTH (x) - 1; i >= 0; i--)
    if (scm_is_false (scm_equal_p (SCM_I_VECTOR_ELTS (x)[i],
				   SCM_I_VECTOR_ELTS (y)[i])))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}


SCM_DEFINE (scm_vector_move_left_x, "vector-move-left!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-left!} copies elements in leftmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-left!} is usually appropriate when\n"
	    "@var{start1} is greater than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_left_x
{
  scm_t_array_handle handle1, handle2;
  const SCM *elts1;
  SCM *elts2;
  size_t len1, len2;
  ssize_t inc1, inc2;
  size_t i, j, e;
  
  elts1 = scm_vector_elements (vec1, &handle1, &len1, &inc1);
  elts2 = scm_vector_writable_elements (vec2, &handle2, &len2, &inc2);

  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  i *= inc1;
  e *= inc1;
  j *= inc2;
  for (; i < e; i += inc1, j += inc2)
    elts2[j] = elts1[i];

  scm_array_handle_release (&handle2);
  scm_array_handle_release (&handle1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_move_right_x, "vector-move-right!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-right!} copies elements in rightmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-right!} is usually appropriate when\n"
	    "@var{start1} is less than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_right_x
{
  scm_t_array_handle handle1, handle2;
  const SCM *elts1;
  SCM *elts2;
  size_t len1, len2;
  ssize_t inc1, inc2;
  size_t i, j, e;
  
  elts1 = scm_vector_elements (vec1, &handle1, &len1, &inc1);
  elts2 = scm_vector_writable_elements (vec2, &handle2, &len2, &inc2);

  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  j += (e - i);
  
  i *= inc1;
  e *= inc1;
  j *= inc2;
  while (i < e)
    {
      e -= inc1;
      j -= inc2;
      elts2[j] = elts1[e];
    }

  scm_array_handle_release (&handle2);
  scm_array_handle_release (&handle1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_SCM, scm_make_vector)


void
scm_init_vectors ()
{
#include "libguile/vectors.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
