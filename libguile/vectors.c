/* Copyright (C) 1995,1996,1998,1999,2000,2001 Free Software Foundation, Inc.
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




#include "libguile/_scm.h"
#include "libguile/eq.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/vectors.h"
#include "libguile/unif.h"


SCM_DEFINE (scm_vector_p, "vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_vector_p
{
  return scm_from_bool (SCM_VECTORP (obj));
}
#undef FUNC_NAME

SCM_GPROC (s_vector_length, "vector-length", 1, 0, 0, scm_vector_length, g_vector_length);
/* Returns the number of elements in @var{vector} as an exact integer.  */
SCM
scm_vector_length (SCM v)
{
  SCM_GASSERT1 (SCM_VECTORP(v),
		g_vector_length, v, SCM_ARG1, s_vector_length);
  return SCM_I_MAKINUM (SCM_VECTOR_LENGTH (v));
}

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
  long i;

  /* Dirk:FIXME:: In case of multiple threads, the list might get corrupted
     while the vector is being created. */
  SCM_VALIDATE_LIST_COPYLEN (1, l, i);
  res = scm_c_make_vector (i, SCM_UNSPECIFIED);

  /*
    this code doesn't alloc. -- accessing RES is safe. 
  */
  data = SCM_WRITABLE_VELTS (res);
  while (!SCM_NULL_OR_NIL_P (l)) 
    {
      *data++ = SCM_CAR (l);
      l = SCM_CDR (l);
    }

  return res;
}
#undef FUNC_NAME

SCM_GPROC (s_vector_ref, "vector-ref", 2, 0, 0, scm_vector_ref, g_vector_ref);

/*
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
	   "@end lisp"
*/

SCM
scm_vector_ref (SCM v, SCM k)
#define FUNC_NAME s_vector_ref
{
  SCM_GASSERT2 (SCM_VECTORP (v),
		g_vector_ref, v, k, SCM_ARG1, s_vector_ref);
  SCM_GASSERT2 (SCM_INUMP (k),
		g_vector_ref, v, k, SCM_ARG2, s_vector_ref);
  SCM_ASSERT_RANGE (2, k, SCM_INUM (k) < SCM_VECTOR_LENGTH (v) && SCM_INUM (k) >= 0);
  return SCM_VELTS (v)[(long) SCM_INUM (k)];
}
#undef FUNC_NAME

SCM_GPROC (s_vector_set_x, "vector-set!", 3, 0, 0, scm_vector_set_x, g_vector_set_x);

/* "@var{k} must be a valid index of @var{vector}.\n"
   "@code{Vector-set!} stores @var{obj} in element @var{k} of @var{vector}.\n"
   "The value returned by @samp{vector-set!} is unspecified.\n"
   "@lisp\n"
   "(let ((vec (vector 0 '(2 2 2 2) "Anna")))\n"
   "  (vector-set! vec 1 '("Sue" "Sue"))\n"
   "  vec) @result{}  #(0 ("Sue" "Sue") "Anna")\n"
   "(vector-set! '#(0 1 2) 1 "doe") @result{} @emph{error} ; constant vector\n"
   "@end lisp"
*/

SCM
scm_vector_set_x (SCM v, SCM k, SCM obj)
#define FUNC_NAME s_vector_set_x
{
  SCM_GASSERTn (SCM_VECTORP (v),
		g_vector_set_x, scm_list_3 (v, k, obj),
		SCM_ARG1, s_vector_set_x);
  SCM_GASSERTn (SCM_INUMP (k),
		g_vector_set_x, scm_list_3 (v, k, obj),
		SCM_ARG2, s_vector_set_x);
  SCM_ASSERT_RANGE (2, k, SCM_INUM (k) < SCM_VECTOR_LENGTH (v) && SCM_INUM (k) >= 0);
  SCM_VECTOR_SET (v, (long) SCM_INUM(k), obj);
  return SCM_UNSPECIFIED;
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
  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;

  if (SCM_INUMP (k))
    {
      SCM_ASSERT_RANGE (1, k, SCM_INUM (k) >= 0);
      return scm_c_make_vector (SCM_INUM (k), fill);
    }
  else if (SCM_BIGP (k))
    SCM_OUT_OF_RANGE (1, k);
  else
    SCM_WRONG_TYPE_ARG (1, k);
}
#undef FUNC_NAME


SCM
scm_c_make_vector (unsigned long int k, SCM fill)
#define FUNC_NAME s_scm_make_vector
{
  SCM v;
  scm_t_bits *base;

  if (k > 0) 
    {
      unsigned long int j;

      SCM_ASSERT_RANGE (1, scm_ulong2num (k), k <= SCM_VECTOR_MAX_LENGTH);

      base = scm_gc_malloc (k * sizeof (scm_t_bits), "vector");
      for (j = 0; j != k; ++j)
	base[j] = SCM_UNPACK (fill);
    }
  else
    base = NULL;

  v = scm_cell (SCM_MAKE_VECTOR_TAG (k, scm_tc7_vector), (scm_t_bits) base);
  scm_remember_upto_here_1 (fill);

  return v;
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
  long i;
  SCM const *data;
  SCM_VALIDATE_VECTOR (1, v);
  data = SCM_VELTS(v);
  for(i = SCM_VECTOR_LENGTH(v)-1;i >= 0;i--) res = scm_cons(data[i], res);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_fill_x, "vector-fill!", 2, 0, 0,
            (SCM v, SCM fill),
	    "Store @var{fill} in every position of @var{vector}.  The value\n"
	    "returned by @code{vector-fill!} is unspecified.")
#define FUNC_NAME s_scm_vector_fill_x
{
  register long i;
  SCM_VALIDATE_VECTOR (1, v);

  for(i = SCM_VECTOR_LENGTH (v) - 1; i >= 0; i--)
    SCM_VECTOR_SET(v, i, fill);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_vector_equal_p(SCM x, SCM y)
{
  long i;
  for(i = SCM_VECTOR_LENGTH (x) - 1; i >= 0; i--)
    if (scm_is_false (scm_equal_p (SCM_VELTS (x)[i], SCM_VELTS (y)[i])))
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
  long i;
  long j;
  long e;
  
  SCM_VALIDATE_VECTOR (1, vec1);
  SCM_VALIDATE_INUM_COPY (2, start1, i);
  SCM_VALIDATE_INUM_COPY (3, end1, e);
  SCM_VALIDATE_VECTOR (4, vec2);
  SCM_VALIDATE_INUM_COPY (5, start2, j);
  SCM_ASSERT_RANGE (2, start1, i <= SCM_VECTOR_LENGTH (vec1) && i >= 0);
  SCM_ASSERT_RANGE (5, start2, j <= SCM_VECTOR_LENGTH (vec2) && j >= 0);
  SCM_ASSERT_RANGE (3, end1, e <= SCM_VECTOR_LENGTH (vec1) && e >= 0);
  SCM_ASSERT_RANGE (5, start2, e-i+j <= SCM_VECTOR_LENGTH (vec2));

  while (i<e)
    {
      SCM_VECTOR_SET (vec2, j, SCM_VELTS (vec1)[i]);
      i++;
      j++;
    }
  
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
  long i;
  long j;
  long e;

  SCM_VALIDATE_VECTOR (1, vec1);
  SCM_VALIDATE_INUM_COPY (2, start1, i);
  SCM_VALIDATE_INUM_COPY (3, end1, e);
  SCM_VALIDATE_VECTOR (4, vec2);
  SCM_VALIDATE_INUM_COPY (5, start2, j);
  SCM_ASSERT_RANGE (2, start1, i <= SCM_VECTOR_LENGTH (vec1) && i >= 0);
  SCM_ASSERT_RANGE (5, start2, j <= SCM_VECTOR_LENGTH (vec2) && j >= 0);
  SCM_ASSERT_RANGE (3, end1, e <= SCM_VECTOR_LENGTH (vec1) && e >= 0);
  j = e - i + j;
  SCM_ASSERT_RANGE (5, start2, j <= SCM_VECTOR_LENGTH (vec2));
  while (i < e)
    {
      j--;
      e--;
      SCM_VECTOR_SET (vec2, j, SCM_VELTS (vec1)[e]);
    }
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_vectors ()
{
  scm_nullvect = scm_c_make_vector (0, SCM_UNDEFINED);

#include "libguile/vectors.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
