/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "eval.h"
#include "genio.h"
#include "smob.h"
#include "strop.h"
#include "feature.h"

#include "unif.h"
#include "ramap.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* The set of uniform scm_vector types is:
 *  Vector of:		 Called:
 * unsigned char	string
 * char			byvect
 * boolean		bvect
 * signed long		ivect
 * unsigned long	uvect
 * float		fvect
 * double		dvect
 * complex double	cvect
 * short		svect
 * long_long		llvect
 */

long scm_tc16_array;

/* 
 * This complicates things too much if allowed on any array.
 * C code can safely call it on arrays known to be used in a single
 * threaded manner.
 *
 * SCM_PROC(s_vector_set_length_x, "vector-set-length!", 2, 0, 0, scm_vector_set_length_x); 
 */
static char s_vector_set_length_x[] = "vector-set-length!";


SCM 
scm_vector_set_length_x (vect, len)
     SCM vect;
     SCM len;
{
  long l;
  scm_sizet siz;
  scm_sizet sz;

  l = SCM_INUM (len);
  SCM_ASRTGO (SCM_NIMP (vect), badarg1);
  switch (SCM_TYP7 (vect))
    {
    default:
    badarg1: scm_wta (vect, (char *) SCM_ARG1, s_vector_set_length_x);
    case scm_tc7_string:
      SCM_ASRTGO (vect != scm_nullstr, badarg1);
      sz = sizeof (char);
      l++;
      break;
    case scm_tc7_vector:
    case scm_tc7_wvect:
      SCM_ASRTGO (vect != scm_nullvect, badarg1);
      sz = sizeof (SCM);
      break;
#ifdef ARRAYS
    case scm_tc7_bvect:
      l = (l + SCM_LONG_BIT - 1) / SCM_LONG_BIT;
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      sz = sizeof (long);
      break;
    case scm_tc7_byvect:
      sz = sizeof (char);
      break;

    case scm_tc7_svect:
      sz = sizeof (short);
      break;
#ifdef LONGLONGS
    case scm_tc7_llvect:
      sz = sizeof (long_long);
      break;
#endif 

#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      sz = sizeof (float);
      break;
#endif
    case scm_tc7_dvect:
      sz = sizeof (double);
      break;
    case scm_tc7_cvect:
      sz = 2 * sizeof (double);
      break;
#endif
#endif
    }
  SCM_ASSERT (SCM_INUMP (len), len, SCM_ARG2, s_vector_set_length_x);
  if (!l)
    l = 1L;
  siz = l * sz;
  if (siz != l * sz)
    scm_wta (SCM_MAKINUM (l * sz), (char *) SCM_NALLOC, s_vector_set_length_x);
  SCM_REDEFER_INTS;
  SCM_SETCHARS (vect,
	    ((char *)
	     scm_must_realloc (SCM_CHARS (vect),
			       (long) SCM_LENGTH (vect) * sz,
			       (long) siz,
			       s_vector_set_length_x)));
  if (SCM_VECTORP (vect))
    {
      sz = SCM_LENGTH (vect);
      while (l > sz)
	SCM_VELTS (vect)[--l] = SCM_UNSPECIFIED;
    }
  else if (SCM_STRINGP (vect))
    SCM_CHARS (vect)[l - 1] = 0;
  SCM_SETLENGTH (vect, SCM_INUM (len), SCM_TYP7 (vect));
  SCM_REALLOW_INTS;
  return vect;
}


#ifdef ARRAYS

#ifdef SCM_FLOATS
#ifdef SCM_SINGLES


SCM 
scm_makflo (float x)
{
  SCM z;
  if (x == 0.0)
    return scm_flo0;
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  SCM_SETCAR (z, scm_tc_flo);
  SCM_FLO (z) = x;
  SCM_ALLOW_INTS;
  return z;
}
#endif
#endif


SCM 
scm_make_uve (k, prot)
     long k;
     SCM prot;
{
  SCM v;
  long i, type;
  if (SCM_BOOL_T == prot)
    {
      i = sizeof (long) * ((k + SCM_LONG_BIT - 1) / SCM_LONG_BIT);
      type = scm_tc7_bvect;
    }
  else if (SCM_ICHRP (prot) && (prot == SCM_MAKICHR ('\0')))
    {
      i = sizeof (char) * k;
      type = scm_tc7_byvect;
    }    
  else if (SCM_ICHRP (prot))
    {
      i = sizeof (char) * k;
      type = scm_tc7_string;
    }
  else if (SCM_INUMP (prot))
    {
      i = sizeof (long) * k;
      if (SCM_INUM (prot) > 0)
	type = scm_tc7_uvect;
      else
	type = scm_tc7_ivect;
    }
  else if (SCM_NIMP (prot) && SCM_SYMBOLP (prot) && (1 == SCM_LENGTH (prot)))
    {
      char s;

      s = SCM_CHARS (prot)[0];
      if (s == 's')
	{
	  i = sizeof (short) * k;
	  type = scm_tc7_svect;
	}
#ifdef LONGLONGS
      else if (s == 'l')
	{
	  i = sizeof (long_long) * k;
	  type = scm_tc7_llvect;
	}
#endif
      else
	{
	  return scm_make_vector (SCM_MAKINUM (k), SCM_UNDEFINED);
	}
    }
  else
#ifdef SCM_FLOATS
  if (SCM_IMP (prot) || !SCM_INEXP (prot))
#endif
    /* Huge non-unif vectors are NOT supported. */
    return scm_make_vector (SCM_MAKINUM (k), SCM_UNDEFINED);	/* no special scm_vector */
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
  else if (SCM_SINGP (prot))

    {
      i = sizeof (float) * k;
      type = scm_tc7_fvect;
    }
#endif
  else if (SCM_CPLXP (prot))
    {
      i = 2 * sizeof (double) * k;
      type = scm_tc7_cvect;
    }
  else
    {
      i = sizeof (double) * k;
      type = scm_tc7_dvect;
    }
#endif

  SCM_NEWCELL (v);
  SCM_DEFER_INTS;
  {
    char *m;
    m = scm_must_malloc ((i ? i : 1L), "vector");
    SCM_SETCHARS (v, (char *) m);
  }
  SCM_SETLENGTH (v, (k < SCM_LENGTH_MAX ? k : SCM_LENGTH_MAX), type);
  SCM_ALLOW_INTS;
  return v;
}

SCM_PROC(s_uniform_vector_length, "uniform-vector-length", 1, 0, 0, scm_uniform_vector_length);

SCM 
scm_uniform_vector_length (v)
     SCM v;
{
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  switch SCM_TYP7
    (v)
    {
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_uniform_vector_length);
    case scm_tc7_bvect:
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_svect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
      return SCM_MAKINUM (SCM_LENGTH (v));
    }
}

SCM_PROC(s_array_p, "array?", 1, 1, 0, scm_array_p);

SCM 
scm_array_p (v, prot)
     SCM v;
     SCM prot;
{
  int nprot;
  int enclosed;
  nprot = SCM_UNBNDP (prot);
  enclosed = 0;
  if (SCM_IMP (v))
    return SCM_BOOL_F;
loop:
  switch (SCM_TYP7 (v))
    {
    case scm_tc7_smob:
      if (!SCM_ARRAYP (v))
	return SCM_BOOL_F;
      if (nprot)
	return SCM_BOOL_T;
      if (enclosed++)
	return SCM_BOOL_F;
      v = SCM_ARRAY_V (v);
      goto loop;
    case scm_tc7_bvect:
      return nprot || SCM_BOOL_T==prot ? SCM_BOOL_T : SCM_BOOL_F;
    case scm_tc7_string:
      return nprot || (SCM_ICHRP(prot) && (prot != SCM_MAKICHR('\0'))) ? SCM_BOOL_T : SCM_BOOL_F;
    case scm_tc7_byvect:
      return nprot || (prot == SCM_MAKICHR('\0')) ? SCM_BOOL_T : SCM_BOOL_F;
    case scm_tc7_uvect:
      return nprot || (SCM_INUMP(prot) && SCM_INUM(prot)>0) ? SCM_BOOL_T : SCM_BOOL_F;
    case scm_tc7_ivect:
      return nprot || (SCM_INUMP(prot) && SCM_INUM(prot)<=0) ? SCM_BOOL_T : SCM_BOOL_F;
    case scm_tc7_svect:
      return (   nprot
	      || (SCM_NIMP (prot)
		  && SCM_SYMBOLP (prot)
		  && (1 == SCM_LENGTH (prot))
		  && ('s' == SCM_CHARS (prot)[0])));
#ifdef LONGLONGS
    case scm_tc7_llvect:
      return (   nprot
	      || (SCM_NIMP (prot)
		  && SCM_SYMBOLP (prot)
		  && (1 == SCM_LENGTH (prot))
		  && ('s' == SCM_CHARS (prot)[0])));
#endif
# ifdef SCM_FLOATS
#  ifdef SCM_SINGLES
    case scm_tc7_fvect:
      return nprot || (SCM_NIMP(prot) && SCM_SINGP(prot)) ? SCM_BOOL_T : SCM_BOOL_F;
#  endif
    case scm_tc7_dvect:
      return nprot || (SCM_NIMP(prot) && SCM_REALP(prot)) ? SCM_BOOL_T : SCM_BOOL_F;
    case scm_tc7_cvect:
      return nprot || (SCM_NIMP(prot) && SCM_CPLXP(prot)) ? SCM_BOOL_T : SCM_BOOL_F;
# endif
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return nprot || SCM_NULLP(prot) ? SCM_BOOL_T : SCM_BOOL_F;
    default:;
    }
  return SCM_BOOL_F;
}


SCM_PROC(s_array_rank, "array-rank", 1, 0, 0, scm_array_rank);

SCM 
scm_array_rank (ra)
     SCM ra;
{
  if (SCM_IMP (ra))
 return SCM_INUM0;
  switch (SCM_TYP7 (ra))
    {
    default:
      return SCM_INUM0;
    case scm_tc7_string:
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_cvect:
    case scm_tc7_dvect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
    case scm_tc7_svect:
      return SCM_MAKINUM (1L);
    case scm_tc7_smob:
      if (SCM_ARRAYP (ra))
	return SCM_MAKINUM (SCM_ARRAY_NDIM (ra));
      return SCM_INUM0;
    }
}


SCM_PROC(s_array_dimensions, "array-dimensions", 1, 0, 0, scm_array_dimensions);

SCM 
scm_array_dimensions (ra)
     SCM ra;
{
  SCM res = SCM_EOL;
  scm_sizet k;
  scm_array_dim *s;
  if (SCM_IMP (ra))
 return SCM_BOOL_F;
  switch (SCM_TYP7 (ra))
    {
    default:
      return SCM_BOOL_F;
    case scm_tc7_string:
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_bvect:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_cvect:
    case scm_tc7_dvect:
    case scm_tc7_svect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
      return scm_cons (SCM_MAKINUM (SCM_LENGTH (ra)), SCM_EOL);
    case scm_tc7_smob:
      if (!SCM_ARRAYP (ra))
	return SCM_BOOL_F;
      k = SCM_ARRAY_NDIM (ra);
      s = SCM_ARRAY_DIMS (ra);
      while (k--)
	res = scm_cons (s[k].lbnd ? scm_cons2 (SCM_MAKINUM (s[k].lbnd), SCM_MAKINUM (s[k].ubnd), SCM_EOL) :
			SCM_MAKINUM (1 + (s[k].ubnd))
			, res);
      return res;
    }
}


static char s_bad_ind[] = "Bad scm_array index";


long 
scm_aind (ra, args, what)
     SCM ra;
     SCM args;
     const char *what;
{
  SCM ind;
  register long j;
  register scm_sizet pos = SCM_ARRAY_BASE (ra);
  register scm_sizet k = SCM_ARRAY_NDIM (ra);
  scm_array_dim *s = SCM_ARRAY_DIMS (ra);
  if (SCM_INUMP (args))
    {
      SCM_ASSERT (1 == k, scm_makfrom0str (what), SCM_WNA, NULL);
      return pos + (SCM_INUM (args) - s->lbnd) * (s->inc);
    }
  while (k && SCM_NIMP (args))
    {
      ind = SCM_CAR (args);
      args = SCM_CDR (args);
      SCM_ASSERT (SCM_INUMP (ind), ind, s_bad_ind, what);
      j = SCM_INUM (ind);
      SCM_ASSERT (j >= (s->lbnd) && j <= (s->ubnd), ind, SCM_OUTOFRANGE, what);
      pos += (j - s->lbnd) * (s->inc);
      k--;
      s++;
    }
  SCM_ASSERT (0 == k && SCM_NULLP (args), scm_makfrom0str (what), SCM_WNA,
	      NULL);
  return pos;
}



SCM 
scm_make_ra (ndim)
     int ndim;
{
  SCM ra;
  SCM_NEWCELL (ra);
  SCM_DEFER_INTS;
  SCM_SETCDR (ra, scm_must_malloc ((long) (sizeof (scm_array) + ndim * sizeof (scm_array_dim)),
			       "array"));
  SCM_SETCAR (ra, ((long) ndim << 17) + scm_tc16_array);
  SCM_ARRAY_V (ra) = scm_nullvect;
  SCM_ALLOW_INTS;
  return ra;
}

static char s_bad_spec[] = "Bad scm_array dimension";
/* Increments will still need to be set. */


SCM 
scm_shap2ra (args, what)
     SCM args;
     const char *what;
{
  scm_array_dim *s;
  SCM ra, spec, sp;
  int ndim = scm_ilength (args);
  SCM_ASSERT (0 <= ndim, args, s_bad_spec, what);
  ra = scm_make_ra (ndim);
  SCM_ARRAY_BASE (ra) = 0;
  s = SCM_ARRAY_DIMS (ra);
  for (; SCM_NIMP (args); s++, args = SCM_CDR (args))
    {
      spec = SCM_CAR (args);
      if (SCM_IMP (spec))

	{
	  SCM_ASSERT (SCM_INUMP (spec) && SCM_INUM (spec) >= 0, spec,
		      s_bad_spec, what);
	  s->lbnd = 0;
	  s->ubnd = SCM_INUM (spec) - 1;
	  s->inc = 1;
	}
      else
	{
	  SCM_ASSERT (SCM_CONSP (spec) && SCM_INUMP (SCM_CAR (spec)), spec,
		      s_bad_spec, what);
	  s->lbnd = SCM_INUM (SCM_CAR (spec));
	  sp = SCM_CDR (spec);
	  SCM_ASSERT (SCM_NIMP (sp) && SCM_CONSP (sp)
		      && SCM_INUMP (SCM_CAR (sp)) && SCM_NULLP (SCM_CDR (sp)),
		      spec, s_bad_spec, what);
	  s->ubnd = SCM_INUM (SCM_CAR (sp));
	  s->inc = 1;
	}
    }
  return ra;
}

SCM_PROC(s_dimensions_to_uniform_array, "dimensions->uniform-array", 2, 0, 1, scm_dimensions_to_uniform_array);

SCM 
scm_dimensions_to_uniform_array (dims, prot, fill)
     SCM dims;
     SCM prot;
     SCM fill;
{
  scm_sizet k, vlen = 1;
  long rlen = 1;
  scm_array_dim *s;
  SCM ra;
  if (SCM_INUMP (dims))
    {
      if (SCM_INUM (dims) < SCM_LENGTH_MAX)
	{
	  SCM answer;
	  answer = scm_make_uve (SCM_INUM (dims), prot);
	  if (SCM_NNULLP (fill))
	    {
	      SCM_ASSERT (1 == scm_ilength (fill),
			  scm_makfrom0str (s_dimensions_to_uniform_array),
			  SCM_WNA, NULL);
	      scm_array_fill_x (answer, SCM_CAR (fill));
	    }
	  else if (SCM_NIMP (prot) && SCM_SYMBOLP (prot))
	    scm_array_fill_x (answer, SCM_MAKINUM (0));
	  else
	    scm_array_fill_x (answer, prot);
	  return answer;
	}
    else
      dims = scm_cons (dims, SCM_EOL);
    }
  SCM_ASSERT (SCM_NULLP (dims) || (SCM_NIMP (dims) && SCM_CONSP (dims)),
	  dims, SCM_ARG1, s_dimensions_to_uniform_array);
  ra = scm_shap2ra (dims, s_dimensions_to_uniform_array);
  SCM_SETOR_CAR (ra, SCM_ARRAY_CONTIGUOUS);
  s = SCM_ARRAY_DIMS (ra);
  k = SCM_ARRAY_NDIM (ra);
  while (k--)
    {
      s[k].inc = (rlen > 0 ? rlen : 0);
      rlen = (s[k].ubnd - s[k].lbnd + 1) * s[k].inc;
      vlen *= (s[k].ubnd - s[k].lbnd + 1);
    }
  if (rlen < SCM_LENGTH_MAX)
    SCM_ARRAY_V (ra) = scm_make_uve ((rlen > 0 ? rlen : 0L), prot);
  else
    {
      scm_sizet bit;
      switch (SCM_TYP7 (scm_make_uve (0L, prot)))
	{
	default:
	  bit = SCM_LONG_BIT;
	  break;
	case scm_tc7_bvect:
	  bit = 1;
	  break;
	case scm_tc7_string:
	  bit = SCM_CHAR_BIT;
	  break;
	case scm_tc7_fvect:
	  bit = sizeof (float) * SCM_CHAR_BIT / sizeof (char);
	  break;
	case scm_tc7_dvect:
	  bit = sizeof (double) * SCM_CHAR_BIT / sizeof (char);
	  break;
	case scm_tc7_cvect:
	  bit = 2 * sizeof (double) * SCM_CHAR_BIT / sizeof (char);
	  break;
	}
      SCM_ARRAY_BASE (ra) = (SCM_LONG_BIT + bit - 1) / bit;
      rlen += SCM_ARRAY_BASE (ra);
      SCM_ARRAY_V (ra) = scm_make_uve (rlen, prot);
      *((long *) SCM_VELTS (SCM_ARRAY_V (ra))) = rlen;
    }
  if (SCM_NNULLP (fill))
    {
      SCM_ASSERT (1 == scm_ilength (fill),
		  scm_makfrom0str (s_dimensions_to_uniform_array), SCM_WNA,
		  NULL);
      scm_array_fill_x (ra, SCM_CAR (fill));
    }
  else if (SCM_NIMP (prot) && SCM_SYMBOLP (prot))
    scm_array_fill_x (ra, SCM_MAKINUM (0));
  else
    scm_array_fill_x (ra, prot);
  if (1 == SCM_ARRAY_NDIM (ra) && 0 == SCM_ARRAY_BASE (ra))
    if (s->ubnd < s->lbnd || (0 == s->lbnd && 1 == s->inc))
      return SCM_ARRAY_V (ra);
  return ra;
}


void 
scm_ra_set_contp (ra)
     SCM ra;
{
  scm_sizet k = SCM_ARRAY_NDIM (ra);
  if (k)
    {
      long inc = SCM_ARRAY_DIMS (ra)[k - 1].inc;
      while (k--)
	{
	  if (inc != SCM_ARRAY_DIMS (ra)[k].inc)
	    {
	      SCM_SETAND_CAR (ra, ~SCM_ARRAY_CONTIGUOUS);
	      return;
	    }
	  inc *= (SCM_ARRAY_DIMS (ra)[k].ubnd 
		  - SCM_ARRAY_DIMS (ra)[k].lbnd + 1);
	}
    }
  SCM_SETOR_CAR (ra, SCM_ARRAY_CONTIGUOUS);
}


SCM_PROC(s_make_shared_array, "make-shared-array", 2, 0, 1, scm_make_shared_array);

SCM 
scm_make_shared_array (oldra, mapfunc, dims)
     SCM oldra;
     SCM mapfunc;
     SCM dims;
{
  SCM ra;
  SCM inds, indptr;
  SCM imap;
  scm_sizet i, k;
  long old_min, new_min, old_max, new_max;
  scm_array_dim *s;
  SCM_ASSERT (SCM_BOOL_T == scm_procedure_p (mapfunc), mapfunc, SCM_ARG2, s_make_shared_array);
  SCM_ASSERT (SCM_NIMP (oldra) && (SCM_BOOL_F != scm_array_p (oldra, SCM_UNDEFINED)), oldra, SCM_ARG1, s_make_shared_array);
  ra = scm_shap2ra (dims, s_make_shared_array);
  if (SCM_ARRAYP (oldra))
    {
      SCM_ARRAY_V (ra) = SCM_ARRAY_V (oldra);
      old_min = old_max = SCM_ARRAY_BASE (oldra);
      s = SCM_ARRAY_DIMS (oldra);
      k = SCM_ARRAY_NDIM (oldra);
      while (k--)
	{
	  if (s[k].inc > 0)
	    old_max += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	  else
	    old_min += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	}
    }
  else
    {
      SCM_ARRAY_V (ra) = oldra;
      old_min = 0;
      old_max = (long) SCM_LENGTH (oldra) - 1;
    }
  inds = SCM_EOL;
  s = SCM_ARRAY_DIMS (ra);
  for (k = 0; k < SCM_ARRAY_NDIM (ra); k++)
    {
      inds = scm_cons (SCM_MAKINUM (s[k].lbnd), inds);
      if (s[k].ubnd < s[k].lbnd)
	{
	  if (1 == SCM_ARRAY_NDIM (ra))
	    ra = scm_make_uve (0L, scm_array_prototype (ra));
	  else
	    SCM_ARRAY_V (ra) = scm_make_uve (0L, scm_array_prototype (ra));
	  return ra;
	}
    }
  imap = scm_apply (mapfunc, scm_reverse (inds), SCM_EOL);
  if (SCM_ARRAYP (oldra))
      i = (scm_sizet) scm_aind (oldra, imap, s_make_shared_array);
  else
    {
      if (SCM_NINUMP (imap))

	{
	  SCM_ASSERT (1 == scm_ilength (imap) && SCM_INUMP (SCM_CAR (imap)),
		  imap, s_bad_ind, s_make_shared_array);
	  imap = SCM_CAR (imap);
	}
      i = SCM_INUM (imap);
    }
  SCM_ARRAY_BASE (ra) = new_min = new_max = i;
  indptr = inds;
  k = SCM_ARRAY_NDIM (ra);
  while (k--)
    {
      if (s[k].ubnd > s[k].lbnd)
	{
	  SCM_SETCAR (indptr, SCM_MAKINUM (SCM_INUM (SCM_CAR (indptr)) + 1));
	  imap = scm_apply (mapfunc, scm_reverse (inds), SCM_EOL);
	  if (SCM_ARRAYP (oldra))

	      s[k].inc = scm_aind (oldra, imap, s_make_shared_array) - i;
	  else
	    {
	      if (SCM_NINUMP (imap))

		{
		  SCM_ASSERT (1 == scm_ilength (imap) && SCM_INUMP (SCM_CAR (imap)),
			  imap, s_bad_ind, s_make_shared_array);
		  imap = SCM_CAR (imap);
		}
	      s[k].inc = (long) SCM_INUM (imap) - i;
	    }
	  i += s[k].inc;
	  if (s[k].inc > 0)
	    new_max += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	  else
	    new_min += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	}
      else
	s[k].inc = new_max - new_min + 1;	/* contiguous by default */
      indptr = SCM_CDR (indptr);
    }
  SCM_ASSERT (old_min <= new_min && old_max >= new_max, SCM_UNDEFINED,
	  "mapping out of range", s_make_shared_array);
  if (1 == SCM_ARRAY_NDIM (ra) && 0 == SCM_ARRAY_BASE (ra))
    {
      if (1 == s->inc && 0 == s->lbnd
	  && SCM_LENGTH (SCM_ARRAY_V (ra)) == 1 + s->ubnd)
	return SCM_ARRAY_V (ra);
      if (s->ubnd < s->lbnd)
	return scm_make_uve (0L, scm_array_prototype (ra));
    }
  scm_ra_set_contp (ra);
  return ra;
}


/* args are RA . DIMS */
SCM_PROC(s_transpose_array, "transpose-array", 0, 0, 1, scm_transpose_array);

SCM 
scm_transpose_array (args)
     SCM args;
{
  SCM ra, res, vargs, *ve = &vargs;
  scm_array_dim *s, *r;
  int ndim, i, k;
  SCM_ASSERT (SCM_NNULLP (args), scm_makfrom0str (s_transpose_array),
	      SCM_WNA, NULL);
  ra = SCM_CAR (args);
  SCM_ASSERT (SCM_NIMP (ra), ra, SCM_ARG1, s_transpose_array);
  args = SCM_CDR (args);
  switch (SCM_TYP7 (ra))
    {
    default:
    badarg:scm_wta (ra, (char *) SCM_ARG1, s_transpose_array);
    case scm_tc7_bvect:
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_svect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
      SCM_ASSERT (SCM_NIMP (args) && SCM_NULLP (SCM_CDR (args)),
		  scm_makfrom0str (s_transpose_array), SCM_WNA, NULL);
      SCM_ASSERT (SCM_INUMP (SCM_CAR (args)), SCM_CAR (args), SCM_ARG2,
		  s_transpose_array);
      SCM_ASSERT (SCM_INUM0 == SCM_CAR (args), SCM_CAR (args), SCM_OUTOFRANGE,
		  s_transpose_array);
      return ra;
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (ra), badarg);
      vargs = scm_vector (args);
      SCM_ASSERT (SCM_LENGTH (vargs) == SCM_ARRAY_NDIM (ra),
		  scm_makfrom0str (s_transpose_array), SCM_WNA, NULL);
		  ve = SCM_VELTS (vargs);
      ndim = 0;
      for (k = 0; k < SCM_ARRAY_NDIM (ra); k++)
	{
	  SCM_ASSERT (SCM_INUMP (ve[k]), ve[k], (SCM_ARG2 + k),
		      s_transpose_array);
	  i = SCM_INUM (ve[k]);
	  SCM_ASSERT (i >= 0 && i < SCM_ARRAY_NDIM (ra), ve[k],
		      SCM_OUTOFRANGE, s_transpose_array);
	  if (ndim < i)
	    ndim = i;
	}
      ndim++;
      res = scm_make_ra (ndim);
      SCM_ARRAY_V (res) = SCM_ARRAY_V (ra);
      SCM_ARRAY_BASE (res) = SCM_ARRAY_BASE (ra);
      for (k = ndim; k--;)
	{
	  SCM_ARRAY_DIMS (res)[k].lbnd = 0;
	  SCM_ARRAY_DIMS (res)[k].ubnd = -1;
	}
      for (k = SCM_ARRAY_NDIM (ra); k--;)
	{
	  i = SCM_INUM (ve[k]);
	  s = &(SCM_ARRAY_DIMS (ra)[k]);
	  r = &(SCM_ARRAY_DIMS (res)[i]);
	  if (r->ubnd < r->lbnd)
	    {
	      r->lbnd = s->lbnd;
	      r->ubnd = s->ubnd;
	      r->inc = s->inc;
	      ndim--;
	    }
	  else
	    {
	      if (r->ubnd > s->ubnd)
		r->ubnd = s->ubnd;
	      if (r->lbnd < s->lbnd)
		{
		  SCM_ARRAY_BASE (res) += (s->lbnd - r->lbnd) * r->inc;
		  r->lbnd = s->lbnd;
		}
	      r->inc += s->inc;
	    }
	}
      SCM_ASSERT (ndim <= 0, args, "bad argument list", s_transpose_array);
      scm_ra_set_contp (res);
      return res;
    }
}

/* args are RA . AXES */
SCM_PROC(s_enclose_array, "enclose-array", 0, 0, 1, scm_enclose_array);

SCM 
scm_enclose_array (axes)
     SCM axes;
{
  SCM axv, ra, res, ra_inr;
  scm_array_dim vdim, *s = &vdim;
  int ndim, j, k, ninr, noutr;
  SCM_ASSERT (SCM_NIMP (axes), scm_makfrom0str (s_enclose_array), SCM_WNA,
	      NULL);
  ra = SCM_CAR (axes);
  axes = SCM_CDR (axes);
  if (SCM_NULLP (axes))

      axes = scm_cons ((SCM_ARRAYP (ra) ? SCM_MAKINUM (SCM_ARRAY_NDIM (ra) - 1) : SCM_INUM0), SCM_EOL);
  ninr = scm_ilength (axes);
  ra_inr = scm_make_ra (ninr);
  SCM_ASRTGO (SCM_NIMP (ra), badarg1);
  switch SCM_TYP7
    (ra)
    {
    default:
    badarg1:scm_wta (ra, (char *) SCM_ARG1, s_enclose_array);
    case scm_tc7_string:
    case scm_tc7_bvect:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_svect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
      s->lbnd = 0;
      s->ubnd = SCM_LENGTH (ra) - 1;
      s->inc = 1;
      SCM_ARRAY_V (ra_inr) = ra;
      SCM_ARRAY_BASE (ra_inr) = 0;
      ndim = 1;
      break;
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (ra), badarg1);
      s = SCM_ARRAY_DIMS (ra);
      SCM_ARRAY_V (ra_inr) = SCM_ARRAY_V (ra);
      SCM_ARRAY_BASE (ra_inr) = SCM_ARRAY_BASE (ra);
      ndim = SCM_ARRAY_NDIM (ra);
      break;
    }
  noutr = ndim - ninr;
  axv = scm_make_string (SCM_MAKINUM (ndim), SCM_MAKICHR (0));
  SCM_ASSERT (0 <= noutr && 0 <= ninr, scm_makfrom0str (s_enclose_array),
	      SCM_WNA, NULL);
  res = scm_make_ra (noutr);
  SCM_ARRAY_BASE (res) = SCM_ARRAY_BASE (ra_inr);
  SCM_ARRAY_V (res) = ra_inr;
  for (k = 0; k < ninr; k++, axes = SCM_CDR (axes))
    {
      SCM_ASSERT (SCM_INUMP (SCM_CAR (axes)), SCM_CAR (axes), "bad axis", s_enclose_array);
      j = SCM_INUM (SCM_CAR (axes));
      SCM_ARRAY_DIMS (ra_inr)[k].lbnd = s[j].lbnd;
      SCM_ARRAY_DIMS (ra_inr)[k].ubnd = s[j].ubnd;
      SCM_ARRAY_DIMS (ra_inr)[k].inc = s[j].inc;
      SCM_CHARS (axv)[j] = 1;
    }
  for (j = 0, k = 0; k < noutr; k++, j++)
    {
      while (SCM_CHARS (axv)[j])
	j++;
      SCM_ARRAY_DIMS (res)[k].lbnd = s[j].lbnd;
      SCM_ARRAY_DIMS (res)[k].ubnd = s[j].ubnd;
      SCM_ARRAY_DIMS (res)[k].inc = s[j].inc;
    }
  scm_ra_set_contp (ra_inr);
  scm_ra_set_contp (res);
  return res;
}



SCM_PROC(s_array_in_bounds_p, "array-in-bounds?", 0, 0, 1, scm_array_in_bounds_p);

SCM 
scm_array_in_bounds_p (args)
     SCM args;
{
  SCM v, ind = SCM_EOL;
  long pos = 0;
  register scm_sizet k;
  register long j;
  scm_array_dim *s;
  SCM_ASSERT (SCM_NIMP (args), scm_makfrom0str (s_array_in_bounds_p),
	      SCM_WNA, NULL);
  v = SCM_CAR (args);
  args = SCM_CDR (args);
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  if (SCM_NIMP (args))

    {
      ind = SCM_CAR (args);
      args = SCM_CDR (args);
      SCM_ASSERT (SCM_INUMP (ind), ind, SCM_ARG2, s_array_in_bounds_p);
      pos = SCM_INUM (ind);
    }
tail:
  switch SCM_TYP7
    (v)
    {
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_array_in_bounds_p);
    wna: scm_wrong_num_args (scm_makfrom0str (s_array_in_bounds_p));
    case scm_tc7_smob:
      k = SCM_ARRAY_NDIM (v);
      s = SCM_ARRAY_DIMS (v);
      pos = SCM_ARRAY_BASE (v);
      if (!k)
	{
	  SCM_ASRTGO (SCM_NULLP (ind), wna);
	  ind = SCM_INUM0;
	}
      else
	while (!0)
	  {
	    j = SCM_INUM (ind);
	    if (!(j >= (s->lbnd) && j <= (s->ubnd)))
	      {
		SCM_ASRTGO (--k == scm_ilength (args), wna);
		return SCM_BOOL_F;
	      }
	    pos += (j - s->lbnd) * (s->inc);
	    if (!(--k && SCM_NIMP (args)))
	      break;
	    ind = SCM_CAR (args);
	    args = SCM_CDR (args);
	    s++;
	    SCM_ASSERT (SCM_INUMP (ind), ind, s_bad_ind, s_array_in_bounds_p);
	  }
      SCM_ASRTGO (0 == k, wna);
      v = SCM_ARRAY_V (v);
      goto tail;
    case scm_tc7_bvect:
    case scm_tc7_string:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_svect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
    case scm_tc7_vector:
    case scm_tc7_wvect:
      SCM_ASRTGO (SCM_NULLP (args) && SCM_INUMP (ind), wna);
      return pos >= 0 && pos < SCM_LENGTH (v) ? SCM_BOOL_T : SCM_BOOL_F;
    }
}


SCM_PROC(s_array_ref, "array-ref", 1, 0, 1, scm_uniform_vector_ref);
SCM_PROC(s_uniform_vector_ref, "uniform-vector-ref", 2, 0, 0, scm_uniform_vector_ref);

SCM 
scm_uniform_vector_ref (v, args)
     SCM v;
     SCM args;
{
  long pos;

  if (SCM_IMP (v))
    {
      SCM_ASRTGO (SCM_NULLP (args), badarg);
      return v;
    }
  else if (SCM_ARRAYP (v))
    {
      pos = scm_aind (v, args, s_uniform_vector_ref);
      v = SCM_ARRAY_V (v);
    }
  else
    {
      if (SCM_NIMP (args))

	{
	  SCM_ASSERT (SCM_CONSP (args) && SCM_INUMP (SCM_CAR (args)), args, SCM_ARG2, s_uniform_vector_ref);
	  pos = SCM_INUM (SCM_CAR (args));
	  SCM_ASRTGO (SCM_NULLP (SCM_CDR (args)), wna);
	}
      else
	{
	  SCM_ASSERT (SCM_INUMP (args), args, SCM_ARG2, s_uniform_vector_ref);
	  pos = SCM_INUM (args);
	}
      SCM_ASRTGO (pos >= 0 && pos < SCM_LENGTH (v), outrng);
    }
  switch SCM_TYP7
    (v)
    {
    default:
      if (SCM_NULLP (args))
 return v;
    badarg:
      scm_wta (v, (char *) SCM_ARG1, s_uniform_vector_ref);
      abort ();
    outrng:scm_out_of_range (s_uniform_vector_ref, SCM_MAKINUM (pos));
    wna: scm_wrong_num_args (scm_makfrom0str (s_uniform_vector_ref));
    case scm_tc7_smob:
      {				/* enclosed */
	int k = SCM_ARRAY_NDIM (v);
	SCM res = scm_make_ra (k);
	SCM_ARRAY_V (res) = SCM_ARRAY_V (v);
	SCM_ARRAY_BASE (res) = pos;
	while (k--)
	  {
	    SCM_ARRAY_DIMS (res)[k].lbnd = SCM_ARRAY_DIMS (v)[k].lbnd;
	    SCM_ARRAY_DIMS (res)[k].ubnd = SCM_ARRAY_DIMS (v)[k].ubnd;
	    SCM_ARRAY_DIMS (res)[k].inc = SCM_ARRAY_DIMS (v)[k].inc;
	  }
	return res;
      }
    case scm_tc7_bvect:
      if (SCM_VELTS (v)[pos / SCM_LONG_BIT] & (1L << (pos % SCM_LONG_BIT)))
	return SCM_BOOL_T;
      else
	return SCM_BOOL_F;
    case scm_tc7_string:
      return SCM_MAKICHR (SCM_UCHARS (v)[pos]);
    case scm_tc7_byvect:
      return SCM_MAKINUM (((char *)SCM_CHARS (v))[pos]);
# ifdef SCM_INUMS_ONLY
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      return SCM_MAKINUM (SCM_VELTS (v)[pos]);
# else
  case scm_tc7_uvect:
    return scm_ulong2num(SCM_VELTS(v)[pos]);
  case scm_tc7_ivect:
    return scm_long2num(SCM_VELTS(v)[pos]);
# endif    

    case scm_tc7_svect:
      return SCM_MAKINUM (((short *) SCM_CDR (v))[pos]);
#ifdef LONGLONGS
    case scm_tc7_llvect:
      return scm_long_long2num (((long_long *) SCM_CDR (v))[pos]);
#endif

#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      return scm_makflo (((float *) SCM_CDR (v))[pos]);
#endif
    case scm_tc7_dvect:
      return scm_makdbl (((double *) SCM_CDR (v))[pos], 0.0);
    case scm_tc7_cvect:
      return scm_makdbl (((double *) SCM_CDR (v))[2 * pos],
			 ((double *) SCM_CDR (v))[2 * pos + 1]);
#endif
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return SCM_VELTS (v)[pos];
    }
}

/* Internal version of scm_uniform_vector_ref for uves that does no error checking and
   tries to recycle conses.  (Make *sure* you want them recycled.) */

SCM 
scm_cvref (v, pos, last)
     SCM v;
     scm_sizet pos;
     SCM last;
{
  switch SCM_TYP7
    (v)
    {
    default:
      scm_wta (v, (char *) SCM_ARG1, "PROGRAMMING ERROR: scm_cvref");
    case scm_tc7_bvect:
      if (SCM_VELTS (v)[pos / SCM_LONG_BIT] & (1L << (pos % SCM_LONG_BIT)))
	return SCM_BOOL_T;
      else
	return SCM_BOOL_F;
    case scm_tc7_string:
      return SCM_MAKICHR (SCM_UCHARS (v)[pos]);
    case scm_tc7_byvect:
      return SCM_MAKINUM (((char *)SCM_CHARS (v))[pos]);
# ifdef SCM_INUMS_ONLY
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      return SCM_MAKINUM (SCM_VELTS (v)[pos]);
# else
    case scm_tc7_uvect:
      return scm_ulong2num(SCM_VELTS(v)[pos]);
    case scm_tc7_ivect:
      return scm_long2num(SCM_VELTS(v)[pos]);
# endif    
    case scm_tc7_svect:
      return SCM_MAKINUM (((short *) SCM_CDR (v))[pos]);
#ifdef LONGLONGS
    case scm_tc7_llvect:
      return scm_long_long2num (((long_long *) SCM_CDR (v))[pos]);
#endif
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      if (SCM_NIMP (last) && (last != scm_flo0) && (scm_tc_flo == SCM_CAR (last)))
	{
	  SCM_FLO (last) = ((float *) SCM_CDR (v))[pos];
	  return last;
	}
      return scm_makflo (((float *) SCM_CDR (v))[pos]);
#endif
    case scm_tc7_dvect:
#ifdef SCM_SINGLES
      if (SCM_NIMP (last) && scm_tc_dblr == SCM_CAR (last))
#else
      if (SCM_NIMP (last) && (last != scm_flo0) && (scm_tc_dblr == SCM_CAR (last)))
#endif
	{
	  SCM_REAL (last) = ((double *) SCM_CDR (v))[pos];
	  return last;
	}
      return scm_makdbl (((double *) SCM_CDR (v))[pos], 0.0);
    case scm_tc7_cvect:
      if (SCM_NIMP (last) && scm_tc_dblc == SCM_CAR (last))
	{
	  SCM_REAL (last) = ((double *) SCM_CDR (v))[2 * pos];
	  SCM_IMAG (last) = ((double *) SCM_CDR (v))[2 * pos + 1];
	  return last;
	}
      return scm_makdbl (((double *) SCM_CDR (v))[2 * pos],
			 ((double *) SCM_CDR (v))[2 * pos + 1]);
#endif
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return SCM_VELTS (v)[pos];
    case scm_tc7_smob:
      {				/* enclosed scm_array */
	int k = SCM_ARRAY_NDIM (v);
	SCM res = scm_make_ra (k);
	SCM_ARRAY_V (res) = SCM_ARRAY_V (v);
	SCM_ARRAY_BASE (res) = pos;
	while (k--)
	  {
	    SCM_ARRAY_DIMS (res)[k].ubnd = SCM_ARRAY_DIMS (v)[k].ubnd;
	    SCM_ARRAY_DIMS (res)[k].lbnd = SCM_ARRAY_DIMS (v)[k].lbnd;
	    SCM_ARRAY_DIMS (res)[k].inc = SCM_ARRAY_DIMS (v)[k].inc;
	  }
	return res;
      }
    }
}

SCM_PROC(s_uniform_array_set1_x, "uniform-array-set1!", 3, 0, 0, scm_array_set_x);
SCM_PROC(s_array_set_x, "array-set!", 2, 0, 1, scm_array_set_x);

/* Note that args may be a list or an immediate object, depending which
   PROC is used (and it's called from C too).  */
SCM 
scm_array_set_x (v, obj, args)
     SCM v;
     SCM obj;
     SCM args;
{
  long pos = 0;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  if (SCM_ARRAYP (v))
    {
      pos = scm_aind (v, args, s_array_set_x);
      v = SCM_ARRAY_V (v);
    }
  else
    {
      if (SCM_NIMP (args))
	{
	  SCM_ASSERT (SCM_CONSP(args) && SCM_INUMP (SCM_CAR (args)), args,
		 SCM_ARG3, s_array_set_x);
	  SCM_ASRTGO (SCM_NULLP (SCM_CDR (args)), wna);
	  pos = SCM_INUM (SCM_CAR (args));
	}
      else
	{
	  SCM_ASSERT (SCM_INUMP (args), args, SCM_ARG3, s_array_set_x);
	  pos = SCM_INUM (args);
	}
      SCM_ASRTGO (pos >= 0 && pos < SCM_LENGTH (v), outrng);
    }
  switch (SCM_TYP7 (v))
    {
    default: badarg1:
      scm_wta (v, (char *) SCM_ARG1, s_array_set_x);
      abort ();
    outrng:scm_out_of_range (s_array_set_x, SCM_MAKINUM (pos));
    wna: scm_wrong_num_args (scm_makfrom0str (s_array_set_x));
    case scm_tc7_smob:		/* enclosed */
      goto badarg1;
    case scm_tc7_bvect:
      if (SCM_BOOL_F == obj)
	SCM_VELTS (v)[pos / SCM_LONG_BIT] &= ~(1L << (pos % SCM_LONG_BIT));
      else if (SCM_BOOL_T == obj)
	SCM_VELTS (v)[pos / SCM_LONG_BIT] |= (1L << (pos % SCM_LONG_BIT));
      else
      badobj:scm_wta (obj, (char *) SCM_ARG2, s_array_set_x);
      break;
    case scm_tc7_string:
      SCM_ASRTGO (SCM_ICHRP (obj), badobj);
      SCM_UCHARS (v)[pos] = SCM_ICHR (obj);
      break;
    case scm_tc7_byvect:
      if (SCM_ICHRP (obj))
	obj = SCM_MAKINUM ((char) SCM_ICHR (obj));
      SCM_ASRTGO (SCM_INUMP (obj), badobj);
      ((char *)SCM_CHARS (v))[pos] = SCM_INUM (obj);
      break;
# ifdef SCM_INUMS_ONLY
    case scm_tc7_uvect:
      SCM_ASRTGO (SCM_INUM (obj) >= 0, badobj);
    case scm_tc7_ivect:
    SCM_ASRTGO(SCM_INUMP(obj), badobj); SCM_VELTS(v)[pos] = SCM_INUM(obj); break;
# else
  case scm_tc7_uvect:
    SCM_VELTS(v)[pos] = scm_num2ulong(obj, (char *)SCM_ARG2, s_array_set_x); break;
  case scm_tc7_ivect:
    SCM_VELTS(v)[pos] = scm_num2long(obj, (char *)SCM_ARG2, s_array_set_x); break;
# endif
      break;

    case scm_tc7_svect:
      SCM_ASRTGO (SCM_INUMP (obj), badobj);
      ((short *) SCM_CDR (v))[pos] = SCM_INUM (obj);
      break;
#ifdef LONGLONGS
    case scm_tc7_llvect:
      ((long_long *) SCM_CDR (v))[pos] = scm_num2long_long (obj, (char *)SCM_ARG2, s_array_set_x);
      break;
#endif


#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      ((float *) SCM_CDR (v))[pos] = (float)scm_num2dbl(obj, s_array_set_x); break;
      break;
#endif
    case scm_tc7_dvect:
      ((double *) SCM_CDR (v))[pos] = scm_num2dbl(obj, s_array_set_x); break;
      break;
    case scm_tc7_cvect:
      SCM_ASRTGO (SCM_NIMP (obj) && SCM_INEXP (obj), badobj);
      ((double *) SCM_CDR (v))[2 * pos] = SCM_REALPART (obj);
      ((double *) SCM_CDR (v))[2 * pos + 1] = SCM_CPLXP (obj) ? SCM_IMAG (obj) : 0.0;
      break;
#endif
    case scm_tc7_vector:
    case scm_tc7_wvect:
      SCM_VELTS (v)[pos] = obj;
      break;
    }
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_array_contents, "array-contents", 1, 1, 0, scm_array_contents);

SCM 
scm_array_contents (ra, strict)
     SCM ra;
     SCM strict;
{
  SCM sra;
  if (SCM_IMP (ra))
    return SCM_BOOL_F;
  switch SCM_TYP7
    (ra)
    {
    default:
      return SCM_BOOL_F;
    case scm_tc7_vector:
    case scm_tc7_wvect:
    case scm_tc7_string:
    case scm_tc7_bvect:
    case scm_tc7_byvect:
    case scm_tc7_uvect:
    case scm_tc7_ivect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_svect:
#ifdef LONGLONGS
    case scm_tc7_llvect:
#endif
      return ra;
    case scm_tc7_smob:
      {
	scm_sizet k, ndim = SCM_ARRAY_NDIM (ra), len = 1;
	if (!SCM_ARRAYP (ra) || !SCM_ARRAY_CONTP (ra))
	  return SCM_BOOL_F;
	for (k = 0; k < ndim; k++)
	  len *= SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1;
	if (!SCM_UNBNDP (strict))
	  {
	    if (ndim && (1 != SCM_ARRAY_DIMS (ra)[ndim - 1].inc))
	      return SCM_BOOL_F;
	    if (scm_tc7_bvect == SCM_TYP7 (SCM_ARRAY_V (ra)))
	      {
		if (len != SCM_LENGTH (SCM_ARRAY_V (ra)) ||
		    SCM_ARRAY_BASE (ra) % SCM_LONG_BIT ||
		    len % SCM_LONG_BIT)
		  return SCM_BOOL_F;
	      }
	  }
	if ((len == SCM_LENGTH (SCM_ARRAY_V (ra))) && 0 == SCM_ARRAY_BASE (ra) && SCM_ARRAY_DIMS (ra)->inc)
	  return SCM_ARRAY_V (ra);
	sra = scm_make_ra (1);
	SCM_ARRAY_DIMS (sra)->lbnd = 0;
	SCM_ARRAY_DIMS (sra)->ubnd = len - 1;
	SCM_ARRAY_V (sra) = SCM_ARRAY_V (ra);
	SCM_ARRAY_BASE (sra) = SCM_ARRAY_BASE (ra);
	SCM_ARRAY_DIMS (sra)->inc = (ndim ? SCM_ARRAY_DIMS (ra)[ndim - 1].inc : 1);
	return sra;
      }
    }
}


SCM 
scm_ra2contig (ra, copy)
     SCM ra;
     int copy;
{
  SCM ret;
  long inc = 1;
  scm_sizet k, len = 1;
  for (k = SCM_ARRAY_NDIM (ra); k--;)
    len *= SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1;
  k = SCM_ARRAY_NDIM (ra);
  if (SCM_ARRAY_CONTP (ra) && ((0 == k) || (1 == SCM_ARRAY_DIMS (ra)[k - 1].inc)))
    {
      if (scm_tc7_bvect != SCM_TYP7 (ra))
	return ra;
      if ((len == SCM_LENGTH (SCM_ARRAY_V (ra)) &&
	   0 == SCM_ARRAY_BASE (ra) % SCM_LONG_BIT &&
	   0 == len % SCM_LONG_BIT))
	return ra;
    }
  ret = scm_make_ra (k);
  SCM_ARRAY_BASE (ret) = 0;
  while (k--)
    {
      SCM_ARRAY_DIMS (ret)[k].lbnd = SCM_ARRAY_DIMS (ra)[k].lbnd;
      SCM_ARRAY_DIMS (ret)[k].ubnd = SCM_ARRAY_DIMS (ra)[k].ubnd;
      SCM_ARRAY_DIMS (ret)[k].inc = inc;
      inc *= SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1;
    }
  SCM_ARRAY_V (ret) = scm_make_uve ((inc - 1), scm_array_prototype (ra));
  if (copy)
    scm_array_copy_x (ra, ret);
  return ret;
}



SCM_PROC(s_uniform_array_read_x, "uniform-array-read!", 1, 3, 0, scm_uniform_array_read_x);

SCM 
scm_uniform_array_read_x (ra, port_or_fd, start, end)
     SCM ra;
     SCM port_or_fd;
     SCM start;
     SCM end;
{
  SCM cra = SCM_UNDEFINED, v = ra;
  long sz, vlen, ans;
  long cstart = 0;
  long cend;
  long offset = 0;

  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_cur_inp;
  else
    SCM_ASSERT (SCM_INUMP (port_or_fd)
		|| (SCM_NIMP (port_or_fd) && SCM_OPINFPORTP (port_or_fd)),
		port_or_fd, SCM_ARG2, s_uniform_array_read_x);
  vlen = SCM_LENGTH (v);

loop:
  switch SCM_TYP7 (v)
    {
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_uniform_array_read_x);
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (v), badarg1);
      cra = scm_ra2contig (ra, 0);
      cstart += SCM_ARRAY_BASE (cra);
      vlen = SCM_ARRAY_DIMS (cra)->inc *
	(SCM_ARRAY_DIMS (cra)->ubnd - SCM_ARRAY_DIMS (cra)->lbnd + 1);
      v = SCM_ARRAY_V (cra);
      goto loop;
    case scm_tc7_string:
    case scm_tc7_byvect:
      sz = sizeof (char);
      break;
    case scm_tc7_bvect:
      vlen = (vlen + SCM_LONG_BIT - 1) / SCM_LONG_BIT;
      cstart /= SCM_LONG_BIT;
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      sz = sizeof (long);
      break;
    case scm_tc7_svect:
      sz = sizeof (short);
      break;
#ifdef LONGLONGS
    case scm_tc7_llvect:
      sz = sizeof (long_long);
      break;
#endif
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      sz = sizeof (float);
      break;
#endif
    case scm_tc7_dvect:
      sz = sizeof (double);
      break;
    case scm_tc7_cvect:
      sz = 2 * sizeof (double);
      break;
#endif
    }
  
  cend = vlen;
  if (!SCM_UNBNDP (start))
    {
      offset = 
	scm_num2long (start, (char *) SCM_ARG3, s_uniform_array_read_x);

      if (offset < 0 || offset >= cend)
	scm_out_of_range (s_uniform_array_read_x, start);

      if (!SCM_UNBNDP (end))
	{
	  long tend =
	    scm_num2long (end, (char *) SCM_ARG4, s_uniform_array_read_x);
      
	  if (tend <= offset || tend > cend)
	    scm_out_of_range (s_uniform_array_read_x, end);
	  cend = tend;
	}
    }

  if (SCM_NIMP (port_or_fd))
    {
      /* if we have stored a character from the port in our own buffer,
	 push it back onto the stream.  */
      /* An ungetc before an fread will not work on some systems if
	 setbuf(0).  do #define NOSETBUF in scmfig.h to fix this. */
      if (SCM_CRDYP (port_or_fd))
	{
	  ungetc (SCM_CGETUN (port_or_fd), (FILE *)SCM_STREAM (port_or_fd));
	  SCM_CLRDY (port_or_fd); /* Clear ungetted char */
	}
      SCM_SYSCALL (ans = fread (SCM_CHARS (v) + (cstart + offset) * sz,
			       (scm_sizet) sz, (scm_sizet) (cend - offset),
				(FILE *)SCM_STREAM (port_or_fd)));
    }
  else /* file descriptor.  */
    {
      SCM_SYSCALL (ans = read (SCM_INUM (port_or_fd),
			       SCM_CHARS (v) + (cstart + offset) * sz,
			       (scm_sizet) (sz * (cend - offset))));
      if (ans == -1)
	scm_syserror (s_uniform_array_read_x);
    }
  if (SCM_TYP7 (v) == scm_tc7_bvect)
    ans *= SCM_LONG_BIT;

  if (v != ra && cra != ra)
    scm_array_copy_x (cra, ra);

  return SCM_MAKINUM (ans);
}

SCM_PROC(s_uniform_array_write, "uniform-array-write", 1, 3, 0, scm_uniform_array_write);

SCM 
scm_uniform_array_write (v, port_or_fd, start, end)
     SCM v;
     SCM port_or_fd;
     SCM start;
     SCM end;
{
  long sz, vlen, ans;
  long offset = 0;
  long cstart = 0;
  long cend;

  port_or_fd = SCM_COERCE_OUTPORT (port_or_fd);

  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_cur_outp;
  else
    SCM_ASSERT (SCM_INUMP (port_or_fd)
		|| (SCM_NIMP (port_or_fd) && SCM_OPOUTFPORTP (port_or_fd)),
		port_or_fd, SCM_ARG2, s_uniform_array_write);
  vlen = SCM_LENGTH (v);

loop:
  switch SCM_TYP7 (v)
    {
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_uniform_array_write);
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (v), badarg1);
      v = scm_ra2contig (v, 1);
      cstart = SCM_ARRAY_BASE (v);
      vlen = SCM_ARRAY_DIMS (v)->inc
	* (SCM_ARRAY_DIMS (v)->ubnd - SCM_ARRAY_DIMS (v)->lbnd + 1);
      v = SCM_ARRAY_V (v);
      goto loop;
    case scm_tc7_string:
    case scm_tc7_byvect:
      sz = sizeof (char);
      break;
    case scm_tc7_bvect:
      vlen = (vlen + SCM_LONG_BIT - 1) / SCM_LONG_BIT;
      cstart /= SCM_LONG_BIT;
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      sz = sizeof (long);
      break;
    case scm_tc7_svect:
      sz = sizeof (short);
      break;
#ifdef LONGLONGS
    case scm_tc7_llvect:
      sz = sizeof (long_long);
      break;
#endif
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      sz = sizeof (float);
      break;
#endif
    case scm_tc7_dvect:
      sz = sizeof (double);
      break;
    case scm_tc7_cvect:
      sz = 2 * sizeof (double);
      break;
#endif
    }

  cend = vlen;
  if (!SCM_UNBNDP (start))
    {
      offset = 
	scm_num2long (start, (char *) SCM_ARG3, s_uniform_array_write);

      if (offset < 0 || offset >= cend)
	scm_out_of_range (s_uniform_array_write, start);

      if (!SCM_UNBNDP (end))
	{
	  long tend = 
	    scm_num2long (end, (char *) SCM_ARG4, s_uniform_array_write);
      
	  if (tend <= offset || tend > cend)
	    scm_out_of_range (s_uniform_array_write, end);
	  cend = tend;
	}
    }

  if (SCM_NIMP (port_or_fd))
    {
      SCM_SYSCALL (ans = fwrite (SCM_CHARS (v) + (cstart + offset) * sz,
				 (scm_sizet) sz, (scm_sizet) (cend - offset),
				 (FILE *)SCM_STREAM (port_or_fd)));
    }
  else /* file descriptor.  */
    {
      SCM_SYSCALL (ans = write (SCM_INUM (port_or_fd),
				SCM_CHARS (v) + (cstart + offset) * sz,
				(scm_sizet) (sz * (cend - offset))));
      if (ans == -1)
	scm_syserror (s_uniform_array_write);
    }
  if (SCM_TYP7 (v) == scm_tc7_bvect)
    ans *= SCM_LONG_BIT;

  return SCM_MAKINUM (ans);
}


static char cnt_tab[16] =
{0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};

SCM_PROC(s_bit_count, "bit-count", 2, 0, 0, scm_bit_count);

SCM 
scm_bit_count (item, seq)
     SCM item;
     SCM seq;
{
  long i;
  register unsigned long cnt = 0, w;
  SCM_ASSERT (SCM_NIMP (seq), seq, SCM_ARG2, s_bit_count);
  switch SCM_TYP7
    (seq)
    {
    default:
      scm_wta (seq, (char *) SCM_ARG2, s_bit_count);
    case scm_tc7_bvect:
      if (0 == SCM_LENGTH (seq))
	return SCM_INUM0;
      i = (SCM_LENGTH (seq) - 1) / SCM_LONG_BIT;
      w = SCM_VELTS (seq)[i];
      if (SCM_FALSEP (item))
	w = ~w;
      w <<= SCM_LONG_BIT - 1 - ((SCM_LENGTH (seq) - 1) % SCM_LONG_BIT);
      while (!0)
	{
	  for (; w; w >>= 4)
	    cnt += cnt_tab[w & 0x0f];
	  if (0 == i--)
	    return SCM_MAKINUM (cnt);
	  w = SCM_VELTS (seq)[i];
	  if (SCM_FALSEP (item))
	    w = ~w;
	}
    }
}


SCM_PROC(s_bit_position, "bit-position", 3, 0, 0, scm_bit_position);

SCM 
scm_bit_position (item, v, k)
     SCM item;
     SCM v;
     SCM k;
{
  long i, lenw, xbits, pos = SCM_INUM (k);
  register unsigned long w;
  SCM_ASSERT (SCM_NIMP (v), v, SCM_ARG2, s_bit_position);
  SCM_ASSERT (SCM_INUMP (k), k, SCM_ARG3, s_bit_position);
  SCM_ASSERT ((pos <= SCM_LENGTH (v)) && (pos >= 0),
	  k, SCM_OUTOFRANGE, s_bit_position);
  if (pos == SCM_LENGTH (v))
    return SCM_BOOL_F;
  switch SCM_TYP7
    (v)
    {
    default:
      scm_wta (v, (char *) SCM_ARG2, s_bit_position);
    case scm_tc7_bvect:
      if (0 == SCM_LENGTH (v))
	return SCM_MAKINUM (-1L);
      lenw = (SCM_LENGTH (v) - 1) / SCM_LONG_BIT;	/* watch for part words */
      i = pos / SCM_LONG_BIT;
      w = SCM_VELTS (v)[i];
      if (SCM_FALSEP (item))
	w = ~w;
      xbits = (pos % SCM_LONG_BIT);
      pos -= xbits;
      w = ((w >> xbits) << xbits);
      xbits = SCM_LONG_BIT - 1 - (SCM_LENGTH (v) - 1) % SCM_LONG_BIT;
      while (!0)
	{
	  if (w && (i == lenw))
	    w = ((w << xbits) >> xbits);
	  if (w)
	    while (w)
	      switch (w & 0x0f)
		{
		default:
		  return SCM_MAKINUM (pos);
		case 2:
		case 6:
		case 10:
		case 14:
		  return SCM_MAKINUM (pos + 1);
		case 4:
		case 12:
		  return SCM_MAKINUM (pos + 2);
		case 8:
		  return SCM_MAKINUM (pos + 3);
		case 0:
		  pos += 4;
		  w >>= 4;
		}
	  if (++i > lenw)
	    break;
	  pos += SCM_LONG_BIT;
	  w = SCM_VELTS (v)[i];
	  if (SCM_FALSEP (item))
	    w = ~w;
	}
      return SCM_BOOL_F;
    }
}


SCM_PROC(s_bit_set_star_x, "bit-set*!", 3, 0, 0, scm_bit_set_star_x);

SCM 
scm_bit_set_star_x (v, kv, obj)
     SCM v;
     SCM kv;
     SCM obj;
{
  register long i, k, vlen;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  SCM_ASRTGO (SCM_NIMP (kv), badarg2);
  switch SCM_TYP7
    (kv)
    {
    default:
    badarg2:scm_wta (kv, (char *) SCM_ARG2, s_bit_set_star_x);
    case scm_tc7_uvect:
      switch SCM_TYP7
	(v)
	{
	default:
	badarg1:scm_wta (v, (char *) SCM_ARG1, s_bit_set_star_x);
	case scm_tc7_bvect:
	  vlen = SCM_LENGTH (v);
	  if (SCM_BOOL_F == obj)
	    for (i = SCM_LENGTH (kv); i;)
	      {
		k = SCM_VELTS (kv)[--i];
		SCM_ASSERT ((k < vlen), SCM_MAKINUM (k), SCM_OUTOFRANGE, s_bit_set_star_x);
		SCM_VELTS (v)[k / SCM_LONG_BIT] &= ~(1L << (k % SCM_LONG_BIT));
	      }
	  else if (SCM_BOOL_T == obj)
	    for (i = SCM_LENGTH (kv); i;)
	      {
		k = SCM_VELTS (kv)[--i];
		SCM_ASSERT ((k < vlen), SCM_MAKINUM (k), SCM_OUTOFRANGE, s_bit_set_star_x);
		SCM_VELTS (v)[k / SCM_LONG_BIT] |= (1L << (k % SCM_LONG_BIT));
	      }
	  else
	  badarg3:scm_wta (obj, (char *) SCM_ARG3, s_bit_set_star_x);
	}
      break;
    case scm_tc7_bvect:
      SCM_ASRTGO (SCM_TYP7 (v) == scm_tc7_bvect && SCM_LENGTH (v) == SCM_LENGTH (kv), badarg1);
      if (SCM_BOOL_F == obj)
	for (k = (SCM_LENGTH (v) + SCM_LONG_BIT - 1) / SCM_LONG_BIT; k--;)
	  SCM_VELTS (v)[k] &= ~(SCM_VELTS (kv)[k]);
      else if (SCM_BOOL_T == obj)
	for (k = (SCM_LENGTH (v) + SCM_LONG_BIT - 1) / SCM_LONG_BIT; k--;)
	  SCM_VELTS (v)[k] |= SCM_VELTS (kv)[k];
      else
	goto badarg3;
      break;
    }
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_bit_count_star, "bit-count*", 3, 0, 0, scm_bit_count_star);

SCM 
scm_bit_count_star (v, kv, obj)
     SCM v;
     SCM kv;
     SCM obj;
{
  register long i, vlen, count = 0;
  register unsigned long k;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  SCM_ASRTGO (SCM_NIMP (kv), badarg2);
  switch SCM_TYP7
    (kv)
    {
    default:
    badarg2:scm_wta (kv, (char *) SCM_ARG2, s_bit_count_star);
    case scm_tc7_uvect:
      switch SCM_TYP7
	(v)
	{
	default:
	badarg1:scm_wta (v, (char *) SCM_ARG1, s_bit_count_star);
	case scm_tc7_bvect:
	  vlen = SCM_LENGTH (v);
	  if (SCM_BOOL_F == obj)
	    for (i = SCM_LENGTH (kv); i;)
	      {
		k = SCM_VELTS (kv)[--i];
		SCM_ASSERT ((k < vlen), SCM_MAKINUM (k), SCM_OUTOFRANGE, s_bit_count_star);
		if (!(SCM_VELTS (v)[k / SCM_LONG_BIT] & (1L << (k % SCM_LONG_BIT))))
		  count++;
	      }
	  else if (SCM_BOOL_T == obj)
	    for (i = SCM_LENGTH (kv); i;)
	      {
		k = SCM_VELTS (kv)[--i];
		SCM_ASSERT ((k < vlen), SCM_MAKINUM (k), SCM_OUTOFRANGE, s_bit_count_star);
		if (SCM_VELTS (v)[k / SCM_LONG_BIT] & (1L << (k % SCM_LONG_BIT)))
		  count++;
	      }
	  else
	  badarg3:scm_wta (obj, (char *) SCM_ARG3, s_bit_count_star);
	}
      break;
    case scm_tc7_bvect:
      SCM_ASRTGO (SCM_TYP7 (v) == scm_tc7_bvect && SCM_LENGTH (v) == SCM_LENGTH (kv), badarg1);
      if (0 == SCM_LENGTH (v))
	return SCM_INUM0;
      SCM_ASRTGO (SCM_BOOL_T == obj || SCM_BOOL_F == obj, badarg3);
      obj = (SCM_BOOL_T == obj);
      i = (SCM_LENGTH (v) - 1) / SCM_LONG_BIT;
      k = SCM_VELTS (kv)[i] & (obj ? SCM_VELTS (v)[i] : ~SCM_VELTS (v)[i]);
      k <<= SCM_LONG_BIT - 1 - ((SCM_LENGTH (v) - 1) % SCM_LONG_BIT);
      while (!0)
	{
	  for (; k; k >>= 4)
	    count += cnt_tab[k & 0x0f];
	  if (0 == i--)
	    return SCM_MAKINUM (count);
	  k = SCM_VELTS (kv)[i] & (obj ? SCM_VELTS (v)[i] : ~SCM_VELTS (v)[i]);
	}
    }
  return SCM_MAKINUM (count);
}


SCM_PROC(s_bit_invert_x, "bit-invert!", 1, 0, 0, scm_bit_invert_x);

SCM 
scm_bit_invert_x (v)
     SCM v;
{
  register long k;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  k = SCM_LENGTH (v);
  switch SCM_TYP7
    (v)
    {
    case scm_tc7_bvect:
      for (k = (k + SCM_LONG_BIT - 1) / SCM_LONG_BIT; k--;)
	SCM_VELTS (v)[k] = ~SCM_VELTS (v)[k];
      break;
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_bit_invert_x);
    }
  return SCM_UNSPECIFIED;
}


SCM 
scm_istr2bve (str, len)
     char *str;
     long len;
{
  SCM v = scm_make_uve (len, SCM_BOOL_T);
  long *data = (long *) SCM_VELTS (v);
  register unsigned long mask;
  register long k;
  register long j;
  for (k = 0; k < (len + SCM_LONG_BIT - 1) / SCM_LONG_BIT; k++)
    {
      data[k] = 0L;
      j = len - k * SCM_LONG_BIT;
      if (j > SCM_LONG_BIT)
	j = SCM_LONG_BIT;
      for (mask = 1L; j--; mask <<= 1)
	switch (*str++)
	  {
	  case '0':
	    break;
	  case '1':
	    data[k] |= mask;
	    break;
	  default:
	    return SCM_BOOL_F;
	  }
    }
  return v;
}



static SCM ra2l SCM_P ((SCM ra, scm_sizet base, scm_sizet k));

static SCM 
ra2l (ra, base, k)
     SCM ra;
     scm_sizet base;
     scm_sizet k;
{
  register SCM res = SCM_EOL;
  register long inc = SCM_ARRAY_DIMS (ra)[k].inc;
  register scm_sizet i;
  if (SCM_ARRAY_DIMS (ra)[k].ubnd < SCM_ARRAY_DIMS (ra)[k].lbnd)
    return SCM_EOL;
  i = base + (1 + SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd) * inc;
  if (k < SCM_ARRAY_NDIM (ra) - 1)
    {
      do
	{
	  i -= inc;
	  res = scm_cons (ra2l (ra, i, k + 1), res);
	}
      while (i != base);
    }
  else
    do
      {
	i -= inc;
	res = scm_cons (scm_uniform_vector_ref (SCM_ARRAY_V (ra), SCM_MAKINUM (i)), res);
      }
    while (i != base);
  return res;
}


SCM_PROC(s_array_to_list, "array->list", 1, 0, 0, scm_array_to_list);

SCM 
scm_array_to_list (v)
     SCM v;
{
  SCM res = SCM_EOL;
  register long k;
  SCM_ASRTGO (SCM_NIMP (v), badarg1);
  switch SCM_TYP7
    (v)
    {
    default:
    badarg1:scm_wta (v, (char *) SCM_ARG1, s_array_to_list);
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (v), badarg1);
      return ra2l (v, SCM_ARRAY_BASE (v), 0);
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return scm_vector_to_list (v);
    case scm_tc7_string:
      return scm_string_to_list (v);
    case scm_tc7_bvect:
      {
	long *data = (long *) SCM_VELTS (v);
	register unsigned long mask;
	for (k = (SCM_LENGTH (v) - 1) / SCM_LONG_BIT; k > 0; k--)
	  for (mask = 1UL << (SCM_LONG_BIT - 1); mask; mask >>= 1)
	    res = scm_cons (((long *) data)[k] & mask ? SCM_BOOL_T : SCM_BOOL_F, res);
	for (mask = 1L << ((SCM_LENGTH (v) % SCM_LONG_BIT) - 1); mask; mask >>= 1)
	  res = scm_cons (((long *) data)[k] & mask ? SCM_BOOL_T : SCM_BOOL_F, res);
	return res;
      }
# ifdef SCM_INUMS_ONLY
    case scm_tc7_uvect:
    case scm_tc7_ivect:
      {
	long *data = (long *) SCM_VELTS (v);
	for (k = SCM_LENGTH (v) - 1; k >= 0; k--)
	  res = scm_cons (SCM_MAKINUM (data[k]), res);
	return res;
      }
# else
  case scm_tc7_uvect: {
    long *data = (long *)SCM_VELTS(v);
    for (k = SCM_LENGTH(v) - 1; k >= 0; k--)
      res = scm_cons(scm_ulong2num(data[k]), res);
    return res;
  }
  case scm_tc7_ivect: {
    long *data = (long *)SCM_VELTS(v);
    for (k = SCM_LENGTH(v) - 1; k >= 0; k--)
      res = scm_cons(scm_long2num(data[k]), res);
    return res;
  }
# endif
    case scm_tc7_svect: {
      short *data;
      data = (short *)SCM_VELTS(v);
      for (k = SCM_LENGTH(v) - 1; k >= 0; k--)
	res = scm_cons(SCM_MAKINUM (data[k]), res);
      return res;
    }
#ifdef LONGLONGS
    case scm_tc7_llvect: {
      long_long *data;
      data = (long_long *)SCM_VELTS(v);
      for (k = SCM_LENGTH(v) - 1; k >= 0; k--)
	res = scm_cons(scm_long_long2num(data[k]), res);
      return res;
    }
#endif


#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      {
	float *data = (float *) SCM_VELTS (v);
	for (k = SCM_LENGTH (v) - 1; k >= 0; k--)
	  res = scm_cons (scm_makflo (data[k]), res);
	return res;
      }
#endif /*SCM_SINGLES*/
    case scm_tc7_dvect:
      {
	double *data = (double *) SCM_VELTS (v);
	for (k = SCM_LENGTH (v) - 1; k >= 0; k--)
	  res = scm_cons (scm_makdbl (data[k], 0.0), res);
	return res;
      }
    case scm_tc7_cvect:
      {
	double (*data)[2] = (double (*)[2]) SCM_VELTS (v);
	for (k = SCM_LENGTH (v) - 1; k >= 0; k--)
	  res = scm_cons (scm_makdbl (data[k][0], data[k][1]), res);
	return res;
      }
#endif /*SCM_FLOATS*/
    }
}


static char s_bad_ralst[] = "Bad scm_array contents list";

static int l2ra SCM_P ((SCM lst, SCM ra, scm_sizet base, scm_sizet k));

SCM_PROC(s_list_to_uniform_array, "list->uniform-array", 3, 0, 0, scm_list_to_uniform_array);

SCM 
scm_list_to_uniform_array (ndim, prot, lst)
     SCM ndim;
     SCM prot;
     SCM lst;
{
  SCM shp = SCM_EOL;
  SCM row = lst;
  SCM ra;
  scm_sizet k;
  long n;
  SCM_ASSERT (SCM_INUMP (ndim), ndim, SCM_ARG1, s_list_to_uniform_array);
  k = SCM_INUM (ndim);
  while (k--)
    {
      n = scm_ilength (row);
      SCM_ASSERT (n >= 0, lst, SCM_ARG3, s_list_to_uniform_array);
      shp = scm_cons (SCM_MAKINUM (n), shp);
      if (SCM_NIMP (row))
	row = SCM_CAR (row);
    }
  ra = scm_dimensions_to_uniform_array (scm_reverse (shp), prot, SCM_EOL);
  if (SCM_NULLP (shp))

    {
      SCM_ASRTGO (1 == scm_ilength (lst), badlst);
      scm_array_set_x (ra, SCM_CAR (lst), SCM_EOL);
      return ra;
    }
  if (!SCM_ARRAYP (ra))
    {
      for (k = 0; k < SCM_LENGTH (ra); k++, lst = SCM_CDR (lst))
	scm_array_set_x (ra, SCM_CAR (lst), SCM_MAKINUM (k));
      return ra;
    }
  if (l2ra (lst, ra, SCM_ARRAY_BASE (ra), 0))
    return ra;
  else
  badlst:scm_wta (lst, s_bad_ralst, s_list_to_uniform_array);
  return SCM_BOOL_F;
}

static int 
l2ra (lst, ra, base, k)
     SCM lst;
     SCM ra;
     scm_sizet base;
     scm_sizet k;
{
  register long inc = SCM_ARRAY_DIMS (ra)[k].inc;
  register long n = (1 + SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd);
  int ok = 1;
  if (n <= 0)
    return (SCM_EOL == lst);
  if (k < SCM_ARRAY_NDIM (ra) - 1)
    {
      while (n--)
	{
	  if (SCM_IMP (lst) || SCM_NCONSP (lst))
	    return 0;
	  ok = ok && l2ra (SCM_CAR (lst), ra, base, k + 1);
	  base += inc;
	  lst = SCM_CDR (lst);
	}
      if (SCM_NNULLP (lst))
 return 0;
    }
  else
    {
      while (n--)
	{
	  if (SCM_IMP (lst) || SCM_NCONSP (lst))
	    return 0;
	  ok = ok && scm_array_set_x (SCM_ARRAY_V (ra), SCM_CAR (lst), SCM_MAKINUM (base));
	  base += inc;
	  lst = SCM_CDR (lst);
	}
      if (SCM_NNULLP (lst))
 return 0;
    }
  return ok;
}


static void rapr1 SCM_P ((SCM ra, scm_sizet j, scm_sizet k, SCM port, scm_print_state *pstate));

static void 
rapr1 (ra, j, k, port, pstate)
     SCM ra;
     scm_sizet j;
     scm_sizet k;
     SCM port;
     scm_print_state *pstate;
{
  long inc = 1;
  long n = SCM_LENGTH (ra);
  int enclosed = 0;
tail:
  switch SCM_TYP7
    (ra)
    {
    case scm_tc7_smob:
      if (enclosed++)
	{
	  SCM_ARRAY_BASE (ra) = j;
	  if (n-- > 0)
	    scm_iprin1 (ra, port, pstate);
	  for (j += inc; n-- > 0; j += inc)
	    {
	      scm_putc (' ', port);
	      SCM_ARRAY_BASE (ra) = j;
	      scm_iprin1 (ra, port, pstate);
	    }
	  break;
	}
      if (k + 1 < SCM_ARRAY_NDIM (ra))
	{
	  long i;
	  inc = SCM_ARRAY_DIMS (ra)[k].inc;
	  for (i = SCM_ARRAY_DIMS (ra)[k].lbnd; i < SCM_ARRAY_DIMS (ra)[k].ubnd; i++)
	    {
	      scm_putc ('(', port);
	      rapr1 (ra, j, k + 1, port, pstate);
	      scm_puts (") ", port);
	      j += inc;
	    }
	  if (i == SCM_ARRAY_DIMS (ra)[k].ubnd)
	    {			/* could be zero size. */
	      scm_putc ('(', port);
	      rapr1 (ra, j, k + 1, port, pstate);
	      scm_putc (')', port);
	    }
	  break;
	}
      if SCM_ARRAY_NDIM
	(ra)
	{			/* Could be zero-dimensional */
	  inc = SCM_ARRAY_DIMS (ra)[k].inc;
	  n = (SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1);
	}
      else
	n = 1;
      ra = SCM_ARRAY_V (ra);
      goto tail;
    default:
      if (n-- > 0)
	scm_iprin1 (scm_uniform_vector_ref (ra, SCM_MAKINUM (j)), port, pstate);
      for (j += inc; n-- > 0; j += inc)
	{
	  scm_putc (' ', port);
	  scm_iprin1 (scm_cvref (ra, j, SCM_UNDEFINED), port, pstate);
	}
      break;
    case scm_tc7_string:
      if (n-- > 0)
	scm_iprin1 (SCM_MAKICHR (SCM_UCHARS (ra)[j]), port, pstate);
      if (SCM_WRITINGP (pstate))
	for (j += inc; n-- > 0; j += inc)
	  {
	    scm_putc (' ', port);
	    scm_iprin1 (SCM_MAKICHR (SCM_UCHARS (ra)[j]), port, pstate);
	  }
      else
	for (j += inc; n-- > 0; j += inc)
	  scm_putc (SCM_CHARS (ra)[j], port);
      break;
    case scm_tc7_byvect:
      if (n-- > 0)
	scm_intprint (((char *)SCM_CDR (ra))[j], 10, port);
      for (j += inc; n-- > 0; j += inc)
	{
	  scm_putc (' ', port);
	  scm_intprint (((char *)SCM_CDR (ra))[j], 10, port);
	}
      break;

    case scm_tc7_uvect:
    case scm_tc7_ivect:
      if (n-- > 0)
	scm_intprint (SCM_VELTS (ra)[j], 10, port);
      for (j += inc; n-- > 0; j += inc)
	{
	  scm_putc (' ', port);
	  scm_intprint (SCM_VELTS (ra)[j], 10, port);
	}
      break;

    case scm_tc7_svect:
      if (n-- > 0)
	scm_intprint (((short *)SCM_CDR (ra))[j], 10, port);
      for (j += inc; n-- > 0; j += inc)
	{
	  scm_putc (' ', port);
	  scm_intprint (((short *)SCM_CDR (ra))[j], 10, port);
	}
      break;

#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      if (n-- > 0)
	{
	  SCM z = scm_makflo (1.0);
	  SCM_FLO (z) = ((float *) SCM_VELTS (ra))[j];
	  scm_floprint (z, port, pstate);
	  for (j += inc; n-- > 0; j += inc)
	    {
	      scm_putc (' ', port);
	      SCM_FLO (z) = ((float *) SCM_VELTS (ra))[j];
	      scm_floprint (z, port, pstate);
	    }
	}
      break;
#endif /*SCM_SINGLES*/
    case scm_tc7_dvect:
      if (n-- > 0)
	{
	  SCM z = scm_makdbl (1.0 / 3.0, 0.0);
	  SCM_REAL (z) = ((double *) SCM_VELTS (ra))[j];
	  scm_floprint (z, port, pstate);
	  for (j += inc; n-- > 0; j += inc)
	    {
	      scm_putc (' ', port);
	      SCM_REAL (z) = ((double *) SCM_VELTS (ra))[j];
	      scm_floprint (z, port, pstate);
	    }
	}
      break;
    case scm_tc7_cvect:
      if (n-- > 0)
	{
	  SCM cz = scm_makdbl (0.0, 1.0), z = scm_makdbl (1.0 / 3.0, 0.0);
	  SCM_REAL (z) = SCM_REAL (cz) = (((double *) SCM_VELTS (ra))[2 * j]);
	  SCM_IMAG (cz) = ((double *) SCM_VELTS (ra))[2 * j + 1];
	  scm_floprint ((0.0 == SCM_IMAG (cz) ? z : cz), port, pstate);
	  for (j += inc; n-- > 0; j += inc)
	    {
	      scm_putc (' ', port);
	      SCM_REAL (z) = SCM_REAL (cz) = ((double *) SCM_VELTS (ra))[2 * j];
	      SCM_IMAG (cz) = ((double *) SCM_VELTS (ra))[2 * j + 1];
	      scm_floprint ((0.0 == SCM_IMAG (cz) ? z : cz), port, pstate);
	    }
	}
      break;
#endif /*SCM_FLOATS*/
    }
}



int 
scm_raprin1 (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  SCM v = exp;
  scm_sizet base = 0;
  scm_putc ('#', port);
tail:
  switch SCM_TYP7
    (v)
    {
    case scm_tc7_smob:
      {
	long ndim = SCM_ARRAY_NDIM (v);
	base = SCM_ARRAY_BASE (v);
	v = SCM_ARRAY_V (v);
	if (SCM_ARRAYP (v))

	  {
	    scm_puts ("<enclosed-array ", port);
	    rapr1 (exp, base, 0, port, pstate);
	    scm_putc ('>', port);
	    return 1;
	  }
	else
	  {
	    scm_intprint (ndim, 10, port);
	    goto tail;
	  }
      }
    case scm_tc7_bvect:
      if (exp == v)
	{			/* a uve, not an scm_array */
	  register long i, j, w;
	  scm_putc ('*', port);
	  for (i = 0; i < (SCM_LENGTH (exp)) / SCM_LONG_BIT; i++)
	    {
	      w = SCM_VELTS (exp)[i];
	      for (j = SCM_LONG_BIT; j; j--)
		{
		  scm_putc (w & 1 ? '1' : '0', port);
		  w >>= 1;
		}
	    }
	  j = SCM_LENGTH (exp) % SCM_LONG_BIT;
	  if (j)
	    {
	      w = SCM_VELTS (exp)[SCM_LENGTH (exp) / SCM_LONG_BIT];
	      for (; j; j--)
		{
		  scm_putc (w & 1 ? '1' : '0', port);
		  w >>= 1;
		}
	    }
	  return 1;
	}
      else
	scm_putc ('b', port);
      break;
    case scm_tc7_string:
      scm_putc ('a', port);
      break;
    case scm_tc7_byvect:
      scm_putc ('y', port);
      break;
    case scm_tc7_uvect:
      scm_putc ('u', port);
      break;
    case scm_tc7_ivect:
      scm_putc ('e', port);
      break;
    case scm_tc7_svect:
      scm_putc ('h', port);
      break;
#ifdef LONGLONGS
    case scm_tc7_llvect:
      scm_puts ("long_long", port);
      break;
#endif
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      scm_putc ('s', port);
      break;
#endif /*SCM_SINGLES*/
    case scm_tc7_dvect:
      scm_putc ('i', port);
      break;
    case scm_tc7_cvect:
      scm_putc ('c', port);
      break;
#endif /*SCM_FLOATS*/
    }
  scm_putc ('(', port);
  rapr1 (exp, base, 0, port, pstate);
  scm_putc (')', port);
  return 1;
}

SCM_PROC(s_array_prototype, "array-prototype", 1, 0, 0, scm_array_prototype);

SCM 
scm_array_prototype (ra)
     SCM ra;
{
  int enclosed = 0;
  SCM_ASRTGO (SCM_NIMP (ra), badarg);
loop:
  switch SCM_TYP7
    (ra)
    {
    default:
    badarg:scm_wta (ra, (char *) SCM_ARG1, s_array_prototype);
    case scm_tc7_smob:
      SCM_ASRTGO (SCM_ARRAYP (ra), badarg);
      if (enclosed++)
	return SCM_UNSPECIFIED;
      ra = SCM_ARRAY_V (ra);
      goto loop;
    case scm_tc7_vector:
    case scm_tc7_wvect:
      return SCM_EOL;
    case scm_tc7_bvect:
      return SCM_BOOL_T;
    case scm_tc7_string:
      return SCM_MAKICHR ('a');
    case scm_tc7_byvect:
      return SCM_MAKICHR ('\0');
    case scm_tc7_uvect:
      return SCM_MAKINUM (1L);
    case scm_tc7_ivect:
      return SCM_MAKINUM (-1L);
    case scm_tc7_svect:
      return SCM_CDR (scm_intern ("s", 1));
#ifdef LONGLONGS
    case scm_tc7_llvect:
      return SCM_CDR (scm_intern ("l", 1));
#endif
#ifdef SCM_FLOATS
#ifdef SCM_SINGLES
    case scm_tc7_fvect:
      return scm_makflo (1.0);
#endif
    case scm_tc7_dvect:
      return scm_makdbl (1.0 / 3.0, 0.0);
    case scm_tc7_cvect:
      return scm_makdbl (0.0, 1.0);
#endif
    }
}


static SCM markra SCM_P ((SCM ptr));

static SCM
markra (ptr)
     SCM ptr;
{
  return SCM_ARRAY_V (ptr);
}


static scm_sizet freera SCM_P ((SCM ptr));

static scm_sizet
freera (ptr)
     SCM ptr;
{
  scm_must_free (SCM_CHARS (ptr));
  return sizeof (scm_array) + SCM_ARRAY_NDIM (ptr) * sizeof (scm_array_dim);
}

static scm_smobfuns rasmob =
{markra, freera, scm_raprin1, scm_array_equal_p};


/* This must be done after scm_init_scl() */

void
scm_init_unif ()
{
#include "unif.x"
  scm_tc16_array = scm_newsmob (&rasmob);
  scm_add_feature ("array");
}

#else /* ARRAYS */


int 
scm_raprin1 (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  return 0;
}


SCM 
scm_istr2bve (str, len)
     char *str;
     long len;
{
  return SCM_BOOL_F;
}

void 
scm_init_unif ()
{
#include "unif.x"
  scm_make_subr (s_resizuve, scm_tc7_subr_2, scm_vector_set_length_x);
}

#endif /* ARRAYS */
