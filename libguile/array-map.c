/* Copyright (C) 1996, 1998, 2000, 2001, 2004, 2005, 2006, 2008, 2009,
 *   2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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
#include "libguile/strings.h"
#include "libguile/arrays.h"
#include "libguile/smob.h"
#include "libguile/chars.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/bitvectors.h"
#include "libguile/srfi-4.h"
#include "libguile/generalized-arrays.h"

#include "libguile/validate.h"
#include "libguile/array-map.h"


/* The WHAT argument for `scm_gc_malloc ()' et al.  */
static const char indices_gc_hint[] = "array-indices";

static SCM
AREF (SCM v, size_t pos)
{
  return scm_c_array_ref_1 (v, pos);
}

static void
ASET (SCM v, size_t pos, SCM val)
{
  scm_c_array_set_1_x (v, val, pos);
}

static unsigned long
cind (SCM ra, long *ve)
{
  unsigned long i;
  int k;
  if (!SCM_I_ARRAYP (ra))
    return *ve;
  i = SCM_I_ARRAY_BASE (ra);
  for (k = 0; k < SCM_I_ARRAY_NDIM (ra); k++)
    i += (ve[k] - SCM_I_ARRAY_DIMS (ra)[k].lbnd) * SCM_I_ARRAY_DIMS (ra)[k].inc;
  return i;
}


/* Checker for scm_array mapping functions:
   return values:
   5 --> empty axes;
   4 --> shapes, increments, and bases are the same;
   3 --> shapes and increments are the same;
   2 --> shapes are the same;
   1 --> ras are at least as big as ra0;
   0 --> no match.
   */

int
scm_ra_matchp (SCM ra0, SCM ras)
{
  int i, exact = 4, empty = 0;
  scm_t_array_handle h0;

  scm_array_get_handle (ra0, &h0);
  for (i = 0; i < h0.ndims; ++i)
    {
      empty = empty || (h0.dims[i].lbnd > h0.dims[i].ubnd);
    }

  while (scm_is_pair (ras))
    {
      scm_t_array_handle h1;

      scm_array_get_handle (SCM_CAR (ras), &h1);

      if (h0.ndims != h1.ndims)
        {
          scm_array_handle_release (&h0);
          scm_array_handle_release (&h1);
          return 0;
        }
      if (h0.base != h1.base)
        exact = min(3, exact);

      for (i = 0; i < h0.ndims; ++i)
        {
          empty = empty || (h1.dims[i].lbnd > h1.dims[i].ubnd);
          switch (exact)
            {
            case 4:
            case 3:
              if (h0.dims[i].inc != h1.dims[i].inc)
                exact = 2;
            case 2:
              if (h0.dims[i].lbnd == h1.dims[i].lbnd && h0.dims[i].ubnd == h1.dims[i].ubnd)
                break;
              exact = 1;
            default:
              if (h0.dims[i].lbnd < h1.dims[i].lbnd || h0.dims[i].ubnd > h1.dims[i].ubnd)
                {
                  scm_array_handle_release (&h0);
                  scm_array_handle_release (&h1);
                  return 0;
                }
            }
        }
      scm_array_handle_release (&h1);
      ras = SCM_CDR (ras);
    }
  scm_array_handle_release (&h0);
  return empty ? 5 : exact;
}

/* array mapper: apply cproc to each dimension of the given arrays?.
     int (*cproc) ();   procedure to call on unrolled arrays?
			   cproc (dest, source list) or
			   cproc (dest, data, source list).
     SCM data;          data to give to cproc or unbound.
     SCM ra0;           destination array.
     SCM lra;           list of source arrays.
     const char *what;  caller, for error reporting. */
int
scm_ramapc (void *cproc_ptr, SCM data, SCM ra0, SCM lra, const char *what)
{
  SCM z;
  SCM vra0, ra1, vra1;
  SCM lvra, *plvra;
  long *vinds;
  int k, kmax;
  int (*cproc) () = cproc_ptr;

  switch (scm_ra_matchp (ra0, lra))
    {
    default:
    case 0:
      scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
    case 2:
    case 3:
    case 4:			/* Try unrolling arrays */
      kmax = (SCM_I_ARRAYP (ra0) ? SCM_I_ARRAY_NDIM (ra0) - 1 : 0);
      if (kmax < 0)
	goto gencase;
      vra0 = scm_array_contents (ra0, SCM_UNDEFINED);
      if (scm_is_false (vra0))
        goto gencase;
      if (!SCM_I_ARRAYP (vra0))
	{
	  size_t length = scm_c_array_length (vra0);
	  vra1 = scm_i_make_array (1);
	  SCM_I_ARRAY_BASE (vra1) = 0;
	  SCM_I_ARRAY_DIMS (vra1)->lbnd = 0;
	  SCM_I_ARRAY_DIMS (vra1)->ubnd = length - 1;
	  SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	  SCM_I_ARRAY_V (vra1) = vra0;
	  vra0 = vra1;
	}
      lvra = SCM_EOL;
      plvra = &lvra;
      for (z = lra; scm_is_pair (z); z = SCM_CDR (z))
	{
	  ra1 = SCM_CAR (z);
	  vra1 = scm_i_make_array (1);
	  SCM_I_ARRAY_DIMS (vra1)->lbnd = SCM_I_ARRAY_DIMS (vra0)->lbnd;
	  SCM_I_ARRAY_DIMS (vra1)->ubnd = SCM_I_ARRAY_DIMS (vra0)->ubnd;
	  if (!SCM_I_ARRAYP (ra1))
	    {
	      SCM_I_ARRAY_BASE (vra1) = 0;
	      SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	      SCM_I_ARRAY_V (vra1) = ra1;
	    }
	  else if (!SCM_I_ARRAY_CONTP (ra1))
	    goto gencase;
	  else
	    {
	      SCM_I_ARRAY_BASE (vra1) = SCM_I_ARRAY_BASE (ra1);
	      SCM_I_ARRAY_DIMS (vra1)->inc = SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
	      SCM_I_ARRAY_V (vra1) = SCM_I_ARRAY_V (ra1);
	    }
	  *plvra = scm_cons (vra1, SCM_EOL);
	  plvra = SCM_CDRLOC (*plvra);
	}
      return (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra));
    case 1:
    gencase:			/* Have to loop over all dimensions. */
      vra0 = scm_i_make_array (1);
    if (SCM_I_ARRAYP (ra0))
      {
	kmax = SCM_I_ARRAY_NDIM (ra0) - 1;
	if (kmax < 0)
	  {
	    SCM_I_ARRAY_DIMS (vra0)->lbnd = 0;
	    SCM_I_ARRAY_DIMS (vra0)->ubnd = 0;
	    SCM_I_ARRAY_DIMS (vra0)->inc = 1;
	  }
	else
	  {
	    SCM_I_ARRAY_DIMS (vra0)->lbnd = SCM_I_ARRAY_DIMS (ra0)[kmax].lbnd;
	    SCM_I_ARRAY_DIMS (vra0)->ubnd = SCM_I_ARRAY_DIMS (ra0)[kmax].ubnd;
	    SCM_I_ARRAY_DIMS (vra0)->inc = SCM_I_ARRAY_DIMS (ra0)[kmax].inc;
	  }
	SCM_I_ARRAY_BASE (vra0) = SCM_I_ARRAY_BASE (ra0);
	SCM_I_ARRAY_V (vra0) = SCM_I_ARRAY_V (ra0);
      }
    else
      {
	size_t length = scm_c_array_length (ra0);
	kmax = 0;
	SCM_I_ARRAY_DIMS (vra0)->lbnd = 0;
	SCM_I_ARRAY_DIMS (vra0)->ubnd = length - 1;
	SCM_I_ARRAY_DIMS (vra0)->inc = 1;
	SCM_I_ARRAY_BASE (vra0) = 0;
	SCM_I_ARRAY_V (vra0) = ra0;
	ra0 = vra0;
      }
    lvra = SCM_EOL;
    plvra = &lvra;
    for (z = lra; scm_is_pair (z); z = SCM_CDR (z))
      {
	ra1 = SCM_CAR (z);
	vra1 = scm_i_make_array (1);
	SCM_I_ARRAY_DIMS (vra1)->lbnd = SCM_I_ARRAY_DIMS (vra0)->lbnd;
	SCM_I_ARRAY_DIMS (vra1)->ubnd = SCM_I_ARRAY_DIMS (vra0)->ubnd;
	if (SCM_I_ARRAYP (ra1))
	  {
	    if (kmax >= 0)
	      SCM_I_ARRAY_DIMS (vra1)->inc = SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
	    SCM_I_ARRAY_V (vra1) = SCM_I_ARRAY_V (ra1);
	  }
	else
	  {
	    SCM_I_ARRAY_DIMS (vra1)->inc = 1;
	    SCM_I_ARRAY_V (vra1) = ra1;
	  }
	*plvra = scm_cons (vra1, SCM_EOL);
	plvra = SCM_CDRLOC (*plvra);
      }

    vinds = scm_gc_malloc_pointerless (sizeof(long) * SCM_I_ARRAY_NDIM (ra0),
				       indices_gc_hint);

    for (k = 0; k <= kmax; k++)
      vinds[k] = SCM_I_ARRAY_DIMS (ra0)[k].lbnd;
    k = kmax;
    do
      {
	if (k == kmax)
	  {
	    SCM y = lra;
	    SCM_I_ARRAY_BASE (vra0) = cind (ra0, vinds);
	    for (z = lvra; scm_is_pair (z); z = SCM_CDR (z), y = SCM_CDR (y))
	      SCM_I_ARRAY_BASE (SCM_CAR (z)) = cind (SCM_CAR (y), vinds);
	    if (0 == (SCM_UNBNDP (data) ? cproc(vra0, lvra) : cproc(vra0, data, lvra)))
	      return 0;
	    k--;
	    continue;
	  }
	if (vinds[k] < SCM_I_ARRAY_DIMS (ra0)[k].ubnd)
	  {
	    vinds[k]++;
	    k++;
	    continue;
	  }
	vinds[k] = SCM_I_ARRAY_DIMS (ra0)[k].lbnd - 1;
	k--;
      }
    while (k >= 0);

    case 5:
    return 1;
    }
}

static int
rafill (SCM dst, SCM fill)
{
  long n = (SCM_I_ARRAY_DIMS (dst)->ubnd - SCM_I_ARRAY_DIMS (dst)->lbnd + 1);
  scm_t_array_handle h;
  size_t i;
  ssize_t inc;
  scm_array_get_handle (SCM_I_ARRAY_V (dst), &h);
  i = SCM_I_ARRAY_BASE (dst);
  inc = SCM_I_ARRAY_DIMS (dst)->inc;

  for (; n-- > 0; i += inc)
    h.vset (h.vector, i, fill);

  scm_array_handle_release (&h);
  return 1;
}

SCM_DEFINE (scm_array_fill_x, "array-fill!", 2, 0, 0,
	    (SCM ra, SCM fill),
	    "Store @var{fill} in every element of array @var{ra}.  The value\n"
	    "returned is unspecified.")
#define FUNC_NAME s_scm_array_fill_x
{
  scm_ramapc (rafill, fill, ra, SCM_EOL, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* FIXME src-dst is the wrong order for scm_ra_matchp, but scm_ramapc
   doesn't send SCM_I_ARRAYP for both src and dst, and this segfaults
   with the 'right' order. */
static int
racp (SCM src, SCM dst)
{
  long n = (SCM_I_ARRAY_DIMS (src)->ubnd - SCM_I_ARRAY_DIMS (src)->lbnd + 1);
  scm_t_array_handle h_s, h_d;
  size_t i_s, i_d;
  ssize_t inc_s, inc_d;

  dst = SCM_CAR (dst);
  i_s = SCM_I_ARRAY_BASE (src);
  i_d = SCM_I_ARRAY_BASE (dst);
  inc_s = SCM_I_ARRAY_DIMS (src)->inc;
  inc_d = SCM_I_ARRAY_DIMS (dst)->inc;

  scm_array_get_handle (SCM_I_ARRAY_V (src), &h_s);
  scm_array_get_handle (SCM_I_ARRAY_V (dst), &h_d);

  if (h_s.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM
      && h_d.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
    {
      SCM const * el_s = h_s.elements;
      SCM * el_d = h_d.writable_elements;
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
        el_d[i_d] = el_s[i_s];
    }
  else
    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
      h_d.vset (h_d.vector, i_d, h_s.vref (h_s.vector, i_s));

  scm_array_handle_release (&h_d);
  scm_array_handle_release (&h_s);

  return 1;
}

SCM_REGISTER_PROC(s_array_copy_in_order_x, "array-copy-in-order!", 2, 0, 0, scm_array_copy_x);


SCM_DEFINE (scm_array_copy_x, "array-copy!", 2, 0, 0,
	    (SCM src, SCM dst),
	    "@deffnx {Scheme Procedure} array-copy-in-order! src dst\n"
	    "Copy every element from vector or array @var{src} to the\n"
	    "corresponding element of @var{dst}.  @var{dst} must have the\n"
	    "same rank as @var{src}, and be at least as large in each\n"
	    "dimension.  The order is unspecified.")
#define FUNC_NAME s_scm_array_copy_x
{
  scm_ramapc (racp, SCM_UNDEFINED, src, scm_cons (dst, SCM_EOL), FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#if SCM_ENABLE_DEPRECATED == 1

/* to be used as cproc in scm_ramapc to fill an array dimension with
   "fill". */
int
scm_array_fill_int (SCM ra, SCM fill, SCM ignore SCM_UNUSED)
{
  unsigned long i;
  unsigned long n = SCM_I_ARRAY_DIMS (ra)->ubnd - SCM_I_ARRAY_DIMS (ra)->lbnd + 1;
  long inc = SCM_I_ARRAY_DIMS (ra)->inc;
  unsigned long base = SCM_I_ARRAY_BASE (ra);

  ra = SCM_I_ARRAY_V (ra);

  for (i = base; n--; i += inc)
    ASET (ra, i, fill);

  return 1;
}

/* Functions callable by ARRAY-MAP! */

int
scm_ra_eqp (SCM ra0, SCM ras)
{
  SCM ra1 = SCM_CAR (ras), ra2 = SCM_CAR (SCM_CDR (ras));
  scm_t_array_handle ra0_handle;
  scm_t_array_dim *ra0_dims;
  size_t n;
  ssize_t inc0;
  size_t i0 = 0;
  unsigned long i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  scm_array_get_handle (ra0, &ra0_handle);
  ra0_dims = scm_array_handle_dims (&ra0_handle);
  n = ra0_dims[0].ubnd - ra0_dims[0].lbnd + 1;
  inc0 = ra0_dims[0].inc;

  {
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if (scm_is_true (scm_array_handle_ref (&ra0_handle, i0)))
	if (!scm_is_eq (AREF (ra1, i1), AREF (ra2, i2)))
	  scm_array_handle_set (&ra0_handle, i0, SCM_BOOL_F);
  }

  scm_array_handle_release (&ra0_handle);
  return 1;
}

/* opt 0 means <, nonzero means >= */

static int
ra_compare (SCM ra0, SCM ra1, SCM ra2, int opt)
{
  scm_t_array_handle ra0_handle;
  scm_t_array_dim *ra0_dims;
  size_t n;
  ssize_t inc0;
  size_t i0 = 0;
  unsigned long i1 = SCM_I_ARRAY_BASE (ra1), i2 = SCM_I_ARRAY_BASE (ra2);
  long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
  long inc2 = SCM_I_ARRAY_DIMS (ra1)->inc;
  ra1 = SCM_I_ARRAY_V (ra1);
  ra2 = SCM_I_ARRAY_V (ra2);

  scm_array_get_handle (ra0, &ra0_handle);
  ra0_dims = scm_array_handle_dims (&ra0_handle);
  n = ra0_dims[0].ubnd - ra0_dims[0].lbnd + 1;
  inc0 = ra0_dims[0].inc;

  {
    for (; n-- > 0; i0 += inc0, i1 += inc1, i2 += inc2)
      if (scm_is_true (scm_array_handle_ref (&ra0_handle, i0)))
	if (opt ?
	    scm_is_true (scm_less_p (AREF (ra1, i1), AREF (ra2, i2))) :
	    scm_is_false (scm_less_p (AREF (ra1, i1), AREF (ra2, i2))))
	  scm_array_handle_set (&ra0_handle, i0, SCM_BOOL_F);
  }

  scm_array_handle_release (&ra0_handle);
  return 1;
}



int
scm_ra_lessp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 0);
}


int
scm_ra_leqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 1);
}


int
scm_ra_grp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (SCM_CDR (ras)), SCM_CAR (ras), 0);
}


int
scm_ra_greqp (SCM ra0, SCM ras)
{
  return ra_compare (ra0, SCM_CAR (ras), SCM_CAR (SCM_CDR (ras)), 1);
}


int
scm_ra_sum (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (!scm_is_null(ras))
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      ASET (ra0, i0, scm_sum (AREF(ra0, i0), AREF(ra1, i1)));
	    break;
	  }
	}
    }
  return 1;
}



int
scm_ra_difference (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0)
	      ASET (ra0, i0, scm_difference (AREF(ra0, i0), SCM_UNDEFINED));
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      ASET (ra0, i0, scm_difference (AREF (ra0, i0), AREF (ra1, i1)));
	    break;
	  }
	}
    }
  return 1;
}



int
scm_ra_product (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (!scm_is_null (ras))
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      ASET (ra0, i0, scm_product (AREF (ra0, i0), AREF (ra1, i1)));
	  }
	}
    }
  return 1;
}


int
scm_ra_divide (SCM ra0, SCM ras)
{
  long n = SCM_I_ARRAY_DIMS (ra0)->ubnd - SCM_I_ARRAY_DIMS (ra0)->lbnd + 1;
  unsigned long i0 = SCM_I_ARRAY_BASE (ra0);
  long inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  ra0 = SCM_I_ARRAY_V (ra0);
  if (scm_is_null (ras))
    {
      switch (SCM_TYP7 (ra0))
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0)
	      ASET (ra0, i0, scm_divide (AREF (ra0, i0), SCM_UNDEFINED));
	    break;
	  }
	}
    }
  else
    {
      SCM ra1 = SCM_CAR (ras);
      unsigned long i1 = SCM_I_ARRAY_BASE (ra1);
      long inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ra1 = SCM_I_ARRAY_V (ra1);
      switch (SCM_TYP7 (ra0) == SCM_TYP7 (ra1) ? SCM_TYP7 (ra0) : 0)
	{
	default:
	  {
	    for (; n-- > 0; i0 += inc0, i1 += inc1)
	      {
		SCM res =  scm_divide (AREF (ra0, i0), AREF (ra1, i1));
		ASET (ra0, i0, res);
	      }
	    break;
	  }
	}
    }
  return 1;
}


int
scm_array_identity (SCM dst, SCM src)
{
  return racp (SCM_CAR (src), scm_cons (dst, SCM_EOL));
}

#endif /* SCM_ENABLE_DEPRECATED */

static int
ramap (SCM ra0, SCM proc, SCM ras)
{
  ssize_t i = SCM_I_ARRAY_DIMS (ra0)->lbnd;
  size_t n = SCM_I_ARRAY_DIMS (ra0)->ubnd - i + 1;

  scm_t_array_handle h0;
  size_t i0, i0end;
  ssize_t inc0;
  scm_array_get_handle (SCM_I_ARRAY_V (ra0), &h0);
  i0 = SCM_I_ARRAY_BASE (ra0);
  inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  i0end = i0 + n*inc0;
  if (scm_is_null (ras))
    for (; i0 < i0end; i0 += inc0)
      h0.vset (h0.vector, i0, scm_call_0 (proc));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      scm_t_array_handle h1;
      size_t i1;
      ssize_t inc1;
      scm_array_get_handle (SCM_I_ARRAY_V (ra1), &h1);
      i1 = SCM_I_ARRAY_BASE (ra1);
      inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      ras = SCM_CDR (ras);
      if (scm_is_null (ras))
          for (; i0 < i0end; i0 += inc0, i1 += inc1)
            h0.vset (h0.vector, i0, scm_call_1 (proc, h1.vref (h1.vector, i1)));
      else
        {
          ras = scm_vector (ras);
          for (; i0 < i0end; i0 += inc0, i1 += inc1, ++i)
            {
              SCM args = SCM_EOL;
              unsigned long k;
              for (k = scm_c_vector_length (ras); k--;)
                args = scm_cons (AREF (scm_c_vector_ref (ras, k), i), args);
              h0.vset (h0.vector, i0,
                       scm_apply_1 (proc, h1.vref (h1.vector, i1), args));
            }
        }
      scm_array_handle_release (&h1);
    }
  scm_array_handle_release (&h0);
  return 1;
}


SCM_REGISTER_PROC(s_array_map_in_order_x, "array-map-in-order!", 2, 0, 1, scm_array_map_x);

SCM_SYMBOL (sym_b, "b");

SCM_DEFINE (scm_array_map_x, "array-map!", 2, 0, 1,
	    (SCM ra0, SCM proc, SCM lra),
	    "@deffnx {Scheme Procedure} array-map-in-order! ra0 proc . lra\n"
	    "@var{array1}, @dots{} must have the same number of dimensions\n"
	    "as @var{ra0} and have a range for each index which includes the\n"
	    "range for the corresponding index in @var{ra0}.  @var{proc} is\n"
	    "applied to each tuple of elements of @var{array1}, @dots{} and\n"
	    "the result is stored as the corresponding element in @var{ra0}.\n"
	    "The value returned is unspecified.  The order of application is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_array_map_x
{
  SCM_VALIDATE_PROC (2, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);

  scm_ramapc (ramap, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
rafe (SCM ra0, SCM proc, SCM ras)
{
  ssize_t i = SCM_I_ARRAY_DIMS (ra0)->lbnd;
  size_t n = SCM_I_ARRAY_DIMS (ra0)->ubnd - i + 1;

  scm_t_array_handle h0;
  size_t i0, i0end;
  ssize_t inc0;
  scm_array_get_handle (SCM_I_ARRAY_V (ra0), &h0);
  i0 = SCM_I_ARRAY_BASE (ra0);
  inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  i0end = i0 + n*inc0;
  if (scm_is_null (ras))
    for (; i0 < i0end; i0 += inc0)
      scm_call_1 (proc, h0.vref (h0.vector, i0));
  else
    {
      ras = scm_vector (ras);
      for (; i0 < i0end; i0 += inc0, ++i)
        {
          SCM args = SCM_EOL;
          unsigned long k;
          for (k = scm_c_vector_length (ras); k--;)
            args = scm_cons (AREF (scm_c_vector_ref (ras, k), i), args);
          scm_apply_1 (proc, h0.vref (h0.vector, i0), args);
        }
    }
  scm_array_handle_release (&h0);
  return 1;
}

SCM_DEFINE (scm_array_for_each, "array-for-each", 2, 0, 1,
	    (SCM proc, SCM ra0, SCM lra),
	    "Apply @var{proc} to each tuple of elements of @var{ra0} @dots{}\n"
	    "in row-major order.  The value returned is unspecified.")
#define FUNC_NAME s_scm_array_for_each
{
  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);
  scm_ramapc (rafe, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
array_index_map_1 (SCM ra, SCM proc)
{
  scm_t_array_handle h;
  ssize_t i, inc;
  size_t p;
  scm_array_get_handle (ra, &h);
  inc = h.dims[0].inc;
  for (i = h.dims[0].lbnd, p = h.base; i <= h.dims[0].ubnd; ++i, p += inc)
    h.vset (h.vector, p, scm_call_1 (proc, scm_from_ulong (i)));
  scm_array_handle_release (&h);
}

/* Here we assume that the array is a scm_tc7_array, as that is the only
   kind of array in Guile that supports rank > 1.  */
static void
array_index_map_n (SCM ra, SCM proc)
{
  size_t i;
  SCM args = SCM_EOL;
  int j, k, kmax = SCM_I_ARRAY_NDIM (ra) - 1;
  long *vinds;

  vinds = scm_gc_malloc_pointerless (sizeof(long) * SCM_I_ARRAY_NDIM (ra),
                                     indices_gc_hint);

  for (k = 0; k <= kmax; k++)
    {
      vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
      if (vinds[k] > SCM_I_ARRAY_DIMS (ra)[k].ubnd)
        return;
    }
  k = kmax;
  do
    {
      if (k == kmax)
        {
          vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
          i = cind (ra, vinds);
          for (; vinds[k] <= SCM_I_ARRAY_DIMS (ra)[k].ubnd; vinds[k]++)
            {
              for (j = kmax + 1, args = SCM_EOL; j--;)
                args = scm_cons (scm_from_long (vinds[j]), args);
              ASET (SCM_I_ARRAY_V (ra), i, scm_apply_0 (proc, args));
              i += SCM_I_ARRAY_DIMS (ra)[k].inc;
            }
          k--;
        }
      else if (vinds[k] < SCM_I_ARRAY_DIMS (ra)[k].ubnd)
        {
          vinds[k]++;
          k++;
        }
      else
        {
          vinds[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd - 1;
          k--;
        }
    }
  while (k >= 0);
}

SCM_DEFINE (scm_array_index_map_x, "array-index-map!", 2, 0, 0,
	    (SCM ra, SCM proc),
	    "Apply @var{proc} to the indices of each element of @var{ra} in\n"
	    "turn, storing the result in the corresponding element.  The value\n"
	    "returned and the order of application are unspecified.\n\n"
	    "One can implement @var{array-indexes} as\n"
	    "@lisp\n"
	    "(define (array-indexes array)\n"
	    "    (let ((ra (apply make-array #f (array-shape array))))\n"
	    "      (array-index-map! ra (lambda x x))\n"
	    "      ra))\n"
	    "@end lisp\n"
	    "Another example:\n"
	    "@lisp\n"
	    "(define (apl:index-generator n)\n"
	    "    (let ((v (make-uniform-vector n 1)))\n"
	    "      (array-index-map! v (lambda (i) i))\n"
	    "      v))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_array_index_map_x
{
  SCM_VALIDATE_PROC (2, proc);

  switch (scm_c_array_rank (ra))
    {
    case 0:
      scm_array_set_x (ra, scm_call_0 (proc), SCM_EOL);
      break;
    case 1:
      array_index_map_1 (ra, proc);
      break;
    default:
      array_index_map_n (ra, proc);
      break;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
array_compare (scm_t_array_handle *hx, scm_t_array_handle *hy,
               size_t dim, unsigned long posx, unsigned long posy)
{
  if (dim == scm_array_handle_rank (hx))
    return scm_is_true (scm_equal_p (scm_array_handle_ref (hx, posx),
                                     scm_array_handle_ref (hy, posy)));
  else
    {
      long incx, incy;
      size_t i;

      if (hx->dims[dim].lbnd != hy->dims[dim].lbnd
          || hx->dims[dim].ubnd != hy->dims[dim].ubnd)
        return 0;

      i = hx->dims[dim].ubnd - hx->dims[dim].lbnd + 1;
      
      incx = hx->dims[dim].inc;
      incy = hy->dims[dim].inc;
      posx += (i - 1) * incx;
      posy += (i - 1) * incy;

      for (; i > 0; i--, posx -= incx, posy -= incy)
        if (!array_compare (hx, hy, dim + 1, posx, posy))
          return 0;
      return 1;
    }
}

SCM
scm_array_equal_p (SCM x, SCM y)
{
  scm_t_array_handle hx, hy;
  SCM res;  
  
  scm_array_get_handle (x, &hx);
  scm_array_get_handle (y, &hy);
  
  res = scm_from_bool (hx.ndims == hy.ndims
                       && hx.element_type == hy.element_type);

  if (scm_is_true (res))
    res = scm_from_bool (array_compare (&hx, &hy, 0, 0, 0));

  scm_array_handle_release (&hy);
  scm_array_handle_release (&hx);

  return res;
}

static SCM scm_i_array_equal_p (SCM, SCM, SCM);
SCM_DEFINE (scm_i_array_equal_p, "array-equal?", 0, 2, 1,
            (SCM ra0, SCM ra1, SCM rest),
	    "Return @code{#t} iff all arguments are arrays with the same\n"
	    "shape, the same type, and have corresponding elements which are\n"
	    "either @code{equal?}  or @code{array-equal?}.  This function\n"
	    "differs from @code{equal?} in that all arguments must be arrays.")
#define FUNC_NAME s_scm_i_array_equal_p
{
  if (SCM_UNBNDP (ra0) || SCM_UNBNDP (ra1))
    return SCM_BOOL_T;
  
  while (!scm_is_null (rest))
    { if (scm_is_false (scm_array_equal_p (ra0, ra1)))
        return SCM_BOOL_F;
      ra0 = ra1;
      ra1 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_array_equal_p (ra0, ra1);
}
#undef FUNC_NAME


void
scm_init_array_map (void)
{
#include "libguile/array-map.x"
  scm_add_feature (s_scm_array_for_each);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
