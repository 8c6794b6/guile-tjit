/*	Copyright (C) 1999 Free Software Foundation, Inc.
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


#include "_scm.h"

#include "eval.h"
#include "macros.h"

#include "lang.h"



/* {Multi-language support}
 */

/* Representation of pairs:
 *
 * Since we're going to share data with Scheme, we use EOL instead of nil
 * in all data structures.
 */

SCM_PROC (s_nil_cons, "nil-cons", 2, 0, 0, scm_nil_cons);

SCM 
scm_nil_cons (SCM x, SCM y)
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCAR (z, x);
  SCM_SETCDR (z, SCM_NIL2EOL (y, y));
  return z;
}


SCM_PROC (s_nil_car, "nil-car", 1, 0, 0, scm_nil_car);

SCM
scm_nil_car (SCM x)
{
  if (SCM_NILP (x))
    return scm_nil;
  SCM_ASSERT (SCM_NIMP (x) && SCM_CONSP (x), x, SCM_ARG1, s_nil_car);
  return SCM_CAR (x);
}

SCM_PROC (s_nil_cdr, "nil-cdr", 1, 0, 0, scm_nil_cdr);

SCM
scm_nil_cdr (SCM x)
{
  if (SCM_NILP (x))
    return scm_nil;
  SCM_ASSERT (SCM_NIMP (x) && SCM_CONSP (x), x, SCM_ARG1, s_nil_cdr);
  return SCM_EOL2NIL (SCM_CDR (x), x);
}

SCM_PROC (s_null, "null", 1, 0, 0, scm_null);

SCM
scm_null (SCM x)
{
  return (SCM_NILP (x) || SCM_NULLP (x) || SCM_FALSEP (x)) ? scm_t : scm_nil;
}

SCM
scm_m_while (SCM exp, SCM env)
{
  register SCM x = exp = SCM_CDR (exp);
  SCM z = scm_eval_car (x, env);
  while (!SCM_NILP (z) && SCM_NFALSEP (z))
    {
      while (SCM_NNULLP (x = SCM_CDR (x)))
	{
	  if (SCM_NIMP (SCM_CAR (x)))
	    (*scm_ceval_ptr) (SCM_CAR (x), env);
	}
      z = scm_eval_car (x = exp, env);
    }
  return scm_nil;
}

SCM_PROC1 (s_nil_eq, "nil-eq", scm_tc7_rpsubr, scm_nil_eq);

SCM
scm_nil_eq (SCM x, SCM y)
{
  return (((x==y)
	   || (SCM_NILP (x) && (SCM_NULLP (y) || SCM_FALSEP (y)))
	   || (SCM_NILP (y) && (SCM_NULLP (x) || SCM_FALSEP (x))))
	  ? scm_t
	  : scm_nil);
}



void
scm_init_lang ()
{
#include "lang.x"
  scm_make_synt ("nil-while", scm_makacro, scm_m_while);
}
