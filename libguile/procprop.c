/*	Copyright (C) 1995,1996,1998 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "_scm.h"

#include "alist.h"
#include "eval.h"
#include "procs.h"
#include "gsubr.h"
#include "objects.h"
#include "root.h"
#include "vectors.h"

#include "validate.h"
#include "procprop.h"


SCM_GLOBAL_SYMBOL (scm_sym_system_procedure, "system-procedure");
SCM_GLOBAL_SYMBOL (scm_sym_arity, "arity");

SCM
scm_i_procedure_arity (SCM proc)
{
  int a = 0, o = 0, r = 0;
  if (SCM_IMP (proc))
    return SCM_BOOL_F;
 loop:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_1o:
      o = 1;
    case scm_tc7_subr_0:
      break;
    case scm_tc7_subr_2o:
      o = 1;
    case scm_tc7_subr_1:
    case scm_tc7_cxr:
    case scm_tc7_contin:
      a += 1;
      break;
    case scm_tc7_subr_2:
      a += 2;
      break;
    case scm_tc7_subr_3:
      a += 3;
      break;
    case scm_tc7_asubr:
    case scm_tc7_rpsubr:
    case scm_tc7_lsubr:
      r = 1;
      break;
    case scm_tc7_lsubr_2:
      a += 2;
      r = 1;
      break;
#ifdef CCLO
    case scm_tc7_cclo:
      if (SCM_CCLO_SUBR (proc) == scm_f_gsubr_apply)
	{
	  int type = SCM_INUM (SCM_GSUBR_TYPE (proc));
	  a += SCM_GSUBR_REQ (type);
	  o = SCM_GSUBR_OPT (type);
	  r = SCM_GSUBR_REST (type);
	  break;
	}
      proc = SCM_CCLO_SUBR (proc);
      a -= 1;
      goto loop;
#endif
    case scm_tc7_pws:
      proc = SCM_PROCEDURE (proc);
      goto loop;
    case scm_tcs_closures:
      proc = SCM_CAR (SCM_CODE (proc));
      if (SCM_IMP (proc))
	break;
      while (SCM_CONSP (proc))
	{
	  ++a;
	  proc = SCM_CDR (proc);
	}
      if (SCM_NIMP (proc))
	r = 1;
      break;
    case scm_tcs_cons_gloc:
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	{
	  r = 1;
	  break;
	}
      else if (!SCM_I_OPERATORP (proc))
	return SCM_BOOL_F;
      proc = (SCM_I_ENTITYP (proc)
	      ? SCM_ENTITY_PROCEDURE (proc)
	      : SCM_OPERATOR_PROCEDURE (proc));
      a -= 1;
      goto loop;
    default:
      return SCM_BOOL_F;
    }
  return SCM_LIST3 (SCM_MAKINUM (a),
		    SCM_MAKINUM (o),
		    SCM_BOOL(r));
}

static SCM
scm_stand_in_scm_proc(SCM proc)
{
  SCM answer;
  answer = scm_assoc (proc, scm_stand_in_procs);
  if (answer == SCM_BOOL_F)
    {
      answer = scm_closure (scm_listify (SCM_EOL, SCM_BOOL_F, SCM_UNDEFINED),
			    SCM_EOL);
      scm_stand_in_procs = scm_cons (scm_cons (proc, answer),
				     scm_stand_in_procs);
    }
  else
    answer = SCM_CDR (answer);
  return answer;
}

SCM_DEFINE (scm_procedure_properties, "procedure-properties", 1, 0, 0, 
           (SCM proc),
	    "Return @var{obj}'s property list.")
#define FUNC_NAME s_scm_procedure_properties
{
  SCM_VALIDATE_PROC (1,proc);
  return scm_acons (scm_sym_arity, scm_i_procedure_arity (proc),
		    SCM_PROCPROPS (SCM_CLOSUREP (proc)
				   ? proc
				   : scm_stand_in_scm_proc (proc)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0,
           (SCM proc, SCM new_val),
	    "Set @var{obj}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_procedure_properties_x
{
  if (!SCM_CLOSUREP (proc))
    proc = scm_stand_in_scm_proc(proc);
  SCM_VALIDATE_CLOSURE (1,proc);
  SCM_SETPROCPROPS (proc, new_val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_property, "procedure-property", 2, 0, 0,
           (SCM p, SCM k),
	    "Return the property of @var{obj} with name @var{key}.")
#define FUNC_NAME s_scm_procedure_property
{
  SCM assoc;
  if (k == scm_sym_arity)
    {
      SCM arity;
      SCM_ASSERT (SCM_NFALSEP (arity = scm_i_procedure_arity (p)),
		  p, SCM_ARG1, FUNC_NAME);
      return arity;
    }
  SCM_VALIDATE_PROC (1,p);
  assoc = scm_sloppy_assq (k,
			   SCM_PROCPROPS (SCM_CLOSUREP (p)
					  ? p
					  : scm_stand_in_scm_proc (p)));
  return (SCM_NIMP (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_property_x, "set-procedure-property!", 3, 0, 0,
           (SCM p, SCM k, SCM v),
	    "In @var{obj}'s property list, set the property named @var{key} to\n"
	    "@var{value}.")
#define FUNC_NAME s_scm_set_procedure_property_x
{
  SCM assoc;
  if (!SCM_CLOSUREP (p))
    p = scm_stand_in_scm_proc(p);
  SCM_VALIDATE_CLOSURE (1,p);
  if (k == scm_sym_arity)
    SCM_MISC_ERROR ("arity is a read-only property", SCM_EOL);
  assoc = scm_sloppy_assq (k, SCM_PROCPROPS (p));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, v);
  else
    SCM_SETPROCPROPS (p, scm_acons (k, v, SCM_PROCPROPS (p)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_init_procprop ()
{
#include "procprop.x"
}

