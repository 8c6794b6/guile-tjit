/*	Copyright (C) 1998 Free Software Foundation, Inc.
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

#include "evalext.h"

SCM_SYMBOL (scm_sym_setter, "setter");

SCM 
scm_m_generalized_set_x (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (2 == scm_ilength (x), xorig, scm_s_expression, scm_s_set_x);
  if (SCM_NIMP (SCM_CAR (x)) && SCM_SYMBOLP (SCM_CAR (x)))
    return scm_cons (SCM_IM_SET_X, x);
  else if (SCM_NIMP (SCM_CAR (x)) && SCM_CONSP (SCM_CAR (x)))
    return scm_cons (SCM_LIST2 (scm_sym_setter, SCM_CAAR (x)),
		     scm_append (SCM_LIST2 (SCM_CDAR (x), SCM_CDR (x))));
  return scm_wta (xorig, scm_s_variable, scm_s_set_x);
}

SCM_PROC (s_definedp, "defined?", 1, 1, 0, scm_definedp);

SCM 
scm_definedp (SCM sym, SCM env)
{
  SCM vcell;

  SCM_ASSERT (SCM_NIMP (sym) && SCM_SYMBOLP (sym), sym, SCM_ARG1, s_definedp);

  if (SCM_UNBNDP (env))
    vcell = scm_sym2vcell(sym,
			  SCM_CDR (scm_top_level_lookup_closure_var),
			  SCM_BOOL_F);
  else
    {
      SCM frames = env;
      register SCM b;
      for (; SCM_NIMP (frames); frames = SCM_CDR (frames))
	{
	  SCM_ASSERT (SCM_CONSP (frames), env, SCM_ARG2, s_definedp);
	  b = SCM_CAR (frames);
	  if (SCM_NFALSEP (scm_procedure_p (b)))
	    break;
	  SCM_ASSERT (SCM_NIMP (b) && SCM_CONSP (b),
		      env, SCM_ARG2, s_definedp);
	  for (b = SCM_CAR (b); SCM_NIMP (b); b = SCM_CDR (b))
	    {
	      if (SCM_NCONSP (b))
		{
		  if (b == sym)
		    return SCM_BOOL_T;
		  else
		    break;
		}
	      if (SCM_CAR (b) == sym)
		return SCM_BOOL_T;
	    }
	}
    vcell = scm_sym2vcell (sym,
			   SCM_NIMP (frames) ? SCM_CAR (frames) : SCM_BOOL_F,
			   SCM_BOOL_F);
    }
	      
  return (vcell == SCM_BOOL_F || SCM_UNBNDP (SCM_CDR (vcell))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}


SCM_SYNTAX (s_undefine, "undefine", scm_makacro, scm_m_undefine);

SCM
scm_m_undefine (x, env)
     SCM x, env;
{
  SCM arg1 = x;
  x = SCM_CDR (x);
  SCM_ASSYNT (SCM_TOP_LEVEL (env), arg1, "bad placement ", s_undefine);
  SCM_ASSYNT (SCM_NIMP (x) && SCM_CONSP (x) && SCM_CDR (x) == SCM_EOL,
	      arg1, scm_s_expression, s_undefine);
  x = SCM_CAR (x);
  SCM_ASSYNT (SCM_NIMP (x) && SCM_SYMBOLP (x), arg1, scm_s_variable, s_undefine);
  arg1 = scm_sym2vcell (x, scm_env_top_level (env), SCM_BOOL_F);
  SCM_ASSYNT (SCM_NFALSEP (arg1) && !SCM_UNBNDP (SCM_CDR (arg1)),
	      x, "variable already unbound ", s_undefine);
#if 0
#ifndef SCM_RECKLESS
  if (SCM_NIMP (SCM_CDR (arg1)) && ((SCM) SCM_SNAME (SCM_CDR (arg1)) == x))
    scm_warn ("undefining built-in ", SCM_CHARS (x));
  else
#endif
    if (5 <= scm_verbose && SCM_UNDEFINED != SCM_CDR (arg1))
      scm_warn ("redefining ", SCM_CHARS (x));
#endif
  SCM_SETCDR (arg1, SCM_UNDEFINED);
#ifdef SICP
  return SCM_CAR (arg1);
#else
  return SCM_UNSPECIFIED;
#endif
}

/* This name is obsolete.  Will be removed in 1.5.  */
SCM_PROC (s_serial_map, "serial-map", 2, 0, 1, scm_map);

SCM_PROC (s_map_in_order, "map-in-order", 2, 0, 1, scm_map);

void 
scm_init_evalext ()
{
  scm_make_synt (scm_s_set_x, scm_makmmacro, scm_m_generalized_set_x);
#include "evalext.x"
}
