/* Debugging extensions for Guile
 * Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */

#include <stdio.h>
#include "_scm.h"
#include "eval.h"
#include "stackchk.h"
#include "throw.h"
#include "genio.h"
#include "macros.h"
#include "smob.h"
#include "procprop.h"
#include "srcprop.h"
#include "alist.h"
#include "continuations.h"
#include "strports.h"
#include "read.h"
#include "feature.h"
#include "dynwind.h"

#include "debug.h"


/* {Run time control of the debugging evaluator}
 */

SCM_PROC (s_debug_options, "debug-options-interface", 0, 1, 0, scm_debug_options);

SCM
scm_debug_options (setting)
     SCM setting;
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_debug_opts,
		     SCM_N_DEBUG_OPTIONS,
		     s_debug_options);
#ifndef SCM_RECKLESS
  if (!(1 <= SCM_N_FRAMES && SCM_N_FRAMES <= SCM_MAX_FRAME_SIZE))
    {
      scm_options (ans, scm_debug_opts, SCM_N_DEBUG_OPTIONS, s_debug_options);
      scm_out_of_range (s_debug_options, setting);
    }
#endif
  SCM_RESET_DEBUG_MODE;
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
  scm_debug_eframe_size = 2 * SCM_N_FRAMES;
  SCM_ALLOW_INTS
  return ans;
}

SCM_PROC (s_with_traps, "with-traps", 1, 0, 0, scm_with_traps);

static void
with_traps_before (void *data)
{
  int *trap_flag = data;
  *trap_flag = SCM_TRAPS_P;
  SCM_TRAPS_P = 1;
}

static void
with_traps_after (void *data)
{
  int *trap_flag = data;
  SCM_TRAPS_P = *trap_flag;
}

static SCM
with_traps_inner (void *data)
{
  SCM thunk = (SCM) data;
  return scm_apply (thunk, SCM_EOL, SCM_EOL);
}

SCM
scm_with_traps (SCM thunk)
{
  int trap_flag;
  SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)),
	      thunk,
	      SCM_ARG1,
	      s_with_traps);
  return scm_internal_dynamic_wind (with_traps_before,
				    with_traps_inner,
				    with_traps_after,
				    (void *) thunk,
				    &trap_flag);
}


static SCM scm_i_source, scm_i_more;
static SCM scm_i_proc, scm_i_args, scm_i_eval_args;
static SCM scm_i_procname;

/* {Memoized Source}
 */

long scm_tc16_memoized;


static int prinmemoized SCM_P ((SCM obj, SCM port, scm_print_state *pstate));

static int
prinmemoized (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<memoized ", port);
  SCM_SET_WRITINGP (pstate, 1);
#ifdef GUILE_DEBUG
  scm_iprin1 (SCM_MEMOIZED_EXP (obj), port, pstate);
#else
  scm_iprin1 (scm_unmemoize (obj), port, pstate);
#endif
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return 1;
}

static scm_smobfuns memoizedsmob =
{scm_markcdr, scm_free0, prinmemoized, 0};

SCM_PROC (s_memoized_p, "memoized?", 1, 0, 0, scm_memoized_p);

SCM
scm_memoized_p (obj)
     SCM obj;
{
  return SCM_NIMP (obj) && SCM_MEMOIZEDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
scm_make_memoized (exp, env)
     SCM exp;
     SCM env;
{
  /* *fixme* Check that env is a valid environment. */
  register SCM z, ans;
  SCM_ENTER_A_SECTION;
  SCM_NEWCELL (z);
  SCM_SETCDR (z, env);
  SCM_SETCAR (z, exp);
  SCM_NEWCELL (ans);
  SCM_SETCDR (ans, z);
  SCM_SETCAR (ans, scm_tc16_memoized);
  SCM_EXIT_A_SECTION;
  return ans;
}

#ifdef GUILE_DEBUG
/*
 * Some primitives for construction of memoized code
 *
 * - procedure: memcons CAR CDR [ENV]
 *
 *     Construct a pair, encapsulated in a memoized object.
 *
 *     The CAR and CDR can be either normal or memoized.  If ENV isn't
 *     specified, the top-level environment of the current module will
 *     be assumed.  All environments must match.
 *
 * - procedure: make-gloc VARIABLE [ENV]
 *
 *     Return a gloc, encapsulated in a memoized object.
 *
 *     (Glocs can't exist in normal list structures, since they will
 *     be mistaken for structs.)
 *
 * - procedure: gloc? OBJECT
 *
 *     Return #t if OBJECT is a memoized gloc.
 *
 * - procedure: make-iloc FRAME BINDING CDRP
 *
 *     Return an iloc referring to frame no. FRAME, binding
 *     no. BINDING.  If CDRP is non-#f, the iloc is referring to a
 *     frame consisting of a single pair, with the value stored in the
 *     CDR.
 *
 * - procedure: iloc? OBJECT
 *
 *     Return #t if OBJECT is an iloc.
 *
 * - procedure: mem->proc MEMOIZED
 *
 *     Construct a closure from the memoized lambda expression MEMOIZED
 *
 *     WARNING! The code is not copied!
 *
 * - procedure: proc->mem CLOSURE
 *
 *     Turn the closure CLOSURE into a memoized object.
 *
 *     WARNING! The code is not copied!
 *
 * - constant: SCM_IM_AND
 * - constant: SCM_IM_BEGIN
 * - constant: SCM_IM_CASE
 * - constant: SCM_IM_COND
 * - constant: SCM_IM_DO
 * - constant: SCM_IM_IF
 * - constant: SCM_IM_LAMBDA
 * - constant: SCM_IM_LET
 * - constant: SCM_IM_LETSTAR
 * - constant: SCM_IM_LETREC
 * - constant: SCM_IM_OR
 * - constant: SCM_IM_QUOTE
 * - constant: SCM_IM_SET
 * - constant: SCM_IM_DEFINE
 * - constant: SCM_IM_APPLY
 * - constant: SCM_IM_CONT
 * - constant: SCM_IM_DISPATCH
 */

#include "variable.h"
#include "procs.h"

SCM_PROC (s_make_gloc, "make-gloc", 1, 1, 0, scm_make_gloc);

SCM
scm_make_gloc (var, env)
     SCM var;
     SCM env;
{
#if 1 /* Unsafe */
  if (SCM_NIMP (var) && SCM_CONSP (var))
    var = scm_cons (SCM_BOOL_F, var);
  else
#endif
  SCM_ASSERT (SCM_NIMP (var) && SCM_VARIABLEP (var),
	      var,
	      SCM_ARG1,
	      s_make_gloc);
  if (SCM_UNBNDP (env))
    env = scm_top_level_env (SCM_CDR (scm_top_level_lookup_closure_var));
  else
    SCM_ASSERT (SCM_NULLP (env) || (SCM_NIMP (env) && SCM_CONSP (env)),
		env,
		SCM_ARG2,
		s_make_gloc);
  return scm_make_memoized (SCM_VARVCELL (var) + 1, env);
}

SCM_PROC (s_gloc_p, "gloc?", 1, 0, 0, scm_gloc_p);

SCM
scm_gloc_p (obj)
     SCM obj;
{
  return ((SCM_NIMP (obj)
	   && SCM_MEMOIZEDP (obj)
	   && (SCM_MEMOIZED_EXP (obj) & 7) == 1)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC (s_make_iloc, "make-iloc", 3, 0, 0, scm_make_iloc);

SCM
scm_make_iloc (frame, binding, cdrp)
     SCM frame;
     SCM binding;
     SCM cdrp;
{
  SCM_ASSERT (SCM_INUMP (frame), frame, SCM_ARG1, s_make_iloc);
  SCM_ASSERT (SCM_INUMP (binding), binding, SCM_ARG2, s_make_iloc);
  return (SCM_ILOC00
	  + SCM_IFRINC * SCM_INUM (frame)
	  + (SCM_NFALSEP (cdrp) ? SCM_ICDR : 0)
	  + SCM_IDINC * SCM_INUM (binding));
}

SCM_PROC (s_iloc_p, "iloc?", 1, 0, 0, scm_iloc_p);

SCM
scm_iloc_p (obj)
     SCM obj;
{
  return SCM_ILOCP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_memcons, "memcons", 2, 1, 0, scm_memcons);

SCM
scm_memcons (car, cdr, env)
     SCM car;
     SCM cdr;
     SCM env;
{
  if (SCM_NIMP (car) && SCM_MEMOIZEDP (car))
    {
      /*fixme* environments may be two different but equal top-level envs */
      if (!SCM_UNBNDP (env) && SCM_MEMOIZED_ENV (car) != env)
	scm_misc_error (s_memcons,
			"environment mismatch arg1 <-> arg3",
			scm_cons2 (car, env, SCM_EOL));
      else
	env = SCM_MEMOIZED_ENV (car);
      car = SCM_MEMOIZED_EXP (car);
    }
  if (SCM_NIMP (cdr) && SCM_MEMOIZEDP (cdr))
    {
      if (!SCM_UNBNDP (env) && SCM_MEMOIZED_ENV (cdr) != env)
	scm_misc_error (s_memcons,
			"environment mismatch arg2 <-> arg3",
			scm_cons2 (cdr, env, SCM_EOL));
      else
	env = SCM_MEMOIZED_ENV (cdr);
      cdr = SCM_MEMOIZED_EXP (cdr);
    }
  if (SCM_UNBNDP (env))
    env = scm_top_level_env (SCM_CDR (scm_top_level_lookup_closure_var));
  else
    SCM_ASSERT (SCM_NULLP (env) || (SCM_NIMP (env) && SCM_CONSP (env)),
		env,
		SCM_ARG3,
		s_make_iloc);
  return scm_make_memoized (scm_cons (car, cdr), env);
}

SCM_PROC (s_mem_to_proc, "mem->proc", 1, 0, 0, scm_mem_to_proc);

SCM
scm_mem_to_proc (obj)
     SCM obj;
{
  SCM env;
  SCM_ASSERT (SCM_NIMP (obj) && SCM_MEMOIZEDP (obj),
	      obj,
	      SCM_ARG1,
	      s_mem_to_proc);
  env = SCM_MEMOIZED_ENV (obj);
  obj = SCM_MEMOIZED_EXP (obj);
  if (!(SCM_NIMP (obj) && SCM_CAR (obj) == SCM_IM_LAMBDA))
    scm_misc_error (s_mem_to_proc,
		    "expected lambda expression",
		    scm_cons (obj, SCM_EOL));
  return scm_closure (SCM_CDR (obj), env);
}

SCM_PROC (s_proc_to_mem, "proc->mem", 1, 0, 0, scm_proc_to_mem);

SCM
scm_proc_to_mem (obj)
     SCM obj;
{
  SCM_ASSERT (SCM_NIMP (obj) && SCM_CLOSUREP (obj),
	      obj,
	      SCM_ARG1,
	      s_proc_to_mem);
  return scm_make_memoized (scm_cons (SCM_IM_LAMBDA, SCM_CODE (obj)),
			    SCM_ENV (obj));
}

#endif /* GUILE_DEBUG */

SCM_PROC (s_unmemoize, "unmemoize", 1, 0, 0, scm_unmemoize);

SCM
scm_unmemoize (m)
     SCM m;
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_MEMOIZEDP (m), m, SCM_ARG1, s_unmemoize);
  return scm_unmemocopy (SCM_MEMOIZED_EXP (m), SCM_MEMOIZED_ENV (m));
}

SCM_PROC (s_memoized_environment, "memoized-environment", 1, 0, 0, scm_memoized_environment);

SCM
scm_memoized_environment (m)
     SCM m;
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_MEMOIZEDP (m), m, SCM_ARG1, s_unmemoize);
  return SCM_MEMOIZED_ENV (m);
}

SCM_PROC (s_procedure_name, "procedure-name", 1, 0, 0, scm_procedure_name);

SCM
scm_procedure_name (proc)
     SCM proc;
{
  SCM_ASSERT(scm_procedure_p (proc) == SCM_BOOL_T,
	     proc,
	     SCM_ARG1,
	     s_procedure_name);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
  case scm_tc7_cclo:
    {
      SCM name = scm_procedure_property (proc, scm_i_name);
#if 0
      /* Source property scm_i_procname not implemented yet... */
      SCM name = scm_source_property (SCM_CAR (SCM_CDR (SCM_CODE (proc))), scm_i_procname);
      if (SCM_FALSEP (name))
	name = scm_procedure_property (proc, scm_i_name);
#endif
      if (SCM_FALSEP (name))
	name = scm_procedure_property (proc, scm_i_inner_name);
      return name;
    }
  case scm_tcs_subrs:
    return SCM_SNAME (proc);
  default:
    return SCM_BOOL_F;
  }
}

SCM_PROC (s_procedure_source, "procedure-source", 1, 0, 0, scm_procedure_source);

SCM
scm_procedure_source (proc)
     SCM proc;
{
  SCM_ASSERT(SCM_NIMP (proc), proc, SCM_ARG1, s_procedure_source);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
    {
      SCM src;
      src = scm_source_property (SCM_CDR (SCM_CODE (proc)), scm_i_copy);
      if (src != SCM_BOOL_F)
	return scm_cons2 (scm_i_lambda, SCM_CAR (SCM_CODE (proc)), src);
      src = SCM_CODE (proc);
      return scm_cons (scm_i_lambda,
		       scm_unmemocopy (src,
				       SCM_EXTEND_ENV (SCM_CAR (src),
							   SCM_EOL,
							   SCM_ENV (proc))));
    }
  case scm_tc7_contin:
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
    /* It would indeed be a nice thing if we supplied source even for
       built in procedures! */
    return scm_procedure_property (proc, scm_i_source);
  default:
    scm_wta (proc, (char *) SCM_ARG1, s_procedure_source);
    return 0;
  }
}

SCM_PROC (s_procedure_environment, "procedure-environment", 1, 0, 0, scm_procedure_environment);

SCM
scm_procedure_environment (proc)
     SCM proc;
{
  SCM_ASSERT (SCM_NIMP (proc), proc, SCM_ARG1, s_procedure_environment);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
    return SCM_ENV (proc);
  case scm_tc7_contin:
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
    return SCM_EOL;
  default:
    scm_wta (proc, (char *) SCM_ARG1, s_procedure_environment);
    return 0;
  }
}



/* Eval in a local environment.  We would like to have the ability to
 * evaluate in a specified local environment, but due to the
 * memoization this isn't normally possible.  We solve it by copying
 * the code before evaluating.  One solution would be to have eval.c
 * generate yet another evaluator.  They are not very big actually.
 */
SCM_PROC (s_local_eval, "local-eval", 1, 1, 0, scm_local_eval);

SCM
scm_local_eval (exp, env)
     SCM exp;
     SCM env;
{
  if (SCM_UNBNDP (env))
  {
    SCM_ASSERT (SCM_NIMP (exp) && SCM_MEMOIZEDP (exp), exp, SCM_ARG1, s_local_eval);
    return scm_eval_3 (SCM_MEMOIZED_EXP (exp), 0, SCM_MEMOIZED_ENV (exp));
  }
  return scm_eval_3 (exp, 1, env);
}

SCM
scm_start_stack (id, exp, env)
     SCM id;
     SCM exp;
     SCM env;
{
  SCM answer;
  scm_debug_frame vframe;
  scm_debug_info vframe_vect_body;
  vframe.prev = scm_last_debug_frame;
  vframe.status = SCM_VOIDFRAME;
  vframe.vect = &vframe_vect_body;
  vframe.vect[0].id = id;
  scm_last_debug_frame = &vframe;
  answer = scm_eval_3 (exp, 1, env);
  scm_last_debug_frame = vframe.prev;
  return answer;
}

SCM_SYNTAX(s_start_stack, "start-stack", scm_makacro, scm_m_start_stack);

static SCM
scm_m_start_stack (exp, env)
     SCM exp;
     SCM env;
{
  exp = SCM_CDR (exp);
  SCM_ASSERT (SCM_NIMP (exp)
	      && SCM_ECONSP (exp)
	      && SCM_NIMP (SCM_CDR (exp))
	      && SCM_ECONSP (SCM_CDR (exp))
	      && SCM_NULLP (SCM_CDDR (exp)),
	      exp,
	      SCM_WNA,
	      s_start_stack);
  return scm_start_stack (scm_eval_car (exp, env), SCM_CADR (exp), env);
}

/* {Debug Objects}
 *
 * The debugging evaluator throws these on frame traps.
 */

long scm_tc16_debugobj;

static int prindebugobj SCM_P ((SCM obj, SCM port, scm_print_state *pstate));

static int
prindebugobj (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts ("#<debug-object ", port);
  scm_intprint (SCM_DEBUGOBJ_FRAME (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

static scm_smobfuns debugobjsmob =
{0, scm_free0, prindebugobj, 0};

SCM_PROC (s_debug_object_p, "debug-object?", 1, 0, 0, scm_debug_object_p);

SCM
scm_debug_object_p (obj)
     SCM obj;
{
  return SCM_NIMP (obj) && SCM_DEBUGOBJP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM
scm_make_debugobj (frame)
     scm_debug_frame *frame;
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_ENTER_A_SECTION;
  SCM_SET_DEBUGOBJ_FRAME (z, (SCM) frame);
  SCM_SETCAR (z, scm_tc16_debugobj);
  SCM_EXIT_A_SECTION;
  return z;
}



/* Undocumented debugging procedure */
#ifdef GUILE_DEBUG
SCM_PROC (s_debug_hang, "debug-hang", 0, 1, 0, scm_debug_hang);

SCM
scm_debug_hang (obj)
     SCM obj;
{
  int go = 0;
  while (!go) ;
  return SCM_UNSPECIFIED;
}
#endif



void
scm_init_debug ()
{
  scm_init_opts (scm_debug_options, scm_debug_opts, SCM_N_DEBUG_OPTIONS);

  scm_tc16_memoized = scm_newsmob (&memoizedsmob);
  scm_tc16_debugobj = scm_newsmob (&debugobjsmob);

  scm_i_procname = SCM_CAR (scm_sysintern ("procname", SCM_UNDEFINED));
  scm_i_more = SCM_CAR (scm_sysintern ("...", SCM_UNDEFINED));
  scm_i_source = SCM_CAR (scm_sysintern ("source", SCM_UNDEFINED));
  scm_i_proc = SCM_CAR (scm_sysintern ("proc", SCM_UNDEFINED));
  scm_i_args = SCM_CAR (scm_sysintern ("args", SCM_UNDEFINED));
  scm_i_eval_args = SCM_CAR (scm_sysintern ("eval-args", SCM_UNDEFINED));

#ifdef GUILE_DEBUG
  scm_sysintern ("SCM_IM_AND", SCM_IM_AND);
  scm_sysintern ("SCM_IM_BEGIN", SCM_IM_BEGIN);
  scm_sysintern ("SCM_IM_CASE", SCM_IM_CASE);
  scm_sysintern ("SCM_IM_COND", SCM_IM_COND);
  scm_sysintern ("SCM_IM_DO", SCM_IM_DO);
  scm_sysintern ("SCM_IM_IF", SCM_IM_IF);
  scm_sysintern ("SCM_IM_LAMBDA", SCM_IM_LAMBDA);
  scm_sysintern ("SCM_IM_LET", SCM_IM_LET);
  scm_sysintern ("SCM_IM_LETSTAR", SCM_IM_LETSTAR);
  scm_sysintern ("SCM_IM_LETREC", SCM_IM_LETREC);
  scm_sysintern ("SCM_IM_OR", SCM_IM_OR);
  scm_sysintern ("SCM_IM_QUOTE", SCM_IM_QUOTE);
  scm_sysintern ("SCM_IM_SET_X", SCM_IM_SET_X);
  scm_sysintern ("SCM_IM_DEFINE", SCM_IM_DEFINE);
  scm_sysintern ("SCM_IM_APPLY", SCM_IM_APPLY);
  scm_sysintern ("SCM_IM_CONT", SCM_IM_CONT);
  scm_sysintern ("SCM_IM_DISPATCH", SCM_IM_DISPATCH);
#endif
  scm_add_feature ("debug-extensions");

#include "debug.x"
}
