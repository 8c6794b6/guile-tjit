/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
 *   2005,2006,2007,2008,2009,2010,2011,2012,2013,2014
 * Free Software Foundation, Inc.
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

#include <alloca.h>
#include <stdarg.h>

#include "libguile/__scm.h"

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/async.h"
#include "libguile/continuations.h"
#include "libguile/control.h"
#include "libguile/debug.h"
#include "libguile/deprecation.h"
#include "libguile/dynwind.h"
#include "libguile/eq.h"
#include "libguile/expand.h"
#include "libguile/feature.h"
#include "libguile/goops.h"
#include "libguile/hash.h"
#include "libguile/hashtab.h"
#include "libguile/list.h"
#include "libguile/macros.h"
#include "libguile/memoize.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/procprop.h"
#include "libguile/programs.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/strings.h"
#include "libguile/threads.h"
#include "libguile/throw.h"
#include "libguile/validate.h"
#include "libguile/values.h"
#include "libguile/vectors.h"
#include "libguile/vm.h"

#include "libguile/eval.h"
#include "libguile/private-options.h"




/* We have three levels of EVAL here:

   - eval (exp, env)

     evaluates EXP in environment ENV.  ENV is a lexical environment
     structure as used by the actual tree code evaluator.  When ENV is
     a top-level environment, then changes to the current module are
     tracked by updating ENV so that it continues to be in sync with
     the current module.

   - scm_primitive_eval (exp)

     evaluates EXP in the top-level environment as determined by the
     current module.  This is done by constructing a suitable
     environment and calling eval.  Thus, changes to the
     top-level module are tracked normally.

   - scm_eval (exp, mod)

     evaluates EXP while MOD is the current module. This is done
     by setting the current module to MOD_OR_STATE, invoking
     scm_primitive_eval on EXP, and then restoring the current module
     to the value it had previously.  That is, while EXP is evaluated,
     changes to the current module (or dynamic state) are tracked,
     but these changes do not persist when scm_eval returns.

*/


/* Boot closures. We only see these when compiling eval.scm, because once
   eval.scm is in the house, closures are standard VM closures.
 */

static scm_t_bits scm_tc16_boot_closure;
#define RETURN_BOOT_CLOSURE(code, env) \
  SCM_RETURN_NEWSMOB2 (scm_tc16_boot_closure, SCM_UNPACK (code), SCM_UNPACK (env))
#define BOOT_CLOSURE_P(obj) SCM_TYP16_PREDICATE (scm_tc16_boot_closure, (obj))
#define BOOT_CLOSURE_CODE(x) SCM_SMOB_OBJECT (x)
#define BOOT_CLOSURE_ENV(x) SCM_SMOB_OBJECT_2 (x)
#define BOOT_CLOSURE_BODY(x) CAR (BOOT_CLOSURE_CODE (x))
#define BOOT_CLOSURE_NUM_REQUIRED_ARGS(x) (SCM_I_INUM (CADDR (BOOT_CLOSURE_CODE (x))))
#define BOOT_CLOSURE_IS_FIXED(x)  (scm_is_null (CDDDR (BOOT_CLOSURE_CODE (x))))
/* NB: One may only call the following accessors if the closure is not FIXED. */
#define BOOT_CLOSURE_HAS_REST_ARGS(x) scm_is_true (CADDR (SCM_CDR (BOOT_CLOSURE_CODE (x))))
#define BOOT_CLOSURE_IS_REST(x) scm_is_null (SCM_CDR (CDDDR (BOOT_CLOSURE_CODE (x))))
/* NB: One may only call the following accessors if the closure is not REST. */
#define BOOT_CLOSURE_IS_FULL(x) (1)
#define BOOT_CLOSURE_PARSE_FULL(fu_,body,nargs,rest,nopt,kw,ninits,unbound,alt) \
  do { SCM fu = fu_;                                            \
    body = CAR (fu); fu = CDDR (fu);                            \
                                                                \
    rest = kw = alt = SCM_BOOL_F;                               \
    unbound = SCM_BOOL_F;                                       \
    nopt = ninits = 0;                                          \
                                                                \
    nreq = SCM_I_INUM (CAR (fu)); fu = CDR (fu);                \
    if (scm_is_pair (fu))                                       \
      {                                                         \
        rest = CAR (fu); fu = CDR (fu);                         \
        if (scm_is_pair (fu))                                   \
          {                                                     \
            nopt = SCM_I_INUM (CAR (fu)); fu = CDR (fu);        \
            kw = CAR (fu); fu = CDR (fu);                       \
            ninits = SCM_I_INUM (CAR (fu)); fu = CDR (fu);      \
            unbound = CAR (fu); fu = CDR (fu);                  \
            alt = CAR (fu);                                     \
          }                                                     \
      }                                                         \
  } while (0)
static void prepare_boot_closure_env_for_apply (SCM proc, SCM args,
                                                SCM *out_body, SCM *out_env);
static void prepare_boot_closure_env_for_eval (SCM proc, unsigned int argc,
                                               SCM exps, SCM *out_body,
                                               SCM *inout_env);


#define CAR(x)   SCM_CAR(x)
#define CDR(x)   SCM_CDR(x)
#define CAAR(x)  SCM_CAAR(x)
#define CADR(x)  SCM_CADR(x)
#define CDAR(x)  SCM_CDAR(x)
#define CDDR(x)  SCM_CDDR(x)
#define CADDR(x) SCM_CADDR(x)
#define CDDDR(x) SCM_CDDDR(x)

#define VECTOR_REF(v, i) (SCM_SIMPLE_VECTOR_REF (v, i))
#define VECTOR_SET(v, i, x) (SCM_SIMPLE_VECTOR_SET (v, i, x))
#define VECTOR_LENGTH(v) (SCM_SIMPLE_VECTOR_LENGTH (v))

static SCM
make_env (int n, SCM init, SCM next)
{
  SCM env = scm_c_make_vector (n + 1, init);
  VECTOR_SET (env, 0, next);
  return env;
}

static SCM
next_rib (SCM env)
{
  return VECTOR_REF (env, 0);
}

static SCM
env_tail (SCM env)
{
  while (SCM_I_IS_VECTOR (env))
    env = next_rib (env);
  return env;
}

static SCM
env_ref (SCM env, int depth, int width)
{
  while (depth--)
    env = next_rib (env);
  return VECTOR_REF (env, width + 1);
}

static void
env_set (SCM env, int depth, int width, SCM val)
{
  while (depth--)
    env = next_rib (env);
  VECTOR_SET (env, width + 1, val);
}


static void error_invalid_keyword (SCM proc, SCM obj)
{
  scm_error_scm (scm_from_latin1_symbol ("keyword-argument-error"), proc,
                 scm_from_locale_string ("Invalid keyword"), SCM_EOL,
                 scm_list_1 (obj));
}

static void error_unrecognized_keyword (SCM proc, SCM kw)
{
  scm_error_scm (scm_from_latin1_symbol ("keyword-argument-error"), proc,
                 scm_from_locale_string ("Unrecognized keyword"), SCM_EOL,
                 scm_list_1 (kw));
}


/* Multiple values truncation.  */
static SCM
truncate_values (SCM x)
{
  if (SCM_LIKELY (!SCM_VALUESP (x)))
    return x;
  else
    {
      SCM l = scm_struct_ref (x, SCM_INUM0);
      if (SCM_LIKELY (scm_is_pair (l)))
        return scm_car (l);
      else
        {
          scm_ithrow (scm_from_latin1_symbol ("vm-run"),
                      scm_list_3 (scm_from_latin1_symbol ("vm-run"),
                                  scm_from_locale_string
                                  ("Too few values returned to continuation"),
                                  SCM_EOL),
                      1);
          /* Not reached.  */
          return SCM_BOOL_F;
        }
    }
}
#define EVAL1(x, env) (truncate_values (eval ((x), (env))))

static SCM
eval (SCM x, SCM env)
{
  SCM mx;
  SCM proc = SCM_UNDEFINED, args = SCM_EOL;
  unsigned int argc;

 loop:
  SCM_TICK;
  
  mx = SCM_MEMOIZED_ARGS (x);
  switch (SCM_I_INUM (SCM_CAR (x)))
    {
    case SCM_M_SEQ:
      eval (CAR (mx), env);
      x = CDR (mx);
      goto loop;

    case SCM_M_IF:
      if (scm_is_true (EVAL1 (CAR (mx), env)))
        x = CADR (mx);
      else
        x = CDDR (mx);
      goto loop;

    case SCM_M_LET:
      {
        SCM inits = CAR (mx);
        SCM new_env;
        int i;

        new_env = make_env (VECTOR_LENGTH (inits), SCM_UNDEFINED, env);
        for (i = 0; i < VECTOR_LENGTH (inits); i++)
          env_set (new_env, 0, i, EVAL1 (VECTOR_REF (inits, i), env));
        env = new_env;
        x = CDR (mx);
        goto loop;
      }
          
    case SCM_M_LAMBDA:
      RETURN_BOOT_CLOSURE (mx, env);

    case SCM_M_CAPTURE_ENV:
      {
        SCM locs = CAR (mx);
        SCM new_env;
        int i;

        new_env = make_env (VECTOR_LENGTH (locs), SCM_BOOL_F, env);
        for (i = 0; i < VECTOR_LENGTH (locs); i++)
          {
            SCM loc = VECTOR_REF (locs, i);
            int depth, width;

            depth = SCM_I_INUM (CAR (loc));
            width = SCM_I_INUM (CDR (loc));
            env_set (new_env, 0, i, env_ref (env, depth, width));
          }

        env = new_env;
        x = CDR (mx);
        goto loop;
      }

    case SCM_M_QUOTE:
      return mx;

    case SCM_M_CAPTURE_MODULE:
      return eval (mx, scm_current_module ());

    case SCM_M_APPLY:
      /* Evaluate the procedure to be applied.  */
      proc = EVAL1 (CAR (mx), env);
      /* Evaluate the argument holding the list of arguments */
      args = EVAL1 (CADR (mx), env);
          
    apply_proc:
      /* Go here to tail-apply a procedure.  PROC is the procedure and
       * ARGS is the list of arguments. */
      if (BOOT_CLOSURE_P (proc))
        {
          prepare_boot_closure_env_for_apply (proc, args, &x, &env);
          goto loop;
        }
      else
        return scm_apply_0 (proc, args);

    case SCM_M_CALL:
      /* Evaluate the procedure to be applied.  */
      proc = EVAL1 (CAR (mx), env);
      argc = scm_ilength (CDR (mx));
      mx = CDR (mx);

      if (BOOT_CLOSURE_P (proc))
        {
          prepare_boot_closure_env_for_eval (proc, argc, mx, &x, &env);
          goto loop;
        }
      else
        {
	  SCM *argv;
	  unsigned int i;

	  argv = alloca (argc * sizeof (SCM));
	  for (i = 0; i < argc; i++, mx = CDR (mx))
	    argv[i] = EVAL1 (CAR (mx), env);

	  return scm_call_n (proc, argv, argc);
        }

    case SCM_M_CONT:
      return scm_i_call_with_current_continuation (EVAL1 (mx, env));

    case SCM_M_CALL_WITH_VALUES:
      {
        SCM producer;
        SCM v;

        producer = EVAL1 (CAR (mx), env);
        /* `proc' is the consumer.  */
        proc = EVAL1 (CDR (mx), env);
        v = scm_call_0 (producer);
        if (SCM_VALUESP (v))
          args = scm_struct_ref (v, SCM_INUM0);
        else
          args = scm_list_1 (v);
        goto apply_proc;
      }

    case SCM_M_LEXICAL_REF:
      {
        SCM pos;
        int depth, width;

        pos = mx;
        depth = SCM_I_INUM (CAR (pos));
        width = SCM_I_INUM (CDR (pos));

        return env_ref (env, depth, width);
      }

    case SCM_M_LEXICAL_SET:
      {
        SCM pos;
        int depth, width;
        SCM val = EVAL1 (CDR (mx), env);

        pos = CAR (mx);
        depth = SCM_I_INUM (CAR (pos));
        width = SCM_I_INUM (CDR (pos));

        env_set (env, depth, width, val);

        return SCM_UNSPECIFIED;
      }

    case SCM_M_BOX_REF:
      {
        SCM box = mx;

        return scm_variable_ref (EVAL1 (box, env));
      }

    case SCM_M_BOX_SET:
      {
        SCM box = CAR (mx), val = CDR (mx);

        return scm_variable_set_x (EVAL1 (box, env), EVAL1 (val, env));
      }

    case SCM_M_RESOLVE:
      if (SCM_VARIABLEP (mx))
        return mx;
      else
        {
          SCM var;

          var = scm_sys_resolve_variable (mx, env_tail (env));
          scm_set_cdr_x (x, var);

          return var;
        }

    case SCM_M_CALL_WITH_PROMPT:
      {
        struct scm_vm *vp;
        SCM k, handler, res;
        scm_i_jmp_buf registers;
        scm_t_ptrdiff saved_stack_depth;

        k = EVAL1 (CAR (mx), env);
        handler = EVAL1 (CDDR (mx), env);
        vp = scm_the_vm ();

        saved_stack_depth = vp->stack_top - vp->sp;

        /* Push the prompt onto the dynamic stack. */
        scm_dynstack_push_prompt (&SCM_I_CURRENT_THREAD->dynstack,
                                  SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY,
                                  k,
                                  vp->stack_top - vp->fp,
                                  saved_stack_depth,
                                  vp->ip,
                                  &registers);

        if (SCM_I_SETJMP (registers))
          {
            /* The prompt exited nonlocally. */
            scm_gc_after_nonlocal_exit ();
            proc = handler;
            args = scm_i_prompt_pop_abort_args_x (vp, saved_stack_depth);
            goto apply_proc;
          }
        
        res = scm_call_0 (eval (CADR (mx), env));
        scm_dynstack_pop (&SCM_I_CURRENT_THREAD->dynstack);
        return res;
      }

    default:
      abort ();
    }
}



/* Simple procedure calls
 */

SCM
scm_call_0 (SCM proc)
{
  return scm_call_n (proc, NULL, 0);
}

SCM
scm_call_1 (SCM proc, SCM arg1)
{
  return scm_call_n (proc, &arg1, 1);
}

SCM
scm_call_2 (SCM proc, SCM arg1, SCM arg2)
{
  SCM args[] = { arg1, arg2 };
  return scm_call_n (proc, args, 2);
}

SCM
scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  SCM args[] = { arg1, arg2, arg3 };
  return scm_call_n (proc, args, 3);
}

SCM
scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  SCM args[] = { arg1, arg2, arg3, arg4 };
  return scm_call_n (proc, args, 4);
}

SCM
scm_call_5 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  SCM args[] = { arg1, arg2, arg3, arg4, arg5 };
  return scm_call_n (proc, args, 5);
}

SCM
scm_call_6 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,
            SCM arg6)
{
  SCM args[] = { arg1, arg2, arg3, arg4, arg5, arg6 };
  return scm_call_n (proc, args, 6);
}

SCM
scm_call_7 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,
            SCM arg6, SCM arg7)
{
  SCM args[] = { arg1, arg2, arg3, arg4, arg5, arg6, arg7 };
  return scm_call_n (proc, args, 7);
}

SCM
scm_call_8 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,
            SCM arg6, SCM arg7, SCM arg8)
{
  SCM args[] = { arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 };
  return scm_call_n (proc, args, 8);
}

SCM
scm_call_9 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,
            SCM arg6, SCM arg7, SCM arg8, SCM arg9)
{
  SCM args[] = { arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 };
  return scm_call_n (proc, args, 9);
}

/* scm_call_n defined in vm.c */

SCM
scm_call (SCM proc, ...)
{
  va_list argp;
  SCM *argv = NULL;
  size_t i, nargs = 0;

  va_start (argp, proc);
  while (!SCM_UNBNDP (va_arg (argp, SCM)))
    nargs++;
  va_end (argp);

  argv = alloca (nargs * sizeof (SCM));
  va_start (argp, proc);
  for (i = 0; i < nargs; i++)
    argv[i] = va_arg (argp, SCM);
  va_end (argp);

  return scm_call_n (proc, argv, nargs);
}

/* Simple procedure applies
 */

SCM
scm_apply_0 (SCM proc, SCM args)
{
  SCM *argv;
  int i, nargs;

  nargs = scm_ilength (args);
  if (SCM_UNLIKELY (nargs < 0))
    scm_wrong_type_arg_msg ("apply", 2, args, "list");
  
  /* FIXME: Use vm_builtin_apply instead of alloca.  */
  argv = alloca (nargs * sizeof(SCM));
  for (i = 0; i < nargs; i++)
    {
      argv[i] = SCM_CAR (args);
      args = SCM_CDR (args);
    }

  return scm_call_n (proc, argv, nargs);
}

SCM
scm_apply_1 (SCM proc, SCM arg1, SCM args)
{
  return scm_apply_0 (proc, scm_cons (arg1, args));
}

SCM
scm_apply_2 (SCM proc, SCM arg1, SCM arg2, SCM args)
{
  return scm_apply_0 (proc, scm_cons2 (arg1, arg2, args));
}

SCM
scm_apply_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM args)
{
  return scm_apply_0 (proc, scm_cons (arg1, scm_cons2 (arg2, arg3, args)));
}

static SCM map_var, for_each_var;

static void init_map_var (void)
{
  map_var = scm_private_variable (scm_the_root_module (),
                                  scm_from_latin1_symbol ("map"));
}

static void init_for_each_var (void)
{
  for_each_var = scm_private_variable (scm_the_root_module (),
                                       scm_from_latin1_symbol ("for-each"));
}

SCM 
scm_map (SCM proc, SCM arg1, SCM args)
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_map_var);

  return scm_apply_0 (scm_variable_ref (map_var),
                      scm_cons (proc, scm_cons (arg1, args)));
}

SCM 
scm_for_each (SCM proc, SCM arg1, SCM args)
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_for_each_var);

  return scm_apply_0 (scm_variable_ref (for_each_var),
                      scm_cons (proc, scm_cons (arg1, args)));
}


static SCM
scm_c_primitive_eval (SCM exp)
{
  if (!SCM_EXPANDED_P (exp))
    exp = scm_call_1 (scm_current_module_transformer (), exp);
  return eval (scm_memoize_expression (exp), SCM_BOOL_F);
}

static SCM var_primitive_eval;
SCM
scm_primitive_eval (SCM exp)
{
  return scm_call_n (scm_variable_ref (var_primitive_eval),
                     &exp, 1);
}


/* Eval does not take the second arg optionally.  This is intentional
 * in order to be R5RS compatible, and to prepare for the new module
 * system, where we would like to make the choice of evaluation
 * environment explicit.  */

SCM_DEFINE (scm_eval, "eval", 2, 0, 0, 
	    (SCM exp, SCM module_or_state),
	    "Evaluate @var{exp}, a list representing a Scheme expression,\n"
            "in the top-level environment specified by\n"
	    "@var{module_or_state}.\n"
            "While @var{exp} is evaluated (using @code{primitive-eval}),\n"
            "@var{module_or_state} is made the current module when\n"
	    "it is a module, or the current dynamic state when it is\n"
	    "a dynamic state."
	    "Example: (eval '(+ 1 2) (interaction-environment))")
#define FUNC_NAME s_scm_eval
{
  SCM res;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  if (scm_is_dynamic_state (module_or_state))
    scm_dynwind_current_dynamic_state (module_or_state);
  else if (scm_module_system_booted_p)
    {
      SCM_VALIDATE_MODULE (2, module_or_state);
      scm_dynwind_current_module (module_or_state);
    }
  /* otherwise if the module system isn't booted, ignore the module arg */

  res = scm_primitive_eval (exp);

  scm_dynwind_end ();
  return res;
}
#undef FUNC_NAME


static SCM f_apply;

/* Apply a function to a list of arguments.

   This function's interface is a bit wonly.  It takes two required
   arguments and a tail argument, as if it were:

	(lambda (proc arg1 . args) ...)

   Usually you want to use scm_apply_0 or one of its cousins.  */

SCM 
scm_apply (SCM proc, SCM arg1, SCM args)
{
  return scm_apply_0 (proc,
                      scm_is_null (args) ? arg1 : scm_cons_star (arg1, args));
}

static void
prepare_boot_closure_env_for_apply (SCM proc, SCM args,
                                    SCM *out_body, SCM *out_env)
{
  int nreq = BOOT_CLOSURE_NUM_REQUIRED_ARGS (proc);
  SCM env = BOOT_CLOSURE_ENV (proc);
  int i;

  if (BOOT_CLOSURE_IS_FIXED (proc)
      || (BOOT_CLOSURE_IS_REST (proc)
          && !BOOT_CLOSURE_HAS_REST_ARGS (proc)))
    {
      if (SCM_UNLIKELY (scm_ilength (args) != nreq))
        scm_wrong_num_args (proc);

      env = make_env (nreq, SCM_UNDEFINED, env);
      for (i = 0; i < nreq; args = CDR (args), i++)
        env_set (env, 0, i, CAR (args));
      *out_body = BOOT_CLOSURE_BODY (proc);
      *out_env = env;
    }
  else if (BOOT_CLOSURE_IS_REST (proc))
    {
      if (SCM_UNLIKELY (scm_ilength (args) < nreq))
        scm_wrong_num_args (proc);

      env = make_env (nreq + 1, SCM_UNDEFINED, env);
      for (i = 0; i < nreq; args = CDR (args), i++)
        env_set (env, 0, i, CAR (args));
      env_set (env, 0, i++, args);

      *out_body = BOOT_CLOSURE_BODY (proc);
      *out_env = env;
    }
  else
    {
      int i, argc, nreq, nopt, ninits, nenv;
      SCM body, rest, kw, unbound, alt;
      SCM mx = BOOT_CLOSURE_CODE (proc);
      
    loop:
      BOOT_CLOSURE_PARSE_FULL (mx, body, nargs, rest, nopt, kw,
                               ninits, unbound, alt);

      argc = scm_ilength (args);
      if (argc < nreq)
        {
          if (scm_is_true (alt))
            {
              mx = alt;
              goto loop;
            }
          else
            scm_wrong_num_args (proc);
        }
      if (scm_is_false (kw) && argc > nreq + nopt && scm_is_false (rest))
        {
          if (scm_is_true (alt))
            {
              mx = alt;
              goto loop;
            }
          else
            scm_wrong_num_args (proc);
        }
      if (scm_is_true (kw) && scm_is_false (rest))
        {
          int npos = 0;
          SCM walk;
          for (walk = args; scm_is_pair (walk); walk = CDR (walk), npos++)
            if (npos >= nreq && scm_is_keyword (CAR (walk)))
              break;

          if (npos > nreq + nopt)
            {
              /* Too many positional args and no rest arg.  */
              if (scm_is_true (alt))
                {
                  mx = alt;
                  goto loop;
                }
              else
                scm_wrong_num_args (proc);
            }
        }

      /* At this point we are committed to the chosen clause.  */
      nenv = nreq + (scm_is_true (rest) ? 1 : 0) + ninits;
      env = make_env (nenv, unbound, env);

      for (i = 0; i < nreq; i++, args = CDR (args))
        env_set (env, 0, i, CAR (args));

      if (scm_is_false (kw))
        {
          /* Optional args (possibly), but no keyword args. */
          for (; i < argc && i < nreq + nopt; i++, args = CDR (args))
            env_set (env, 0, i, CAR (args));
          if (scm_is_true (rest))
            env_set (env, 0, nreq + nopt, args);
        }
      else
        {
          SCM aok;

          aok = CAR (kw);
          kw = CDR (kw);

          /* Optional args. As before, but stop at the first keyword. */
          for (; i < argc && i < nreq + nopt && !scm_is_keyword (CAR (args));
               i++, args = CDR (args))
            env_set (env, 0, i, CAR (args));
          if (scm_is_true (rest))
            env_set (env, 0, nreq + nopt, args);

          /* Parse keyword args. */
          {
            SCM walk;

            if (scm_is_pair (args) && scm_is_pair (CDR (args)))
              for (; scm_is_pair (args) && scm_is_pair (CDR (args));
                   args = CDR (args))
                {
                  SCM k = CAR (args), v = CADR (args);
                  if (!scm_is_keyword (k))
                    {
                      if (scm_is_true (rest))
                        continue;
                      else
                        break;
                    }
                  for (walk = kw; scm_is_pair (walk); walk = CDR (walk))
                    if (scm_is_eq (k, CAAR (walk)))
                      {
                        env_set (env, 0, SCM_I_INUM (CDAR (walk)), v);
                        args = CDR (args);
                        break;
                      }
                  if (scm_is_null (walk) && scm_is_false (aok))
                    error_unrecognized_keyword (proc, k);
                }
            if (scm_is_pair (args) && scm_is_false (rest))
              error_invalid_keyword (proc, CAR (args));
          }
        }

      *out_body = body;
      *out_env = env;
    }
}

static void
prepare_boot_closure_env_for_eval (SCM proc, unsigned int argc,
                                   SCM exps, SCM *out_body, SCM *inout_env)
{
  int nreq = BOOT_CLOSURE_NUM_REQUIRED_ARGS (proc);
  SCM new_env = BOOT_CLOSURE_ENV (proc);
  if ((BOOT_CLOSURE_IS_FIXED (proc)
       || (BOOT_CLOSURE_IS_REST (proc)
           && !BOOT_CLOSURE_HAS_REST_ARGS (proc)))
      && nreq == argc)
    {
      int i;

      new_env = make_env (nreq, SCM_UNDEFINED, new_env);
      for (i = 0; i < nreq; exps = CDR (exps), i++)
        env_set (new_env, 0, i, EVAL1 (CAR (exps), *inout_env));

      *out_body = BOOT_CLOSURE_BODY (proc);
      *inout_env = new_env;
    }
  else if (!BOOT_CLOSURE_IS_FIXED (proc) &&
           BOOT_CLOSURE_IS_REST (proc) && argc >= nreq)
    {
      SCM rest;
      int i;

      new_env = make_env (nreq + 1, SCM_UNDEFINED, new_env);
      for (i = 0; i < nreq; exps = CDR (exps), i++)
        env_set (new_env, 0, i, EVAL1 (CAR (exps), *inout_env));
      for (rest = SCM_EOL; scm_is_pair (exps); exps = CDR (exps))
        rest = scm_cons (EVAL1 (CAR (exps), *inout_env), rest);
      env_set (new_env, 0, i++, scm_reverse_x (rest, SCM_UNDEFINED));

      *out_body = BOOT_CLOSURE_BODY (proc);
      *inout_env = new_env;
    }
  else
    {
      SCM args = SCM_EOL;
      for (; scm_is_pair (exps); exps = CDR (exps))
        args = scm_cons (EVAL1 (CAR (exps), *inout_env), args);
      args = scm_reverse_x (args, SCM_UNDEFINED);
      prepare_boot_closure_env_for_apply (proc, args, out_body, inout_env);
    }
}

static SCM
boot_closure_apply (SCM closure, SCM args)
{
  SCM body, env;
  prepare_boot_closure_env_for_apply (closure, args, &body, &env);
  return eval (body, env);
}

static int
boot_closure_print (SCM closure, SCM port, scm_print_state *pstate)
{
  SCM args;
  scm_puts_unlocked ("#<boot-closure ", port);
  scm_uintprint (SCM_UNPACK (closure), 16, port);
  scm_putc_unlocked (' ', port);
  args = scm_make_list (scm_from_int (BOOT_CLOSURE_NUM_REQUIRED_ARGS (closure)),
                        scm_from_latin1_symbol ("_"));
  if (!BOOT_CLOSURE_IS_FIXED (closure) && BOOT_CLOSURE_HAS_REST_ARGS (closure))
    args = scm_cons_star (scm_from_latin1_symbol ("_"), args);
  /* FIXME: optionals and rests */
  scm_display (args, port);
  scm_putc_unlocked ('>', port);
  return 1;
}

void 
scm_init_eval ()
{
  SCM primitive_eval;

  f_apply = scm_c_define_gsubr ("apply", 2, 0, 1, scm_apply);

  scm_tc16_boot_closure = scm_make_smob_type ("boot-closure", 0);
  scm_set_smob_apply (scm_tc16_boot_closure, boot_closure_apply, 0, 0, 1);
  scm_set_smob_print (scm_tc16_boot_closure, boot_closure_print);

  primitive_eval = scm_c_make_gsubr ("primitive-eval", 1, 0, 0,
                                     scm_c_primitive_eval);
  var_primitive_eval = scm_define (SCM_SUBR_NAME (primitive_eval),
                                   primitive_eval);

#include "libguile/eval.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

