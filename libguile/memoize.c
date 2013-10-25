/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013
 *   Free Software Foundation, Inc.
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

#include "libguile/__scm.h"
#include "libguile/_scm.h"
#include "libguile/continuations.h"
#include "libguile/eq.h"
#include "libguile/expand.h"
#include "libguile/list.h"
#include "libguile/macros.h"
#include "libguile/memoize.h"
#include "libguile/modules.h"
#include "libguile/srcprop.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/strings.h"
#include "libguile/throw.h"
#include "libguile/validate.h"





#define CAR(x)   SCM_CAR(x)
#define CDR(x)   SCM_CDR(x)
#define CAAR(x)  SCM_CAAR(x)
#define CADR(x)  SCM_CADR(x)
#define CDAR(x)  SCM_CDAR(x)
#define CDDR(x)  SCM_CDDR(x)
#define CADDR(x) SCM_CADDR(x)
#define CDDDR(x) SCM_CDDDR(x)
#define CADDDR(x) SCM_CADDDR(x)

#define VECTOR_REF(v, i) (SCM_SIMPLE_VECTOR_REF (v, i))
#define VECTOR_SET(v, i, x) (SCM_SIMPLE_VECTOR_SET (v, i, x))
#define VECTOR_LENGTH(v) (SCM_SIMPLE_VECTOR_LENGTH (v))

SCM_SYMBOL (sym_case_lambda_star, "case-lambda*");




/* Primitives not exposed to general Scheme. */
static SCM wind;
static SCM unwind;
static SCM push_fluid;
static SCM pop_fluid;

static SCM
do_wind (SCM in, SCM out)
{
  scm_dynstack_push_dynwind (&SCM_I_CURRENT_THREAD->dynstack, in, out);
  return SCM_UNSPECIFIED;
}

static SCM
do_unwind (void)
{
  scm_dynstack_pop (&SCM_I_CURRENT_THREAD->dynstack);
  return SCM_UNSPECIFIED;
}

static SCM
do_push_fluid (SCM fluid, SCM val)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_dynstack_push_fluid (&thread->dynstack, fluid, val,
                           thread->dynamic_state);
  return SCM_UNSPECIFIED;
}

static SCM
do_pop_fluid (void)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_dynstack_unwind_fluid (&thread->dynstack, thread->dynamic_state);
  return SCM_UNSPECIFIED;
}




/* {Evaluator memoized expressions}
 */

scm_t_bits scm_tc16_memoized;

#define MAKMEMO(n, args)                                                \
  (scm_cell (scm_tc16_memoized | ((n) << 16), SCM_UNPACK (args)))

#define MAKMEMO_SEQ(head,tail) \
  MAKMEMO (SCM_M_SEQ, scm_cons (head, tail))
#define MAKMEMO_IF(test, then, else_) \
  MAKMEMO (SCM_M_IF, scm_cons (test, scm_cons (then, else_)))
#define FIXED_ARITY(nreq) \
  scm_list_1 (SCM_I_MAKINUM (nreq))
#define REST_ARITY(nreq, rest) \
  scm_list_2 (SCM_I_MAKINUM (nreq), rest)
#define FULL_ARITY(nreq, rest, nopt, kw, inits, alt) \
  scm_list_n (SCM_I_MAKINUM (nreq), rest, SCM_I_MAKINUM (nopt), kw, inits, \
              alt, SCM_UNDEFINED)
#define MAKMEMO_LAMBDA(body, arity, docstring)			\
  MAKMEMO (SCM_M_LAMBDA,					\
	   scm_cons (body, scm_cons (docstring, arity)))
#define MAKMEMO_LET(inits, body) \
  MAKMEMO (SCM_M_LET, scm_cons (inits, body))
#define MAKMEMO_QUOTE(exp) \
  MAKMEMO (SCM_M_QUOTE, exp)
#define MAKMEMO_DEFINE(var, val) \
  MAKMEMO (SCM_M_DEFINE, scm_cons (var, val))
#define MAKMEMO_APPLY(proc, args)\
  MAKMEMO (SCM_M_APPLY, scm_list_2 (proc, args))
#define MAKMEMO_CONT(proc) \
  MAKMEMO (SCM_M_CONT, proc)
#define MAKMEMO_CALL_WITH_VALUES(prod, cons) \
  MAKMEMO (SCM_M_CALL_WITH_VALUES, scm_cons (prod, cons))
#define MAKMEMO_CALL(proc, nargs, args) \
  MAKMEMO (SCM_M_CALL, scm_cons (proc, scm_cons (SCM_I_MAKINUM (nargs), args)))
#define MAKMEMO_LEX_REF(pos) \
  MAKMEMO (SCM_M_LEXICAL_REF, pos)
#define MAKMEMO_LEX_SET(pos, val)                                      \
  MAKMEMO (SCM_M_LEXICAL_SET, scm_cons (pos, val))
#define MAKMEMO_TOP_REF(var) \
  MAKMEMO (SCM_M_TOPLEVEL_REF, var)
#define MAKMEMO_TOP_SET(var, val) \
  MAKMEMO (SCM_M_TOPLEVEL_SET, scm_cons (var, val))
#define MAKMEMO_MOD_REF(mod, var, public) \
  MAKMEMO (SCM_M_MODULE_REF, scm_cons (mod, scm_cons (var, public)))
#define MAKMEMO_MOD_SET(val, mod, var, public) \
  MAKMEMO (SCM_M_MODULE_SET, scm_cons (val, scm_cons (mod, scm_cons (var, public))))
#define MAKMEMO_CALL_WITH_PROMPT(tag, thunk, handler) \
  MAKMEMO (SCM_M_CALL_WITH_PROMPT, scm_cons (tag, scm_cons (thunk, handler)))




/* This table must agree with the list of M_ constants in memoize.h */
static const char *const memoized_tags[] =
{
  "seq",
  "if",
  "lambda",
  "let",
  "quote",
  "define",
  "apply",
  "call/cc",
  "call-with-values",
  "call",
  "lexical-ref",
  "lexical-set!",
  "toplevel-ref",
  "toplevel-set!",
  "module-ref",
  "module-set!",
  "call-with-prompt",
};

static int
scm_print_memoized (SCM memoized, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<memoized ", port);
  scm_write (scm_unmemoize_expression (memoized), port);
  scm_puts_unlocked (">", port);
  return 1;
}





static int
try_lookup_rib (SCM x, SCM rib)
{
  int idx = 0;
  for (; idx < VECTOR_LENGTH (rib); idx++)
    if (scm_is_eq (x, VECTOR_REF (rib, idx)))
      return idx; /* bound */
  return -1;
}

static int
lookup_rib (SCM x, SCM rib)
{
  int idx = try_lookup_rib (x, rib);
  if (idx < 0)
    abort ();
  return idx;
}

static SCM
make_pos (int depth, int width)
{
  return scm_cons (SCM_I_MAKINUM (depth), SCM_I_MAKINUM (width));
}

static SCM
lookup (SCM x, SCM env)
{
  int d = 0;
  for (; scm_is_pair (env); env = CDR (env), d++)
    {
      int w = try_lookup_rib (x, CAR (env));
      if (w < 0)
        continue;
      return make_pos (d, w);
    }
  abort ();
}

/* Abbreviate SCM_EXPANDED_REF. Copied because I'm not sure about symbol pasting */
#define REF(x,type,field) \
  (scm_struct_ref (x, SCM_I_MAKINUM (SCM_EXPANDED_##type##_##field)))

static SCM list_of_guile = SCM_BOOL_F;

static SCM memoize (SCM exp, SCM env);

static SCM
memoize_exps (SCM exps, SCM env)
{
  SCM ret;
  for (ret = SCM_EOL; scm_is_pair (exps); exps = CDR (exps))
    ret = scm_cons (memoize (CAR (exps), env), ret);
  return scm_reverse_x (ret, SCM_UNDEFINED);
}
  
static SCM
memoize (SCM exp, SCM env)
{
  if (!SCM_EXPANDED_P (exp))
    abort ();

  switch (SCM_EXPANDED_TYPE (exp))
    {
    case SCM_EXPANDED_VOID:
      return MAKMEMO_QUOTE (SCM_UNSPECIFIED);
      
    case SCM_EXPANDED_CONST:
      return MAKMEMO_QUOTE (REF (exp, CONST, EXP));

    case SCM_EXPANDED_PRIMITIVE_REF:
      if (scm_is_eq (scm_current_module (), scm_the_root_module ()))
        return MAKMEMO_TOP_REF (REF (exp, PRIMITIVE_REF, NAME));
      else
        return MAKMEMO_MOD_REF (list_of_guile, REF (exp, PRIMITIVE_REF, NAME),
                                SCM_BOOL_F);
                                
    case SCM_EXPANDED_LEXICAL_REF:
      return MAKMEMO_LEX_REF (lookup (REF (exp, LEXICAL_REF, GENSYM), env));

    case SCM_EXPANDED_LEXICAL_SET:
      return MAKMEMO_LEX_SET (lookup (REF (exp, LEXICAL_SET, GENSYM), env),
                              memoize (REF (exp, LEXICAL_SET, EXP), env));

    case SCM_EXPANDED_MODULE_REF:
      return MAKMEMO_MOD_REF (REF (exp, MODULE_REF, MOD),
                              REF (exp, MODULE_REF, NAME),
                              REF (exp, MODULE_REF, PUBLIC));

    case SCM_EXPANDED_MODULE_SET:
      return MAKMEMO_MOD_SET (memoize (REF (exp, MODULE_SET, EXP), env),
                              REF (exp, MODULE_SET, MOD),
                              REF (exp, MODULE_SET, NAME),
                              REF (exp, MODULE_SET, PUBLIC));

    case SCM_EXPANDED_TOPLEVEL_REF:
      return MAKMEMO_TOP_REF (REF (exp, TOPLEVEL_REF, NAME));

    case SCM_EXPANDED_TOPLEVEL_SET:
      return MAKMEMO_TOP_SET (REF (exp, TOPLEVEL_SET, NAME),
                              memoize (REF (exp, TOPLEVEL_SET, EXP), env));

    case SCM_EXPANDED_TOPLEVEL_DEFINE:
      return MAKMEMO_DEFINE (REF (exp, TOPLEVEL_DEFINE, NAME),
                             memoize (REF (exp, TOPLEVEL_DEFINE, EXP), env));

    case SCM_EXPANDED_CONDITIONAL:
      return MAKMEMO_IF (memoize (REF (exp, CONDITIONAL, TEST), env),
                         memoize (REF (exp, CONDITIONAL, CONSEQUENT), env),
                         memoize (REF (exp, CONDITIONAL, ALTERNATE), env));

    case SCM_EXPANDED_CALL:
      {
        SCM proc, args;

        proc = REF (exp, CALL, PROC);
        args = memoize_exps (REF (exp, CALL, ARGS), env);

        return MAKMEMO_CALL (memoize (proc, env), scm_ilength (args), args);
      }

    case SCM_EXPANDED_PRIMCALL:
      {
        SCM name, args;
        int nargs;

        name = REF (exp, PRIMCALL, NAME);
        args = memoize_exps (REF (exp, PRIMCALL, ARGS), env);
        nargs = scm_ilength (args);

        if (nargs == 3
            && scm_is_eq (name, scm_from_latin1_symbol ("call-with-prompt")))
          return MAKMEMO_CALL_WITH_PROMPT (CAR (args),
                                           CADR (args),
                                           CADDR (args));
        else if (nargs == 2
                 && scm_is_eq (name, scm_from_latin1_symbol ("apply")))
          return MAKMEMO_APPLY (CAR (args), CADR (args));
        else if (nargs == 1
                 && scm_is_eq (name,
                               scm_from_latin1_symbol
                               ("call-with-current-continuation")))
          return MAKMEMO_CONT (CAR (args));
        else if (nargs == 2
                 && scm_is_eq (name,
                               scm_from_latin1_symbol ("call-with-values")))
          return MAKMEMO_CALL_WITH_VALUES (CAR (args), CADR (args));
        else if (nargs == 2
                 && scm_is_eq (name, scm_from_latin1_symbol ("wind")))
          return MAKMEMO_CALL (MAKMEMO_QUOTE (wind), 2, args);
        else if (nargs == 0
                 && scm_is_eq (name, scm_from_latin1_symbol ("unwind")))
          return MAKMEMO_CALL (MAKMEMO_QUOTE (unwind), 0, SCM_EOL);
        else if (nargs == 2
                 && scm_is_eq (name, scm_from_latin1_symbol ("push-fluid")))
          return MAKMEMO_CALL (MAKMEMO_QUOTE (push_fluid), 2, args);
        else if (nargs == 0
                 && scm_is_eq (name, scm_from_latin1_symbol ("pop-fluid")))
          return MAKMEMO_CALL (MAKMEMO_QUOTE (pop_fluid), 0, SCM_EOL);
        else if (scm_is_eq (scm_current_module (), scm_the_root_module ()))
          return MAKMEMO_CALL (MAKMEMO_TOP_REF (name), nargs, args);
        else
          return MAKMEMO_CALL (MAKMEMO_MOD_REF (list_of_guile, name,
                                                SCM_BOOL_F),
                               nargs,
                               args);
      }

    case SCM_EXPANDED_SEQ:
      return MAKMEMO_SEQ (memoize (REF (exp, SEQ, HEAD), env),
                          memoize (REF (exp, SEQ, TAIL), env));

    case SCM_EXPANDED_LAMBDA:
      /* The body will be a lambda-case or #f. */
      {
	SCM meta, docstring, body, proc;

	meta = REF (exp, LAMBDA, META);
	docstring = scm_assoc_ref (meta, scm_sym_documentation);

        body = REF (exp, LAMBDA, BODY);
        if (scm_is_false (body))
          /* Give a body to case-lambda with no clauses.  */
          proc = MAKMEMO_LAMBDA
            (MAKMEMO_CALL
             (MAKMEMO_MOD_REF (list_of_guile,
                               scm_from_latin1_symbol ("throw"),
                               SCM_BOOL_F),
              5,
              scm_list_5 (MAKMEMO_QUOTE (scm_args_number_key),
                          MAKMEMO_QUOTE (SCM_BOOL_F),
                          MAKMEMO_QUOTE (scm_from_latin1_string
                                         ("Wrong number of arguments")),
                          MAKMEMO_QUOTE (SCM_EOL),
                          MAKMEMO_QUOTE (SCM_BOOL_F))),
             FIXED_ARITY (0),
             SCM_BOOL_F /* docstring */);
        else
          proc = memoize (body, env);

	if (scm_is_string (docstring))
	  {
	    SCM args = SCM_MEMOIZED_ARGS (proc);
	    SCM_SETCAR (SCM_CDR (args), docstring);
	  }

	return proc;
      }

    case SCM_EXPANDED_LAMBDA_CASE:
      {
        SCM req, rest, opt, kw, inits, vars, body, alt;
        SCM walk, minits, arity, rib, new_env;
        int nreq, nopt;

        req = REF (exp, LAMBDA_CASE, REQ);
        rest = scm_not (scm_not (REF (exp, LAMBDA_CASE, REST)));
        opt = REF (exp, LAMBDA_CASE, OPT);
        kw = REF (exp, LAMBDA_CASE, KW);
        inits = REF (exp, LAMBDA_CASE, INITS);
        vars = REF (exp, LAMBDA_CASE, GENSYMS);
        body = REF (exp, LAMBDA_CASE, BODY);
        alt = REF (exp, LAMBDA_CASE, ALTERNATE);

        nreq = scm_ilength (req);
        nopt = scm_is_pair (opt) ? scm_ilength (opt) : 0;

        /* The vars are the gensyms, according to the divine plan. But we need
           to memoize the inits within their appropriate environment,
           complicating things. */
        rib = scm_vector (vars);
        new_env = scm_cons (rib, env);

        minits = SCM_EOL;
        for (walk = inits; scm_is_pair (walk); walk = CDR (walk))
          minits = scm_cons (memoize (CAR (walk), new_env), minits);
        minits = scm_reverse_x (minits, SCM_UNDEFINED);

        if (scm_is_true (kw))
          {
            /* (aok? (kw name sym) ...) -> (aok? (kw . index) ...) */
            SCM aok = CAR (kw), indices = SCM_EOL;
            for (kw = CDR (kw); scm_is_pair (kw); kw = CDR (kw))
              {
                SCM k;
                int idx;

                k = CAR (CAR (kw));
                idx = lookup_rib (CADDR (CAR (kw)), rib);
                indices = scm_acons (k, SCM_I_MAKINUM (idx), indices);
              }
            kw = scm_cons (aok, scm_reverse_x (indices, SCM_UNDEFINED));
          }

        if (scm_is_false (alt) && scm_is_false (kw) && scm_is_false (opt))
          {
            if (scm_is_false (rest))
              arity = FIXED_ARITY (nreq);
            else
              arity = REST_ARITY (nreq, SCM_BOOL_T);
          }
        else if (scm_is_true (alt))
          arity = FULL_ARITY (nreq, rest, nopt, kw, minits,
                              SCM_MEMOIZED_ARGS (memoize (alt, env)));
        else
          arity = FULL_ARITY (nreq, rest, nopt, kw, minits, SCM_BOOL_F);

        return MAKMEMO_LAMBDA (memoize (body, new_env), arity,
			       SCM_BOOL_F /* docstring */);
      }

    case SCM_EXPANDED_LET:
      {
        SCM vars, exps, body, varsv, inits, new_env;
        int i;
        
        vars = REF (exp, LET, GENSYMS);
        exps = REF (exp, LET, VALS);
        body = REF (exp, LET, BODY);
        
        varsv = scm_vector (vars);
        inits = scm_c_make_vector (VECTOR_LENGTH (varsv),
                                   SCM_BOOL_F);
        new_env = scm_cons (varsv, env);
        for (i = 0; scm_is_pair (exps); exps = CDR (exps), i++)
          VECTOR_SET (inits, i, memoize (CAR (exps), env));

        return MAKMEMO_LET (inits, memoize (body, new_env));
      }

    case SCM_EXPANDED_LETREC:
      {
        SCM vars, varsv, exps, expsv, body, undefs, new_env;
        int i, nvars, in_order_p;
        
        vars = REF (exp, LETREC, GENSYMS);
        exps = REF (exp, LETREC, VALS);
        body = REF (exp, LETREC, BODY);
        in_order_p = scm_is_true (REF (exp, LETREC, IN_ORDER_P));

        varsv = scm_vector (vars);
        nvars = VECTOR_LENGTH (varsv);
        expsv = scm_vector (exps);

        undefs = scm_c_make_vector (nvars, MAKMEMO_QUOTE (SCM_UNDEFINED));
        new_env = scm_cons (varsv, env);

        if (in_order_p)
          {
            SCM body_exps = memoize (body, new_env);
            for (i = nvars - 1; i >= 0; i--)
              {
                SCM init = memoize (VECTOR_REF (expsv, i), new_env);
                body_exps = MAKMEMO_SEQ (MAKMEMO_LEX_SET (make_pos (0, i), init),
                                         body_exps);
              }
            return MAKMEMO_LET (undefs, body_exps);
          }
        else
          {
            SCM sets = SCM_BOOL_F, inits = scm_c_make_vector (nvars, SCM_BOOL_F);
            for (i = nvars - 1; i >= 0; i--)
              {
                SCM init, set;

                init = memoize (VECTOR_REF (expsv, i), new_env);
                VECTOR_SET (inits, i, init);

                set = MAKMEMO_LEX_SET (make_pos (1, i),
                                       MAKMEMO_LEX_REF (make_pos (0, i)));
                if (scm_is_false (sets))
                  sets = set;
                else
                  sets = MAKMEMO_SEQ (set, sets);
              }

            if (scm_is_false (sets))
              return memoize (body, env);

            return MAKMEMO_LET (undefs,
                                MAKMEMO_SEQ (MAKMEMO_LET (inits, sets),
                                             memoize (body, new_env)));
          }
      }

    default:
      abort ();
    }
}




SCM_DEFINE (scm_memoize_expression, "memoize-expression", 1, 0, 0, 
            (SCM exp),
	    "Memoize the expression @var{exp}.")
#define FUNC_NAME s_scm_memoize_expression
{
  SCM_ASSERT_TYPE (SCM_EXPANDED_P (exp), exp, 1, FUNC_NAME, "expanded");
  return memoize (exp, scm_current_module ());
}
#undef FUNC_NAME




SCM_SYMBOL (sym_placeholder, "_");

static SCM unmemoize (SCM expr);

static SCM
unmemoize_exprs (SCM exprs)
{
  SCM ret, tail;
  if (scm_is_null (exprs))
    return SCM_EOL;
  ret = scm_list_1 (unmemoize (CAR (exprs)));
  tail = ret;
  for (exprs = CDR (exprs); !scm_is_null (exprs); exprs = CDR (exprs))
    {
      SCM_SETCDR (tail, scm_list_1 (unmemoize (CAR (exprs))));
      tail = CDR (tail);
    }
  return ret;
}

static SCM
unmemoize_bindings (SCM inits)
{
  SCM ret = SCM_EOL;
  int n = scm_c_vector_length (inits);

  while (n--)
    ret = scm_cons (unmemoize (scm_c_vector_ref (inits, n)), ret);

  return ret;
}

static SCM
unmemoize_lexical (SCM n)
{
  char buf[32];
  buf[31] = 0;
  snprintf (buf, 31, "<%u,%u>", scm_to_uint32 (CAR (n)),
            scm_to_uint32 (CDR (n)));
  return scm_from_utf8_symbol (buf);
}

static SCM
unmemoize (const SCM expr)
{
  SCM args;
  
  if (!SCM_MEMOIZED_P (expr))
    abort ();

  args = SCM_MEMOIZED_ARGS (expr);
  switch (SCM_MEMOIZED_TAG (expr))
    {
    case SCM_M_APPLY:
      return scm_cons (scm_from_latin1_symbol ("apply"),
                       unmemoize_exprs (args));
    case SCM_M_SEQ:
      return scm_list_3 (scm_sym_begin, unmemoize (CAR (args)),
                         unmemoize (CDR (args)));
    case SCM_M_CALL:
      return scm_cons (unmemoize (CAR (args)), unmemoize_exprs (CDDR (args)));
    case SCM_M_CONT:
      return scm_list_2 (scm_from_latin1_symbol
                         ("call-with-current_continuation"),
                         unmemoize (args));
    case SCM_M_CALL_WITH_VALUES:
      return scm_list_3 (scm_from_latin1_symbol ("call-with-values"),
                         unmemoize (CAR (args)), unmemoize (CDR (args)));
    case SCM_M_DEFINE:
      return scm_list_3 (scm_sym_define, CAR (args), unmemoize (CDR (args)));
    case SCM_M_IF:
      return scm_list_4 (scm_sym_if, unmemoize (scm_car (args)),
                         unmemoize (scm_cadr (args)), unmemoize (scm_cddr (args)));
    case SCM_M_LAMBDA:
      {
	SCM body = CAR (args), spec = CDDR (args);

	if (scm_is_null (CDR (spec)))
	  return scm_list_3 (scm_sym_lambda,
			     scm_make_list (CAR (spec), sym_placeholder),
			     unmemoize (CAR (args)));
	else if (scm_is_null (SCM_CDDR (spec)))
	  {
	    SCM formals = scm_make_list (CAR (spec), sym_placeholder);
	    return scm_list_3 (scm_sym_lambda,
			       scm_is_true (CADR (spec))
			       ? scm_cons_star (sym_placeholder, formals)
			       : formals,
			       unmemoize (CAR (args)));
	  }
	else
	  {
	    SCM alt, tail;

	    alt = CADDR (CDDDR (spec));
	    if (scm_is_true (alt))
	      tail = CDR (unmemoize (alt));
	    else
	      tail = SCM_EOL;

	    return scm_cons
	      (sym_case_lambda_star,
	       scm_cons (scm_list_2 (scm_list_5 (CAR (spec),
						 CADR (spec),
						 CADDR (spec),
						 CADDDR (spec),
						 unmemoize_exprs (CADR (CDDDR (spec)))),
				     unmemoize (body)),
			 tail));
	  }
      }
    case SCM_M_LET:
      return scm_list_3 (scm_sym_let,
                         unmemoize_bindings (CAR (args)),
                         unmemoize (CDR (args)));
    case SCM_M_QUOTE:
      return scm_list_2 (scm_sym_quote, args);
    case SCM_M_LEXICAL_REF:
      return unmemoize_lexical (args);
    case SCM_M_LEXICAL_SET:
      return scm_list_3 (scm_sym_set_x, unmemoize_lexical (CAR (args)),
                         unmemoize (CDR (args)));
    case SCM_M_TOPLEVEL_REF:
      return args;
    case SCM_M_TOPLEVEL_SET:
      return scm_list_3 (scm_sym_set_x, CAR (args), unmemoize (CDR (args)));
    case SCM_M_MODULE_REF:
      return SCM_VARIABLEP (args) ? args
        : scm_list_3 (scm_is_true (CDDR (args)) ? scm_sym_at : scm_sym_atat,
                      scm_i_finite_list_copy (CAR (args)),
                      CADR (args));
    case SCM_M_MODULE_SET:
      return scm_list_3 (scm_sym_set_x,
                         SCM_VARIABLEP (CDR (args)) ? CDR (args)
                         : scm_list_3 (scm_is_true (CDDDR (args))
                                       ? scm_sym_at : scm_sym_atat,
                                       scm_i_finite_list_copy (CADR (args)),
                                       CADDR (args)),
                         unmemoize (CAR (args)));
    case SCM_M_CALL_WITH_PROMPT:
      return scm_list_4 (scm_from_latin1_symbol ("call-with-prompt"),
                         unmemoize (CAR (args)),
                         unmemoize (CADR (args)),
                         unmemoize (CDDR (args)));
    default:
      abort ();
    }
}




SCM_DEFINE (scm_memoized_p, "memoized?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is memoized.")
#define FUNC_NAME s_scm_memoized_p
{
  return scm_from_bool (SCM_MEMOIZED_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_unmemoize_expression, "unmemoize-expression", 1, 0, 0, 
            (SCM m),
	    "Unmemoize the memoized expression @var{m}.")
#define FUNC_NAME s_scm_unmemoize_expression
{
  SCM_VALIDATE_MEMOIZED (1, m);
  return unmemoize (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_expression_typecode, "memoized-expression-typecode", 1, 0, 0, 
            (SCM m),
	    "Return the typecode from the memoized expression @var{m}.")
#define FUNC_NAME s_scm_memoized_expression_typecode
{
  SCM_VALIDATE_MEMOIZED (1, m);

  /* The tag is a 16-bit integer so it fits in an inum.  */
  return SCM_I_MAKINUM (SCM_MEMOIZED_TAG (m));
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_expression_data, "memoized-expression-data", 1, 0, 0, 
            (SCM m),
	    "Return the data from the memoized expression @var{m}.")
#define FUNC_NAME s_scm_memoized_expression_data
{
  SCM_VALIDATE_MEMOIZED (1, m);
  return SCM_MEMOIZED_ARGS (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_memoized_typecode, "memoized-typecode", 1, 0, 0, 
            (SCM sym),
	    "Return the memoized typecode corresponding to the symbol @var{sym}.")
#define FUNC_NAME s_scm_memoized_typecode
{
  int i;

  SCM_VALIDATE_SYMBOL (1, sym);

  for (i = 0; i < sizeof(memoized_tags)/sizeof(const char*); i++)
    if (strcmp (scm_i_symbol_chars (sym), memoized_tags[i]) == 0)
      return scm_from_int32 (i);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_SYMBOL (scm_unbound_variable_key, "unbound-variable");
static void error_unbound_variable (SCM symbol) SCM_NORETURN;
static void error_unbound_variable (SCM symbol)
{
  scm_error (scm_unbound_variable_key, NULL, "Unbound variable: ~S",
	     scm_list_1 (symbol), SCM_BOOL_F);
}

SCM_DEFINE (scm_memoize_variable_access_x, "memoize-variable-access!", 2, 0, 0, 
            (SCM m, SCM mod),
	    "Look up and cache the variable that @var{m} will access, returning the variable.")
#define FUNC_NAME s_scm_memoize_variable_access_x
{
  SCM mx;
  SCM_VALIDATE_MEMOIZED (1, m);
  mx = SCM_MEMOIZED_ARGS (m);
  switch (SCM_MEMOIZED_TAG (m))
    {
    case SCM_M_TOPLEVEL_REF:
      if (SCM_VARIABLEP (mx))
        return mx;
      else
        {
          SCM var = scm_module_variable (mod, mx);
          if (scm_is_false (var) || scm_is_false (scm_variable_bound_p (var)))
            error_unbound_variable (mx);
          SCM_SET_SMOB_OBJECT (m, var);
          return var;
        }

    case SCM_M_TOPLEVEL_SET:
      {
        SCM var = CAR (mx);
        if (SCM_VARIABLEP (var))
          return var;
        else
          {
            var = scm_module_variable (mod, var);
            if (scm_is_false (var))
              error_unbound_variable (CAR (mx));
            SCM_SETCAR (mx, var);
            return var;
          }
      }

    case SCM_M_MODULE_REF:
      if (SCM_VARIABLEP (mx))
        return mx;
      else
        {
          SCM var;
          mod = scm_resolve_module (CAR (mx));
          if (scm_is_true (CDDR (mx)))
            mod = scm_module_public_interface (mod);
          var = scm_module_lookup (mod, CADR (mx));
          if (scm_is_false (scm_variable_bound_p (var)))
            error_unbound_variable (CADR (mx));
          SCM_SET_SMOB_OBJECT (m, var);
          return var;
        }

    case SCM_M_MODULE_SET:
      /* FIXME: not quite threadsafe */
      if (SCM_VARIABLEP (CDR (mx)))
        return CDR (mx);
      else
        {
          SCM var;
          mod = scm_resolve_module (CADR (mx));
          if (scm_is_true (CDDDR (mx)))
            mod = scm_module_public_interface (mod);
          var = scm_module_lookup (mod, CADDR (mx));
          SCM_SETCDR (mx, var);
          return var;
        }

    default:
      scm_wrong_type_arg (FUNC_NAME, 1, m);
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME




void
scm_init_memoize ()
{
  scm_tc16_memoized = scm_make_smob_type ("%memoized", 0);
  scm_set_smob_print (scm_tc16_memoized, scm_print_memoized);

#include "libguile/memoize.x"

  wind = scm_c_make_gsubr ("wind", 2, 0, 0, do_wind);
  unwind = scm_c_make_gsubr ("unwind", 0, 0, 0, do_unwind);
  push_fluid = scm_c_make_gsubr ("push-fluid", 2, 0, 0, do_push_fluid);
  pop_fluid = scm_c_make_gsubr ("pop-fluid", 0, 0, 0, do_pop_fluid);

  list_of_guile = scm_list_1 (scm_from_latin1_symbol ("guile"));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
