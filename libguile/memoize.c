/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014
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
  (scm_cons (SCM_I_MAKINUM (n), args))

#define MAKMEMO_SEQ(head,tail) \
  MAKMEMO (SCM_M_SEQ, scm_cons (head, tail))
#define MAKMEMO_IF(test, then, else_) \
  MAKMEMO (SCM_M_IF, scm_cons (test, scm_cons (then, else_)))
#define FIXED_ARITY(nreq) \
  scm_list_1 (SCM_I_MAKINUM (nreq))
#define REST_ARITY(nreq, rest) \
  scm_list_2 (SCM_I_MAKINUM (nreq), rest)
#define FULL_ARITY(nreq, rest, nopt, kw, ninits, unbound, alt) \
  scm_list_n (SCM_I_MAKINUM (nreq), rest, SCM_I_MAKINUM (nopt), kw, \
              SCM_I_MAKINUM (ninits), unbound, alt, SCM_UNDEFINED)
#define MAKMEMO_LAMBDA(body, arity, meta)			\
  MAKMEMO (SCM_M_LAMBDA,					\
	   scm_cons (body, scm_cons (meta, arity)))
#define MAKMEMO_CAPTURE_ENV(vars, body)			\
  MAKMEMO (SCM_M_CAPTURE_ENV, scm_cons (vars, body))
#define MAKMEMO_LET(inits, body) \
  MAKMEMO (SCM_M_LET, scm_cons (inits, body))
#define MAKMEMO_QUOTE(exp) \
  MAKMEMO (SCM_M_QUOTE, exp)
#define MAKMEMO_CAPTURE_MODULE(exp) \
  MAKMEMO (SCM_M_CAPTURE_MODULE, exp)
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
#define MAKMEMO_BOX_REF(box) \
  MAKMEMO (SCM_M_BOX_REF, box)
#define MAKMEMO_BOX_SET(box, val)                                      \
  MAKMEMO (SCM_M_BOX_SET, scm_cons (box, val))
#define MAKMEMO_TOP_BOX(mode, var)               \
  MAKMEMO (SCM_M_RESOLVE, scm_cons (SCM_I_MAKINUM (mode), var))
#define MAKMEMO_MOD_BOX(mode, mod, var, public)                         \
  MAKMEMO (SCM_M_RESOLVE, \
           scm_cons (SCM_I_MAKINUM (mode),                              \
                     scm_cons (mod, scm_cons (var, public))))
#define MAKMEMO_CALL_WITH_PROMPT(tag, thunk, handler) \
  MAKMEMO (SCM_M_CALL_WITH_PROMPT, scm_cons (tag, scm_cons (thunk, handler)))




/* This table must agree with the list of M_ constants in memoize.h */
static const char *const memoized_tags[] =
{
  "seq",
  "if",
  "lambda",
  "capture-env",
  "let",
  "quote",
  "capture-module",
  "apply",
  "call/cc",
  "call-with-values",
  "call",
  "lexical-ref",
  "lexical-set!",
  "box-ref",
  "box-set!",
  "resolve",
  "call-with-prompt",
};





/* Memoization-time environments mirror the structure of eval-time
   environments.  Each link in the chain at memoization-time corresponds
   to a link at eval-time.

   env := module | (link, env)
   module := #f | #t
   link := flat-link . nested-link
   flat-link := (#t . ((var . pos) ...))
   nested-link := (#f . #(var ...))

   A module of #f indicates that the current module has not yet been
   captured.  Memoizing a capture-module expression will capture the
   module.

   Flat environments copy the values for a set of free variables into a
   flat environment, via the capture-env expression.  During memoization
   a flat link collects the values of free variables, along with their
   resolved outer locations.  We are able to copy values because the
   incoming expression has already been assignment-converted.  Flat
   environments prevent closures from hanging on to too much memory.

   Nested environments have a rib of "let" bindings, and link to an
   outer environment.
*/

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
push_nested_link (SCM vars, SCM env)
{
  return scm_acons (SCM_BOOL_F, vars, env);
}

static SCM
push_flat_link (SCM env)
{
  return scm_acons (SCM_BOOL_T, SCM_EOL, env);
}

static int
env_link_is_flat (SCM env_link)
{
  return scm_is_true (CAR (env_link));
}

static SCM
env_link_vars (SCM env_link)
{
  return CDR (env_link);
}

static void
env_link_add_flat_var (SCM env_link, SCM var, SCM pos)
{
  SCM vars = env_link_vars (env_link);
  if (scm_is_false (scm_assq (var, vars)))
    scm_set_cdr_x (env_link, scm_acons (var, pos, vars));
}

static SCM
lookup (SCM x, SCM env)
{
  int d = 0;
  for (; scm_is_pair (env); env = CDR (env), d++)
    {
      SCM link = CAR (env);
      if (env_link_is_flat (link))
        {
          int w;
          SCM vars;

          for (vars = env_link_vars (link), w = scm_ilength (vars) - 1;
               scm_is_pair (vars);
               vars = CDR (vars), w--)
            if (scm_is_eq (x, (CAAR (vars))))
              return make_pos (d, w);

          env_link_add_flat_var (link, x, lookup (x, CDR (env)));
          return make_pos (d, scm_ilength (env_link_vars (link)) - 1);
        }
      else
        {
          int w = try_lookup_rib (x, env_link_vars (link));
          if (w < 0)
            continue;
          return make_pos (d, w);
        }
    }
  abort ();
}

static SCM
capture_flat_env (SCM lambda, SCM env)
{
  int nenv;
  SCM vars, link, locs;

  link = CAR (env);
  vars = env_link_vars (link);
  nenv = scm_ilength (vars);
  locs = scm_c_make_vector (nenv, SCM_BOOL_F);

  for (; scm_is_pair (vars); vars = CDR (vars))
    scm_c_vector_set_x (locs, --nenv, CDAR (vars));

  return MAKMEMO_CAPTURE_ENV (locs, lambda);
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
capture_env (SCM env)
{
  if (scm_is_false (env))
    return SCM_BOOL_T;
  return env;
}

static SCM
maybe_makmemo_capture_module (SCM exp, SCM env)
{
  if (scm_is_false (env))
    return MAKMEMO_CAPTURE_MODULE (exp);
  return exp;
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
        return maybe_makmemo_capture_module
          (MAKMEMO_BOX_REF (MAKMEMO_TOP_BOX (SCM_EXPANDED_TOPLEVEL_REF,
                                             REF (exp, PRIMITIVE_REF, NAME))),
           env);
      else
        return MAKMEMO_BOX_REF (MAKMEMO_MOD_BOX (SCM_EXPANDED_MODULE_REF,
                                                 list_of_guile,
                                                 REF (exp, PRIMITIVE_REF, NAME),
                                                 SCM_BOOL_F));
                                
    case SCM_EXPANDED_LEXICAL_REF:
      return MAKMEMO_LEX_REF (lookup (REF (exp, LEXICAL_REF, GENSYM), env));

    case SCM_EXPANDED_LEXICAL_SET:
      return MAKMEMO_LEX_SET (lookup (REF (exp, LEXICAL_SET, GENSYM), env),
                              memoize (REF (exp, LEXICAL_SET, EXP), env));

    case SCM_EXPANDED_MODULE_REF:
      return MAKMEMO_BOX_REF (MAKMEMO_MOD_BOX
                              (SCM_EXPANDED_MODULE_REF,
                               REF (exp, MODULE_REF, MOD),
                               REF (exp, MODULE_REF, NAME),
                               REF (exp, MODULE_REF, PUBLIC)));

    case SCM_EXPANDED_MODULE_SET:
      return MAKMEMO_BOX_SET (MAKMEMO_MOD_BOX
                              (SCM_EXPANDED_MODULE_SET,
                               REF (exp, MODULE_SET, MOD),
                               REF (exp, MODULE_SET, NAME),
                               REF (exp, MODULE_SET, PUBLIC)),
                              memoize (REF (exp, MODULE_SET, EXP), env));

    case SCM_EXPANDED_TOPLEVEL_REF:
      return maybe_makmemo_capture_module
        (MAKMEMO_BOX_REF (MAKMEMO_TOP_BOX (SCM_EXPANDED_TOPLEVEL_REF,
                                           REF (exp, TOPLEVEL_REF, NAME))),
         env);

    case SCM_EXPANDED_TOPLEVEL_SET:
      return maybe_makmemo_capture_module
        (MAKMEMO_BOX_SET (MAKMEMO_TOP_BOX (SCM_EXPANDED_TOPLEVEL_SET,
                                           REF (exp, TOPLEVEL_SET, NAME)),
                          memoize (REF (exp, TOPLEVEL_SET, EXP),
                                   capture_env (env))),
         env);

    case SCM_EXPANDED_TOPLEVEL_DEFINE:
      return maybe_makmemo_capture_module
        (MAKMEMO_BOX_SET (MAKMEMO_TOP_BOX (SCM_EXPANDED_TOPLEVEL_DEFINE,
                                           REF (exp, TOPLEVEL_DEFINE, NAME)),
                          memoize (REF (exp, TOPLEVEL_DEFINE, EXP),
                                   capture_env (env))),
         env);

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
        else if (nargs == 1
                 && scm_is_eq (name,
                               scm_from_latin1_symbol ("variable-ref")))
          return MAKMEMO_BOX_REF (CAR (args));
        else if (nargs == 2
                 && scm_is_eq (name,
                               scm_from_latin1_symbol ("variable-set!")))
          return MAKMEMO_BOX_SET (CAR (args), CADR (args));
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
          return MAKMEMO_CALL (maybe_makmemo_capture_module
                               (MAKMEMO_BOX_REF
                                (MAKMEMO_TOP_BOX (SCM_EXPANDED_TOPLEVEL_REF,
                                                  name)),
                                env),
                               nargs, args);
        else
          return MAKMEMO_CALL (MAKMEMO_BOX_REF
                               (MAKMEMO_MOD_BOX (SCM_EXPANDED_MODULE_REF,
                                                 list_of_guile,
                                                 name,
                                                 SCM_BOOL_F)),
                               nargs,
                               args);
      }

    case SCM_EXPANDED_SEQ:
      return MAKMEMO_SEQ (memoize (REF (exp, SEQ, HEAD), env),
                          memoize (REF (exp, SEQ, TAIL), env));

    case SCM_EXPANDED_LAMBDA:
      /* The body will be a lambda-case. */
      {
	SCM meta, body, proc, new_env;

	meta = REF (exp, LAMBDA, META);
        body = REF (exp, LAMBDA, BODY);
        new_env = push_flat_link (capture_env (env));
        proc = memoize (body, new_env);
        SCM_SETCAR (SCM_CDR (SCM_MEMOIZED_ARGS (proc)), meta);

	return maybe_makmemo_capture_module (capture_flat_env (proc, new_env),
                                             env);
      }

    case SCM_EXPANDED_LAMBDA_CASE:
      {
        SCM req, rest, opt, kw, inits, vars, body, alt;
        SCM unbound, arity, rib, new_env;
        int nreq, nopt, ninits;

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
        ninits = scm_ilength (inits);
        /* This relies on assignment conversion turning inits into a
           sequence of CONST expressions whose values are a unique
           "unbound" token.  */
        unbound = ninits ? REF (CAR (inits), CONST, EXP) : SCM_BOOL_F;
        rib = scm_vector (vars);
        new_env = push_nested_link (rib, env);

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
          arity = FULL_ARITY (nreq, rest, nopt, kw, ninits, unbound,
                              SCM_MEMOIZED_ARGS (memoize (alt, env)));
        else
          arity = FULL_ARITY (nreq, rest, nopt, kw, ninits, unbound,
                              SCM_BOOL_F);

        return MAKMEMO_LAMBDA (memoize (body, new_env), arity,
                               SCM_EOL /* meta, filled in later */);
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
        new_env = push_nested_link (varsv, capture_env (env));
        for (i = 0; scm_is_pair (exps); exps = CDR (exps), i++)
          VECTOR_SET (inits, i, memoize (CAR (exps), env));

        return maybe_makmemo_capture_module
          (MAKMEMO_LET (inits, memoize (body, new_env)), env);
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
  return memoize (scm_convert_assignment (exp), SCM_BOOL_F);
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
    case SCM_M_CAPTURE_MODULE:
      return scm_list_2 (scm_from_latin1_symbol ("capture-module"),
                         unmemoize (args));
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

	    alt = CADDDR (CDDDR (spec));
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
                                                 CADR (CDDDR (spec))),
				     unmemoize (body)),
			 tail));
	  }
      }
    case SCM_M_CAPTURE_ENV:
      return scm_list_3 (scm_from_latin1_symbol ("capture-env"),
                         CAR (args),
                         unmemoize (CDR (args)));
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
    case SCM_M_BOX_REF:
      return scm_list_2 (scm_from_latin1_symbol ("variable-ref"),
                         unmemoize (args));
    case SCM_M_BOX_SET:
      return scm_list_3 (scm_from_latin1_symbol ("variable-set!"),
                         unmemoize (CAR (args)),
                         unmemoize (CDR (args)));
    case SCM_M_RESOLVE:
      if (SCM_VARIABLEP (args))
        return args;
      else if (scm_is_symbol (CDR (args)))
        return CDR (args);
      else
        return scm_list_3
          (scm_is_true (CDDDR (args)) ? scm_sym_at : scm_sym_atat,
           scm_i_finite_list_copy (CADR (args)),
           CADDR (args));
    case SCM_M_CALL_WITH_PROMPT:
      return scm_list_4 (scm_from_latin1_symbol ("call-with-prompt"),
                         unmemoize (CAR (args)),
                         unmemoize (CADR (args)),
                         unmemoize (CDDR (args)));
    default:
      abort ();
    }
}




SCM_DEFINE (scm_unmemoize_expression, "unmemoize-expression", 1, 0, 0, 
            (SCM m),
	    "Unmemoize the memoized expression @var{m}.")
#define FUNC_NAME s_scm_unmemoize_expression
{
  return unmemoize (m);
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

SCM_DEFINE (scm_sys_resolve_variable, "%resolve-variable", 2, 0, 0,
            (SCM loc, SCM mod),
	    "Look up and return the variable for @var{loc}.")
#define FUNC_NAME s_scm_sys_resolve_variable
{
  int mode;

  if (scm_is_false (mod))
    mod = scm_the_root_module ();

  mode = scm_to_int (scm_car (loc));
  loc = scm_cdr (loc);

  switch (mode)
    {
    case SCM_EXPANDED_TOPLEVEL_REF:
    case SCM_EXPANDED_TOPLEVEL_SET:
      {
        SCM var = scm_module_variable (mod, loc);
        if (scm_is_false (var)
            || (mode == SCM_EXPANDED_TOPLEVEL_REF
                && scm_is_false (scm_variable_bound_p (var))))
          error_unbound_variable (loc);
        return var;
      }

    case SCM_EXPANDED_TOPLEVEL_DEFINE:
      {
        return scm_module_ensure_local_variable (mod, loc);
      }

    case SCM_EXPANDED_MODULE_REF:
    case SCM_EXPANDED_MODULE_SET:
      {
        SCM var;
        mod = scm_resolve_module (scm_car (loc));
        if (scm_is_true (scm_cddr (loc)))
          mod = scm_module_public_interface (mod);
        var = scm_module_lookup (mod, scm_cadr (loc));
        if (mode == SCM_EXPANDED_MODULE_SET
            && scm_is_false (scm_variable_bound_p (var)))
          error_unbound_variable (scm_cadr (loc));
        return var;
      }

    default:
      scm_wrong_type_arg (FUNC_NAME, 1, loc);
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME




void
scm_init_memoize ()
{
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
