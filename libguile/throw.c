/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003, 2004, 2006, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <unistdio.h>
#include "libguile/_scm.h"
#include "libguile/smob.h"
#include "libguile/eval.h"
#include "libguile/eq.h"
#include "libguile/control.h"
#include "libguile/deprecation.h"
#include "libguile/backtrace.h"
#include "libguile/debug.h"
#include "libguile/stackchk.h"
#include "libguile/stacks.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/validate.h"
#include "libguile/vm.h"
#include "libguile/throw.h"
#include "libguile/init.h"
#include "libguile/strings.h"

#include "libguile/private-options.h"


/* Pleasantly enough, the guts of catch are defined in Scheme, in terms
   of prompt, abort, and the %exception-handler fluid.  Check boot-9 for
   the definitions.

   Still, it's useful to be able to throw unwind-only exceptions from C,
   for example so that we can recover from stack overflow.  We also need
   to have an implementation of catch and throw handy before boot time.
   For that reason we have a parallel implementation of "catch" that
   uses the same fluids here.  Throws from C still call out to Scheme
   though, so that pre-unwind handlers can be run.  Getting the dynamic
   environment right for pre-unwind handlers is tricky, and it's
   important to have all of the implementation in one place.

   All of these function names and prototypes carry a fair bit of historical
   baggage. */




static SCM throw_var;

static SCM exception_handler_fluid;

static SCM
catch (SCM tag, SCM thunk, SCM handler, SCM pre_unwind_handler)
{
  struct scm_vm *vp;
  SCM eh, prompt_tag;
  SCM res;
  scm_t_dynstack *dynstack = &SCM_I_CURRENT_THREAD->dynstack;
  SCM dynamic_state = SCM_I_CURRENT_THREAD->dynamic_state;
  scm_i_jmp_buf registers;
  scm_t_ptrdiff saved_stack_depth;

  if (!scm_is_eq (tag, SCM_BOOL_T) && !scm_is_symbol (tag))
    scm_wrong_type_arg ("catch", 1, tag);

  if (SCM_UNBNDP (handler))
    handler = SCM_BOOL_F;
  else if (!scm_is_true (scm_procedure_p (handler)))
    scm_wrong_type_arg ("catch", 3, handler);

  if (SCM_UNBNDP (pre_unwind_handler))
    pre_unwind_handler = SCM_BOOL_F;
  else if (!scm_is_true (scm_procedure_p (pre_unwind_handler)))
    scm_wrong_type_arg ("catch", 4, pre_unwind_handler);

  prompt_tag = scm_cons (SCM_INUM0, SCM_EOL);

  eh = scm_c_make_vector (4, SCM_BOOL_F);
  scm_c_vector_set_x (eh, 0, scm_fluid_ref (exception_handler_fluid));
  scm_c_vector_set_x (eh, 1, tag);
  scm_c_vector_set_x (eh, 2, prompt_tag);
  scm_c_vector_set_x (eh, 3, pre_unwind_handler);

  vp = scm_the_vm ();
  saved_stack_depth = vp->sp - vp->stack_base;

  /* Push the prompt and exception handler onto the dynamic stack. */
  scm_dynstack_push_prompt (dynstack,
                            SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY
                            | SCM_F_DYNSTACK_PROMPT_PUSH_NARGS,
                            prompt_tag,
                            vp->fp - vp->stack_base,
                            saved_stack_depth,
                            vp->ip,
                            &registers);
  scm_dynstack_push_fluid (dynstack, exception_handler_fluid, eh,
                           dynamic_state);

  if (SCM_I_SETJMP (registers))
    {
      /* A non-local return.  */

      /* FIXME: We know where the args will be on the stack; we could
         avoid consing them.  */
      SCM args = scm_i_prompt_pop_abort_args_x (vp);

      /* Cdr past the continuation. */
      args = scm_cdr (args);

      return scm_apply_0 (handler, args);
    }

  res = scm_call_0 (thunk);

  scm_dynstack_unwind_fluid (dynstack, dynamic_state);
  scm_dynstack_pop (dynstack);

  return res;
}

static void
default_exception_handler (SCM k, SCM args)
{
  static int error_printing_error = 0;
  static int error_printing_fallback = 0;

  if (error_printing_fallback)
    fprintf (stderr, "\nFailed to print exception.\n");
  else if (error_printing_error)
    {
      fprintf (stderr, "\nError while printing exception:\n");
      error_printing_fallback = 1;
      fprintf (stderr, "Key: ");
      scm_write (k, scm_current_error_port ());
      fprintf (stderr, ", args: ");
      scm_write (args, scm_current_error_port ());
      scm_newline (scm_current_error_port ());
   }
  else
    {
      fprintf (stderr, "Uncaught exception:\n");
      error_printing_error = 1;
      scm_handle_by_message (NULL, k, args);
    }

  /* Normally we don't get here, because scm_handle_by_message will
     exit.  */
  fprintf (stderr, "Aborting.\n");
  abort ();
}

static SCM
throw_without_pre_unwind (SCM tag, SCM args)
{
  SCM eh;

  for (eh = scm_fluid_ref (exception_handler_fluid);
       scm_is_true (eh);
       eh = scm_c_vector_ref (eh, 0))
    {
      SCM catch_key, prompt_tag;

      catch_key = scm_c_vector_ref (eh, 1);
      if (!scm_is_eq (catch_key, SCM_BOOL_T) && !scm_is_eq (catch_key, tag))
        continue;

      if (scm_is_true (scm_c_vector_ref (eh, 3)))
        {
          char *key_chars;

          key_chars = scm_to_locale_string (scm_symbol_to_string (tag));
          fprintf (stderr, "Warning: Unwind-only `%s' exception; "
                   "skipping pre-unwind handler.\n", key_chars);
          free (key_chars);
        }

      prompt_tag = scm_c_vector_ref (eh, 2);
      if (scm_is_true (prompt_tag))
        scm_abort_to_prompt_star (prompt_tag, scm_cons (tag, args));
    }

  default_exception_handler (tag, args);
  return SCM_UNSPECIFIED;
}

SCM
scm_catch (SCM key, SCM thunk, SCM handler)
{
  return catch (key, thunk, handler, SCM_UNDEFINED);
}

SCM
scm_catch_with_pre_unwind_handler (SCM key, SCM thunk, SCM handler,
                                   SCM pre_unwind_handler)
{
  return catch (key, thunk, handler, pre_unwind_handler);
}

SCM
scm_with_throw_handler (SCM key, SCM thunk, SCM handler)
{
  return catch (key, thunk, SCM_UNDEFINED, handler);
}

SCM
scm_throw (SCM key, SCM args)
{
  return scm_apply_1 (scm_variable_ref (throw_var), key, args);
}



/* Now some support for C bodies and catch handlers */

static scm_t_bits tc16_catch_closure;

enum {
  CATCH_CLOSURE_BODY,
  CATCH_CLOSURE_HANDLER
};

static SCM
make_catch_body_closure (scm_t_catch_body body, void *body_data)
{
  SCM ret;
  SCM_NEWSMOB2 (ret, tc16_catch_closure, body, body_data);
  SCM_SET_SMOB_FLAGS (ret, CATCH_CLOSURE_BODY);
  return ret;
}

static SCM
make_catch_handler_closure (scm_t_catch_handler handler, void *handler_data)
{
  SCM ret;
  SCM_NEWSMOB2 (ret, tc16_catch_closure, handler, handler_data);
  SCM_SET_SMOB_FLAGS (ret, CATCH_CLOSURE_HANDLER);
  return ret;
}

static SCM
apply_catch_closure (SCM clo, SCM args)
{
  void *data = (void*)SCM_SMOB_DATA_2 (clo);

  switch (SCM_SMOB_FLAGS (clo))
    {
    case CATCH_CLOSURE_BODY:
      {
        scm_t_catch_body body = (void*)SCM_SMOB_DATA (clo);
        return body (data);
      }
    case CATCH_CLOSURE_HANDLER:
      {
        scm_t_catch_handler handler = (void*)SCM_SMOB_DATA (clo);
        return handler (data, scm_car (args), scm_cdr (args));
      }
    default:
      abort ();
    }
}

/* TAG is the catch tag.  Typically, this is a symbol, but this
   function doesn't actually care about that.

   BODY is a pointer to a C function which runs the body of the catch;
   this is the code you can throw from.  We call it like this:
      BODY (BODY_DATA)
   where:
      BODY_DATA is just the BODY_DATA argument we received; we pass it
	 through to BODY as its first argument.  The caller can make
	 BODY_DATA point to anything useful that BODY might need.

   HANDLER is a pointer to a C function to deal with a throw to TAG,
   should one occur.  We call it like this:
      HANDLER (HANDLER_DATA, THROWN_TAG, THROW_ARGS)
   where
      HANDLER_DATA is the HANDLER_DATA argument we recevied; it's the
         same idea as BODY_DATA above.
      THROWN_TAG is the tag that the user threw to; usually this is
         TAG, but it could be something else if TAG was #t (i.e., a
         catch-all), or the user threw to a jmpbuf.
      THROW_ARGS is the list of arguments the user passed to the THROW
         function, after the tag.

   BODY_DATA is just a pointer we pass through to BODY.  HANDLER_DATA
   is just a pointer we pass through to HANDLER.  We don't actually
   use either of those pointers otherwise ourselves.  The idea is
   that, if our caller wants to communicate something to BODY or
   HANDLER, it can pass a pointer to it as MUMBLE_DATA, which BODY and
   HANDLER can then use.  Think of it as a way to make BODY and
   HANDLER closures, not just functions; MUMBLE_DATA points to the
   enclosed variables.

   Of course, it's up to the caller to make sure that any data a
   MUMBLE_DATA needs is protected from GC.  A common way to do this is
   to make MUMBLE_DATA a pointer to data stored in an automatic
   structure variable; since the collector must scan the stack for
   references anyway, this assures that any references in MUMBLE_DATA
   will be found.  */

SCM
scm_c_catch (SCM tag,
	     scm_t_catch_body body, void *body_data,
	     scm_t_catch_handler handler, void *handler_data,
	     scm_t_catch_handler pre_unwind_handler, void *pre_unwind_handler_data)
{
  SCM sbody, shandler, spre_unwind_handler;
  
  sbody = make_catch_body_closure (body, body_data);
  shandler = make_catch_handler_closure (handler, handler_data);
  if (pre_unwind_handler)
    spre_unwind_handler = make_catch_handler_closure (pre_unwind_handler,
                                                      pre_unwind_handler_data);
  else
    spre_unwind_handler = SCM_UNDEFINED;
  
  return scm_catch_with_pre_unwind_handler (tag, sbody, shandler,
                                            spre_unwind_handler);
}

SCM
scm_internal_catch (SCM tag,
		    scm_t_catch_body body, void *body_data,
		    scm_t_catch_handler handler, void *handler_data)
{
  return scm_c_catch (tag,
                      body, body_data,
                      handler, handler_data,
                      NULL, NULL);
}


SCM
scm_c_with_throw_handler (SCM tag,
			  scm_t_catch_body body,
			  void *body_data,
			  scm_t_catch_handler handler,
			  void *handler_data,
			  int lazy_catch_p)
{
  SCM sbody, shandler;

  if (lazy_catch_p)
    scm_c_issue_deprecation_warning
      ("The LAZY_CATCH_P argument to `scm_c_with_throw_handler' is no longer.\n"
       "supported. Instead the handler will be invoked from within the dynamic\n"
       "context of the corresponding `throw'.\n"
       "\nTHIS COULD CHANGE YOUR PROGRAM'S BEHAVIOR.\n\n"
       "Please modify your program to pass 0 as the LAZY_CATCH_P argument,\n"
       "and adapt it (if necessary) to expect to be within the dynamic context\n"
       "of the throw.");

  sbody = make_catch_body_closure (body, body_data);
  shandler = make_catch_handler_closure (handler, handler_data);
  
  return scm_with_throw_handler (tag, sbody, shandler);
}


/* body and handler functions for use with any of the above catch variants */

/* This is a body function you can pass to scm_internal_catch if you
   want the body to be like Scheme's `catch' --- a thunk.

   BODY_DATA is a pointer to a scm_body_thunk_data structure, which
   contains the Scheme procedure to invoke as the body, and the tag
   we're catching.  */

SCM
scm_body_thunk (void *body_data)
{
  struct scm_body_thunk_data *c = (struct scm_body_thunk_data *) body_data;

  return scm_call_0 (c->body_proc);
}


/* This is a handler function you can pass to scm_internal_catch if
   you want the handler to act like Scheme's catch: (throw TAG ARGS ...)
   applies a handler procedure to (TAG ARGS ...).

   If the user does a throw to this catch, this function runs a
   handler procedure written in Scheme.  HANDLER_DATA is a pointer to
   an SCM variable holding the Scheme procedure object to invoke.  It
   ought to be a pointer to an automatic variable (i.e., one living on
   the stack), or the procedure object should be otherwise protected
   from GC.  */
SCM
scm_handle_by_proc (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;

  return scm_apply_1 (*handler_proc_p, tag, throw_args);
}

/* SCM_HANDLE_BY_PROC_CATCHING_ALL is like SCM_HANDLE_BY_PROC but
   catches all throws that the handler might emit itself.  The handler
   used for these `secondary' throws is SCM_HANDLE_BY_MESSAGE_NO_EXIT.  */

struct hbpca_data {
  SCM proc;
  SCM args;
};

static SCM
hbpca_body (void *body_data)
{
  struct hbpca_data *data = (struct hbpca_data *)body_data;
  return scm_apply_0 (data->proc, data->args);
}

SCM
scm_handle_by_proc_catching_all (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;
  struct hbpca_data data;
  data.proc = *handler_proc_p;
  data.args = scm_cons (tag, throw_args);

  return scm_internal_catch (SCM_BOOL_T,
			     hbpca_body, &data,
			     scm_handle_by_message_noexit, NULL);
}

/* Derive the an exit status from the arguments to (quit ...).  */
int
scm_exit_status (SCM args)
{
  if (scm_is_pair (args))
    {
      SCM cqa = SCM_CAR (args);
      
      if (scm_is_integer (cqa))
	return (scm_to_int (cqa));
      else if (scm_is_false (cqa))
	return EXIT_FAILURE;
      else
        return EXIT_SUCCESS;
    }
  else if (scm_is_null (args))
    return EXIT_SUCCESS;
  else
    /* A type error.  Strictly speaking we shouldn't get here.  */
    return EXIT_FAILURE;
}
	

static int
should_print_backtrace (SCM tag, SCM stack)
{
  return SCM_BACKTRACE_P
    && scm_is_true (stack)
    && scm_initialized_p
    /* It's generally not useful to print backtraces for errors reading
       or expanding code in these fallback catch statements. */
    && !scm_is_eq (tag, scm_from_latin1_symbol ("read-error"))
    && !scm_is_eq (tag, scm_from_latin1_symbol ("syntax-error"));
}

static void
handler_message (void *handler_data, SCM tag, SCM args)
{
  SCM p, stack, frame;

  p = scm_current_error_port ();
  /* Usually we get here via a throw to a catch-all.  In that case
     there is the throw frame active, and the catch closure, so narrow by
     two frames.  It is possible for a user to invoke
     scm_handle_by_message directly, though, so it could be this
     narrows too much.  We'll have to see how this works out in
     practice.  */
  stack = scm_make_stack (SCM_BOOL_T, scm_list_1 (scm_from_int (2)));
  frame = scm_is_true (stack) ? scm_stack_ref (stack, SCM_INUM0) : SCM_BOOL_F;

  if (should_print_backtrace (tag, stack))
    {
      scm_puts_unlocked ("Backtrace:\n", p);
      scm_display_backtrace_with_highlights (stack, p,
                                             SCM_BOOL_F, SCM_BOOL_F,
                                             SCM_EOL);
      scm_newline (p);
    }

  scm_print_exception (p, frame, tag, args);
}


/* This is a handler function to use if you want scheme to print a
   message and die.  Useful for dealing with throws to uncaught keys
   at the top level.

   At boot time, we establish a catch-all that uses this as its handler.
   1) If the user wants something different, they can use (catch #t
   ...) to do what they like.
   2) Outside the context of a read-eval-print loop, there isn't
   anything else good to do; libguile should not assume the existence
   of a read-eval-print loop.
   3) Given that we shouldn't do anything complex, it's much more
   robust to do it in C code.

   HANDLER_DATA, if non-zero, is assumed to be a char * pointing to a
   message header to print; if zero, we use "guile" instead.  That
   text is followed by a colon, then the message described by ARGS.  */

/* Dirk:FIXME:: The name of the function should make clear that the
 * application gets terminated.
 */

SCM
scm_handle_by_message (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_latin1_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);
  scm_i_pthread_exit (NULL);

  /* this point not reached, but suppress gcc warning about no return value
     in case scm_i_pthread_exit isn't marked as "noreturn" (which seemed not
     to be the case on cygwin for instance) */
  return SCM_BOOL_F;
}


/* This is just like scm_handle_by_message, but it doesn't exit; it
   just returns #f.  It's useful in cases where you don't really know
   enough about the body to handle things in a better way, but don't
   want to let throws fall off the bottom of the wind list.  */
SCM
scm_handle_by_message_noexit (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_latin1_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);

  return SCM_BOOL_F;
}


SCM
scm_handle_by_throw (void *handler_data SCM_UNUSED, SCM tag, SCM args)
{
  scm_ithrow (tag, args, 1);
  return SCM_UNSPECIFIED;  /* never returns */
}

SCM
scm_ithrow (SCM key, SCM args, int no_return SCM_UNUSED)
{
  return scm_throw (key, args);
}

SCM_SYMBOL (scm_stack_overflow_key, "stack-overflow");

void
scm_report_stack_overflow (void)
{
  /* Arguments as if from:

       scm_error (stack-overflow, NULL, "Stack overflow", #f, #f);

     We build the arguments manually because we throw without running
     pre-unwind handlers.  (Pre-unwind handlers could rewind the
     stack.)  */
  SCM args = scm_list_4 (SCM_BOOL_F,
                         scm_from_latin1_string ("Stack overflow"),
                         SCM_BOOL_F,
                         SCM_BOOL_F);
  throw_without_pre_unwind (scm_stack_overflow_key, args);

  /* Not reached.  */
  abort ();
}

void
scm_init_throw ()
{
  tc16_catch_closure = scm_make_smob_type ("catch-closure", 0);
  scm_set_smob_apply (tc16_catch_closure, apply_catch_closure, 0, 0, 1);

  exception_handler_fluid = scm_make_fluid_with_default (SCM_BOOL_F);
  /* This binding is later removed when the Scheme definitions of catch,
     throw, and with-throw-handler are created in boot-9.scm.  */
  scm_c_define ("%exception-handler", exception_handler_fluid);

  scm_c_define ("catch", scm_c_make_gsubr ("catch", 3, 1, 0, catch));
  throw_var = scm_c_define ("throw", scm_c_make_gsubr ("throw", 1, 0, 1,
                                                       throw_without_pre_unwind));

#include "libguile/throw.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
