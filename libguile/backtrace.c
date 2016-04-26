/* Printing of backtraces and error messages
 * Copyright (C) 1996,1997,1998,1999,2000,2001, 2003, 2004, 2006, 2009,
 *   2010, 2011, 2014 Free Software Foundation
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

#include <stdio.h>
#include <ctype.h>

#include "libguile/_scm.h"

#include <unistd.h>
#ifdef HAVE_IO_H
#include <io.h>
#endif

#include "libguile/deprecation.h"
#include "libguile/stacks.h"
#include "libguile/srcprop.h"
#include "libguile/struct.h"
#include "libguile/strports.h"
#include "libguile/throw.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/dynwind.h"
#include "libguile/frames.h"

#include "libguile/validate.h"
#include "libguile/backtrace.h"
#include "libguile/filesys.h"
#include "libguile/private-options.h"

/* {Error reporting and backtraces}
 *
 * Note that these functions shouldn't generate errors themselves.
 */

static SCM
boot_print_exception (SCM port, SCM frame, SCM key, SCM args)
#define FUNC_NAME "boot-print-exception"
{
  scm_puts ("Throw to key ", port);
  scm_write (key, port);
  scm_puts (" with args ", port);
  scm_write (args, port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM print_exception_var;
static SCM print_frame_var;
static SCM kw_count;
static SCM print_frames_var;
static SCM frame_to_stack_vector_var;

static void
init_print_exception_var (void)
{
  print_exception_var
    = scm_module_variable (scm_the_root_module (),
                           scm_from_latin1_symbol ("print-exception"));
}

static void
init_print_frame_var (void)
{
  print_frame_var =
    scm_c_public_variable ("system repl debug", "print-frame");
}

static void
init_print_frames_var_and_frame_to_stack_vector_var (void)
{
  kw_count = scm_from_latin1_keyword ("count");
  print_frames_var =
    scm_c_public_variable ("system repl debug", "print-frames");
  frame_to_stack_vector_var =
    scm_c_public_variable ("system repl debug", "frame->stack-vector");
}

SCM
scm_print_exception (SCM port, SCM frame, SCM key, SCM args)
#define FUNC_NAME "print-exception"
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_print_exception_var);

  SCM_VALIDATE_OPOUTPORT (1, port);
  if (scm_is_true (frame))
    SCM_VALIDATE_FRAME (2, frame);
  SCM_VALIDATE_SYMBOL (3, key);
  SCM_VALIDATE_LIST (4, args);

  return scm_call_4 (scm_variable_ref (print_exception_var),
                     port, frame, key, args);
}
#undef FUNC_NAME




/* Print parameters for error messages. */

#define DISPLAY_ERROR_MESSAGE_MAX_LEVEL   7
#define DISPLAY_ERROR_MESSAGE_MAX_LENGTH 10

/* Print parameters for failing expressions in error messages.
 * (See also `print_params' below for backtrace print parameters.)
 */

#define DISPLAY_EXPRESSION_MAX_LEVEL      2
#define DISPLAY_EXPRESSION_MAX_LENGTH     3

#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          return SCM_BOOL_F;


void
scm_display_error_message (SCM message, SCM args, SCM port)
{
  scm_print_exception (port, SCM_BOOL_F, scm_misc_error_key,
                       scm_list_3 (SCM_BOOL_F, message, args));
}


/* The function scm_i_display_error prints out a detailed error message.  This
 * function will be called directly within libguile to signal error messages.
 * No parameter checks will be performed by scm_i_display_error.  Thus, User
 * code should rather use the function scm_display_error.
 */
void
scm_i_display_error (SCM frame, SCM port, SCM subr, SCM message, SCM args, SCM rest)
{
  scm_print_exception (port, frame, scm_misc_error_key,
                       scm_list_3 (subr, message, args));
}


SCM_DEFINE (scm_display_error, "display-error", 6, 0, 0,
	    (SCM frame, SCM port, SCM subr, SCM message, SCM args, SCM rest),
	    "Display an error message to the output port @var{port}.\n"
	    "@var{frame} is the frame in which the error occurred, @var{subr} is\n"
	    "the name of the procedure in which the error occurred and\n"
	    "@var{message} is the actual error message, which may contain\n"
	    "formatting instructions. These will format the arguments in\n"
	    "the list @var{args} accordingly.  @var{rest} is currently\n"
	    "ignored.")
#define FUNC_NAME s_scm_display_error
{
  SCM_VALIDATE_OUTPUT_PORT (2, port);

#if SCM_ENABLE_DEPRECATED
  if (SCM_STACKP (frame))
    {
      scm_c_issue_deprecation_warning
        ("Passing a stack as the first argument to `scm_display_error' is "
         "deprecated.  Pass a frame instead.");
      if (SCM_STACK_LENGTH (frame))
        frame = scm_stack_ref (frame, SCM_INUM0);
      else
        frame = SCM_BOOL_F;
    }
#endif

  scm_i_display_error (frame, port, subr, message, args, rest);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_display_application, "display-application", 1, 2, 0, 
           (SCM frame, SCM port, SCM indent),
	    "Display a procedure application @var{frame} to the output port\n"
	    "@var{port}. @var{indent} specifies the indentation of the\n"
	    "output.")
#define FUNC_NAME s_scm_display_application
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_print_frame_var);

  /* FIXME perhaps: ignoring indent.  But really we should deprecate
     this procedure in favor of print-frame.  */
  return scm_call_2 (scm_variable_ref (print_frame_var), frame, port);
}
#undef FUNC_NAME

struct display_backtrace_args {
  SCM stack;
  SCM port;
  SCM first;
  SCM depth;
  SCM highlight_objects;
};

static SCM
display_backtrace_body (struct display_backtrace_args *a)
#define FUNC_NAME "display-backtrace"
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  SCM frames;

  scm_i_pthread_once (&once,
                      init_print_frames_var_and_frame_to_stack_vector_var);

  a->port = SCM_COERCE_OUTPORT (a->port);

  /* Argument checking and extraction. */
  SCM_VALIDATE_STACK (1, a->stack);
  SCM_VALIDATE_OPOUTPORT (2, a->port);

  if (scm_is_false (a->first))
    a->first = SCM_INUM0;
  if (scm_is_false (a->depth))
    a->depth = scm_from_int (SCM_BACKTRACE_DEPTH);

  if (scm_is_false (scm_less_p (a->first, scm_stack_length (a->stack))))
    return SCM_UNSPECIFIED;

  frames = scm_call_1 (scm_variable_ref (frame_to_stack_vector_var),
                       scm_stack_ref (a->stack, a->first));

  /* FIXME: highlight_objects */
  scm_call_4 (scm_variable_ref (print_frames_var), frames, a->port,
              kw_count, a->depth);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
error_during_backtrace (void *data, SCM tag, SCM throw_args)
{
  SCM port = SCM_PACK_POINTER (data);
  
  scm_puts ("Exception thrown while printing backtrace:\n", port);
  scm_print_exception (port, SCM_BOOL_F, tag, throw_args);

  return SCM_UNSPECIFIED;
}


SCM_DEFINE (scm_display_backtrace_with_highlights, "display-backtrace", 2, 3, 0, 
	    (SCM stack, SCM port, SCM first, SCM depth, SCM highlights),
	    "Display a backtrace to the output port @var{port}.  @var{stack}\n"
	    "is the stack to take the backtrace from, @var{first} specifies\n"
	    "where in the stack to start and @var{depth} how many frames\n"
	    "to display.  @var{first} and @var{depth} can be @code{#f},\n"
	    "which means that default values will be used.\n"
	    "If @var{highlights} is given it should be a list; the elements\n"
	    "of this list will be highlighted wherever they appear in the\n"
	    "backtrace.")
#define FUNC_NAME s_scm_display_backtrace_with_highlights
{
  struct display_backtrace_args a;
  a.stack = stack;
  a.port  = port;
  a.first = SCM_UNBNDP (first) ? SCM_BOOL_F : first;
  a.depth = SCM_UNBNDP (depth) ? SCM_BOOL_F : depth;
  a.highlight_objects = SCM_UNBNDP (highlights) ? SCM_EOL : highlights;

  scm_internal_catch (SCM_BOOL_T,
		      (scm_t_catch_body) display_backtrace_body, &a,
		      (scm_t_catch_handler) error_during_backtrace, SCM_UNPACK_POINTER (port));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_display_backtrace (SCM stack, SCM port, SCM first, SCM depth)
{
  return scm_display_backtrace_with_highlights (stack, port, first, depth,
						SCM_EOL);
}

SCM_VARIABLE (scm_has_shown_backtrace_hint_p_var, "has-shown-backtrace-hint?");

SCM_DEFINE (scm_backtrace_with_highlights, "backtrace", 0, 1, 0, 
	    (SCM highlights),
	    "Display a backtrace of the current stack to the current\n"
            "output port.  If @var{highlights} is given, it should be\n"
	    "a list; the elements of this list will be highlighted\n"
	    "wherever they appear in the backtrace.")
#define FUNC_NAME s_scm_backtrace_with_highlights
{
  SCM port = scm_current_output_port ();
  SCM stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  
  if (SCM_UNBNDP (highlights))
    highlights = SCM_EOL;

  scm_newline (port);
  scm_puts ("Backtrace:\n", port);
  scm_display_backtrace_with_highlights (stack, port, SCM_BOOL_F, SCM_BOOL_F,
                                         highlights);
  scm_newline (port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_backtrace (void)
{
  return scm_backtrace_with_highlights (SCM_EOL);
}



void
scm_init_backtrace ()
{
  scm_c_define_gsubr ("print-exception", 4, 0, 0, boot_print_exception);
#include "libguile/backtrace.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
