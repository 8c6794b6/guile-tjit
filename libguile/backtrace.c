/* Printing of backtraces and error messages
 * Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include <stdio.h>
#include <ctype.h>

#include "_scm.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "stacks.h"
#include "srcprop.h"
#include "genio.h"
#include "struct.h"
#include "strports.h"
#include "throw.h"
#include "fluids.h"

#include "scm_validate.h"
#include "backtrace.h"

/* {Error reporting and backtraces}
 * (A first approximation.)
 *
 * Note that these functions shouldn't generate errors themselves.
 */

#ifndef SCM_RECKLESS
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          return SCM_BOOL_F;
#endif

SCM scm_the_last_stack_fluid;

static void
display_header (SCM source, SCM port)
{
  SCM fname = (SCM_MEMOIZEDP (source)
	       ? scm_source_property (source, scm_sym_filename)
	       : SCM_BOOL_F);
  if (SCM_STRINGP (fname))
    {
      scm_prin1 (fname, port, 0);
      scm_putc (':', port);
      scm_intprint (SCM_INUM (scm_source_property (source, scm_sym_line)) + 1,
		    10,
		    port);
      scm_putc (':', port);
      scm_intprint (SCM_INUM (scm_source_property (source, scm_sym_column)) + 1,
		    10,
		    port);
    }
  else
    scm_puts ("ERROR", port);
  scm_puts (": ", port);
}


void
scm_display_error_message (SCM message, SCM args, SCM port)
{
  int writingp;
  char *start;
  char *p;
  
  if (SCM_IMP (message) || !SCM_ROSTRINGP (message) || SCM_IMP (args)
      || !scm_list_p (args))
    {
      scm_prin1 (message, port, 0);
      scm_putc ('\n', port);
      return;
    }

  SCM_COERCE_SUBSTR (message);
  start = SCM_ROCHARS (message);
  for (p = start; *p != '\0'; ++p)
    if (*p == '%')
      {
	if (SCM_IMP (args) || SCM_NCONSP (args))
	  continue;
	
	++p;
	if (*p == 's')
	  writingp = 0;
	else if (*p == 'S')
	  writingp = 1;
	else
	  continue;

	scm_lfwrite (start, p - start - 1, port);
	scm_prin1 (SCM_CAR (args), port, writingp);
	args = SCM_CDR (args);
	start = p + 1;
      }
  scm_lfwrite (start, p - start, port);
  scm_putc ('\n', port);
}

static void
display_expression (SCM frame,SCM pname,SCM source,SCM port)
{
  SCM print_state = scm_make_print_state ();
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 0;
  pstate->fancyp = 1;
  pstate->level = 2;
  pstate->length = 3;
  if (SCM_ROSTRINGP (pname))
    {
      if (SCM_FRAMEP (frame)
	  && SCM_FRAME_EVAL_ARGS_P (frame))
	scm_puts ("While evaluating arguments to ", port);
      else
	scm_puts ("In procedure ", port);
      scm_iprin1 (pname, port, pstate);
      if (SCM_MEMOIZEDP (source))
	{
	  scm_puts (" in expression ", port);
	  pstate->writingp = 1;
	  scm_iprin1 (scm_unmemoize (source), port, pstate);
	}
    }
  else if (SCM_NIMP (source))
    {
      scm_puts ("In expression ", port);
      pstate->writingp = 1;
      scm_iprin1 (scm_unmemoize (source), port, pstate);
    }
  scm_puts (":\n", port);
  scm_free_print_state (print_state);
}

struct display_error_args {
  SCM stack;
  SCM port;
  SCM subr;
  SCM message;
  SCM args;
  SCM rest;
};

static SCM
display_error_body (struct display_error_args *a)
{
  SCM current_frame = SCM_BOOL_F;
  SCM source = SCM_BOOL_F;
  SCM pname = SCM_BOOL_F;
  SCM prev_frame = SCM_BOOL_F;

  if (SCM_DEBUGGINGP
      && SCM_STACKP (a->stack)
      && SCM_STACK_LENGTH (a->stack) > 0)
    {
      current_frame = scm_stack_ref (a->stack, SCM_INUM0);
      source = SCM_FRAME_SOURCE (current_frame);
      prev_frame = SCM_FRAME_PREV (current_frame);
      if (!SCM_MEMOIZEDP (source)
	  && prev_frame != SCM_BOOL_F)
	source = SCM_FRAME_SOURCE (prev_frame);
      if (SCM_FRAME_PROC_P (current_frame)
	  && scm_procedure_p (SCM_FRAME_PROC (current_frame)) == SCM_BOOL_T)
	pname = scm_procedure_name (SCM_FRAME_PROC (current_frame));
    }
  if (!SCM_ROSTRINGP (pname))
    pname = a->subr;
  if (SCM_ROSTRINGP (pname) || SCM_MEMOIZEDP (source))
    {
      display_header (source, a->port);
      display_expression (current_frame, pname, source, a->port);
    }
  display_header (source, a->port);
  scm_display_error_message (a->message, a->args, a->port);
  return SCM_UNSPECIFIED;
}

struct display_error_handler_data {
  char *mode;
  SCM port;
};

/* This is the exception handler for error reporting routines.
   Note that it is very important that this handler *doesn't* try to
   print more than the error tag, since the error very probably is
   caused by an erroneous print call-back routine.  If we would
   tru to print all objects, we would enter an infinite loop. */
static SCM
display_error_handler (struct display_error_handler_data *data,
		       SCM tag, SCM args)
{
  SCM print_state = scm_make_print_state ();
  scm_puts ("\nException during displaying of ", data->port);
  scm_puts (data->mode, data->port);
  scm_puts (": ", data->port);
  scm_iprin1 (tag, data->port, SCM_PRINT_STATE (print_state));
  scm_putc ('\n', data->port);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_display_error, "display-error", 6, 0, 0,
           (SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest),
"")
#define FUNC_NAME s_scm_display_error
{
  struct display_error_args a;
  struct display_error_handler_data data;
  a.stack = stack;
  a.port  = port;
  a.subr  = subr;
  a.message = message;
  a.args  = args;
  a.rest  = rest;
  data.mode = "error";
  data.port = port;
  scm_internal_catch (SCM_BOOL_T,
		      (scm_catch_body_t) display_error_body, &a,
		      (scm_catch_handler_t) display_error_handler, &data);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

typedef struct {
  int level;
  int length;
} print_params_t;

static int n_print_params = 9;
static print_params_t default_print_params[] = {
  { 4, 9 }, { 4, 3 },
  { 3, 4 }, { 3, 3 },
  { 2, 4 }, { 2, 3 },
  { 1, 4 }, { 1, 3 }, { 1, 2 }
};
static print_params_t *print_params = default_print_params;

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_set_print_params_x, "set-print-params!", 1, 0, 0,
           (SCM params),
"")
#define FUNC_NAME s_scm_set_print_params_x
{
  int i, n = scm_ilength (params);
  SCM ls;
  print_params_t *new_params;
  SCM_ASSERT (n >= 1, params, SCM_ARG2, FUNC_NAME);
  for (ls = params; SCM_NIMP (ls); ls = SCM_CDR (ls))
    SCM_ASSERT (scm_ilength (SCM_CAR (params)) == 2
		&& SCM_INUMP (SCM_CAAR (ls))
		&& SCM_INUM (SCM_CAAR (ls)) >= 0
		&& SCM_INUMP (SCM_CADAR (ls))
		&& SCM_INUM (SCM_CADAR (ls)) >= 0,
		params,
		SCM_ARG2,
		s_scm_set_print_params_x);
  new_params = scm_must_malloc (n * sizeof (print_params_t),
				FUNC_NAME);
  if (print_params != default_print_params)
    scm_must_free (print_params);
  print_params = new_params;
  for (i = 0; i < n; ++i)
    {
      print_params[i].level = SCM_INUM (SCM_CAAR (params));
      print_params[i].length = SCM_INUM (SCM_CADAR (params));
      params = SCM_CDR (params);
    }
  n_print_params = n;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

static void
indent (int n, SCM port)
{
  int i;
  for (i = 0; i < n; ++i)
    scm_putc (' ', port);
}

static void
display_frame_expr (char *hdr,SCM exp,char *tlr,int indentation,SCM sport,SCM port,scm_print_state *pstate)
{
  SCM string;
  int i = 0, n;
  scm_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (sport);
  do
    {
      pstate->length = print_params[i].length;
      ptob->seek (sport, 0, SEEK_SET);
      if (SCM_CONSP (exp))
	{
	  pstate->level = print_params[i].level - 1;
	  scm_iprlist (hdr, exp, tlr[0], sport, pstate);
	  scm_puts (&tlr[1], sport);
	}
      else
	{
	  pstate->level = print_params[i].level;
	  scm_iprin1 (exp, sport, pstate);
	}
      ptob->flush (sport);
      n = ptob->seek (sport, 0, SEEK_CUR);
      ++i;
    }
  while (indentation + n > SCM_BACKTRACE_WIDTH && i < n_print_params);
  ptob->truncate (sport, n);
  string = scm_strport_to_string (sport);
  /* Remove control characters */
  for (i = 0; i < n; ++i)
    if (iscntrl (SCM_CHARS (string)[i]))
      SCM_CHARS (string)[i] = ' ';
  /* Truncate */
  if (indentation + n > SCM_BACKTRACE_WIDTH)
    {
      n = SCM_BACKTRACE_WIDTH - indentation;
      SCM_CHARS (string)[n - 1] = '$';
    }
      
  scm_lfwrite (SCM_CHARS (string), n, port);
}

static void
display_application (SCM frame,int indentation,SCM sport,SCM port,scm_print_state *pstate)
{
  SCM proc = SCM_FRAME_PROC (frame);
  SCM name = (SCM_NFALSEP (scm_procedure_p (proc))
	      ? scm_procedure_name (proc)
	      : SCM_BOOL_F);
  display_frame_expr ("[",
		      scm_cons (SCM_NFALSEP (name) ? name : proc,
				SCM_FRAME_ARGS (frame)),
		      SCM_FRAME_EVAL_ARGS_P (frame) ? " ..." : "]",
		      indentation,
		      sport,
		      port,
		      pstate);
}

SCM_DEFINE (scm_display_application, "display-application", 1, 2, 0, 
           (SCM frame, SCM port, SCM indent),
"")
#define FUNC_NAME s_scm_display_application
{
  SCM_VALIDATE_FRAME (1,frame);
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_VALIDATE_OPOUTPORT (2,port);
  if (SCM_UNBNDP (indent))
    indent = SCM_INUM0;
  else
    SCM_VALIDATE_INUM (3,indent);
  
  if (SCM_FRAME_PROC_P (frame))
    /* Display an application. */
    {
      SCM sport, print_state;
      scm_print_state *pstate;
      
      /* Create a string port used for adaptation of printing parameters. */
      sport = scm_mkstrport (SCM_INUM0,
			     scm_make_string (SCM_MAKINUM (240),
					      SCM_UNDEFINED),
			     SCM_OPN | SCM_WRTNG,
			     FUNC_NAME);

      /* Create a print state for printing of frames. */
      print_state = scm_make_print_state ();
      pstate = SCM_PRINT_STATE (print_state);
      pstate->writingp = 1;
      pstate->fancyp = 1;
      
      display_application (frame, SCM_INUM (indent), sport, port, pstate);
      return SCM_BOOL_T;
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

static void
display_frame (SCM frame,int nfield,int indentation,SCM sport,SCM port,scm_print_state *pstate)
{
  int n, i, j;

  /* Announce missing frames? */
  if (!SCM_BACKWARDS_P && SCM_FRAME_OVERFLOW_P (frame))
    {
      indent (nfield + 1 + indentation, port);
      scm_puts ("...\n", port);
    }

  /* Check size of frame number. */
  n = SCM_FRAME_NUMBER (frame);
  for (i = 0, j = n; j > 0; ++i) j /= 10;

  /* Number indentation. */
  indent (nfield - (i ? i : 1), port);

  /* Frame number. */
  scm_iprin1 (SCM_MAKINUM (n), port, pstate);

  /* Real frame marker */
  scm_putc (SCM_FRAME_REAL_P (frame) ? '*' : ' ', port);

  /* Indentation. */
  indent (indentation, port);

  if (SCM_FRAME_PROC_P (frame))
    /* Display an application. */
    display_application (frame, nfield + 1 + indentation, sport, port, pstate);
  else
    /* Display a special form. */
    {
      SCM source = SCM_FRAME_SOURCE (frame);
      SCM copy = (SCM_CONSP (source) 
		  ? scm_source_property (source, scm_sym_copy)
		  : SCM_BOOL_F);
      SCM umcopy = (SCM_MEMOIZEDP (source)
		    ? scm_unmemoize (source)
		    : SCM_BOOL_F);
      display_frame_expr ("(",
			  SCM_CONSP (copy) ? copy : umcopy,
			  ")",
			  nfield + 1 + indentation,
			  sport,
			  port,
			  pstate);
    }
  scm_putc ('\n', port);

  /* Announce missing frames? */
  if (SCM_BACKWARDS_P && SCM_FRAME_OVERFLOW_P (frame))
    {
      indent (nfield + 1 + indentation, port);
      scm_puts ("...\n", port);
    }
}

struct display_backtrace_args {
  SCM stack;
  SCM port;
  SCM first;
  SCM depth;
};

static SCM
display_backtrace_body(struct display_backtrace_args *a)
#define FUNC_NAME "display_backtrace_body"
{
  int n_frames, beg, end, n, i, j;
  int nfield, indent_p, indentation;
  SCM frame, sport, print_state;
  scm_print_state *pstate;

  a->port = SCM_COERCE_OUTPORT (a->port);

  /* Argument checking and extraction. */
  SCM_ASSERT (SCM_STACKP (a->stack),
	      a->stack,
	      SCM_ARG1,
	      s_display_backtrace);
  SCM_ASSERT (SCM_OPOUTPORTP (a->port),
	      a->port,
	      SCM_ARG2,
	      s_display_backtrace);
  n_frames = SCM_INUM (scm_stack_length (a->stack));
  n = SCM_INUMP (a->depth) ? SCM_INUM (a->depth) : SCM_BACKTRACE_DEPTH;
  if (SCM_BACKWARDS_P)
    {
      beg = SCM_INUMP (a->first) ? SCM_INUM (a->first) : 0;
      end = beg + n - 1;
      if (end >= n_frames)
	end = n_frames - 1;
      n = end - beg + 1;
    }
  else
    {
      if (SCM_INUMP (a->first))
	{
	  beg = SCM_INUM (a->first);
	  end = beg - n + 1;
	  if (end < 0)
	    end = 0;
	}
      else
	{
	  beg = n - 1;
	  end = 0;
	  if (beg >= n_frames)
	    beg = n_frames - 1;
	}
      n = beg - end + 1;
    }
  SCM_ASSERT (beg >= 0 && beg < n_frames, a->first, SCM_ARG3, s_display_backtrace);
  SCM_ASSERT (n > 0, a->depth, SCM_ARG4, s_display_backtrace);

  /* Create a string port used for adaptation of printing parameters. */
  sport = scm_mkstrport (SCM_INUM0,
			 scm_make_string (SCM_MAKINUM (240), SCM_UNDEFINED),
			 SCM_OPN | SCM_WRTNG,
			 FUNC_NAME);

  /* Create a print state for printing of frames. */
  print_state = scm_make_print_state ();
  pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 1;
  pstate->fancyp = 1;

  /* First find out if it's reasonable to do indentation. */
  if (SCM_BACKWARDS_P)
    indent_p = 0;
  else
    {
      indent_p = 1;
      frame = scm_stack_ref (a->stack, SCM_MAKINUM (beg));
      for (i = 0, j = 0; i < n; ++i)
	{
	  if (SCM_FRAME_REAL_P (frame))
	    ++j;
	  if (j > SCM_BACKTRACE_INDENT)
	    {
	      indent_p = 0;
	      break;
	    }
	  frame = (SCM_BACKWARDS_P
		   ? SCM_FRAME_PREV (frame)
		   : SCM_FRAME_NEXT (frame));
	}
    }
  
  /* Determine size of frame number field. */
  j = SCM_FRAME_NUMBER (scm_stack_ref (a->stack, SCM_MAKINUM (end)));
  for (i = 0; j > 0; ++i) j /= 10;
  nfield = i ? i : 1;
  
  /* Print frames. */
  frame = scm_stack_ref (a->stack, SCM_MAKINUM (beg));
  indentation = 1;
  display_frame (frame, nfield, indentation, sport, a->port, pstate);
  for (i = 1; i < n; ++i)
    {
      if (indent_p && SCM_FRAME_EVAL_ARGS_P (frame))
	++indentation;
      frame = SCM_BACKWARDS_P ? SCM_FRAME_PREV (frame) : SCM_FRAME_NEXT (frame);
      display_frame (frame, nfield, indentation, sport, a->port, pstate);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_display_backtrace, "display-backtrace", 2, 2, 0, 
           (SCM stack, SCM port, SCM first, SCM depth),
"")
#define FUNC_NAME s_scm_display_backtrace
{
  struct display_backtrace_args a;
  struct display_error_handler_data data;
  a.stack = stack;
  a.port  = port;
  a.first = first;
  a.depth = depth;
  data.mode = "backtrace";
  data.port = port;
  scm_internal_catch (SCM_BOOL_T,
		      (scm_catch_body_t) display_backtrace_body, &a,
		      (scm_catch_handler_t) display_error_handler, &data);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_VCELL (scm_has_shown_backtrace_hint_p_var, "has-shown-backtrace-hint?");

SCM_DEFINE (scm_backtrace, "backtrace", 0, 0, 0, 
           (),
"")
#define FUNC_NAME s_scm_backtrace
{
  SCM the_last_stack = scm_fluid_ref (SCM_CDR (scm_the_last_stack_fluid));
  if (SCM_NFALSEP (the_last_stack))
    {
      scm_newline (scm_cur_outp);
      scm_puts ("Backtrace:\n", scm_cur_outp);
      scm_display_backtrace (the_last_stack,
			     scm_cur_outp,
			     SCM_UNDEFINED,
			     SCM_UNDEFINED);
      scm_newline (scm_cur_outp);
      if (SCM_FALSEP (SCM_CDR (scm_has_shown_backtrace_hint_p_var))
	  && !SCM_BACKTRACE_P)
	{
	  scm_puts ("Type \"(debug-enable 'backtrace)\" if you would like "
		    "a backtrace\n"
		    "automatically if an error occurs in the future.\n",
		    scm_cur_outp);
	  SCM_SETCDR (scm_has_shown_backtrace_hint_p_var, SCM_BOOL_T);
	}
    }
  else
    {
      scm_puts ("No backtrace available.\n", scm_cur_outp);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_backtrace ()
{
  SCM f = scm_make_fluid ();
  scm_the_last_stack_fluid = scm_sysintern ("the-last-stack", f);

#include "backtrace.x"
}
