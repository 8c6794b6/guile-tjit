/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "genio.h"
#include "smob.h"
#include "eval.h"
#include "macros.h"
#include "procprop.h"
#include "read.h"
#include "weaks.h"
#include "unif.h"
#include "alist.h"
#include "struct.h"

#include "print.h"


/* {Names of immediate symbols}
 * 
 * This table must agree with the declarations in scm.h: {Immediate Symbols}.
 */

char *scm_isymnames[] =
{
  /* This table must agree with the declarations */
  "#@and",
  "#@begin",
  "#@case",
  "#@cond",
  "#@do",
  "#@if",
  "#@lambda",
  "#@let",
  "#@let*",
  "#@letrec",
  "#@or",
  "#@quote",
  "#@set!",
  "#@define",
#if 0
  "#@literal-variable-ref",
  "#@literal-variable-set!",
#endif
  "#@apply",
  "#@call-with-current-continuation",

 /* user visible ISYMS */
 /* other keywords */
 /* Flags */

  "#f",
  "#t",
  "#<undefined>",
  "#<eof>",
  "()",
  "#<unspecified>"
};

scm_option scm_print_opts[] = {
  { SCM_OPTION_SCM, "closure-hook", SCM_BOOL_F,
    "Hook for printing closures." },
  { SCM_OPTION_BOOLEAN, "source", 0,
    "Print closures with source." }
};

SCM_PROC (s_print_options, "print-options-interface", 0, 1, 0, scm_print_options);

SCM
scm_print_options (setting)
     SCM setting;
{
  SCM ans = scm_options (setting,
			 scm_print_opts,
			 SCM_N_PRINT_OPTIONS,
			 s_print_options);
  return ans;
}


/* {Printing of Scheme Objects}
 */

/* Detection of circular references.
 *
 * Due to other constraints in the implementation, this code has bad
 * time complexity (O (depth * N)), The printer code will be
 * completely rewritten before next release of Guile.  The new code
 * will be O(N).
 */
#define PUSH_REF(pstate, obj) \
{ \
  pstate->ref_stack[pstate->top++] = (obj); \
  if (pstate->top == pstate->ceiling) \
    grow_ref_stack (pstate); \
}

#define ENTER_NESTED_DATA(pstate, obj, label) \
{ \
  register unsigned long i; \
  for (i = 0; i < pstate->top; ++i) \
    if (pstate->ref_stack[i] == (obj)) \
      goto label; \
  if (pstate->fancyp) \
    { \
      if (pstate->top - pstate->list_offset >= pstate->level) \
	{ \
	  scm_putc ('#', port); \
	  return; \
	} \
    } \
  PUSH_REF(pstate, obj); \
} \

#define EXIT_NESTED_DATA(pstate) { --pstate->top; }

SCM scm_print_state_vtable;

static SCM print_state_pool;

#ifdef GUILE_DEBUG /* Used for debugging purposes */
SCM_PROC(s_current_pstate, "current-pstate", 0, 0, 0, scm_current_pstate);

SCM
scm_current_pstate ()
{
  return SCM_CADR (print_state_pool);
}
#endif

#define PSTATE_SIZE 50L

static SCM make_print_state SCM_P ((void));

static SCM
make_print_state ()
{
  SCM print_state = scm_make_struct (SCM_CAR (print_state_pool), /* pstate type */
				     SCM_INUM0,
				     SCM_EOL);
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->ref_vect = scm_make_vector (SCM_MAKINUM (PSTATE_SIZE),
				      SCM_UNDEFINED);
  pstate->ref_stack = SCM_VELTS (pstate->ref_vect);
  pstate->ceiling = SCM_LENGTH (pstate->ref_vect);
  return print_state;
}

SCM
scm_make_print_state ()
{
  SCM answer = 0;

  /* First try to allocate a print state from the pool */
  SCM_DEFER_INTS;
  if (SCM_NNULLP (SCM_CDR (print_state_pool)))
    {
      answer = SCM_CADR (print_state_pool);
      SCM_SETCDR (print_state_pool, SCM_CDDR (print_state_pool));
    }
  SCM_ALLOW_INTS;
  
  return answer ? answer : make_print_state ();
}

static char s_print_state_printer[] = "print-state-printer";
static SCM
print_state_printer (obj, port)
     SCM obj;
     SCM port;
{
  /* This function can be made visible by means of struct-ref, so
     we need to make sure that it gets what it wants. */
  SCM_ASSERT (SCM_NIMP (obj) && SCM_PRINT_STATE_P (obj),
	      obj,
	      SCM_ARG1,
	      s_print_state_printer);
  SCM_ASSERT (scm_valid_oport_value_p (port),
	      port,
	      SCM_ARG2,
	      s_print_state_printer);
  port = SCM_COERCE_OUTPORT (port);
  scm_puts ("#<print-state ", port);
  scm_intprint (obj, 16, port);
  scm_putc ('>', port);
  return SCM_UNSPECIFIED;
}

void
scm_free_print_state (print_state)
     SCM print_state;
{
  SCM handle;
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  /* Cleanup before returning print state to pool.
   * It is better to do it here.  Doing it in scm_prin1
   * would cost more since that function is called much more
   * often.
   */
  pstate->fancyp = 0;
  pstate->revealed = 0;
  SCM_NEWCELL (handle);
  SCM_DEFER_INTS;
  SCM_SETCAR (handle, print_state);
  SCM_SETCDR (handle, SCM_CDR (print_state_pool));
  SCM_SETCDR (print_state_pool, handle);
  SCM_ALLOW_INTS;
}

static void grow_ref_stack SCM_P ((scm_print_state *pstate));

static void
grow_ref_stack (pstate)
     scm_print_state *pstate;
{
  int new_size = 2 * pstate->ceiling;
  scm_vector_set_length_x (pstate->ref_vect, SCM_MAKINUM (new_size));
  pstate->ref_stack = SCM_VELTS (pstate->ref_vect);
  pstate->ceiling = new_size;
}


static void print_circref SCM_P ((SCM port, scm_print_state *pstate, SCM ref));

static void
print_circref (port, pstate, ref)
     SCM port;
     scm_print_state *pstate;
     SCM ref;
{
  register int i;
  int self = pstate->top - 1;
  i = pstate->top - 1;
  if (SCM_CONSP (pstate->ref_stack[i]))
    {
      while (i > 0)
	{
	  if (SCM_NCONSP (pstate->ref_stack[i - 1])
	      || SCM_CDR (pstate->ref_stack[i - 1]) != pstate->ref_stack[i])
	    break;
	  --i;
	}
      self = i;
    }
  for (i = pstate->top - 1; 1; --i)
    if (pstate->ref_stack[i] == ref)
      break;
  scm_putc ('#', port);
  scm_intprint (i - self, 10, port);
  scm_putc ('#', port);
}

/* Print generally.  Handles both write and display according to PSTATE.
 */


void 
scm_iprin1 (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
taloop:
  switch (7 & (int) exp)
    {
    case 2:
    case 6:
      scm_intprint (SCM_INUM (exp), 10, port);
      break;
    case 4:
      if (SCM_ICHRP (exp))
	{
	  register long i;

	  i = SCM_ICHR (exp);
	  if (SCM_WRITINGP (pstate))
	    {
	      scm_puts ("#\\", port);
	      if ((i >= 0) && (i <= ' ') && scm_charnames[i])
		scm_puts (scm_charnames[i], port);
	      else if (i < 0 || i > '\177')
		scm_intprint (i, 8, port);
	      else
		scm_putc (i, port);
	    }
	  else
	    scm_putc (i, port);
	}
      else if (SCM_IFLAGP (exp)
	       && ((size_t) SCM_ISYMNUM (exp) < (sizeof scm_isymnames / sizeof (char *))))
	  scm_puts (SCM_ISYMCHARS (exp), port);
      else if (SCM_ILOCP (exp))
	{
	  scm_puts ("#@", port);
	  scm_intprint ((long) SCM_IFRAME (exp), 10, port);
	  scm_putc (SCM_ICDRP (exp) ? '-' : '+', port);
	  scm_intprint ((long) SCM_IDIST (exp), 10, port);
	}
      else
	goto idef;
      break;
    case 1:
      /* gloc */
      scm_puts ("#@", port);
      exp = SCM_CAR (exp - 1);
      goto taloop;
    default:
    idef:
      scm_ipruk ("immediate", exp, port);
      break;
    case 0:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_cons_gloc:

	  if (SCM_CDR (SCM_CAR (exp) - 1L) == 0)
	    {
	      ENTER_NESTED_DATA (pstate, exp, circref);
	      scm_print_struct (exp, port, pstate);
	      EXIT_NESTED_DATA (pstate);
	      break;
	    }

	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_iprlist ("(", exp, ')', port, pstate);
	  EXIT_NESTED_DATA (pstate);
	  break;
	circref:
	  print_circref (port, pstate, exp);
	  break;
	macros:
	  if (!SCM_CLOSUREP (SCM_CDR (exp)))
	    goto prinmacro;
	case scm_tcs_closures:
	  /* The user supplied print closure procedure must handle
	     macro closures as well. */
	  if (SCM_FALSEP (scm_procedure_p (SCM_PRINT_CLOSURE))
	      || SCM_FALSEP (scm_printer_apply (SCM_PRINT_CLOSURE,
						exp, port, pstate)))
	  {
	    SCM name, code, env;
	    if (SCM_TYP16 (exp) == scm_tc16_macro)
	      {
		/* Printing a macro. */
	      prinmacro:
		name = scm_macro_name (exp);
		if (!SCM_CLOSUREP (SCM_CDR (exp)))
		  {
		    code = env = 0;
		    scm_puts ("#<primitive-", port);
		  }
		else
		  {
		    code = SCM_CODE (SCM_CDR (exp));
		    env = SCM_ENV (SCM_CDR (exp));
		    scm_puts ("#<", port);
		  }
		if (SCM_CAR (exp) & (3L << 16))
		  scm_puts ("macro", port);
		else
		  scm_puts ("syntax", port);
		if (SCM_CAR (exp) & (2L << 16))
		  scm_putc ('!', port);
	      }
	    else
	      {
		/* Printing a closure. */
		name = scm_procedure_name (exp);
		code = SCM_CODE (exp);
		env = SCM_ENV (exp);
		scm_puts ("#<procedure", port);
	      }
	    if (SCM_NIMP (name) && SCM_ROSTRINGP (name))
	      {
		scm_putc (' ', port);
		scm_puts (SCM_ROCHARS (name), port);
	      }
	    if (code)
	      {
		if (SCM_PRINT_SOURCE_P)
		  {
		    code = scm_unmemocopy (code,
					   SCM_EXTEND_ENV (SCM_CAR (code),
							   SCM_EOL,
							   env));
		    ENTER_NESTED_DATA (pstate, exp, circref);
		    scm_iprlist (" ", code, '>', port, pstate);
		    EXIT_NESTED_DATA (pstate);
		  }
		else
		  {
		    if (SCM_TYP16 (exp) != scm_tc16_macro)
		      {
			scm_putc (' ', port);
			scm_iprin1 (SCM_CAR (code), port, pstate);
		      }
		    scm_putc ('>', port);
		  }
	      }
	    else
	      scm_putc ('>', port);
	  }
	  break;
	case scm_tc7_substring:
	case scm_tc7_string:
	  if (SCM_WRITINGP (pstate))
	    {
	      scm_sizet i;

	      scm_putc ('"', port);
	      for (i = 0; i < SCM_ROLENGTH (exp); ++i)
		switch (SCM_ROCHARS (exp)[i])
		  {
		  case '"':
		  case '\\':
		    scm_putc ('\\', port);
		  default:
		    scm_putc (SCM_ROCHARS (exp)[i], port);
		  }
	      scm_putc ('"', port);
	      break;
	    }
	  else
	    scm_lfwrite (SCM_ROCHARS (exp), (scm_sizet) SCM_ROLENGTH (exp),
			 port);
	  break;
	case scm_tcs_symbols:
	    {
	      int pos;
	      int end;
	      int len;
	      char * str;
	      int weird;
	      int maybe_weird;
	      int mw_pos = 0;

	      len = SCM_LENGTH (exp);
	      str = SCM_CHARS (exp);
	      scm_remember (&exp);
	      pos = 0;
	      weird = 0;
	      maybe_weird = 0;

	      if (len == 0)
		scm_lfwrite ("#{}#", 4, port);

	      for (end = pos; end < len; ++end)
		switch (str[end])
		  {
#ifdef BRACKETS_AS_PARENS
		  case '[':
		  case ']':
#endif
		  case '(':
		  case ')':
		  case '"':
		  case ';':
		  case SCM_WHITE_SPACES:
		  case SCM_LINE_INCREMENTORS:
		  weird_handler:
		    if (maybe_weird)
		      {
			end = mw_pos;
			maybe_weird = 0;
		      }
		    if (!weird)
		      {
			scm_lfwrite ("#{", 2, port);
			weird = 1;
		      }
		    if (pos < end)
		      {
			scm_lfwrite (str + pos, end - pos, port);
		      }
		    {
		      char buf[2];
		      buf[0] = '\\';
		      buf[1] = str[end];
		      scm_lfwrite (buf, 2, port);
		    }
		    pos = end + 1;
		    break;
		  case '\\':
		    if (weird)
		      goto weird_handler;
		    if (!maybe_weird)
		      {
			maybe_weird = 1;
			mw_pos = pos;
		      }
		    break;
		  case '}':
		  case '#':
		    if (weird)
		      goto weird_handler;
		    break;
		  default:
		    break;
		  }
	      if (pos < end)
		scm_lfwrite (str + pos, end - pos, port);
	      if (weird)
		scm_lfwrite ("}#", 2, port);
	      break;
	    }
	case scm_tc7_wvect:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  if (SCM_IS_WHVEC (exp))
	    scm_puts ("#wh(", port);
	  else
	    scm_puts ("#w(", port);
	  goto common_vector_printer;

	case scm_tc7_vector:
	  ENTER_NESTED_DATA (pstate, exp, circref);
	  scm_puts ("#(", port);
	common_vector_printer:
	  {
	    register long i;
	    int last = SCM_LENGTH (exp) - 1;
	    int cutp = 0;
	    if (pstate->fancyp && SCM_LENGTH (exp) > pstate->length)
	      {
		last = pstate->length - 1;
		cutp = 1;
	      }
	    for (i = 0; i < last; ++i)
	      {
		/* CHECK_INTS; */
		scm_iprin1 (SCM_VELTS (exp)[i], port, pstate);
		scm_putc (' ', port);
	      }
	    if (i == last)
	      {
		/* CHECK_INTS; */
		scm_iprin1 (SCM_VELTS (exp)[i], port, pstate);
	      }
	    if (cutp)
	      scm_puts (" ...", port);
	    scm_putc (')', port);
	  }
	  EXIT_NESTED_DATA (pstate);
	  break;
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
#ifdef LONGLONGS
	case scm_tc7_llvect:
#endif
	  scm_raprin1 (exp, port, pstate);
	  break;
	case scm_tcs_subrs:
	  scm_puts ("#<primitive-procedure ", port);
	  scm_puts (SCM_CHARS (SCM_SNAME (exp)), port);
	  scm_putc ('>', port);
	  break;
#ifdef CCLO
	case scm_tc7_cclo:
	  {
	    SCM proc = SCM_CCLO_SUBR (exp);
	    if (proc == scm_f_gsubr_apply)
	      {
		/* Print gsubrs as primitives */
		SCM name = scm_procedure_name (exp);
		scm_puts ("#<primitive-procedure", port);
		if (SCM_NFALSEP (name))
		  {
		    scm_putc (' ', port);
		    scm_puts (SCM_CHARS (name), port);
		  }
	      }
	    else
	      {
		scm_puts ("#<compiled-closure ", port);
		scm_iprin1 (proc, port, pstate);
	      }
	    scm_putc ('>', port);
	  }
	  break;
#endif
	case scm_tc7_pws:
	  scm_puts ("#<procedure-with-setter", port);
	  {
	    SCM name = scm_procedure_name (exp);
	    if (SCM_NFALSEP (name))
	      {
		scm_putc (' ', port);
		scm_puts (SCM_ROCHARS (name), port);
	      }
	  }
	  scm_putc ('>', port);
	  break;
	case scm_tc7_contin:
	  scm_puts ("#<continuation ", port);
	  scm_intprint (SCM_LENGTH (exp), 10, port);
	  scm_puts (" @ ", port);
	  scm_intprint ((long) SCM_CHARS (exp), 16, port);
	  scm_putc ('>', port);
	  break;
	case scm_tc7_port:
	  {
	    register long i = SCM_PTOBNUM (exp);
	    if (i < scm_numptob
		&& scm_ptobs[i].print
		&& (scm_ptobs[i].print) (exp, port, pstate))
	      break;
	    goto punk;
	  }
	case scm_tc7_smob:
	  {
	    register long i;
	    ENTER_NESTED_DATA (pstate, exp, circref);
	    i = SCM_SMOBNUM (exp);
	    if (i < scm_numsmob && scm_smobs[i].print
		&& (scm_smobs[i].print) (exp, port, pstate))
	      {
		EXIT_NESTED_DATA (pstate);
		break;
	      }
	    EXIT_NESTED_DATA (pstate);
	    /* Macros have their print field set to NULL.  They are
	       handled at the same place as closures in order to achieve
	       non-redundancy.  Placing the condition here won't slow
	       down printing of other smobs. */
	    if (SCM_TYP16 (exp) == scm_tc16_macro)
	      goto macros;
	  }
	default:
	punk:
	  scm_ipruk ("type", exp, port);
	}
    }
}

/* Print states are necessary for circular reference safe printing.
 * They are also expensive to allocate.  Therefore print states are
 * kept in a pool so that they can be reused.
 */

/* The PORT argument can also be a print-state/port pair, which will
 * then be used instead of allocating a new print state.  This is
 * useful for continuing a chain of print calls from Scheme.  */

void 
scm_prin1 (exp, port, writingp)
     SCM exp;
     SCM port;
     int writingp;
{
  SCM handle = SCM_BOOL_F; /* Will GC protect the handle whilst unlinked */
  SCM pstate_scm;
  scm_print_state *pstate;

  /* If PORT is a print-state/port pair, use that.  Else create a new
     print-state. */

  if (SCM_NIMP (port) && SCM_CONSP (port))
    {
      pstate_scm = SCM_CDR (port);
      port = SCM_CAR (port);
    }
  else
    {
      /* First try to allocate a print state from the pool */
      SCM_DEFER_INTS;
      if (SCM_NNULLP (SCM_CDR (print_state_pool)))
	{
	  handle = SCM_CDR (print_state_pool);
	  SCM_SETCDR (print_state_pool, SCM_CDDR (print_state_pool));
	}
      SCM_ALLOW_INTS;
      if (handle == SCM_BOOL_F)
	handle = scm_cons (make_print_state (), SCM_EOL);
      pstate_scm = SCM_CAR (handle);
    }

  pstate = SCM_PRINT_STATE (pstate_scm);
  pstate->writingp = writingp;
  scm_iprin1 (exp, port, pstate);

  /* Return print state to pool if it has been created above and
     hasn't escaped to Scheme. */

  if (handle != SCM_BOOL_F && !pstate->revealed)
    {
      SCM_DEFER_INTS;
      SCM_SETCDR (handle, SCM_CDR (print_state_pool));
      SCM_SETCDR (print_state_pool, handle);
      SCM_ALLOW_INTS;
    }
}


/* Print an integer.
 */

void 
scm_intprint (n, radix, port)
     long n;
     int radix;
     SCM port;
{
  char num_buf[SCM_INTBUFLEN];
  scm_lfwrite (num_buf, scm_iint2str (n, radix, num_buf), port);
}

/* Print an object of unrecognized type.
 */

void 
scm_ipruk (hdr, ptr, port)
     char *hdr;
     SCM ptr;
     SCM port;
{
  scm_puts ("#<unknown-", port);
  scm_puts (hdr, port);
  if (SCM_CELLP (ptr))
    {
      scm_puts (" (0x", port);
      scm_intprint (SCM_CAR (ptr), 16, port);
      scm_puts (" . 0x", port);
      scm_intprint (SCM_CDR (ptr), 16, port);
      scm_puts (") @", port);
    }
  scm_puts (" 0x", port);
  scm_intprint (ptr, 16, port);
  scm_putc ('>', port);
}

/* Print a list.
 */


void 
scm_iprlist (hdr, exp, tlr, port, pstate)
     char *hdr;
     SCM exp;
     int tlr;
     SCM port;
     scm_print_state *pstate;
{
  register SCM hare, tortoise;
  int floor = pstate->top - 2;
  scm_puts (hdr, port);
  /* CHECK_INTS; */
  if (pstate->fancyp)
    goto fancy_printing;
  
  /* Run a hare and tortoise so that total time complexity will be
     O(depth * N) instead of O(N^2). */
  hare = SCM_CDR (exp);
  tortoise = exp;
  while (SCM_NIMP (hare) && SCM_ECONSP (hare))
    {
      if (hare == tortoise)
	goto fancy_printing;
      hare = SCM_CDR (hare);
      if (SCM_IMP (hare) || SCM_NECONSP (hare))
	break;
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  
  /* No cdr cycles intrinsic to this list */
  scm_iprin1 (SCM_CAR (exp), port, pstate);
  exp = SCM_CDR (exp);
  for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
    {
      register int i;

      if (SCM_NECONSP (exp))
	break;
      for (i = floor; i >= 0; --i)
	if (pstate->ref_stack[i] == exp)
	  goto circref;
      PUSH_REF (pstate, exp);
      scm_putc (' ', port);
      /* CHECK_INTS; */
      scm_iprin1 (SCM_CAR (exp), port, pstate);
    }
  if (SCM_NNULLP (exp))
    {
      scm_puts (" . ", port);
      scm_iprin1 (exp, port, pstate);
    }

end:
  scm_putc (tlr, port);
  pstate->top = floor + 2;
  return;
  
fancy_printing:
  {
    int n = pstate->length;
    
    scm_iprin1 (SCM_CAR (exp), port, pstate);
    exp = SCM_CDR (exp); --n;
    for (; SCM_NIMP (exp); exp = SCM_CDR (exp))
      {
	register unsigned long i;

	if (SCM_NECONSP (exp))
	  break;
	for (i = 0; i < pstate->top; ++i)
	  if (pstate->ref_stack[i] == exp)
	    goto fancy_circref;
	if (pstate->fancyp)
	  {
	    if (n == 0)
	      {
		scm_puts (" ...", port);
		goto skip_tail;
	      }
	    else
	      --n;
	  }
	PUSH_REF(pstate, exp);
	++pstate->list_offset;
	scm_putc (' ', port);
	/* CHECK_INTS; */
	scm_iprin1 (SCM_CAR (exp), port, pstate);
      }
  }
  if (SCM_NNULLP (exp))
    {
      scm_puts (" . ", port);
      scm_iprin1 (exp, port, pstate);
    }
skip_tail:
  pstate->list_offset -= pstate->top - floor - 2;
  goto end;

fancy_circref:
  pstate->list_offset -= pstate->top - floor - 2;
  
circref:
  scm_puts (" . ", port);
  print_circref (port, pstate, exp);
  goto end;
}



int
scm_valid_oport_value_p	(SCM val)
{
  return (SCM_NIMP (val)
	  && (SCM_OPOUTPORTP (val)
	      || (SCM_CONSP (val)
		  && SCM_NIMP (SCM_CAR (val))
		  && SCM_OPOUTPORTP (SCM_CAR (val))
		  && SCM_NIMP (SCM_CDR (val))
		  && SCM_PRINT_STATE_P (SCM_CDR (val)))));
}

SCM_PROC(s_write, "write", 1, 1, 0, scm_write);

SCM 
scm_write (obj, port)
     SCM obj;
     SCM port;
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_write);

  scm_prin1 (obj, port, 1);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_display, "display", 1, 1, 0, scm_display);

SCM 
scm_display (obj, port)
     SCM obj;
     SCM port;
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_display);

  scm_prin1 (obj, port, 0);
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_newline, "newline", 0, 1, 0, scm_newline);

SCM 
scm_newline (port)
     SCM port;
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG1, s_newline);

  scm_putc ('\n', SCM_COERCE_OUTPORT (port));
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
  else
# endif
#endif
  if (port == scm_cur_outp)
    scm_fflush (port);
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_write_char, "write-char", 1, 1, 0, scm_write_char);

SCM 
scm_write_char (chr, port)
     SCM chr;
     SCM port;
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_valid_oport_value_p (port), port, SCM_ARG2, s_write_char);

  SCM_ASSERT (SCM_ICHRP (chr), chr, SCM_ARG1, s_write_char);
  scm_putc ((int) SCM_ICHR (chr), SCM_COERCE_OUTPORT (port));
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE == errno)
    scm_close_port (port);
# endif
#endif
  return SCM_UNSPECIFIED;
}



/* Call back to Scheme code to do the printing of special objects
(like structs).  SCM_PRINTER_APPLY applies PROC to EXP and a pair
containing PORT and PSTATE.  This pair can be used as the port for
display/write etc to continue the current print chain.  The REVEALED
field of PSTATE is set to true to indicate that the print state has
escaped to Scheme and thus has to be freed by the GC. */

SCM
scm_printer_apply (proc, exp, port, pstate)
     SCM proc, exp, port;
     scm_print_state *pstate;
{
  SCM pair = scm_cons (port, pstate->handle);
  pstate->revealed = 1;
  return scm_apply (proc, exp, scm_cons (pair, scm_listofnull));
}



void
scm_init_print ()
{
  SCM vtable, layout, printer, type;
  
  scm_init_opts (scm_print_options, scm_print_opts, SCM_N_PRINT_OPTIONS);
  vtable = scm_make_vtable_vtable (scm_make_struct_layout (scm_nullstr),
				   SCM_INUM0,
				   SCM_EOL);
  layout = scm_make_struct_layout (scm_makfrom0str (SCM_PRINT_STATE_LAYOUT));
  printer = scm_make_subr_opt (s_print_state_printer,
			       scm_tc7_subr_2,
			       (SCM (*) ()) print_state_printer,
			       0 /* Don't bind the name. */);
  type = scm_make_struct (vtable, SCM_INUM0, SCM_LIST2 (layout, printer));
  print_state_pool = scm_permanent_object (scm_cons (type, SCM_EOL));

  scm_print_state_vtable = type;

#include "print.x"
}
