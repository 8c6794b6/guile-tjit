/*	Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.
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
 */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



/* regex-posix.c -- POSIX regular expression support.

   This code was written against Henry Spencer's famous regex package.
   The principal reference for POSIX behavior was the man page for this
   library, not the 1003.2 document itself.  Ergo, other `POSIX'
   libraries which do not agree with the Spencer implementation may
   produce varying behavior.  Sigh. */

#include <stdio.h>
#include <sys/types.h>

#include "_scm.h"

/* Supposedly, this file is never compiled unless we know we have
   POSIX regular expressions.  But we still put this in an #ifdef so
   the file is CPP'able (for dependency scanning) even on systems that
   don't have a <regex.h> header.  */
#ifdef HAVE_REGCOMP
#ifdef HAVE_REGEX_H
#include <regex.h>
#else
#ifdef HAVE_RXPOSIX_H
#include <rxposix.h>		/* GNU Rx library */
#else
#ifdef HAVE_RX_RXPOSIX_H
#include <rx/rxposix.h>		/* GNU Rx library on Linux */
#endif
#endif
#endif
#endif

#include "smob.h"
#include "symbols.h"
#include "vectors.h"
#include "strports.h"
#include "ports.h"
#include "feature.h"
#include "strings.h"

#include "validate.h"
#include "regex-posix.h"

/* This is defined by some regex libraries and omitted by others. */
#ifndef REG_BASIC
#define REG_BASIC 0
#endif

long scm_tc16_regex;

static scm_sizet
free_regex (SCM obj)
{
  regfree (SCM_RGX (obj));
  free (SCM_RGX (obj));
  return sizeof(regex_t);
}



SCM_SYMBOL (scm_regexp_error_key, "regular-expression-syntax");

static char *
scm_regexp_error_msg (int regerrno, regex_t *rx)
{
  SCM errmsg;
  int l;

  /* FIXME: must we wrap any external calls in SCM_DEFER_INTS...SCM_ALLOW_INTS?
     Or are these only necessary when a SCM object may be left in an
     undetermined state (half-formed)?  If the latter then I believe we
     may do without the critical section code. -twp */

  /* We could simply make errmsg a char pointer, and allocate space with
     malloc.  But since we are about to pass the pointer to scm_error, which
     never returns, we would never have the opportunity to free it.  Creating
     it as a SCM object means that the system will GC it at some point. */

  errmsg = scm_make_string (SCM_MAKINUM (80), SCM_UNDEFINED);
  SCM_DEFER_INTS;
  l = regerror (regerrno, rx, SCM_CHARS (errmsg), 80);
  if (l > 80)
    {
      errmsg = scm_make_string (SCM_MAKINUM (l), SCM_UNDEFINED);
      regerror (regerrno, rx, SCM_CHARS (errmsg), l);
    }
  SCM_ALLOW_INTS;
  return SCM_CHARS (errmsg);
}

SCM_DEFINE (scm_regexp_p, "regexp?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} if @var{obj} is a compiled regular expression, or\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_regexp_p
{
  return SCM_BOOL(SCM_RGXP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_regexp, "make-regexp", 1, 0, 1, 
            (SCM pat, SCM flags),
	    "Compile the regular expression described by @var{str}, and return the\n"
	    "compiled regexp structure.  If @var{str} does not describe a legal\n"
	    "regular expression, @code{make-regexp} throws a\n"
	    "@code{regular-expression-syntax} error.\n\n"
	    "The @var{flag} arguments change the behavior of the compiled regexp.\n"
	    "The following flags may be supplied:\n\n"
	    "@table @code\n"
	    "@item regexp/icase\n"
	    "Consider uppercase and lowercase letters to be the same when matching.\n\n"
	    "@item regexp/newline\n"
	    "If a newline appears in the target string, then permit the @samp{^} and\n"
	    "@samp{$} operators to match immediately after or immediately before the\n"
	    "newline, respectively.  Also, the @samp{.} and @samp{[^...]} operators\n"
	    "will never match a newline character.  The intent of this flag is to\n"
	    "treat the target string as a buffer containing many lines of text, and\n"
	    "the regular expression as a pattern that may match a single one of those\n"
	    "lines.\n\n"
	    "@item regexp/basic\n"
	    "Compile a basic (``obsolete'') regexp instead of the extended\n"
	    "(``modern'') regexps that are the default.  Basic regexps do not\n"
	    "consider @samp{|}, @samp{+} or @samp{?} to be special characters, and\n"
	    "require the @samp{@{...@}} and @samp{(...)} metacharacters to be\n"
	    "backslash-escaped (@pxref{Backslash Escapes}).  There are several other\n"
	    "differences between basic and extended regular expressions, but these\n"
	    "are the most significant.\n\n"
	    "@item regexp/extended\n"
	    "Compile an extended regular expression rather than a basic regexp.  This\n"
	    "is the default behavior; this flag will not usually be needed.  If a\n"
	    "call to @code{make-regexp} includes both @code{regexp/basic} and\n"
	    "@code{regexp/extended} flags, the one which comes last will override\n"
	    "the earlier one.\n"
	    "@end table\n")
#define FUNC_NAME s_scm_make_regexp
{
  SCM flag;
  regex_t *rx;
  int status, cflags;

  SCM_VALIDATE_ROSTRING (1,pat);
  SCM_COERCE_SUBSTR (pat);

  /* Examine list of regexp flags.  If REG_BASIC is supplied, then
     turn off REG_EXTENDED flag (on by default). */
  cflags = REG_EXTENDED;
  flag = flags;
  while (SCM_NNULLP (flag))
    {
      if (SCM_INUM (SCM_CAR (flag)) == REG_BASIC)
	cflags &= ~REG_EXTENDED;
      else
	cflags |= SCM_INUM (SCM_CAR (flag));
      flag = SCM_CDR (flag);
    }
	  
  rx = SCM_MUST_MALLOC_TYPE(regex_t);
  status = regcomp (rx, SCM_ROCHARS (pat),
		    /* Make sure they're not passing REG_NOSUB;
                       regexp-exec assumes we're getting match data.  */
		    cflags & ~REG_NOSUB);
  if (status != 0)
    {
      scm_error (scm_regexp_error_key,
		 FUNC_NAME,
		 scm_regexp_error_msg (status, rx),
		 SCM_BOOL_F,
		 SCM_BOOL_F);
      /* never returns */
    }
  SCM_RETURN_NEWSMOB (scm_tc16_regex, rx);
}
#undef FUNC_NAME

SCM_DEFINE (scm_regexp_exec, "regexp-exec", 2, 2, 0, 
            (SCM rx, SCM str, SCM start, SCM flags),
	    "Match the compiled regular expression @var{regexp} against @code{str}.\n"
	    "If the optional integer @var{start} argument is provided, begin matching\n"
	    "from that position in the string.  Return a match structure describing\n"
	    "the results of the match, or @code{#f} if no match could be found.")
#define FUNC_NAME s_scm_regexp_exec
{
  int status, nmatches, offset;
  regmatch_t *matches;
  SCM mvec = SCM_BOOL_F;

  SCM_VALIDATE_RGXP (1,rx);
  SCM_VALIDATE_ROSTRING (2,str);
  SCM_VALIDATE_INUM_DEF_COPY (3,start,0,offset);
  SCM_ASSERT_RANGE (3,start,offset >= 0 && (unsigned) offset <= SCM_LENGTH (str));
  if (SCM_UNBNDP (flags))
    flags = SCM_INUM0;
  SCM_VALIDATE_INUM (4,flags);
  SCM_COERCE_SUBSTR (str);

  /* re_nsub doesn't account for the `subexpression' representing the
     whole regexp, so add 1 to nmatches. */

  nmatches = SCM_RGX(rx)->re_nsub + 1;
  SCM_DEFER_INTS;
  matches = SCM_MUST_MALLOC_TYPE_NUM (regmatch_t,nmatches);
  status = regexec (SCM_RGX (rx), SCM_ROCHARS (str) + offset,
		    nmatches, matches,
		    SCM_INUM (flags));
  if (!status)
    {
      int i;
      /* The match vector must include a cell for the string that was matched,
	 so add 1. */
      mvec = scm_make_vector (SCM_MAKINUM (nmatches + 1), SCM_UNSPECIFIED);
      SCM_VELTS(mvec)[0] = str;
      for (i = 0; i < nmatches; ++i)
	if (matches[i].rm_so == -1)
	  SCM_VELTS(mvec)[i+1] = scm_cons (SCM_MAKINUM (-1), SCM_MAKINUM (-1));
	else
	  SCM_VELTS(mvec)[i+1]
	    = scm_cons(SCM_MAKINUM(matches[i].rm_so + offset),
		       SCM_MAKINUM(matches[i].rm_eo + offset));
    }
  scm_must_free ((char *) matches);
  SCM_ALLOW_INTS;

  if (status != 0 && status != REG_NOMATCH)
    scm_error (scm_regexp_error_key,
	       FUNC_NAME,
	       scm_regexp_error_msg (status, SCM_RGX (rx)),
	       SCM_BOOL_F,
	       SCM_BOOL_F);
  return mvec;
}
#undef FUNC_NAME

void
scm_init_regex_posix ()
{
  scm_tc16_regex = scm_make_smob_type_mfpe ("regexp", sizeof (regex_t),
                                            NULL, free_regex, NULL, NULL);

  /* Compilation flags.  */
  scm_sysintern ("regexp/basic", scm_long2num (REG_BASIC));
  scm_sysintern ("regexp/extended", scm_long2num (REG_EXTENDED));
  scm_sysintern ("regexp/icase", scm_long2num (REG_ICASE));
  scm_sysintern ("regexp/newline", scm_long2num (REG_NEWLINE));

  /* Execution flags.  */
  scm_sysintern ("regexp/notbol", scm_long2num (REG_NOTBOL));
  scm_sysintern ("regexp/noteol", scm_long2num (REG_NOTEOL));

#include "regex-posix.x"

  scm_add_feature ("regex");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
