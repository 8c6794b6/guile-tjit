/* Copyright (C) 2001 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "libguile/_scm.h"

#include "libguile/deprecation.h"
#include "libguile/strings.h"
#include "libguile/ports.h"

/* Windows defines. */
#ifdef __MINGW32__
#define vsnprintf _vsnprintf
#endif



#if (SCM_ENABLE_DEPRECATED == 1)

struct issued_warning {
  struct issued_warning *prev;
  const char *message;
};

static struct issued_warning *issued_warnings;
static enum { detailed, summary, summary_print } mode;

void
scm_c_issue_deprecation_warning (const char *msg)
{
  if (mode != detailed)
    mode = summary_print;
  else
    {
      struct issued_warning *iw;
      for (iw = issued_warnings; iw; iw = iw->prev)
	if (!strcmp (iw->message, msg))
	  return;
      if (scm_gc_running_p)
	fprintf (stderr, "%s\n", msg);
      else
	{
	  scm_puts (msg, scm_current_error_port ());
	  scm_newline (scm_current_error_port ());
	}
      msg = strdup (msg);
      iw = scm_malloc (sizeof (struct issued_warning));
      if (msg == NULL || iw == NULL)
	return;
      iw->message = msg;
      iw->prev = issued_warnings;
      issued_warnings = iw;
    }
}

void
scm_c_issue_deprecation_warning_fmt (const char *msg, ...)
{
  va_list ap;
  char buf[512];

  va_start (ap, msg);
  vsnprintf (buf, 511, msg, ap);
  buf[511] = '\0';
  scm_c_issue_deprecation_warning (buf);
}

SCM_DEFINE(scm_issue_deprecation_warning,
	   "issue-deprecation-warning", 0, 0, 1, 
	   (SCM msgs),
	   "Output @var{msgs} to @code{(current-error-port)} when this "
	   "is the first call to @code{issue-deprecation-warning} with "
	   "this specific @var{msgs}.  Do nothing otherwise. "
	   "The argument @var{msgs} should be a list of strings; "
	   "they are printed in turn, each one followed by a newline.")
#define FUNC_NAME s_scm_issue_deprecation_warning
{
  if (mode != detailed)
    mode = summary_print;
  else
    {
      SCM nl = scm_str2string ("\n");
      SCM msgs_nl = SCM_EOL;
      while (SCM_CONSP (msgs))
	{
	  if (msgs_nl != SCM_EOL)
	    msgs_nl = scm_cons (nl, msgs_nl);
	  msgs_nl = scm_cons (SCM_CAR (msgs), msgs_nl);
	  msgs = SCM_CDR (msgs);
	}
      msgs_nl = scm_string_append (scm_reverse_x (msgs_nl, SCM_EOL));
      scm_c_issue_deprecation_warning (SCM_STRING_CHARS (msgs_nl));
      scm_remember_upto_here_1 (msgs_nl);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
print_deprecation_summary (void)
{
  if (mode == summary_print)
    {
      fputs ("\n"
	     "Some deprecated features have been used.  Set the environment\n"
             "variable GUILE_WARN_DEPRECATED to \"detailed\" and rerun the\n"
	     "program to get more information.  Set it to \"no\" to suppress\n"
	     "this message.\n", stderr);
    }
}

#endif

SCM_DEFINE(scm_include_deprecated_features,
	   "include-deprecated-features", 0, 0, 0,
	   (),
	   "Return @code{#t} iff deprecated features should be included "
           "in public interfaces.")
#define FUNC_NAME s_scm_include_deprecated_features
{
  return SCM_BOOL (SCM_ENABLE_DEPRECATED == 1);
}
#undef FUNC_NAME




void
scm_init_deprecation ()
{
#if (SCM_ENABLE_DEPRECATED == 1)
  const char *level = getenv ("GUILE_WARN_DEPRECATED");
  if (level == NULL)
    level = SCM_WARN_DEPRECATED_DEFAULT;
  if (!strcmp (level, "detailed"))
    mode = detailed;
  else if (!strcmp (level, "no"))
    mode = summary;
  else
    {
      mode = summary;
      atexit (print_deprecation_summary);
    }
#endif
#include "libguile/deprecation.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End: */
