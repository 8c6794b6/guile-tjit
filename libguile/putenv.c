/* Copyright (C) 1991, 2000, 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA

   As a special exception, the Free Software Foundation gives permission
   for additional uses of the text contained in its release of GUILE.

   The exception is that, if you link the GUILE library with other files
   to produce an executable, this does not by itself cause the
   resulting executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of
   linking the GUILE library code into it.

   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.

   This exception applies only to the code released by the
   Free Software Foundation under the name GUILE.  If you copy
   code from other Free Software Foundation releases into a copy of
   GUILE, as the General Public License permits, the exception does
   not apply to the code that you add in this way.  To avoid misleading
   anyone as to the status of such modified files, you must delete
   this exception notice from them.

   If you write modifications of your own for GUILE, it is your choice
   whether to permit this exception to apply to your modifications.
   If you do not wish that, delete this exception notice.  */


#ifdef HAVE_CONFIG_H
#include "libguile/scmconfig.h"
#endif

#include <sys/types.h>
#include <errno.h>

/* Don't include stdlib.h for non-GNU C libraries because some of them
   contain conflicting prototypes for getopt.
   This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef	__GNU_LIBRARY__
#include <stdlib.h>
#else
char *malloc ();
#endif	/* GNU C library.  */

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
#else
#include <strings.h>
#ifndef strchr
#define strchr index
#endif
#ifndef memcpy
#define memcpy(d, s, n) bcopy((s), (d), (n))
#endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef NULL
#define NULL 0
#endif

extern char **environ;

/* Put STRING, which is of the form "NAME=VALUE", in the environment.  */
int
putenv (const char *string)
{
  char *name_end = strchr (string, '=');
  register size_t size;
  register char **ep;

  if (name_end == NULL)
    {
      /* Remove the variable from the environment.  */
      size = strlen (string);
      for (ep = environ; *ep != NULL; ++ep)
	if (!strncmp (*ep, string, size) && (*ep)[size] == '=')
	  {
	    while (ep[1] != NULL)
	      {
		ep[0] = ep[1];
		++ep;
	      }
	    *ep = NULL;
	    return 0;
	  }
    }

  size = 0;
  for (ep = environ; *ep != NULL; ++ep)
    if (!strncmp (*ep, string, name_end - string) &&
	(*ep)[name_end - string] == '=')
      break;
    else
      ++size;

  if (*ep == NULL)
    {
      static char **last_environ = NULL;
      char **new_environ = (char **) malloc ((size + 2) * sizeof (char *));
      if (new_environ == NULL)
	return -1;
      memcpy ((char *) new_environ, (char *) environ, size * sizeof (char *));
      new_environ[size] = (char *) string;
      new_environ[size + 1] = NULL;
      if (last_environ != NULL)
	free ((char *) last_environ);
      last_environ = new_environ;
      environ = new_environ;
    }
  else
    *ep = (char *) string;

  return 0;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
