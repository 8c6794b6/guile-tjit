/*	Copyright (C) 1995,1996, 1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include "_scm.h"
#include "versiondat.h"

#include "version.h"


/* Return a Scheme string containing Guile's major version number.  */

SCM_DEFINE (scm_major_version, "major-version", 0, 0, 0, 
            (),
            "Return a string containing Guile's major version number.\n"
            "E.g., \"1\".")
#define FUNC_NAME s_scm_major_version
{
  return scm_makfrom0str (GUILE_MAJOR_VERSION);
}
#undef FUNC_NAME

/* Return a Scheme string containing Guile's minor version number.  */

SCM_DEFINE (scm_minor_version, "minor-version", 0, 0, 0, 
            (),
            "Return a string containing Guile's minor version number.\n"
            "E.g., \"3.5\".")
#define FUNC_NAME s_scm_minor_version
{
  return scm_makfrom0str (GUILE_MINOR_VERSION);
}
#undef FUNC_NAME

/* Return a Scheme string containing Guile's complete version.  */

SCM_DEFINE (scm_version, "version", 0, 0, 0, 
            (),
	    "@deffnx primitive major-version\n"
	    "@deffnx primitive minor-version\n"
	    "Return a string describing Guile's version number, or its major or minor\n"
	    "version numbers, respectively.\n\n"
	    "@example\n"
	    "(version) @result{} \"1.3a\"\n"
	    "(major-version) @result{} \"1\"\n"
	    "(minor-version) @result{} \"3a\"\n"
	    "@end example")
#define FUNC_NAME s_scm_version
{
  return scm_makfrom0str (GUILE_VERSION);
}
#undef FUNC_NAME




void
scm_init_version ()
{
#include "version.x"
}
