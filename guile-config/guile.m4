dnl   Autoconf macros for working with Guile.
dnl
dnl   	Copyright (C) 1998,2001 Free Software Foundation, Inc.
dnl
dnl   This program is free software; you can redistribute it and/or modify
dnl   it under the terms of the GNU General Public License as published by
dnl   the Free Software Foundation; either version 2, or (at your option)
dnl   any later version.
dnl
dnl   This program is distributed in the hope that it will be useful,
dnl   but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl   GNU General Public License for more details.
dnl
dnl   You should have received a copy of the GNU General Public License
dnl   along with this software; see the file COPYING.  If not, write to
dnl   the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
dnl   Boston, MA 02111-1307 USA
dnl
dnl   As a special exception, the Free Software Foundation gives permission
dnl   for additional uses of the text contained in its release of GUILE.
dnl
dnl   The exception is that, if you link the GUILE library with other files
dnl   to produce an executable, this does not by itself cause the
dnl   resulting executable to be covered by the GNU General Public License.
dnl   Your use of that executable is in no way restricted on account of
dnl   linking the GUILE library code into it.
dnl
dnl   This exception does not however invalidate any other reasons why
dnl   the executable file might be covered by the GNU General Public License.
dnl
dnl   This exception applies only to the code released by the
dnl   Free Software Foundation under the name GUILE.  If you copy
dnl   code from other Free Software Foundation releases into a copy of
dnl   GUILE, as the General Public License permits, the exception does
dnl   not apply to the code that you add in this way.  To avoid misleading
dnl   anyone as to the status of such modified files, you must delete
dnl   this exception notice from them.
dnl
dnl   If you write modifications of your own for GUILE, it is your choice
dnl   whether to permit this exception to apply to your modifications.
dnl   If you do not wish that, delete this exception notice.


dnl   INDEX
dnl   -----

dnl   GUILE_FLAGS --- set flags for compiling and linking with Guile
dnl   AC_GUILE_MODULE_CHECK --- check feature of a Guile Scheme module
dnl   AC_GUILE_MODULE_AVAILABLE --- check availability of a Guile Scheme module
dnl   AC_GUILE_MODULE_REQUIRED --- fail if a Guile Scheme module is unavailable


dnl   Code
dnl   ----

dnl   GUILE_FLAGS --- set flags for compiling and linking with Guile
dnl
dnl   This macro runs the `guile-config' script, installed with Guile,
dnl   to find out where Guile's header files and libraries are
dnl   installed.  It sets two variables, marked for substitution, as
dnl   by AC_SUBST.
dnl
dnl	GUILE_CFLAGS --- flags to pass to a C or C++ compiler to build
dnl		code that uses Guile header files.  This is almost
dnl		always just a -I flag.
dnl
dnl     GUILE_LDFLAGS --- flags to pass to the linker to link a
dnl		program against Guile.  This includes `-lguile' for
dnl		the Guile library itself, any libraries that Guile
dnl		itself requires (like -lqthreads), and so on.  It may
dnl		also include a -L flag to tell the compiler where to
dnl		find the libraries.

AC_DEFUN([GUILE_FLAGS],[
## The GUILE_FLAGS macro.
  ## First, let's just see if we can find Guile at all.
  AC_MSG_CHECKING(for Guile)
  guile-config link > /dev/null || {
    echo "configure: cannot find guile-config; is Guile installed?" 1>&2
    exit 1
  }
  GUILE_CFLAGS="`guile-config compile`"
  GUILE_LDFLAGS="`guile-config link`"
  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
  AC_MSG_RESULT(yes)
])


dnl   AC_GUILE_MODULE_CHECK --- check feature of a Guile Scheme module
dnl
dnl   AC_GUILE_MODULE_CHECK(var,module,featuretest,description)
dnl   $1 is a shell variable name to be set to "yes" or "no"
dnl   $2 is a list of symbols, like: (ice-9 common-list)
dnl   $3 is a thunk, like: (lambda () BODY ...)
dnl      which returns either 0 or #t to indicate the check passed.
dnl      avoid using the character "#" since that confuses autoconf.
dnl   $4 is a noun phrase passed to AC_MSG_CHECKING

AC_DEFUN([AC_GUILE_MODULE_CHECK],
         [AC_MSG_CHECKING([$2 $4])
          $1=no
          echo '(use-modules $2) (exit ($3))' > conftest
          guile -s conftest > /dev/null 2>&1 && $1=yes
          rm -f conftest
          AC_MSG_RESULT($[$1])
         ])

dnl   AC_GUILE_MODULE_AVAILABLE --- check availability of a Guile Scheme module
dnl
dnl   AC_GUILE_MODULE_AVAILABLE(var,module)
dnl   $1 is a shell variable name to be set to "yes" or "no"
dnl   $2 is a list of symbols, like: (ice-9 common-list)

AC_DEFUN([AC_GUILE_MODULE_AVAILABLE],
         [AC_GUILE_MODULE_CHECK($1,$2,(lambda () 0),availability)
         ])

dnl   AC_GUILE_MODULE_REQUIRED --- fail if a Guile Scheme module is unavailable
dnl
dnl   $1 is a list of symbols, WITHOUT the surrounding parens

AC_DEFUN([AC_GUILE_MODULE_REQUIRED],
         [AC_GUILE_MODULE_AVAILABLE(ac_guile_module_required, ($1))
          if test "$ac_guile_module_required" = "no" ; then
              AC_MSG_ERROR([required guile module not found: ($1)])
          fi
         ])


dnl   guile.m4 ends here
