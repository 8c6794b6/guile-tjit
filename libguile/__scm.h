/* classes: h_files */

#ifndef __SCMH
#define __SCMH
/*	Copyright (C) 1995, 1996, 1998 Free Software Foundation, Inc.
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

/* {Supported Options}
 *
 * These may be defined or undefined.
 */

/* If the compile FLAG `SCM_CAUTIOUS' is #defined then the number of
 * arguments is always checked for application of closures.  If the
 * compile FLAG `SCM_RECKLESS' is #defined then they are not checked.
 * Otherwise, number of argument checks for closures are made only when
 * the function position (whose value is the closure) of a combination is
 * not an ILOC or GLOC.  When the function position of a combination is a
 * symbol it will be checked only the first time it is evaluated because
 * it will then be replaced with an ILOC or GLOC.
 */
#undef SCM_RECKLESS
#define SCM_CAUTIOUS

/* After looking up a local for the first time, rewrite the
 * code graph, caching its position.
 */
#define MEMOIZE_LOCALS

/* All the number support there is.
 */
#define SCM_FLOATS
#define BIGNUMS

/* GC should relinquish empty cons-pair arenas. 
 */
#define GC_FREE_SEGMENTS

/* Provide a scheme-accessible count-down timer that
 * generates a pseudo-interrupt.
 */
#define TICKS


/* Use engineering notation when converting numbers strings?
 */
#undef ENGNOT

/* Include support for uniform arrays?
 *
 * Possibly some of the initialization code depends on this
 * being defined, but that is a bug and should be fixed.
 */
#define ARRAYS

#undef SCM_CAREFUL_INTS

/* {Unsupported Options}
 *
 * These must be defined as given here.
 */


#define CCLO

/* Guile Scheme supports the #f/() distinction; Guile Lisp won't.  We
   have horrible plans for their unification.  */
#undef SICP



/* Random options (not yet supported or in final form). */

#define STACK_CHECKING
#undef NO_CEVAL_STACK_CHECKING
#undef LONGLONGS

/* Some auto-generated .h files contain unused prototypes
 * that need these typedefs.
 */
typedef long long_long;
typedef unsigned long ulong_long;



/* What did the configure script discover about the outside world?  */
#include "libguile/scmconfig.h"


/* Write prototype declarations like this:
     int foo SCM_P ((int a, int b));
   At definitions, use K&R style declarations, but make sure there's a
   declarative prototype (as above) in scope.  This will give you
   argument type checking, when available, and be harmless otherwise.  */
#ifdef __STDC__
#  define SCM_P(x) x
#else
#  define SCM_P(x) ()
#endif



/* Define
 *
 * SCM_CHAR_CODE_LIMIT		== UCHAR_MAX + 1
 * SCM_MOST_POSITIVE_FIXNUM 	(LONG_MAX>>2)
 * SCM_MOST_NEGATIVE_FIXNUM 	== SCM_SRS((long)LONG_MIN, 2)
 */

#ifdef HAVE_LIMITS_H
# include <limits.h>
# ifdef UCHAR_MAX
#  define SCM_CHAR_CODE_LIMIT (UCHAR_MAX+1L)
# else
#  define SCM_CHAR_CODE_LIMIT 256L
# endif /* def UCHAR_MAX */
# define SCM_MOST_POSITIVE_FIXNUM (LONG_MAX>>2)
# ifdef _UNICOS			/* Stupid cray bug */
#  define SCM_MOST_NEGATIVE_FIXNUM ((long)LONG_MIN/4)
# else
#  define SCM_MOST_NEGATIVE_FIXNUM SCM_SRS((long)LONG_MIN, 2)
# endif				/* UNICOS */
#else
# define SCM_CHAR_CODE_LIMIT 256L
# define SCM_MOST_POSITIVE_FIXNUM ((long)((unsigned long)~0L>>3))
# if (0 != ~0)
#  define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM-1)
# else
#  define SCM_MOST_NEGATIVE_FIXNUM (-SCM_MOST_POSITIVE_FIXNUM)
# endif /*  (0 != ~0) */
#endif /* def HAVE_LIMITS_H */


#ifdef STDC_HEADERS
# include <stdlib.h>
# ifdef AMIGA
#  include <stddef.h>
# endif /* def AMIGA */
# define scm_sizet size_t
#else
# ifdef _SIZE_T
#  define scm_sizet size_t
# else
#  define scm_sizet unsigned int
# endif /* def _SIZE_T */
#endif /* def STDC_HEADERS */



#include "libguile/tags.h"


#ifdef vms
# ifndef CHEAP_CONTINUATIONS
   typedef int jmp_buf[17];
   extern int setjump(jmp_buf env);
   extern int longjump(jmp_buf env, int ret);
#  define setjmp setjump
#  define longjmp longjump
# else
#  include <setjmp.h>
# endif
#else				/* ndef vms */
# ifdef _CRAY1
    typedef int jmp_buf[112];
    extern int setjump(jmp_buf env);
    extern int longjump(jmp_buf env, int ret);
#  define setjmp setjump
#  define longjmp longjump
# else				/* ndef _CRAY1 */
#  include <setjmp.h>
# endif				/* ndef _CRAY1 */
#endif				/* ndef vms */

/* James Clark came up with this neat one instruction fix for
 * continuations on the SPARC.  It flushes the register windows so
 * that all the state of the process is contained in the stack. 
 */

#ifdef sparc
# define SCM_FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
# define SCM_FLUSH_REGISTER_WINDOWS /* empty */
#endif

/* If stack is not longword aligned then 
 */

/* #define SHORT_ALIGN */
#ifdef THINK_C
# define SHORT_ALIGN
#endif
#ifdef MSDOS
# define SHORT_ALIGN
#endif
#ifdef atarist
# define SHORT_ALIGN
#endif

#ifdef SHORT_ALIGN
typedef short SCM_STACKITEM;
#else
typedef long SCM_STACKITEM;
#endif


#ifndef USE_THREADS
#define SCM_THREAD_DEFER
#define SCM_THREAD_ALLOW
#define SCM_THREAD_REDEFER
#define SCM_THREAD_REALLOW_1
#define SCM_THREAD_REALLOW_2
#define SCM_THREAD_SWITCHING_CODE
#endif

extern unsigned int scm_async_clock;

#define SCM_ASYNC_TICK \
{ \
  if (0 == --scm_async_clock) \
    scm_async_click (); \
} \

#ifdef SCM_CAREFUL_INTS
#define SCM_CHECK_NOT_DISABLED \
  if (scm_ints_disabled) \
    fputs("ints already disabled\n", stderr); \

#define SCM_CHECK_NOT_ENABLED \
  if (!scm_ints_disabled) \
    fputs("ints already enabled\n", stderr); \

#else
#define SCM_CHECK_NOT_DISABLED
#define SCM_CHECK_NOT_ENABLED
#endif


/* Anthony Green writes:
   When the compiler sees...
	   DEFER_INTS;
	   [critical code here]
	   ALLOW_INTS;
   ...it doesn't actually promise to keep the critical code within the
   boundries of the DEFER/ALLOW_INTS instructions. It may very well
   schedule it outside of the magic defined in those macros.

   However, GCC's volatile asm feature forms a barrier over which code is
   never moved. So if you add...
	   asm ("");
   ...to each of the DEFER_INTS and ALLOW_INTS macros, the critical
   code will always remain in place.  asm's without inputs or outputs
   are implicitly volatile. */
#ifdef __GNUC__
#define SCM_FENCE asm /* volatile */ ("")
#else
#define SCM_FENCE
#endif

#define SCM_DEFER_INTS \
{ \
  SCM_FENCE; \
  SCM_CHECK_NOT_DISABLED; \
  SCM_THREAD_DEFER; \
  SCM_FENCE; \
  scm_ints_disabled = 1; \
  SCM_FENCE; \
} \


#define SCM_ALLOW_INTS_ONLY \
{ \
  SCM_THREAD_ALLOW; \
  scm_ints_disabled = 0; \
} \


#define SCM_ALLOW_INTS \
{ \
  SCM_FENCE; \
  SCM_CHECK_NOT_ENABLED; \
  SCM_THREAD_SWITCHING_CODE; \
  SCM_FENCE; \
  scm_ints_disabled = 0; \
  SCM_FENCE; \
  SCM_THREAD_ALLOW; \
  SCM_ASYNC_TICK; \
  SCM_FENCE; \
} \


#define SCM_REDEFER_INTS  \
{ \
  SCM_FENCE; \
  SCM_THREAD_REDEFER; \
  ++scm_ints_disabled; \
  SCM_FENCE; \
} \


#define SCM_REALLOW_INTS \
{ \
  SCM_FENCE; \
  SCM_THREAD_REALLOW_1; \
  SCM_THREAD_SWITCHING_CODE; \
  SCM_FENCE; \
  --scm_ints_disabled; \
  if (!scm_ints_disabled) \
    { \
      SCM_THREAD_REALLOW_2; \
      SCM_ASYNC_TICK; \
    } \
  SCM_FENCE; \
} \


#define SCM_TICK \
{ \
  SCM_DEFER_INTS; \
  SCM_ALLOW_INTS; \
} \



/* Classification of critical sections
 *
 * When Guile moves to POSIX threads, it won't be possible to prevent
 * context switching.  In fact, the whole idea of context switching is
 * bogus if threads are run by different processors.  Therefore, we
 * must ultimately eliminate all critical sections or enforce them by
 * use of mutecis.
 *
 * All instances of SCM_DEFER_INTS and SCM_ALLOW_INTS should therefore
 * be classified and replaced by one of the delimiters below.  If you
 * understand what this is all about, I'd like to encourage you to
 * help with this task.  The set of classes below must of course be
 * incrementally augmented.
 *
 * MDJ 980419 <djurfeldt@nada.kth.se>
 */

/* A sections
 *
 * Allocation of a cell with type tag in the CAR.
 *
 * With POSIX threads, each thread will have a private pool of free
 * cells.  Therefore, this type of section can be removed.  But!  It
 * is important that the CDR is initialized first (with the CAR still
 * indicating a free cell) so that we can guarantee a consistent heap
 * at all times.
 */

#ifdef SCM_POSIX_THREADS
#define SCM_ENTER_A_SECTION
#define SCM_EXIT_A_SECTION
#else
#define SCM_ENTER_A_SECTION SCM_DEFER_INTS
#define SCM_EXIT_A_SECTION SCM_ALLOW_INTS
#endif



/** SCM_ASSERT
 ** 
 **/


#ifdef SCM_RECKLESS
#define SCM_ASSERT(_cond, _arg, _pos, _subr)
#define SCM_ASRTGO(_cond, _label)
#else
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          scm_wta(_arg, (char *)(_pos), _subr)
#define SCM_ASRTGO(_cond, _label) \
        if (!(_cond)) \
          goto _label
#endif

#define SCM_ARGn 		0
#define SCM_ARG1 		1
#define SCM_ARG2 		2
#define SCM_ARG3 		3
#define SCM_ARG4 		4
#define SCM_ARG5 		5
#define SCM_ARG6 		6
#define SCM_ARG7 		7 
     /* #define SCM_ARGERR(X) 		((X) < SCM_WNA \
				 ? (char *)(X) \
				 : "wrong type argument")
				 */

/* Following must match entry indexes in scm_errmsgs[].
 * Also, SCM_WNA must follow the last SCM_ARGn in sequence.
 */
#define SCM_WNA 		8
     /* #define SCM_OVSCM_FLOW 		9 */
#define SCM_OUTOFRANGE 		10
#define SCM_NALLOC 		11
     /* #define SCM_STACK_OVFLOW	12 */
     /* #define SCM_EXIT 		13 */


/* (...still matching scm_errmsgs)  These
 * are signals.  Signals may become errors
 * but are distinguished because they first
 * try to invoke a handler that can resume
 * the interrupted routine.
 */
#define SCM_HUP_SIGNAL 		14
#define SCM_INT_SIGNAL 		15
#define SCM_FPE_SIGNAL 		16
#define SCM_BUS_SIGNAL 		17
#define SCM_SEGV_SIGNAL 	18
#define SCM_ALRM_SIGNAL 	19
#define SCM_GC_SIGNAL		20
#define SCM_TICK_SIGNAL		21

#define SCM_SIG_ORD(X)		((X) - SCM_HUP_SIGNAL)
#define SCM_ORD_SIG(X)		((X) + SCM_HUP_SIGNAL)
#define SCM_NUM_SIGS		(SCM_SIG_ORD (SCM_TICK_SIGNAL) + 1)

#if 0
struct errdesc
{
  char *msg;
  char *s_response;
  short parent_err;
};


extern struct errdesc scm_errmsgs[];
#endif



/* SCM_EXIT_SUCCESS is the default code to return from SCM if no errors
 * were encountered.  SCM_EXIT_FAILURE is the default code to return from
 * SCM if errors were encountered.  The return code can be explicitly
 * specified in a SCM program with (scm_quit <n>).
 */

#ifndef SCM_EXIT_SUCCESS
#ifdef vms
#define SCM_EXIT_SUCCESS 1
#else
#define SCM_EXIT_SUCCESS 0
#endif /* def vms */
#endif /* ndef SCM_EXIT_SUCCESS */
#ifndef SCM_EXIT_FAILURE
#ifdef vms
#define SCM_EXIT_FAILURE 2
#else
#define SCM_EXIT_FAILURE 1
#endif /* def vms */
#endif /* ndef SCM_EXIT_FAILURE */





#endif  /* __SCMH */
