/*	Copyright (C) 1995,1996,1998 Free Software Foundation, Inc.
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

#include "smob.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif



/* scm_smobs scm_numsmob
 * implement a dynamicly resized array of smob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
int scm_numsmob;
scm_smobfuns *scm_smobs;


long 
scm_newsmob (smob)
     const scm_smobfuns *smob;
{
  char *tmp;
  if (255 <= scm_numsmob)
    goto smoberr;
  SCM_DEFER_INTS;
  SCM_SYSCALL (tmp = (char *) realloc ((char *) scm_smobs, (1 + scm_numsmob) * sizeof (scm_smobfuns)));
  if (tmp)
    {
      scm_smobs = (scm_smobfuns *) tmp;
      scm_smobs[scm_numsmob].mark = smob->mark;
      scm_smobs[scm_numsmob].free = smob->free;
      scm_smobs[scm_numsmob].print = smob->print;
      scm_smobs[scm_numsmob].equalp = smob->equalp;
      scm_numsmob++;
    }
  SCM_ALLOW_INTS;
  if (!tmp)
  smoberr:scm_wta (SCM_MAKINUM ((long) scm_numsmob), (char *) SCM_NALLOC, "newsmob");
  return scm_tc7_smob + (scm_numsmob - 1) * 256;
}


/* {Initialization for i/o types, float, bignum, the type of free cells}
 */

static int
freeprint (SCM exp,
	   SCM port,
	   scm_print_state *pstate)
{
  char buf[100];

  sprintf (buf, "#<freed cell %p; GC missed a reference>", (void *) exp);
  scm_puts (buf, port);

  return 1;
}


static const scm_smobfuns freecell =
{
  0,
  scm_free0,
  freeprint,
  0
};

static const scm_smobfuns flob =
{
  0,
  /*flofree*/ 0,
  scm_floprint,
  scm_floequal
};

static const scm_smobfuns bigob =
{
  0,
  /*bigfree*/ 0,
  scm_bigprint,
  scm_bigequal
};




void
scm_smob_prehistory ()
{
  scm_numsmob = 0;
  scm_smobs = (scm_smobfuns *) malloc (7 * sizeof (scm_smobfuns));

  /* WARNING: These scm_newsmob calls must be done in this order */
  scm_newsmob (&freecell);
  scm_newsmob (&flob);
  scm_newsmob (&bigob);
  scm_newsmob (&bigob);		/* n.b.: two smobs, one smobfuns */
}

