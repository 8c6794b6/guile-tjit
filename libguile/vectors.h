/* classes: h_files */

#ifndef VECTORSH
#define VECTORSH
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"



#define SCM_VECTORP(x) (SCM_NIMP(x) && (SCM_TYP7S(x)==scm_tc7_vector))
#define SCM_NVECTORP(x) (!SCM_VECTORP(x))
#define SCM_VELTS(x) ((SCM *)SCM_CDR(x))
#define SCM_VELTS_AS_STACKITEMS(x) ((SCM_STACKITEM *)SCM_CDR(x))
#define SCM_SETVELTS SCM_SETCDR



/*
  bit vectors
 */
#define SCM_BITVEC_REF(a, i) ((SCM_UNPACK(SCM_VELTS(a)[(i)/SCM_LONG_BIT]) & (1L<<((i)%SCM_LONG_BIT))) ? 1 : 0)
#define SCM_BITVEC_SET(a, i) SCM_UNPACK(SCM_VELTS(a)[(i)/SCM_LONG_BIT]) |= (1L<<((i)%SCM_LONG_BIT))
#define SCM_BITVEC_CLR(a, i) SCM_UNPACK(SCM_VELTS(a)[(i)/SCM_LONG_BIT]) &= ~(1L<<((i)%SCM_LONG_BIT))




extern SCM scm_vector_set_length_x (SCM vect, SCM len);
extern SCM scm_vector_p (SCM x);
extern SCM scm_vector_length (SCM v);
extern SCM scm_vector (SCM l);
extern SCM scm_vector_ref (SCM v, SCM k);
extern SCM scm_vector_set_x (SCM v, SCM k, SCM obj);
extern SCM scm_make_vector (SCM k, SCM fill);
extern SCM scm_vector_to_list (SCM v);
extern SCM scm_vector_fill_x (SCM v, SCM fill_x);
extern SCM scm_vector_equal_p (SCM x, SCM y);
extern SCM scm_vector_move_left_x (SCM vec1, SCM start1, SCM end1,
				   SCM vec2, SCM start2);
extern SCM scm_vector_move_right_x (SCM vec1, SCM start1, SCM end1, 
                                    SCM vec2, SCM start2);
extern void scm_init_vectors (void);

#endif  /* VECTORSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
