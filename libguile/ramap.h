/* classes: h_files */

#ifndef RAMAPH
#define RAMAPH
/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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


#include "libguile/__scm.h"



extern int scm_ra_matchp SCM_P ((SCM ra0, SCM ras));
extern int scm_ramapc SCM_P ((int (*cproc) (), SCM data, SCM ra0, SCM lra,
			      const char *what));
extern int scm_array_fill_int SCM_P ((SCM ra, SCM fill, SCM ignore));
extern SCM scm_array_fill_x SCM_P ((SCM ra, SCM fill));
extern SCM scm_array_copy_x SCM_P ((SCM src, SCM dst));
extern int scm_ra_eqp SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_lessp SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_leqp SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_grp SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_greqp SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_sum SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_difference SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_product SCM_P ((SCM ra0, SCM ras));
extern int scm_ra_divide SCM_P ((SCM ra0, SCM ras));
extern int scm_array_identity SCM_P ((SCM src, SCM dst));
extern SCM scm_array_map_x SCM_P ((SCM ra0, SCM proc, SCM lra));
extern SCM scm_array_for_each SCM_P ((SCM proc, SCM ra0, SCM lra));
extern SCM scm_array_index_map_x SCM_P ((SCM ra, SCM proc));
extern SCM scm_raequal SCM_P ((SCM ra0, SCM ra1));
extern SCM scm_array_equal_p SCM_P ((SCM ra0, SCM ra1));
extern void scm_init_ramap SCM_P ((void));

#endif  /* RAMAPH */
