/* classes: h_files */

#ifndef SCM_CHARSH
#define SCM_CHARSH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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


/* Immediate Characters
 */
#define SCM_ICHRP(x)	(SCM_ITAG8(x) == scm_tc8_char)
#define SCM_ICHR(x)	((unsigned int)SCM_ITAG8_DATA(x))
#define SCM_MAKICHR(x)	SCM_MAKE_ITAG8(x, scm_tc8_char)



extern char *const scm_charnames[];
extern int scm_n_charnames;
extern const char scm_charnums[];



extern SCM scm_char_p SCM_P ((SCM x));
extern SCM scm_char_eq_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_less_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_leq_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_gr_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_geq_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_ci_eq_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_ci_less_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_ci_leq_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_ci_gr_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_ci_geq_p SCM_P ((SCM x, SCM y));
extern SCM scm_char_alphabetic_p SCM_P ((SCM chr));
extern SCM scm_char_numeric_p SCM_P ((SCM chr));
extern SCM scm_char_whitespace_p SCM_P ((SCM chr));
extern SCM scm_char_upper_case_p SCM_P ((SCM chr));
extern SCM scm_char_lower_case_p SCM_P ((SCM chr));
extern SCM scm_char_is_both_p SCM_P ((SCM chr));
extern SCM scm_char_to_integer SCM_P ((SCM chr));
extern SCM scm_integer_to_char SCM_P ((SCM n));
extern SCM scm_char_upcase SCM_P ((SCM chr));
extern SCM scm_char_downcase SCM_P ((SCM chr));
extern void scm_tables_prehistory SCM_P ((void));
extern int scm_upcase SCM_P ((unsigned int c));
extern int scm_downcase SCM_P ((unsigned int c));
extern void scm_init_chars SCM_P ((void));

#endif  /* SCM_CHARSH */
