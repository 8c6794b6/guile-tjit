/* classes: h_files */

#ifndef STRPORTSH
#define STRPORTSH
/*	Copyright (C) 1995,1996, 2000, 2001 Free Software Foundation, Inc.
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




#define SCM_STRPORTP(x)      (!SCM_IMP (x) && \
                              (SCM_TYP16 (x) == scm_tc16_strport))
#define SCM_OPSTRPORTP(x)    (SCM_STRPORTP (x) && \
                              (SCM_CELL_WORD_0 (x) & SCM_OPN))
#define SCM_OPINSTRPORTP(x)  (SCM_OPSTRPORTP (x) && \
 			      (SCM_CELL_WORD_0 (x) & SCM_RDNG))
#define SCM_OPOUTSTRPORTP(x) (SCM_OPSTRPORTP (x) && \
                              (SCM_CELL_WORD_0 (x) & SCM_WRTNG))



extern scm_t_bits scm_tc16_strport;



extern SCM scm_mkstrport (SCM pos, SCM str, long modes, const char * caller);
extern SCM scm_strport_to_string (SCM port);
extern SCM scm_object_to_string (SCM obj, SCM printer);
extern SCM scm_call_with_output_string (SCM proc);
extern SCM scm_call_with_input_string (SCM str, SCM proc);
extern SCM scm_open_input_string (SCM str);
extern SCM scm_open_output_string (void);
extern SCM scm_get_output_string (SCM port);
extern SCM scm_c_read_string (const char *expr);
extern SCM scm_c_eval_string (const char *expr);
extern SCM scm_eval_string (SCM string);
extern void scm_init_strports (void);

#if (SCM_DEBUG_DEPRECATED == 0)

extern SCM scm_strprint_obj (SCM obj);
extern SCM scm_read_0str (char *expr);
extern SCM scm_eval_0str (const char *expr);

#endif /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* STRPORTSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
