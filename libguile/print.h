/* classes: h_files */

#ifndef SCM_PRINT_H
#define SCM_PRINT_H
/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
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

#include "libguile/options.h"

extern scm_t_option scm_print_opts[];

#define SCM_PRINT_CLOSURE	(SCM_PACK (scm_print_opts[0].val))
#define SCM_PRINT_SOURCE_P	((int) scm_print_opts[1].val)
#define SCM_N_PRINT_OPTIONS 2

/* State information passed around during printing.
 */
#define SCM_PRINT_STATE_P(obj) (SCM_STRUCTP(obj) \
				&& (SCM_EQ_P (SCM_STRUCT_VTABLE(obj), \
				              scm_print_state_vtable)))
#define SCM_PRINT_STATE(obj) ((scm_print_state *) SCM_STRUCT_DATA (obj))

#define RESET_PRINT_STATE(pstate) \
do { \
  pstate->list_offset = 0; \
  pstate->top = 0; \
} while (0)

#define SCM_WRITINGP(pstate) ((pstate)->writingp)
#define SCM_SET_WRITINGP(pstate, x) { (pstate)->writingp = (x); }

#define SCM_PORT_WITH_PS_P(p)    SCM_TYP16_PREDICATE (scm_tc16_port_with_ps, p)
#define SCM_PORT_WITH_PS_PORT(p) SCM_CAR (SCM_CELL_OBJECT_1 (p))
#define SCM_PORT_WITH_PS_PS(p)   SCM_CDR (SCM_CELL_OBJECT_1 (p))

#define SCM_COERCE_OUTPORT(p) \
  (SCM_PORT_WITH_PS_P (p) ? SCM_PORT_WITH_PS_PORT (p) : p)

#define SCM_PRINT_STATE_LAYOUT "sruwuwuwuwuwpwuwuwuruopr"
typedef struct scm_print_state {
  SCM handle;			/* Struct handle */
  int revealed;                 /* Has the state escaped to Scheme? */
  unsigned long writingp;	/* Writing? */
  unsigned long fancyp;		/* Fancy printing? */
  unsigned long level;		/* Max level */
  unsigned long length;		/* Max number of objects per level */
  SCM hot_ref;			/* Hot reference */
  unsigned long list_offset;
  unsigned long top;		/* Top of reference stack */
  unsigned long ceiling;	/* Max size of reference stack */
  SCM *ref_stack;		/* Stack of references used during
				   circular reference detection */
  SCM ref_vect;
} scm_print_state;

extern SCM scm_print_state_vtable;

extern scm_t_bits scm_tc16_port_with_ps;

extern SCM scm_print_options (SCM setting);
SCM scm_make_print_state (void);
void scm_free_print_state (SCM print_state);
extern void scm_intprint (long n, int radix, SCM port);
extern void scm_ipruk (char *hdr, SCM ptr, SCM port);
extern void scm_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate);
extern void scm_print_symbol_name (const char *str, size_t len, SCM port);
extern void scm_prin1 (SCM exp, SCM port, int writingp);
extern void scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate);
extern SCM scm_write (SCM obj, SCM port);
extern SCM scm_display (SCM obj, SCM port);
extern SCM scm_simple_format (SCM port, SCM message, SCM args);
extern SCM scm_newline (SCM port);
extern SCM scm_write_char (SCM chr, SCM port);
extern SCM scm_printer_apply (SCM proc, SCM exp, SCM port, scm_print_state *);
extern SCM scm_port_with_print_state (SCM port, SCM pstate);
extern SCM scm_get_print_state (SCM port);
extern int scm_valid_oport_value_p (SCM val);
extern void scm_init_print (void);

#ifdef GUILE_DEBUG
extern SCM scm_current_pstate (void);
#endif 
#endif  /* SCM_PRINT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
