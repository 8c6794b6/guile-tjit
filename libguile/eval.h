/* classes: h_files */

#ifndef EVALH
#define EVALH
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


#include "libguile/__scm.h"



/* {Options}
 */

extern scm_option scm_eval_opts[];

#define SCM_EVAL_STACK	       scm_eval_opts[0].val
#define SCM_N_EVAL_OPTIONS 1

extern int scm_eval_stack;

extern scm_option scm_evaluator_trap_table[];

extern SCM scm_eval_options_interface SCM_P ((SCM setting));

#define SCM_TRAPS_P	       scm_evaluator_trap_table[0].val
#define SCM_ENTER_FRAME_P      scm_evaluator_trap_table[1].val
#define SCM_APPLY_FRAME_P      scm_evaluator_trap_table[2].val
#define SCM_EXIT_FRAME_P       scm_evaluator_trap_table[3].val
#define SCM_N_EVALUATOR_TRAPS 4



/* {Ilocs}
 *
 * Ilocs are relative pointers into local environment structures.
 * 
 */
#define SCM_ILOCP(n)		(SCM_ITAG8(n)==scm_tc8_iloc)
#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)
#define SCM_IDINC		(0x00100000L)
#define SCM_ICDR		(0x00080000L)
#define SCM_IFRINC		(0x00000100L)
#define SCM_IDSTMSK		(-SCM_IDINC)
#define SCM_IFRAME(n) 		((int)((SCM_ICDR-SCM_IFRINC)>>8) & ((int)(n)>>8))
#define SCM_IDIST(n) 		(((unsigned long)(n))>>20)
#define SCM_ICDRP(n) 		(SCM_ICDR & (n))




/* {Evaluator}
 *
 * For an explanation of symbols containing "EVAL", see beginning of eval.c.
 */
#ifdef MEMOIZE_LOCALS
#define SCM_EVALIM(x, env) (SCM_ILOCP(x)?*scm_ilookup((x), env):x)
#else
#define SCM_EVALIM(x, env) x
#endif
#ifdef DEBUG_EXTENSIONS
#define SCM_XEVAL(x, env) (SCM_IMP(x) \
		      ? (x) \
		      : (*scm_ceval_ptr) ((x), (env)))
#define SCM_XEVALCAR(x, env) (SCM_NCELLP(SCM_CAR(x)) \
			  ? (SCM_IMP(SCM_CAR(x)) \
			     ? SCM_EVALIM(SCM_CAR(x), env) \
			     : SCM_GLOC_VAL(SCM_CAR(x))) \
			  : (SCM_SYMBOLP(SCM_CAR(x)) \
			     ? *scm_lookupcar(x, env) \
			     : (*scm_ceval_ptr) (SCM_CAR(x), env)))
#else
#define SCM_XEVAL(x, env) (SCM_IMP(x)?(x):scm_ceval((x), (env)))
#define SCM_XEVALCAR(x, env) EVALCAR(x, env)
#endif /* DEBUG_EXTENSIONS */



#define SCM_EXTEND_ENV scm_acons


extern const char scm_s_expression[];
extern const char scm_s_test[];
extern const char scm_s_body[];
extern const char scm_s_bindings[];
extern const char scm_s_variable[];
extern const char scm_s_clauses[];
extern const char scm_s_formals[];
extern const char scm_s_set_x[];

extern SCM scm_i_dot;
extern SCM scm_i_quote;
extern SCM scm_i_quasiquote;
extern SCM scm_i_lambda;
extern SCM scm_i_let;
extern SCM scm_i_arrow;
extern SCM scm_i_else;
extern SCM scm_i_unquote;
extern SCM scm_i_uq_splicing;
extern SCM scm_i_apply;
extern SCM scm_sym_set_x;

extern long scm_tc16_macro;

/* A resolved global variable reference in the CAR position
 * of a list is stored (in code only) as a pointer to a pair with a 
 * tag of 1.  This is called a "gloc".
 */

#define SCM_GLOC_SYM(x) (SCM_CAR((x)-1L))
#define SCM_GLOC_VAL(x) (SCM_CDR((x)-1L))
#define SCM_GLOC_VAL_LOC(x) (SCM_CDRLOC((x)-1L))



extern SCM * scm_ilookup SCM_P ((SCM iloc, SCM env));
extern SCM * scm_lookupcar SCM_P ((SCM vloc, SCM genv));
extern SCM scm_unmemocar SCM_P ((SCM form, SCM env));
extern SCM scm_unmemocopy SCM_P ((SCM form, SCM env));
extern SCM scm_eval_car SCM_P ((SCM pair, SCM env));
extern SCM scm_eval_args SCM_P ((SCM i, SCM env, SCM proc));
extern SCM scm_deval_args SCM_P ((SCM l, SCM env, SCM proc, SCM *lloc));
extern SCM scm_m_quote SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_begin SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_if SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_set_x SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_vref SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_vset SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_and SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_or SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_case SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_cond SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_lambda SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_letstar SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_do SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_quasiquote SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_delay SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_define SCM_P ((SCM x, SCM env));
extern SCM scm_m_letrec SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_let SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_apply SCM_P ((SCM xorig, SCM env));
extern SCM scm_m_cont SCM_P ((SCM xorig, SCM env));
extern int scm_badargsp SCM_P ((SCM formals, SCM args));
extern SCM scm_ceval SCM_P ((SCM x, SCM env));
extern SCM scm_deval SCM_P ((SCM x, SCM env));
extern SCM scm_nconc2last SCM_P ((SCM lst));
extern SCM scm_apply SCM_P ((SCM proc, SCM arg1, SCM args));
extern SCM scm_dapply SCM_P ((SCM proc, SCM arg1, SCM args));
extern SCM SCM_APPLY SCM_P ((SCM proc, SCM arg1, SCM args));
extern SCM scm_map SCM_P ((SCM proc, SCM arg1, SCM args));
extern SCM scm_for_each SCM_P ((SCM proc, SCM arg1, SCM args));
extern SCM scm_closure SCM_P ((SCM code, SCM env));
extern SCM scm_makprom SCM_P ((SCM code));
extern SCM scm_force SCM_P ((SCM x));
extern SCM scm_promise_p SCM_P ((SCM x));
extern SCM scm_copy_tree SCM_P ((SCM obj));
extern SCM scm_eval_3 SCM_P ((SCM obj, int copyp, SCM env));
extern SCM scm_env_top_level SCM_P ((SCM env));
extern SCM scm_top_level_env SCM_P ((SCM thunk));
extern SCM scm_eval2 SCM_P ((SCM obj, SCM env_thunk));
extern SCM scm_eval SCM_P ((SCM obj));
extern SCM scm_eval_x SCM_P ((SCM obj));
extern void scm_init_eval SCM_P ((void));

#endif  /* EVALH */
