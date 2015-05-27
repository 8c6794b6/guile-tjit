/* Copyright (C) 2014, 2015  Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#ifndef _SCM_LIGHTNING_BINDING_H_
#define _SCM_LIGHTNING_BINDING_H_

#include "libguile/_scm.h"

SCM_API SCM scm_init_jit (SCM arg);
SCM_API SCM scm_finish_jit (void);
SCM_API SCM scm_jit_new_state (void);
SCM_API SCM scm_jit_state (void);
SCM_API SCM scm_jit_code_size (SCM jit);

SCM_API SCM scm_jit_r (SCM i);
SCM_API SCM scm_jit_v (SCM i);
SCM_API SCM scm_jit_f (SCM i);
SCM_API SCM scm_jit_fp (void);
SCM_API SCM scm_jit_r_num (void);
SCM_API SCM scm_jit_v_num (void);
SCM_API SCM scm_jit_f_num (void);

SCM_API SCM scm_jit_clear_state (void);
SCM_API SCM scm_jit_destroy_state (void);

SCM_API SCM scm_jit_address (SCM node);
SCM_API SCM scm_jit_name (SCM name);
SCM_API SCM scm_jit_note (SCM name, SCM pos);
SCM_API SCM scm_jit_label (void);
SCM_API SCM scm_jit_forward (void);
SCM_API SCM scm_jit_indirect (void);
SCM_API SCM scm_jit_link (SCM u);
SCM_API SCM scm_jit_forward_p (SCM u);
SCM_API SCM scm_jit_indirect_p (SCM u);
SCM_API SCM scm_jit_target_p (SCM u);

SCM_API SCM scm_jit_prolog (void);

SCM_API SCM scm_jit_allocai (SCM u);
SCM_API SCM scm_jit_ellipsis (void);

SCM_API SCM scm_jit_arg (void);
SCM_API SCM scm_jit_getarg_c (SCM u, SCM v);
SCM_API SCM scm_jit_getarg_uc (SCM u, SCM v);
SCM_API SCM scm_jit_getarg_s (SCM u, SCM v);
SCM_API SCM scm_jit_getarg_us (SCM u, SCM v);
SCM_API SCM scm_jit_getarg_i (SCM u, SCM v);
SCM_API SCM scm_jit_getarg_ui (SCM u, SCM v);
SCM_API SCM scm_jit_getarg_l (SCM u, SCM v);

SCM_API SCM scm_jit_prepare (void);
SCM_API SCM scm_jit_pushargr (SCM u);
SCM_API SCM scm_jit_pushargi (SCM u);
SCM_API SCM scm_jit_finishr (SCM u);
SCM_API SCM scm_jit_finishi (SCM u);
SCM_API SCM scm_jit_ret (void);
SCM_API SCM scm_jit_retr (SCM u);
SCM_API SCM scm_jit_reti (SCM u);
SCM_API SCM scm_jit_retval_c (SCM u);
SCM_API SCM scm_jit_retval_uc (SCM u);
SCM_API SCM scm_jit_retval_s (SCM u);
SCM_API SCM scm_jit_retval_us (SCM u);
SCM_API SCM scm_jit_retval_i (SCM u);
SCM_API SCM scm_jit_retval_ui (SCM u);
SCM_API SCM scm_jit_retval_l (SCM u);
SCM_API SCM scm_jit_epilog (void);

SCM_API SCM scm_jit_patch (SCM u);
SCM_API SCM scm_jit_patch_at (SCM u, SCM v);
SCM_API SCM scm_jit_patch_abs (SCM u, SCM v);
SCM_API SCM scm_jit_realize (void);
SCM_API SCM scm_jit_get_code (SCM u);
SCM_API SCM scm_jit_set_code (SCM u, SCM v);
SCM_API SCM scm_jit_get_data (SCM u, SCM v);
SCM_API SCM scm_jit_set_data (SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_frame (SCM u);
SCM_API SCM scm_jit_tramp (SCM u);
SCM_API SCM scm_jit_emit (void);

SCM_API SCM scm_jit_print (void);

SCM_API SCM scm_jit_arg_f (void);
SCM_API SCM scm_jit_getarg_f (SCM u, SCM v);
SCM_API SCM scm_jit_putargr_f (SCM u, SCM v);
SCM_API SCM scm_jit_putargi_f (SCM u, SCM v);
SCM_API SCM scm_jit_pushargr_f (SCM u);
SCM_API SCM scm_jit_pushargi_f (SCM u);
SCM_API SCM scm_jit_retr_f (SCM u);
SCM_API SCM scm_jit_reti_f (SCM u);
SCM_API SCM scm_jit_retval_f (SCM u);

SCM_API SCM scm_jit_arg_d (void);
SCM_API SCM scm_jit_getarg_d (SCM u, SCM v);
SCM_API SCM scm_jit_putargr_d (SCM u, SCM v);
SCM_API SCM scm_jit_putargi_d (SCM u, SCM v);
SCM_API SCM scm_jit_pushargr_d (SCM u);
SCM_API SCM scm_jit_pushargi_d (SCM u);
SCM_API SCM scm_jit_retr_d (SCM u);
SCM_API SCM scm_jit_reti_d (SCM u);
SCM_API SCM scm_jit_retval_d (SCM u);

SCM_API SCM scm_jit_new_node (SCM c);
SCM_API SCM scm_jit_new_node_w (SCM c, SCM u);
SCM_API SCM scm_jit_new_node_p (SCM c, SCM u);
SCM_API SCM scm_jit_new_node_ww (SCM c, SCM u, SCM v);
SCM_API SCM scm_jit_new_node_wp (SCM c, SCM u, SCM v);
SCM_API SCM scm_jit_new_node_pw (SCM c, SCM u, SCM v);
SCM_API SCM scm_jit_new_node_wf (SCM c, SCM u, SCM f);
SCM_API SCM scm_jit_new_node_wd (SCM c, SCM u, SCM f);
SCM_API SCM scm_jit_new_node_www (SCM c, SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_new_node_qww (SCM c, SCM l, SCM h, SCM v, SCM w);
SCM_API SCM scm_jit_new_node_wwf (SCM c, SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_new_node_wwd (SCM c, SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_new_node_pww (SCM c, SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_new_node_pwf (SCM c, SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_new_node_pwd (SCM c, SCM u, SCM v, SCM w);

SCM_API SCM scm_jit_arg_register_p (SCM u);
SCM_API SCM scm_jit_callee_save_p (SCM u);
SCM_API SCM scm_jit_pointer_p (SCM u);
SCM_API SCM scm_jit_get_note (SCM n, SCM u, SCM v, SCM w);
SCM_API SCM scm_jit_disassemble (void);

SCM_API SCM scm_make_bytevector_executable_x (SCM bv);

SCM_API void scm_init_lightning (void);

#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
