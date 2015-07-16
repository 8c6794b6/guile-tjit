/* Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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

#ifndef _SCM_VM_TJIT_H_
#define _SCM_VM_TJIT_H_

#include <libguile.h>

enum {
  SCM_TJIT_STATE_INTERPRET,
  SCM_TJIT_STATE_RECORD,
};

typedef SCM (*scm_t_native_code) (scm_i_thread *thread, SCM *fp,
                                  scm_i_jmp_buf *registers);

SCM_API SCM scm_tjit_ip_counter (void);
SCM_API SCM scm_tjit_hot_loop (void);
SCM_API SCM scm_set_tjit_hot_loop_x (SCM count);
SCM_API SCM scm_tjit_hot_exit (void);
SCM_API SCM scm_set_tjit_hot_exit_x (SCM count);
SCM_API SCM scm_tlog_table (void);

SCM_API SCM scm_do_inline_double_cell (scm_i_thread *thread,
                                       scm_t_bits a, scm_t_bits b,
                                       scm_t_bits c, scm_t_bits d);

/* Fields in record-type `tlog', from:
   "module/system/vm/native/tjit/parameters.scm". */
#define SCM_TLOG_CODE(T)         SCM_STRUCT_SLOT_REF (T, 1)
#define SCM_TLOG_EXIT_COUNTS(T)  SCM_STRUCT_SLOT_REF (T, 2)
#define SCM_TLOG_ENTRY_IP(T)     SCM_STRUCT_SLOT_REF (T, 3)

SCM_API void scm_bootstrap_vm_tjit (void);
SCM_API void scm_init_vm_tjit (void);

#endif /* _SCM_VM_MJIT_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
