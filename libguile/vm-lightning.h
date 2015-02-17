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

#ifndef _SCM_VM_LIGHTNING_H_
#define _SCM_VM_LIGHTNING_H_

#include <libguile.h>

SCM_API SCM scm_load_vm_lightning (void);
SCM_API SCM scm_do_inline_cell (scm_i_thread *thread, scm_t_bits car, scm_t_bits cdr);
SCM_API SCM scm_do_inline_cons (scm_i_thread *thread, SCM car, SCM cdr);
SCM_API SCM scm_do_inline_from_double (scm_i_thread *thread, double val);
SCM_API SCM scm_do_i_string_length (SCM str);
SCM_API SCM scm_do_vm_builtin_ref (unsigned idx);
SCM_API SCM scm_do_thread_i_data (SCM thread);

#endif /* _SCM_VM_LIGHTNING_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
