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

#include <ffi.h>
#include <libguile/bdw-gc.h>

SCM_API SCM scm_do_inline_cell (scm_i_thread *thread, scm_t_bits car,
                                scm_t_bits cdr);
SCM_API SCM scm_do_inline_cons (scm_i_thread *thread, SCM car, SCM cdr);
SCM_API SCM scm_do_inline_from_double (scm_i_thread *thread, double val);
SCM_API SCM scm_do_inline_words (scm_i_thread *thread, scm_t_bits car,
                                 scm_t_uint32 n_words);
SCM_API SCM scm_do_vm_builtin_ref (unsigned idx);
SCM_API scm_t_subr scm_do_smob_applicable_p (SCM smob);
SCM_API SCM scm_do_smob_apply_trampoline (SCM smob);
SCM_API SCM scm_do_foreign_call (scm_i_thread *thread, SCM cif, SCM pointer,
                                 const SCM *loc);

SCM_API void scm_do_dynstack_push_fluid (scm_i_thread *thread, SCM fluid,
                                         SCM value);
SCM_API void scm_do_dynstack_push_prompt (scm_i_thread *thread,
                                          scm_t_dynstack_prompt_flags flags,
                                          SCM key,
                                          scm_t_ptrdiff fp_offset,
                                          scm_t_ptrdiff sp_offset,
                                          scm_t_uint32 *ip,
                                          scm_i_jmp_buf *registers);
SCM_API void scm_do_dynstack_push_dynwind (scm_i_thread *thread,
                                           SCM winder, SCM unwinder);
SCM_API void scm_do_dynstack_pop (scm_i_thread *thread);
SCM_API void scm_do_unwind_fluid (scm_i_thread *thread);
SCM_API SCM scm_do_allocate_struct (SCM vtable, SCM nfields);

SCM_API void scm_do_vm_expand_stack (struct scm_vm *vp, SCM *new_sp);
SCM_API void scm_do_vm_abort (struct scm_vm *vp, SCM tag,
                              size_t nstack, SCM *stack_args, SCM tail,
                              SCM *sp, scm_i_jmp_buf *current_registers);
SCM_API void
scm_do_vm_reinstate_partial_continuation (struct scm_vm *vp, SCM cont,
                                          size_t n, SCM *argv,
                                          scm_i_thread *thread,
                                          scm_i_jmp_buf *registers);

SCM_API scm_t_uint32 scm_do_bind_kwargs(scm_i_thread *thread,
                                        scm_t_uintptr *fp,
                                        scm_t_uint32 nargs, scm_t_uint32 *ip,
                                        scm_t_uint32 nreq, char flags,
                                        scm_t_uint32 nreq_and_opt,
                                        scm_t_uint32 ntotal,
                                        scm_t_int32 kw_offset);

SCM_API void scm_compile_lightning (SCM proc);

SCM_API void scm_init_vm_lightning (void);

#endif /* _SCM_VM_LIGHTNING_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
