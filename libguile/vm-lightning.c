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

#if BUILD_VM_LIGHTNING == 1

/*
 * Auxiliary
 */

static SCM vm_lightning_var;

SCM
scm_do_inline_cell (scm_i_thread *thread, scm_t_bits car, scm_t_bits cdr)
{
  return scm_inline_cell (thread, car, cdr);
}

SCM
scm_do_inline_cons (scm_i_thread *thread, SCM car, SCM cdr)
{
  return scm_inline_cons (thread, car, cdr);
}

SCM
scm_do_inline_from_double (scm_i_thread *thread, double val)
{
  SCM z;

  z = SCM_PACK_POINTER
    (scm_inline_gc_malloc_pointerless (thread, sizeof (scm_t_double)));

  SCM_SET_CELL_TYPE (z, scm_tc16_real);
  SCM_REAL_VALUE (z) = val;

  return z;
}

SCM
scm_do_inline_words (scm_i_thread *thread, scm_t_bits car,
                     scm_t_uint32 n_words)
{
  return scm_inline_words (thread, car, n_words);
}

SCM
scm_do_i_string_length (SCM str)
{
  if (SCM_LIKELY (scm_is_string (str)))
    return SCM_I_MAKINUM (scm_i_string_length (str));
  else
    return scm_string_length (str);
}

SCM
scm_do_vm_builtin_ref (unsigned idx)
{
  return scm_vm_builtin_ref (idx);
}

SCM
scm_do_thread_i_data (SCM thread)
{
  return scm_from_pointer (SCM_I_THREAD_DATA (thread), NULL);
}

void
scm_init_vm_lightning (void)
{
  scm_c_define_gsubr ("thread-i-data", 1, 0, 0, scm_do_thread_i_data);
  vm_lightning_var = SCM_VARIABLE_REF (scm_c_lookup ("vm-lightning"));
}


/*
 * Main function for vm-lightning
 */

static SCM
VM_NAME (scm_i_thread *thread, struct scm_vm *vp,
         scm_i_jmp_buf *registers, int resume)
#define FUNC_NAME "vm-lightning-engine"
{
  SCM ret;
  SCM argv[] = {
    scm_from_uintptr_t ((scm_t_uintptr) thread),
    scm_from_uintptr_t ((scm_t_uintptr) vp),
    scm_from_uintptr_t ((scm_t_uintptr) vp->ip),
    scm_from_uintptr_t ((scm_t_uintptr) vp->sp),
    scm_from_uintptr_t ((scm_t_uintptr) vp->fp),
    scm_from_uintptr_t ((scm_t_uintptr) vp->stack_limit),
    scm_from_uintptr_t ((scm_t_uintptr) vp->sp_max_since_gc),
    scm_from_uintptr_t ((scm_t_uintptr) vp->stack_size),
    scm_from_uintptr_t ((scm_t_uintptr) vp->stack_base),
    scm_from_uintptr_t ((scm_t_uintptr) registers),
    scm_from_int (vp->sp + 1 - &SCM_FRAME_LOCAL (vp->fp, 0)),
    scm_from_int (resume)
  };

  /* Call scheme procedure `vm_lightning' with vm-regular engine. */
  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  ret = scm_call_n (vm_lightning_var, argv, 12);
  scm_c_set_vm_engine_x (SCM_VM_LIGHTNING_ENGINE);

  /* Move vp->fp, once for passed procedure  */
  vp->ip = SCM_FRAME_RETURN_ADDRESS (vp->fp);
  vp->sp = SCM_FRAME_PREVIOUS_SP (vp->fp);
  vp->fp = SCM_FRAME_DYNAMIC_LINK (vp->fp);

  /* Move again, for boot continuation added in scm_call_n. */
  vp->ip = SCM_FRAME_RETURN_ADDRESS (vp->fp);
  vp->sp = SCM_FRAME_PREVIOUS_SP (vp->fp);
  vp->fp = SCM_FRAME_DYNAMIC_LINK (vp->fp);

  return ret;
}
#undef FUNC_NAME

#else

static SCM
VM_NAME (scm_i_thread *thread, struct scm_vm *vp,
         scm_i_jmp_buf *registers, int resume)
#define FUNC_NAME "vm-lightning-engine"
{
  scm_puts_unlocked ("NOTE: Run configure with --enable-lightning.\n",
                     scm_current_output_port ());
  return vm_regular_engine (thread, vp, registers, resume);
}
#undef FUNC_NAME

#endif /* BUILD_VM_LIGHTNING */
