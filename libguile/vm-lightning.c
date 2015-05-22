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
scm_do_vm_builtin_ref (unsigned idx)
{
  return scm_vm_builtin_ref (idx);
}

scm_t_subr
scm_do_smob_applicable_p (SCM smob)
{
  return SCM_SMOB_APPLICABLE_P (smob);
}

SCM
scm_do_smob_apply_trampoline (SCM smob)
{
  return SCM_SMOB_DESCRIPTOR (smob).apply_trampoline;
}

SCM
scm_do_foreign_call (scm_i_thread *thread, SCM cif, SCM pointer,
                     const SCM *loc)
{
  return scm_i_foreign_call (scm_inline_cons (thread, cif, pointer), loc);
}

void
scm_do_dynstack_push_fluid (scm_i_thread *thread, SCM fluid, SCM value)
{
  scm_dynstack_push_fluid (&thread->dynstack,
                           fluid, value,
                           thread->dynamic_state);
}

void
scm_do_dynstack_push_dynwind (scm_i_thread *thread, SCM winder, SCM unwinder)
{
  scm_dynstack_push_dynwind (&thread->dynstack, winder, unwinder);
}

void
scm_do_dynstack_push_prompt (scm_i_thread *thread,
                             scm_t_dynstack_prompt_flags flags,
                             SCM key,
                             scm_t_ptrdiff fp_offset,
                             scm_t_ptrdiff sp_offset,
                             scm_t_uint32 *ip,
                             scm_i_jmp_buf *registers)
{
  scm_dynstack_push_prompt (&thread->dynstack, flags, key,
                            fp_offset, sp_offset, ip, registers);
}

void
scm_do_dynstack_pop (scm_i_thread *thread)
{
  scm_dynstack_pop (&thread->dynstack);
}

void
scm_do_unwind_fluid (scm_i_thread *thread)
{
  scm_dynstack_unwind_fluid (&thread->dynstack,
                             thread->dynamic_state);
}

SCM scm_do_allocate_struct (SCM vtable, SCM nfields)
{
  return scm_allocate_struct (vtable, nfields);
}

void
scm_do_vm_expand_stack (struct scm_vm *vp, SCM *new_sp)
{
  vm_expand_stack (vp, new_sp);
}

void
scm_do_vm_abort (struct scm_vm *vp, SCM tag,
                 size_t nstack, SCM *stack_args, SCM tail,
                 SCM *sp, scm_i_jmp_buf *current_registers)
{
  vm_abort (vp, tag, nstack, stack_args, tail, sp, current_registers);
}

void
scm_do_vm_reinstate_partial_continuation (struct scm_vm *vp, SCM cont,
                                          size_t n, SCM *argv,
                                          scm_i_thread *thread,
                                          scm_i_jmp_buf *registers)
{
  vm_reinstate_partial_continuation (vp, cont, n, argv,
                                     &thread->dynstack, registers);
}

/* XXX: Mostly duplicating with vm-engine.c  */
scm_t_uint32
scm_do_bind_kwargs (scm_i_thread *thread, scm_t_uintptr *fp,
                    scm_t_uint32 nargs, scm_t_uint32 *ip,
                    scm_t_uint32 nreq, char flags,
                    scm_t_uint32 nreq_and_opt, scm_t_uint32 ntotal,
                    scm_t_int32 kw_offset)
{
  scm_t_uint32 npositional, nkw, n;
  scm_t_bits kw_bits;
  SCM kw;
  char allow_other_keys, has_rest;

#undef VM_ASSERT
#define VM_ASSERT(condition, handler)     \
  do {                                    \
    if (SCM_UNLIKELY (!(condition)))      \
      {                                   \
        handler;                          \
      }                                   \
  } while (0)

#define LOCAL_REF(i) SCM_PACK (fp[i])
#define LOCAL_SET(i,o) fp[i] = SCM_UNPACK (o)

  allow_other_keys = flags & 0x1;
  has_rest = flags & 0x2;

  kw_bits = (scm_t_bits) (ip + kw_offset);
  kw = SCM_PACK (kw_bits);

  npositional = nreq;
  while (/* while we have args */
         npositional < nargs
         /* and we still have positionals to fill */
         && npositional < nreq_and_opt
         /* and we haven't reached a keyword yet */
         && !scm_is_keyword (LOCAL_REF (npositional)))
    /* bind this optional arg (by leaving it in place) */
    npositional++;

  nkw = nargs - npositional;

  /* shuffle non-positional arguments above ntotal */
  /* ALLOC_FRAME (ntotal + nkw); */

  n = nkw;
  while (n--)
    LOCAL_SET (ntotal + n, LOCAL_REF (npositional + n));

  /* and fill optionals & keyword args with SCM_UNDEFINED */
  n = npositional;

  while (n < ntotal)
    LOCAL_SET (n++, SCM_UNDEFINED);

  VM_ASSERT (has_rest || (nkw % 2) == 0,
             vm_error_kwargs_length_not_even (LOCAL_REF (0)));

  /* Now bind keywords, in the order given.  */
  for (n = 0; n < nkw; n++)
    if (scm_is_keyword (LOCAL_REF (ntotal + n)))
      {
        SCM walk;
        for (walk = kw; scm_is_pair (walk); walk = SCM_CDR (walk))
          if (scm_is_eq (SCM_CAAR (walk), LOCAL_REF (ntotal + n)))
            {
              SCM si = SCM_CDAR (walk);
              LOCAL_SET (SCM_I_INUMP (si) ? SCM_I_INUM (si) : scm_to_uint32 (si),
                         LOCAL_REF (ntotal + n + 1));
              break;
            }
        VM_ASSERT (scm_is_pair (walk) || allow_other_keys,
                   vm_error_kwargs_unrecognized_keyword (LOCAL_REF (0),
                                                         LOCAL_REF (ntotal + n)));
        n++;
      }
    else
      VM_ASSERT (has_rest,
                 vm_error_kwargs_invalid_keyword (LOCAL_REF (0),
                                                  LOCAL_REF (ntotal + n)));

  if (has_rest)
    {
      SCM rest = SCM_EOL;
      n = nkw;
      while (n--)
        rest = scm_inline_cons (thread, LOCAL_REF (ntotal + n), rest);
      LOCAL_SET (nreq_and_opt, rest);
    }

  return ntotal + nkw;

#undef VM_ASSERT
#undef LOCAL_SET
#undef LOCAL_REF
}

static SCM compile_lightning_var;

void
scm_compile_lightning (SCM proc)
{
  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  scm_call_1 (compile_lightning_var, proc);
  scm_c_set_vm_engine_x (SCM_VM_LIGHTNING_ENGINE);
}


/*
 * Main function for vm-lightning
 */

/* Function pointer of runtime function. During initialization, this
   variable get filled with JIT compiled native code in scheme. */
static scm_t_vm_engine scm_lightning_main;

/* Since vm-lightning-engine is a member of the constant array used to
   hold VM engines, VM_NAME need to be a constant function, not a
   variable to dynamically generated function pointer. */
static SCM
VM_NAME (scm_i_thread *thread, struct scm_vm *vp,
         scm_i_jmp_buf *registers, int resume)
#define FUNC_NAME "vm-lightning-engine"
{
  return scm_lightning_main (thread, vp, registers, resume);
}
#undef FUNC_NAME

void
scm_init_vm_lightning (void)
{
  compile_lightning_var =
    SCM_VARIABLE_REF (scm_c_lookup ("compile-lightning"));

  scm_lightning_main =
    (scm_t_vm_engine) (SCM_BYTEVECTOR_CONTENTS
                       (SCM_VARIABLE_REF
                        (scm_c_lookup ("lightning-main-code"))));
}

#else /* !BUILD_VM_LIGHTNING */

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

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
