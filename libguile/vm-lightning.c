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

/* XXX: Duplicate with control.c  */
static const scm_t_uint32 compose_continuation_code[] =
  {
    SCM_PACK_OP_24 (compose_continuation, 0)
  };

/* XXX: Duplicate with control.c  */
static inline SCM
make_partial_continuation (SCM vm_cont)
{
  scm_t_bits nfree = 1;
  scm_t_bits flags = SCM_F_PROGRAM_IS_PARTIAL_CONTINUATION;
  SCM ret;

  ret = scm_words (scm_tc7_program | (nfree << 16) | flags, nfree + 3);
  SCM_SET_CELL_WORD_1 (ret, compose_continuation_code);
  SCM_PROGRAM_FREE_VARIABLE_SET (ret, 0, vm_cont);

  return ret;
}

static inline SCM
do_reify_partial_continuation (SCM *saved_fp,
                               scm_t_uint32 *saved_ip,
                               scm_t_dynstack *dynstack,
                               scm_t_uint32 *ra)
{
  SCM vm_cont;
  /* scm_t_uint32 flags; */

  /* /\* XXX: Non-rewindable continuation ignored. *\/ */
  /* flags = SCM_F_VM_CONT_PARTIAL | SCM_F_VM_CONT_REWINDABLE; */
  vm_cont = scm_cell (scm_tc7_vm_cont, (scm_t_bits) ra);

  return make_partial_continuation (vm_cont);
}

SCM *
scm_do_abort (scm_i_thread *thread, SCM tag, size_t nstack,
              scm_t_uintptr *current_fp, scm_t_uint32 *ra)
{
  scm_t_dynstack *dynstack = &thread->dynstack;
  scm_t_bits *prompt;
  scm_t_dynstack_prompt_flags flags;
  scm_i_jmp_buf *registers;
  scm_t_ptrdiff fp, sp, local_offset;
  scm_t_uint32 *ip;

#define LOCAL_REF(i) SCM_PACK (current_fp[i])

  prompt = scm_dynstack_find_prompt (dynstack, tag,
                                     &flags, &fp, &local_offset, &ip,
                                     &registers);

  if (!prompt)
    scm_misc_error ("abort", "Abort to unknown prompt", scm_list_1 (tag));

  {
    SCM *ret;
    SCM *local = (SCM *)(fp + local_offset);
    SCM cont;
    size_t i;

    if (flags & SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY)
      cont = SCM_BOOL_F;
    else
      {
        scm_t_dynstack *captured;

        captured = scm_dynstack_capture (dynstack, SCM_DYNSTACK_NEXT (prompt));
        cont = do_reify_partial_continuation ((SCM *) fp, ip, captured, ra);
      }

    /* Unwind. */
    scm_dynstack_unwind (dynstack, prompt);

    local[1] = cont;
    for (i = 0; i < nstack; i++)
      local[2 + i] = LOCAL_REF (2 + i);

    sp = fp + local_offset + ((nstack - 1) * sizeof (SCM));

    /* Store address of prompt handler and VM regs. */
    ret = alloca (3 * sizeof (SCM));
    ret[0] = SCM_PACK (ip);
    ret[1] = SCM_PACK (sp);
    ret[2] = SCM_PACK (fp);

    return ret;
  }

#undef LOCAL_REF
}

scm_t_bits scm_do_reinstate_partial_continuation (scm_i_thread *thread,
                                                  SCM cont, size_t n, SCM *argv)
{
  /* XXX: Update locals. */
  return SCM_UNPACK (SCM_CELL_OBJECT_1 (cont));
}

/* XXX: Mostly duplicating with vm-engine.c  */
scm_t_uint32
scm_do_bind_kwargs (scm_t_uintptr *fp,
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

#define LOCAL_REF(i) SCM_PACK (fp[2 + (i)])
#define LOCAL_SET(i,o) fp[2 + (i)] = SCM_UNPACK (o)

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
        rest = scm_cons (LOCAL_REF (ntotal + n), rest);
        /* rest = scm_inline_cons (thread, LOCAL_REF (ntotal + n), rest); */
      LOCAL_SET (nreq_and_opt, rest);
    }

  return ntotal + nkw;

#undef VM_ASSERT
#undef LOCAL_SET
#undef LOCAL_REF
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
    scm_from_uintptr_t ((scm_t_uintptr) vp->fp),
    scm_from_uintptr_t ((scm_t_uintptr) registers),
    scm_from_int (vp->sp + 1 - &SCM_FRAME_LOCAL (vp->fp, 0)),
    scm_from_int (resume)
  };

  /* Call scheme procedure `vm_lightning' with vm-regular engine. */
  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  ret = scm_call_n (vm_lightning_var, argv, 5);
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

void
scm_init_vm_lightning (void)
{
  scm_c_define_gsubr ("smob-apply-trampoline", 1, 0, 0,
                      scm_do_smob_apply_trampoline);
  vm_lightning_var = SCM_VARIABLE_REF (scm_c_lookup ("vm-lightning"));
}

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
