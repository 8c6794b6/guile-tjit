/* Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <alloca.h>
#include <alignof.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#include "libguile/bdw-gc.h"
#include <gc/gc_mark.h>

#include "_scm.h"
#include "control.h"
#include "frames.h"
#include "gc-inline.h"
#include "instructions.h"
#include "loader.h"
#include "programs.h"
#include "simpos.h"
#include "vm.h"
#include "vm-builtins.h"

static int vm_default_engine = SCM_VM_REGULAR_ENGINE;

/* Unfortunately we can't snarf these: snarfed things are only loaded up from
   (system vm vm), which might not be loaded before an error happens. */
static SCM sym_vm_run;
static SCM sym_vm_error;
static SCM sym_keyword_argument_error;
static SCM sym_regular;
static SCM sym_debug;

/* The page size.  */
static size_t page_size;

/* The VM has a number of internal assertions that shouldn't normally be
   necessary, but might be if you think you found a bug in the VM. */
/* #define VM_ENABLE_ASSERTIONS */

static void vm_expand_stack (struct scm_vm *vp,
                             union scm_vm_stack_element *new_sp) SCM_NOINLINE;

/* RESTORE is for the case where we know we have done a PUSH of equal or
   greater stack size in the past.  Otherwise PUSH is the thing, which
   may expand the stack.  */
enum vm_increase_sp_kind { VM_SP_PUSH, VM_SP_RESTORE };

static inline void
vm_increase_sp (struct scm_vm *vp, union scm_vm_stack_element *new_sp,
                enum vm_increase_sp_kind kind)
{
  if (new_sp >= vp->sp_min_since_gc)
    {
      vp->sp = new_sp;
      return;
    }

  if (kind == VM_SP_PUSH && new_sp < vp->stack_limit)
    vm_expand_stack (vp, new_sp);
  else
    vp->sp_min_since_gc = vp->sp = new_sp;
}

static inline void
vm_push_sp (struct scm_vm *vp, union scm_vm_stack_element *new_sp)
{
  vm_increase_sp (vp, new_sp, VM_SP_PUSH);
}

static inline void
vm_restore_sp (struct scm_vm *vp, union scm_vm_stack_element *new_sp)
{
  vm_increase_sp (vp, new_sp, VM_SP_RESTORE);
}


/*
 * VM Continuation
 */

void
scm_i_vm_cont_print (SCM x, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<vm-continuation ", port);
  scm_uintprint (SCM_UNPACK (x), 16, port);
  scm_puts_unlocked (">", port);
}

int
scm_i_vm_cont_to_frame (SCM cont, struct scm_frame *frame)
{
  struct scm_vm_cont *data = SCM_VM_CONT_DATA (cont);
  union scm_vm_stack_element *stack_top;

  stack_top = data->stack_bottom + data->stack_size;
  frame->stack_holder = data;
  frame->fp_offset = stack_top - (data->fp + data->reloc);
  frame->sp_offset = data->stack_size;
  frame->ip = data->ra;

  return 1;
}

/* Ideally we could avoid copying the C stack if the continuation root
   is inside VM code, and call/cc was invoked within that same call to
   vm_run.  That's currently not implemented.  */
SCM
scm_i_vm_capture_stack (union scm_vm_stack_element *stack_top,
                        union scm_vm_stack_element *fp,
                        union scm_vm_stack_element *sp, scm_t_uint32 *ra,
                        scm_t_dynstack *dynstack, scm_t_uint32 flags)
{
  struct scm_vm_cont *p;

  p = scm_gc_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = stack_top - sp;
  p->stack_bottom = scm_gc_malloc (p->stack_size * sizeof (*p->stack_bottom),
                                   "capture_vm_cont");
  p->ra = ra;
  p->fp = fp;
  memcpy (p->stack_bottom, sp, p->stack_size * sizeof (*p->stack_bottom));
  p->reloc = (p->stack_bottom + p->stack_size) - stack_top;
  p->dynstack = dynstack;
  p->flags = flags;
  return scm_cell (scm_tc7_vm_cont, (scm_t_bits) p);
}

struct return_to_continuation_data
{
  struct scm_vm_cont *cp;
  struct scm_vm *vp;
};

/* Called with the GC lock to prevent the stack marker from traversing a
   stack in an inconsistent state.  */
static void *
vm_return_to_continuation_inner (void *data_ptr)
{
  struct return_to_continuation_data *data = data_ptr;
  struct scm_vm *vp = data->vp;
  struct scm_vm_cont *cp = data->cp;
  union scm_vm_stack_element *cp_stack_top;
  scm_t_ptrdiff reloc;

  /* We know that there is enough space for the continuation, because we
     captured it in the past.  However there may have been an expansion
     since the capture, so we may have to re-link the frame
     pointers.  */
  cp_stack_top = cp->stack_bottom + cp->stack_size;
  reloc = (vp->stack_top - (cp_stack_top - cp->reloc));
  vp->fp = cp->fp + reloc;
  memcpy (vp->stack_top - cp->stack_size,
          cp->stack_bottom,
          cp->stack_size * sizeof (*cp->stack_bottom));
  vm_restore_sp (vp, vp->stack_top - cp->stack_size);

  return NULL;
}

static void
vm_return_to_continuation (struct scm_vm *vp, SCM cont, size_t n,
                           union scm_vm_stack_element *argv)
{
  struct scm_vm_cont *cp;
  union scm_vm_stack_element *argv_copy;
  struct return_to_continuation_data data;

  argv_copy = alloca (n * sizeof (*argv));
  memcpy (argv_copy, argv, n * sizeof (*argv));

  cp = SCM_VM_CONT_DATA (cont);

  data.cp = cp;
  data.vp = vp;
  GC_call_with_alloc_lock (vm_return_to_continuation_inner, &data);

  /* Now we have the continuation properly copied over.  We just need to
     copy on an empty frame and the return values, as the continuation
     expects.  */
  vm_push_sp (vp, vp->sp - 3 - n);
  vp->sp[n+2].as_scm = SCM_BOOL_F;
  vp->sp[n+1].as_scm = SCM_BOOL_F;
  vp->sp[n].as_scm = SCM_BOOL_F;
  memcpy(vp->sp, argv_copy, n * sizeof (union scm_vm_stack_element));

  vp->ip = cp->ra;
}

static struct scm_vm * thread_vm (scm_i_thread *t);
SCM
scm_i_capture_current_stack (void)
{
  scm_i_thread *thread;
  struct scm_vm *vp;

  thread = SCM_I_CURRENT_THREAD;
  vp = thread_vm (thread);

  return scm_i_vm_capture_stack (vp->stack_top, vp->fp, vp->sp, vp->ip,
                                 scm_dynstack_capture_all (&thread->dynstack),
                                 0);
}

static void vm_dispatch_apply_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_push_continuation_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_pop_continuation_hook
  (struct scm_vm *vp, union scm_vm_stack_element *old_fp) SCM_NOINLINE;
static void vm_dispatch_next_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_abort_hook (struct scm_vm *vp) SCM_NOINLINE;

static void
vm_dispatch_hook (struct scm_vm *vp, int hook_num,
                  union scm_vm_stack_element *argv, int n)
{
  SCM hook;
  struct scm_frame c_frame;
  scm_t_cell *frame;
  int saved_trace_level;

  hook = vp->hooks[hook_num];

  if (SCM_LIKELY (scm_is_false (hook))
      || scm_is_null (SCM_HOOK_PROCEDURES (hook)))
    return;

  saved_trace_level = vp->trace_level;
  vp->trace_level = 0;

  /* Allocate a frame object on the stack.  This is more efficient than calling
     `scm_c_make_frame ()' to allocate on the heap, but it forces hooks to not
     capture frame objects.

     At the same time, procedures such as `frame-procedure' make sense only
     while the stack frame represented by the frame object is visible, so it
     seems reasonable to limit the lifetime of frame objects.  */

  c_frame.stack_holder = vp;
  c_frame.fp_offset = vp->stack_top - vp->fp;
  c_frame.sp_offset = vp->stack_top - vp->sp;
  c_frame.ip = vp->ip;

  /* Arrange for FRAME to be 8-byte aligned, like any other cell.  */
  frame = alloca (sizeof (*frame) + 8);
  frame = (scm_t_cell *) ROUND_UP ((scm_t_uintptr) frame, 8UL);

  frame->word_0 = SCM_PACK (scm_tc7_frame | (SCM_VM_FRAME_KIND_VM << 8));
  frame->word_1 = SCM_PACK_POINTER (&c_frame);

  if (n == 0)
    {
      SCM args[1];

      args[0] = SCM_PACK_POINTER (frame);
      scm_c_run_hookn (hook, args, 1);
    }
  else if (n == 1)
    {
      SCM args[2];

      args[0] = SCM_PACK_POINTER (frame);
      args[1] = argv[0].as_scm;
      scm_c_run_hookn (hook, args, 2);
    }
  else
    {
      SCM args = SCM_EOL;
      int i;

      for (i = 0; i < n; i++)
        args = scm_cons (argv[i].as_scm, args);
      scm_c_run_hook (hook, scm_cons (SCM_PACK_POINTER (frame), args));
    }

  vp->trace_level = saved_trace_level;
}

static void
vm_dispatch_apply_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_APPLY_HOOK, NULL, 0);
}
static void vm_dispatch_push_continuation_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_PUSH_CONTINUATION_HOOK, NULL, 0);
}
static void vm_dispatch_pop_continuation_hook (struct scm_vm *vp,
                                               union scm_vm_stack_element *old_fp)
{
  return vm_dispatch_hook (vp, SCM_VM_POP_CONTINUATION_HOOK,
                           vp->sp, SCM_FRAME_NUM_LOCALS (old_fp, vp->sp) - 1);
}
static void vm_dispatch_next_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_NEXT_HOOK, NULL, 0);
}
static void vm_dispatch_abort_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_ABORT_CONTINUATION_HOOK,
                           vp->sp, SCM_FRAME_NUM_LOCALS (vp->fp, vp->sp) - 1);
}

static void
vm_abort (struct scm_vm *vp, SCM tag, size_t nargs,
          scm_i_jmp_buf *current_registers) SCM_NORETURN;

static void
vm_abort (struct scm_vm *vp, SCM tag, size_t nargs,
          scm_i_jmp_buf *current_registers)
{
  size_t i;
  SCM *argv;
  
  argv = alloca (nargs * sizeof (SCM));
  for (i = 0; i < nargs; i++)
    argv[i] = vp->sp[nargs - i - 1].as_scm;

  vp->sp = vp->fp;

  scm_c_abort (vp, tag, nargs, argv, current_registers);
}

struct vm_reinstate_partial_continuation_data
{
  struct scm_vm *vp;
  struct scm_vm_cont *cp;
  scm_t_ptrdiff reloc;
};

static void *
vm_reinstate_partial_continuation_inner (void *data_ptr)
{
  struct vm_reinstate_partial_continuation_data *data = data_ptr;
  struct scm_vm *vp = data->vp;
  struct scm_vm_cont *cp = data->cp;
  union scm_vm_stack_element *base_fp;
  scm_t_ptrdiff reloc;

  base_fp = vp->fp;
  reloc = cp->reloc + (base_fp - (cp->stack_bottom + cp->stack_size));

  memcpy (base_fp - cp->stack_size,
          cp->stack_bottom,
          cp->stack_size * sizeof (*cp->stack_bottom));

  vp->fp = cp->fp + reloc;
  vp->ip = cp->ra;

  data->reloc = reloc;

  return NULL;
}

static void
vm_reinstate_partial_continuation (struct scm_vm *vp, SCM cont, size_t nargs,
                                   scm_t_dynstack *dynstack,
                                   scm_i_jmp_buf *registers)
{
  struct vm_reinstate_partial_continuation_data data;
  struct scm_vm_cont *cp;
  union scm_vm_stack_element *args;
  scm_t_ptrdiff reloc;

  args = alloca (nargs * sizeof (*args));
  memcpy (args, vp->sp, nargs * sizeof (*args));

  cp = SCM_VM_CONT_DATA (cont);

  vm_push_sp (vp, vp->fp - (cp->stack_size + nargs + 1));

  data.vp = vp;
  data.cp = cp;
  GC_call_with_alloc_lock (vm_reinstate_partial_continuation_inner, &data);
  reloc = data.reloc;

  /* The resume continuation will expect ARGS on the stack as if from a
     multiple-value return.  Fill in the closure slot with #f, and copy
     the arguments into place.  */
  vp->sp[nargs].as_scm = SCM_BOOL_F;
  memcpy (vp->sp, args, nargs * sizeof (*args));

  /* The prompt captured a slice of the dynamic stack.  Here we wind
     those entries onto the current thread's stack.  We also have to
     relocate any prompts that we see along the way.  */
  {
    scm_t_bits *walk;

    for (walk = SCM_DYNSTACK_FIRST (cp->dynstack);
         SCM_DYNSTACK_TAG (walk);
         walk = SCM_DYNSTACK_NEXT (walk))
      {
        scm_t_bits tag = SCM_DYNSTACK_TAG (walk);

        if (SCM_DYNSTACK_TAG_TYPE (tag) == SCM_DYNSTACK_TYPE_PROMPT)
          scm_dynstack_wind_prompt (dynstack, walk, reloc, registers);
        else
          scm_dynstack_wind_1 (dynstack, walk);
      }
  }
}


/*
 * VM Error Handling
 */

static void vm_error (const char *msg, SCM arg) SCM_NORETURN;
static void vm_error_bad_instruction (scm_t_uint32 inst) SCM_NORETURN SCM_NOINLINE;
static void vm_error_unbound (SCM sym) SCM_NORETURN SCM_NOINLINE;
static void vm_error_unbound_fluid (SCM fluid) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_variable (const char *func_name, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_apply_to_non_list (SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_kwargs_length_not_even (SCM proc) SCM_NORETURN SCM_NOINLINE;
static void vm_error_kwargs_invalid_keyword (SCM proc, SCM obj) SCM_NORETURN SCM_NOINLINE;
static void vm_error_kwargs_unrecognized_keyword (SCM proc, SCM kw) SCM_NORETURN SCM_NOINLINE;
static void vm_error_too_many_args (int nargs) SCM_NORETURN SCM_NOINLINE;
static void vm_error_wrong_num_args (SCM proc) SCM_NORETURN SCM_NOINLINE;
static void vm_error_wrong_type_apply (SCM proc) SCM_NORETURN SCM_NOINLINE;
static void vm_error_stack_underflow (void) SCM_NORETURN SCM_NOINLINE;
static void vm_error_improper_list (SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_pair (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_string (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_bytevector (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_struct (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_vector (const char *subr, SCM v) SCM_NORETURN SCM_NOINLINE;
static void vm_error_out_of_range (const char *subr, SCM k) SCM_NORETURN SCM_NOINLINE;
static void vm_error_out_of_range_uint64 (const char *subr, scm_t_uint64 idx) SCM_NORETURN SCM_NOINLINE;
static void vm_error_out_of_range_int64 (const char *subr, scm_t_int64 idx) SCM_NORETURN SCM_NOINLINE;
static void vm_error_no_values (void) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_enough_values (void) SCM_NORETURN SCM_NOINLINE;
static void vm_error_wrong_number_of_values (scm_t_uint32 expected) SCM_NORETURN SCM_NOINLINE;
static void vm_error_continuation_not_rewindable (SCM cont) SCM_NORETURN SCM_NOINLINE;
static void vm_error_bad_wide_string_length (size_t len) SCM_NORETURN SCM_NOINLINE;

static void
vm_error (const char *msg, SCM arg)
{
  scm_throw (sym_vm_error,
             scm_list_3 (sym_vm_run, scm_from_latin1_string (msg),
                         SCM_UNBNDP (arg) ? SCM_EOL : scm_list_1 (arg)));
  abort(); /* not reached */
}

static void
vm_error_bad_instruction (scm_t_uint32 inst)
{
  vm_error ("VM: Bad instruction: ~s", scm_from_uint32 (inst));
}

static void
vm_error_unbound (SCM sym)
{
  scm_error_scm (scm_misc_error_key, SCM_BOOL_F,
                 scm_from_latin1_string ("Unbound variable: ~s"),
                 scm_list_1 (sym), SCM_BOOL_F);
}

static void
vm_error_unbound_fluid (SCM fluid)
{
  scm_error_scm (scm_misc_error_key, SCM_BOOL_F,
                 scm_from_latin1_string ("Unbound fluid: ~s"),
                 scm_list_1 (fluid), SCM_BOOL_F);
}

static void
vm_error_not_a_variable (const char *func_name, SCM x)
{
  scm_error (scm_arg_type_key, func_name, "Not a variable: ~S",
             scm_list_1 (x), scm_list_1 (x));
}

static void
vm_error_apply_to_non_list (SCM x)
{
  scm_error (scm_arg_type_key, "apply", "Apply to non-list: ~S",
             scm_list_1 (x), scm_list_1 (x));
}

static void
vm_error_kwargs_length_not_even (SCM proc)
{
  scm_error_scm (sym_keyword_argument_error, proc,
                 scm_from_latin1_string ("Odd length of keyword argument list"),
                 SCM_EOL, SCM_BOOL_F);
}

static void
vm_error_kwargs_invalid_keyword (SCM proc, SCM obj)
{
  scm_error_scm (sym_keyword_argument_error, proc,
                 scm_from_latin1_string ("Invalid keyword"),
                 SCM_EOL, scm_list_1 (obj));
}

static void
vm_error_kwargs_unrecognized_keyword (SCM proc, SCM kw)
{
  scm_error_scm (sym_keyword_argument_error, proc,
                 scm_from_latin1_string ("Unrecognized keyword"),
                 SCM_EOL, scm_list_1 (kw));
}

static void
vm_error_too_many_args (int nargs)
{
  vm_error ("VM: Too many arguments", scm_from_int (nargs));
}

static void
vm_error_wrong_num_args (SCM proc)
{
  scm_wrong_num_args (proc);
}

static void
vm_error_wrong_type_apply (SCM proc)
{
  scm_error (scm_arg_type_key, NULL, "Wrong type to apply: ~S",
             scm_list_1 (proc), scm_list_1 (proc));
}

static void
vm_error_stack_underflow (void)
{
  vm_error ("VM: Stack underflow", SCM_UNDEFINED);
}

static void
vm_error_improper_list (SCM x)
{
  vm_error ("Expected a proper list, but got object with tail ~s", x);
}

static void
vm_error_not_a_pair (const char *subr, SCM x)
{
  scm_wrong_type_arg_msg (subr, 1, x, "pair");
}

static void
vm_error_not_a_string (const char *subr, SCM x)
{
  scm_wrong_type_arg_msg (subr, 1, x, "string");
}

static void
vm_error_not_a_bytevector (const char *subr, SCM x)
{
  scm_wrong_type_arg_msg (subr, 1, x, "bytevector");
}

static void
vm_error_not_a_struct (const char *subr, SCM x)
{
  scm_wrong_type_arg_msg (subr, 1, x, "struct");
}

static void
vm_error_not_a_vector (const char *subr, SCM x)
{
  scm_wrong_type_arg_msg (subr, 1, x, "vector");
}

static void
vm_error_out_of_range (const char *subr, SCM k)
{
  scm_to_size_t (k);
  scm_out_of_range (subr, k);
}

static void
vm_error_out_of_range_uint64 (const char *subr, scm_t_uint64 idx)
{
  scm_out_of_range (subr, scm_from_uint64 (idx));
}

static void
vm_error_out_of_range_int64 (const char *subr, scm_t_int64 idx)
{
  scm_out_of_range (subr, scm_from_int64 (idx));
}

static void
vm_error_no_values (void)
{
  vm_error ("Zero values returned to single-valued continuation",
            SCM_UNDEFINED);
}

static void
vm_error_not_enough_values (void)
{
  vm_error ("Too few values returned to continuation", SCM_UNDEFINED);
}

static void
vm_error_wrong_number_of_values (scm_t_uint32 expected)
{
  vm_error ("Wrong number of values returned to continuation (expected ~a)",
            scm_from_uint32 (expected));
}

static void
vm_error_continuation_not_rewindable (SCM cont)
{
  vm_error ("Unrewindable partial continuation", cont);
}

static void
vm_error_bad_wide_string_length (size_t len)
{
  vm_error ("VM: Bad wide string length: ~S", scm_from_size_t (len));
}




static SCM vm_boot_continuation;
static SCM vm_builtin_apply;
static SCM vm_builtin_values;
static SCM vm_builtin_abort_to_prompt;
static SCM vm_builtin_call_with_values;
static SCM vm_builtin_call_with_current_continuation;

static const scm_t_uint32 vm_boot_continuation_code[] = {
  SCM_PACK_OP_24 (halt, 0)
};

static const scm_t_uint32 vm_apply_non_program_code[] = {
  SCM_PACK_OP_24 (apply_non_program, 0)
};

static const scm_t_uint32 vm_builtin_apply_code[] = {
  SCM_PACK_OP_24 (assert_nargs_ge, 3),
  SCM_PACK_OP_24 (tail_apply, 0), /* proc in r1, args from r2 */
};

static const scm_t_uint32 vm_builtin_values_code[] = {
  SCM_PACK_OP_24 (return_values, 0) /* vals from r1 */
};

static const scm_t_uint32 vm_builtin_abort_to_prompt_code[] = {
  SCM_PACK_OP_24 (assert_nargs_ge, 2),
  SCM_PACK_OP_24 (abort, 0), /* tag in r1, vals from r2 */
  /* FIXME: Partial continuation should capture caller regs.  */
  SCM_PACK_OP_24 (return_values, 0) /* vals from r1 */
};

static const scm_t_uint32 vm_builtin_call_with_values_code[] = {
  SCM_PACK_OP_24 (assert_nargs_ee, 3),
  SCM_PACK_OP_24 (alloc_frame, 7),
  SCM_PACK_OP_12_12 (mov, 0, 5),
  SCM_PACK_OP_24 (call, 6), SCM_PACK_OP_ARG_8_24 (0, 1),
  SCM_PACK_OP_24 (long_fmov, 0), SCM_PACK_OP_ARG_8_24 (0, 2),
  SCM_PACK_OP_24 (tail_call_shuffle, 7)
};

static const scm_t_uint32 vm_builtin_call_with_current_continuation_code[] = {
  SCM_PACK_OP_24 (assert_nargs_ee, 2),
  SCM_PACK_OP_24 (call_cc, 0)
};


int
scm_i_vm_is_boot_continuation_code (scm_t_uint32 *ip)
{
  return ip == vm_boot_continuation_code;
}

static SCM
scm_vm_builtin_ref (unsigned idx)
{
  switch (idx)
    {
#define INDEX_TO_NAME(builtin, BUILTIN, req, opt, rest)                 \
      case SCM_VM_BUILTIN_##BUILTIN: return vm_builtin_##builtin;
      FOR_EACH_VM_BUILTIN(INDEX_TO_NAME)
#undef INDEX_TO_NAME
      default: abort();
    }
}

SCM scm_sym_apply;
static SCM scm_sym_values;
static SCM scm_sym_abort_to_prompt;
static SCM scm_sym_call_with_values;
static SCM scm_sym_call_with_current_continuation;

SCM
scm_vm_builtin_name_to_index (SCM name)
#define FUNC_NAME "builtin-name->index"
{
  SCM_VALIDATE_SYMBOL (1, name);

#define NAME_TO_INDEX(builtin, BUILTIN, req, opt, rest) \
  if (scm_is_eq (name, scm_sym_##builtin))              \
    return scm_from_uint (SCM_VM_BUILTIN_##BUILTIN);
  FOR_EACH_VM_BUILTIN(NAME_TO_INDEX)
#undef NAME_TO_INDEX

  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
scm_vm_builtin_index_to_name (SCM index)
#define FUNC_NAME "builtin-index->name"
{
  unsigned idx;

  SCM_VALIDATE_UINT_COPY (1, index, idx);

  switch (idx)
    {
#define INDEX_TO_NAME(builtin, BUILTIN, req, opt, rest)         \
      case SCM_VM_BUILTIN_##BUILTIN: return scm_sym_##builtin;
      FOR_EACH_VM_BUILTIN(INDEX_TO_NAME)
#undef INDEX_TO_NAME
      default: return SCM_BOOL_F;
    }
}
#undef FUNC_NAME

static void
scm_init_vm_builtins (void)
{
  scm_c_define_gsubr ("builtin-name->index", 1, 0, 0,
                      scm_vm_builtin_name_to_index);
  scm_c_define_gsubr ("builtin-index->name", 1, 0, 0,
                      scm_vm_builtin_index_to_name);
}

SCM
scm_i_call_with_current_continuation (SCM proc)
{
  return scm_call_1 (vm_builtin_call_with_current_continuation, proc);
}


/*
 * VM
 */

#define VM_NAME vm_regular_engine
#define VM_USE_HOOKS 0
#define FUNC_NAME "vm-regular-engine"
#include "vm-engine.c"
#undef FUNC_NAME
#undef VM_USE_HOOKS
#undef VM_NAME

#define VM_NAME vm_debug_engine
#define VM_USE_HOOKS 1
#define FUNC_NAME "vm-debug-engine"
#include "vm-engine.c"
#undef FUNC_NAME
#undef VM_USE_HOOKS
#undef VM_NAME

typedef SCM (*scm_t_vm_engine) (scm_i_thread *current_thread, struct scm_vm *vp,
                                scm_i_jmp_buf *registers, int resume);

static const scm_t_vm_engine vm_engines[SCM_VM_NUM_ENGINES] =
  { vm_regular_engine, vm_debug_engine };

static union scm_vm_stack_element*
allocate_stack (size_t size)
{
  void *ret;

  if (size >= ((size_t) -1) / sizeof (union scm_vm_stack_element))
    abort ();

  size *= sizeof (union scm_vm_stack_element);

#if HAVE_SYS_MMAN_H
  ret = mmap (NULL, size, PROT_READ | PROT_WRITE,
              MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (ret == NULL)
    /* Shouldn't happen.  */
    abort ();
  if (ret == MAP_FAILED)
    ret = NULL;
#else
  ret = malloc (size);
#endif

  if (!ret)
    perror ("allocate_stack failed");

  return (union scm_vm_stack_element *) ret;
}

static void
free_stack (union scm_vm_stack_element *stack, size_t size)
{
  size *= sizeof (*stack);

#if HAVE_SYS_MMAN_H
  munmap (stack, size);
#else
  free (stack);
#endif
}

/* Ideally what we would like is an mremap or a realloc that grows at
   the bottom, not the top.  Oh well; mmap and memcpy are fast enough,
   considering that they run very infrequently.  */
static union scm_vm_stack_element*
expand_stack (union scm_vm_stack_element *old_bottom, size_t old_size,
              size_t new_size)
#define FUNC_NAME "expand_stack"
{
  union scm_vm_stack_element *new_bottom;
  size_t extension_size;

  if (new_size >= ((size_t) -1) / sizeof (union scm_vm_stack_element))
    abort ();
  if (new_size <= old_size)
    abort ();

  extension_size = new_size - old_size;

  if ((size_t)old_bottom < extension_size * sizeof (union scm_vm_stack_element))
    abort ();

  new_bottom = allocate_stack (new_size);

  if (!new_bottom)
    return NULL;

  memcpy (new_bottom + extension_size,
          old_bottom,
          old_size * sizeof (union scm_vm_stack_element));
  free_stack (old_bottom, old_size);

  return new_bottom;
}
#undef FUNC_NAME

static struct scm_vm *
make_vm (void)
#define FUNC_NAME "make_vm"
{
  int i;
  struct scm_vm *vp;

  vp = scm_gc_malloc (sizeof (struct scm_vm), "vm");

  vp->stack_size = page_size / sizeof (union scm_vm_stack_element);
  vp->stack_bottom = allocate_stack (vp->stack_size);
  if (!vp->stack_bottom)
    /* As in expand_stack, we don't have any way to throw an exception
       if we can't allocate one measely page -- there's no stack to
       handle it.  For now, abort.  */
    abort ();
  vp->stack_top = vp->stack_bottom + vp->stack_size;
  vp->stack_limit = vp->stack_bottom;
  vp->overflow_handler_stack = SCM_EOL;
  vp->ip = NULL;
  vp->sp = vp->stack_top;
  vp->sp_min_since_gc = vp->sp;
  vp->fp = vp->stack_top;
  vp->engine = vm_default_engine;
  vp->trace_level = 0;
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    vp->hooks[i] = SCM_BOOL_F;

  return vp;
}
#undef FUNC_NAME

static void
return_unused_stack_to_os (struct scm_vm *vp)
{
#if HAVE_SYS_MMAN_H
  scm_t_uintptr lo = (scm_t_uintptr) vp->stack_bottom;
  scm_t_uintptr hi = (scm_t_uintptr) vp->sp;
  /* The second condition is needed to protect against wrap-around.  */
  if (vp->sp_min_since_gc >= vp->stack_bottom && vp->sp >= vp->sp_min_since_gc)
    lo = (scm_t_uintptr) vp->sp_min_since_gc;

  lo &= ~(page_size - 1U); /* round down */
  hi &= ~(page_size - 1U); /* round down */

  /* Return these pages to the OS.  The next time they are paged in,
     they will be zeroed.  */
  if (lo < hi)
    {
      int ret = 0;

      do
        ret = madvise ((void *) lo, hi - lo, MADV_DONTNEED);
      while (ret && errno == -EAGAIN);

      if (ret)
        perror ("madvise failed");
    }

  vp->sp_min_since_gc = vp->sp;
#endif
}

#define SLOT_MAP_CACHE_SIZE 32U
struct slot_map_cache_entry
{
  scm_t_uint32 *ip;
  const scm_t_uint8 *map;
};

struct slot_map_cache
{
  struct slot_map_cache_entry entries[SLOT_MAP_CACHE_SIZE];
};

static const scm_t_uint8 *
find_slot_map (scm_t_uint32 *ip, struct slot_map_cache *cache)
{
  /* The lower two bits should be zero.  FIXME: Use a better hash
     function; we don't expose scm_raw_hashq currently.  */
  size_t slot = (((scm_t_uintptr) ip) >> 2) % SLOT_MAP_CACHE_SIZE;
  const scm_t_uint8 *map;

  if (cache->entries[slot].ip == ip)
    map = cache->entries[slot].map;
  else
    {
      map = scm_find_slot_map_unlocked (ip);
      cache->entries[slot].ip = ip;
      cache->entries[slot].map = map;
    }

  return map;
}

enum slot_desc
  {
    SLOT_DESC_DEAD = 0,
    SLOT_DESC_LIVE_RAW = 1,
    SLOT_DESC_LIVE_SCM = 2,
    SLOT_DESC_UNUSED = 3
  };

/* Mark the active VM stack region.  */
struct GC_ms_entry *
scm_i_vm_mark_stack (struct scm_vm *vp, struct GC_ms_entry *mark_stack_ptr,
                     struct GC_ms_entry *mark_stack_limit)
{
  union scm_vm_stack_element *sp, *fp;
  /* The first frame will be marked conservatively (without a slot map).
     This is because GC can happen at any point within the hottest
     activation, due to multiple threads or per-instruction hooks, and
     providing slot maps for all points in a program would take a
     prohibitive amount of space.  */
  const scm_t_uint8 *slot_map = NULL;
  void *upper = (void *) GC_greatest_plausible_heap_addr;
  void *lower = (void *) GC_least_plausible_heap_addr;
  struct slot_map_cache cache;

  memset (&cache, 0, sizeof (cache));

  for (fp = vp->fp, sp = vp->sp;
       fp < vp->stack_top;
       fp = SCM_FRAME_DYNAMIC_LINK (fp))
    {
      scm_t_ptrdiff nlocals = SCM_FRAME_NUM_LOCALS (fp, sp);
      size_t slot = nlocals - 1;
      for (slot = nlocals - 1; sp < fp; sp++, slot--)
        {
          enum slot_desc desc = SLOT_DESC_LIVE_SCM;

          if (slot_map)
            desc = (slot_map[slot / 4U] >> ((slot % 4U) * 2)) & 3U;

          switch (desc)
            {
            case SLOT_DESC_LIVE_RAW:
              break;
            case SLOT_DESC_UNUSED:
            case SLOT_DESC_LIVE_SCM:
              if (SCM_NIMP (sp->as_scm) &&
                  sp->as_ptr >= lower && sp->as_ptr <= upper)
                mark_stack_ptr = GC_mark_and_push (sp->as_ptr,
                                                   mark_stack_ptr,
                                                   mark_stack_limit,
                                                   NULL);
              break;
            case SLOT_DESC_DEAD:
              /* This value may become dead as a result of GC,
                 so we can't just leave it on the stack.  */
              sp->as_scm = SCM_UNSPECIFIED;
              break;
            }
        }
      sp = SCM_FRAME_PREVIOUS_SP (fp);
      /* Inner frames may have a dead slots map for precise marking.
         Note that there may be other reasons to not have a dead slots
         map, e.g. if all of the frame's slots below the callee frame
         are live.  */
      slot_map = find_slot_map (SCM_FRAME_RETURN_ADDRESS (fp), &cache);
    }

  return_unused_stack_to_os (vp);

  return mark_stack_ptr;
}

/* Free the VM stack, as this thread is exiting.  */
void
scm_i_vm_free_stack (struct scm_vm *vp)
{
  free_stack (vp->stack_bottom, vp->stack_size);
  vp->stack_bottom = vp->stack_top = vp->stack_limit = NULL;
  vp->stack_size = 0;
}

struct vm_expand_stack_data
{
  struct scm_vm *vp;
  size_t stack_size;
  union scm_vm_stack_element *new_sp;
};

static void *
vm_expand_stack_inner (void *data_ptr)
{
  struct vm_expand_stack_data *data = data_ptr;

  struct scm_vm *vp = data->vp;
  union scm_vm_stack_element *old_top, *new_bottom;
  size_t new_size;
  scm_t_ptrdiff reloc;

  old_top = vp->stack_top;
  new_size = vp->stack_size;
  while (new_size < data->stack_size)
    new_size *= 2;

  new_bottom = expand_stack (vp->stack_bottom, vp->stack_size, new_size);
  if (!new_bottom)
    return NULL;

  vp->stack_bottom = new_bottom;
  vp->stack_size = new_size;
  vp->stack_top = vp->stack_bottom + new_size;
  vp->stack_limit = vp->stack_bottom;
  reloc = vp->stack_top - old_top;

  if (vp->fp)
    vp->fp += reloc;
  data->new_sp += reloc;

  return new_bottom;
}

static scm_t_ptrdiff
current_overflow_size (struct scm_vm *vp)
{
  if (scm_is_pair (vp->overflow_handler_stack))
    return scm_to_ptrdiff_t (scm_caar (vp->overflow_handler_stack));
  return -1;
}

static int
should_handle_stack_overflow (struct scm_vm *vp, scm_t_ptrdiff stack_size)
{
  scm_t_ptrdiff overflow_size = current_overflow_size (vp);
  return overflow_size >= 0 && stack_size >= overflow_size;
}

static void
reset_stack_limit (struct scm_vm *vp)
{
  if (should_handle_stack_overflow (vp, vp->stack_size))
    vp->stack_limit = vp->stack_top - current_overflow_size (vp);
  else
    vp->stack_limit = vp->stack_bottom;
}

struct overflow_handler_data
{
  struct scm_vm *vp;
  SCM overflow_handler_stack;
};

static void
wind_overflow_handler (void *ptr)
{
  struct overflow_handler_data *data = ptr;

  data->vp->overflow_handler_stack = data->overflow_handler_stack;

  reset_stack_limit (data->vp);
}

static void
unwind_overflow_handler (void *ptr)
{
  struct overflow_handler_data *data = ptr;

  data->vp->overflow_handler_stack = scm_cdr (data->overflow_handler_stack);

  reset_stack_limit (data->vp);
}

static void
vm_expand_stack (struct scm_vm *vp, union scm_vm_stack_element *new_sp)
{
  scm_t_ptrdiff stack_size = vp->stack_top - new_sp;

  if (stack_size > vp->stack_size)
    {
      struct vm_expand_stack_data data;

      data.vp = vp;
      data.stack_size = stack_size;
      data.new_sp = new_sp;
      
      if (!GC_call_with_alloc_lock (vm_expand_stack_inner, &data))
        /* Throw an unwind-only exception.  */
        scm_report_stack_overflow ();

      new_sp = data.new_sp;
    }

  vp->sp_min_since_gc = vp->sp = new_sp;

  if (should_handle_stack_overflow (vp, stack_size))
    {
      SCM more_stack, new_limit;

      struct overflow_handler_data data;
      data.vp = vp;
      data.overflow_handler_stack = vp->overflow_handler_stack;

      scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);

      scm_dynwind_rewind_handler (unwind_overflow_handler, &data,
                                  SCM_F_WIND_EXPLICITLY);
      scm_dynwind_unwind_handler (wind_overflow_handler, &data,
                                  SCM_F_WIND_EXPLICITLY);

      /* Call the overflow handler.  */
      more_stack = scm_call_0 (scm_cdar (data.overflow_handler_stack));

      /* If the overflow handler returns, its return value should be an
         integral number of words from the outer stack limit to transfer
         to the inner limit.  */
      if (scm_to_ptrdiff_t (more_stack) <= 0)
        scm_out_of_range (NULL, more_stack);
      new_limit = scm_sum (scm_caar (data.overflow_handler_stack), more_stack);
      if (scm_is_pair (scm_cdr (data.overflow_handler_stack)))
        new_limit = scm_min (new_limit,
                             scm_caadr (data.overflow_handler_stack));

      /* Ensure the new limit is in range.  */
      scm_to_ptrdiff_t (new_limit);

      /* Increase the limit that we will restore.  */
      scm_set_car_x (scm_car (data.overflow_handler_stack), new_limit);

      scm_dynwind_end ();

      /* Recurse.  */
      return vm_expand_stack (vp, new_sp);
    }
}

static struct scm_vm *
thread_vm (scm_i_thread *t)
{
  if (SCM_UNLIKELY (!t->vp))
    t->vp = make_vm ();

  return t->vp;
}

struct scm_vm *
scm_the_vm (void)
{
  return thread_vm (SCM_I_CURRENT_THREAD);
}

SCM
scm_call_n (SCM proc, SCM *argv, size_t nargs)
{
  scm_i_thread *thread;
  struct scm_vm *vp;
  union scm_vm_stack_element *return_fp, *call_fp;
  /* Since nargs can only describe the length of a valid argv array in
     elements and each element is at least 4 bytes, nargs will not be
     greater than INTMAX/2 and therefore we don't have to check for
     overflow here or below.  */
  size_t return_nlocals = 1, call_nlocals = nargs + 1, frame_size = 2;
  scm_t_ptrdiff stack_reserve_words;
  size_t i;

  thread = SCM_I_CURRENT_THREAD;
  vp = thread_vm (thread);

  SCM_CHECK_STACK;

  /* It's not valid for argv to point into the stack already.  */
  if ((void *) argv < (void *) vp->stack_top &&
      (void *) argv >= (void *) vp->sp)
    abort();

  /* Check that we have enough space for the two stack frames: the
     innermost one that makes the call, and its continuation which
     receives the resulting value(s) and returns from the engine
     call.  */
  stack_reserve_words = call_nlocals + frame_size + return_nlocals + frame_size;
  vm_push_sp (vp, vp->sp - stack_reserve_words);

  call_fp = vp->sp + call_nlocals;
  return_fp = call_fp + frame_size + return_nlocals;

  SCM_FRAME_SET_RETURN_ADDRESS (return_fp, vp->ip);
  SCM_FRAME_SET_DYNAMIC_LINK (return_fp, vp->fp);
  SCM_FRAME_LOCAL (return_fp, 0) = vm_boot_continuation;

  vp->ip = (scm_t_uint32 *) vm_boot_continuation_code;
  vp->fp = call_fp;

  SCM_FRAME_SET_RETURN_ADDRESS (call_fp, vp->ip);
  SCM_FRAME_SET_DYNAMIC_LINK (call_fp, return_fp);
  SCM_FRAME_LOCAL (call_fp, 0) = proc;
  for (i = 0; i < nargs; i++)
    SCM_FRAME_LOCAL (call_fp, i + 1) = argv[i];

  {
    scm_i_jmp_buf registers;
    int resume = SCM_I_SETJMP (registers);
      
    if (SCM_UNLIKELY (resume))
      {
        scm_gc_after_nonlocal_exit ();
        /* Non-local return.  */
        vm_dispatch_abort_hook (vp);
      }

    return vm_engines[vp->engine](thread, vp, &registers, resume);
  }
}

/* Scheme interface */

#define VM_DEFINE_HOOK(n)				\
{							\
  struct scm_vm *vp;					\
  vp = scm_the_vm ();                                   \
  if (scm_is_false (vp->hooks[n]))			\
    vp->hooks[n] = scm_make_hook (SCM_I_MAKINUM (1));	\
  return vp->hooks[n];					\
}

SCM_DEFINE (scm_vm_apply_hook, "vm-apply-hook", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_apply_hook
{
  VM_DEFINE_HOOK (SCM_VM_APPLY_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_push_continuation_hook, "vm-push-continuation-hook", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_push_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_PUSH_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_pop_continuation_hook, "vm-pop-continuation-hook", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_pop_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_POP_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_next_hook, "vm-next-hook", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_next_hook
{
  VM_DEFINE_HOOK (SCM_VM_NEXT_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_abort_continuation_hook, "vm-abort-continuation-hook", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_abort_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_ABORT_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_trace_level, "vm-trace-level", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_trace_level
{
  return scm_from_int (scm_the_vm ()->trace_level);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_vm_trace_level_x, "set-vm-trace-level!", 1, 0, 0,
	    (SCM level),
	    "")
#define FUNC_NAME s_scm_set_vm_trace_level_x
{
  scm_the_vm ()->trace_level = scm_to_int (level);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * VM engines
 */

static int
symbol_to_vm_engine (SCM engine, const char *FUNC_NAME)
{
  if (scm_is_eq (engine, sym_regular))
    return SCM_VM_REGULAR_ENGINE;
  else if (scm_is_eq (engine, sym_debug))
    return SCM_VM_DEBUG_ENGINE;
  else
    SCM_MISC_ERROR ("Unknown VM engine: ~a", scm_list_1 (engine));
}
  
static SCM
vm_engine_to_symbol (int engine, const char *FUNC_NAME)
{
  switch (engine)
    {
    case SCM_VM_REGULAR_ENGINE:
      return sym_regular;
    case SCM_VM_DEBUG_ENGINE:
      return sym_debug;
    default:
      /* ? */
      SCM_MISC_ERROR ("Unknown VM engine: ~a",
                      scm_list_1 (scm_from_int (engine)));
    }
}
  
SCM_DEFINE (scm_vm_engine, "vm-engine", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_engine
{
  return vm_engine_to_symbol (scm_the_vm ()->engine, FUNC_NAME);
}
#undef FUNC_NAME

void
scm_c_set_vm_engine_x (int engine)
#define FUNC_NAME "set-vm-engine!"
{
  if (engine < 0 || engine >= SCM_VM_NUM_ENGINES)
    SCM_MISC_ERROR ("Unknown VM engine: ~a",
                    scm_list_1 (scm_from_int (engine)));
    
  scm_the_vm ()->engine = engine;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_vm_engine_x, "set-vm-engine!", 1, 0, 0,
	    (SCM engine),
	    "")
#define FUNC_NAME s_scm_set_vm_engine_x
{
  scm_c_set_vm_engine_x (symbol_to_vm_engine (engine, FUNC_NAME));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_set_default_vm_engine_x (int engine)
#define FUNC_NAME "set-default-vm-engine!"
{
  if (engine < 0 || engine >= SCM_VM_NUM_ENGINES)
    SCM_MISC_ERROR ("Unknown VM engine: ~a",
                    scm_list_1 (scm_from_int (engine)));
    
  vm_default_engine = engine;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_default_vm_engine_x, "set-default-vm-engine!", 1, 0, 0,
	    (SCM engine),
	    "")
#define FUNC_NAME s_scm_set_default_vm_engine_x
{
  scm_c_set_default_vm_engine_x (symbol_to_vm_engine (engine, FUNC_NAME));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* FIXME: This function makes no sense, but we keep it to make sure we
   have a way of switching to the debug or regular VM.  */
SCM_DEFINE (scm_call_with_vm, "call-with-vm", 1, 0, 1,
	    (SCM proc, SCM args),
	    "Apply @var{proc} to @var{args} in a dynamic extent in which\n"
            "@var{vm} is the current VM.")
#define FUNC_NAME s_scm_call_with_vm
{
  return scm_apply_0 (proc, args);
}
#undef FUNC_NAME

SCM_DEFINE (scm_call_with_stack_overflow_handler,
            "call-with-stack-overflow-handler", 3, 0, 0,
	    (SCM limit, SCM thunk, SCM handler),
	    "Call @var{thunk} in an environment in which the stack limit has\n"
            "been reduced to @var{limit} additional words.  If the limit is\n"
            "reached, @var{handler} (a thunk) will be invoked in the dynamic\n"
            "environment of the error.  For the extent of the call to\n"
            "@var{handler}, the stack limit and handler are restored to the\n"
            "values that were in place when\n"
            "@code{call-with-stack-overflow-handler} was called.")
#define FUNC_NAME s_scm_call_with_stack_overflow_handler
{
  struct scm_vm *vp;
  scm_t_ptrdiff c_limit, stack_size;
  struct overflow_handler_data data;
  SCM new_limit, ret;

  vp = scm_the_vm ();
  stack_size = vp->stack_top - vp->sp;

  c_limit = scm_to_ptrdiff_t (limit);
  if (c_limit <= 0)
    scm_out_of_range (FUNC_NAME, limit);

  new_limit = scm_sum (scm_from_ptrdiff_t (stack_size), limit);
  if (scm_is_pair (vp->overflow_handler_stack))
    new_limit = scm_min (new_limit, scm_caar (vp->overflow_handler_stack));

  /* Hacky check that the current stack depth plus the limit is within
     the range of a ptrdiff_t.  */
  scm_to_ptrdiff_t (new_limit);

  data.vp = vp;
  data.overflow_handler_stack =
    scm_acons (limit, handler, vp->overflow_handler_stack);

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);

  scm_dynwind_rewind_handler (wind_overflow_handler, &data,
                              SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (unwind_overflow_handler, &data,
                              SCM_F_WIND_EXPLICITLY);

  /* Reset vp->sp_min_since_gc so that the VM checks actually
     trigger.  */
  return_unused_stack_to_os (vp);

  ret = scm_call_0 (thunk);

  scm_dynwind_end ();

  return ret;
}
#undef FUNC_NAME


/*
 * Initialize
 */

SCM
scm_load_compiled_with_vm (SCM file)
{
  return scm_call_0 (scm_load_thunk_from_file (file));
}

  
void
scm_init_vm_builtin_properties (void)
{
  /* FIXME: Seems hacky to do this here, but oh well :/ */
  scm_sym_apply = scm_from_utf8_symbol ("apply");
  scm_sym_values = scm_from_utf8_symbol ("values");
  scm_sym_abort_to_prompt = scm_from_utf8_symbol ("abort-to-prompt");
  scm_sym_call_with_values = scm_from_utf8_symbol ("call-with-values");
  scm_sym_call_with_current_continuation =
    scm_from_utf8_symbol ("call-with-current-continuation");

#define INIT_BUILTIN(builtin, BUILTIN, req, opt, rest)                  \
  scm_set_procedure_property_x (vm_builtin_##builtin, scm_sym_name,     \
                                scm_sym_##builtin);                     \
  scm_set_procedure_minimum_arity_x (vm_builtin_##builtin,              \
                                     SCM_I_MAKINUM (req),               \
                                     SCM_I_MAKINUM (opt),               \
                                     scm_from_bool (rest));
  FOR_EACH_VM_BUILTIN (INIT_BUILTIN);
#undef INIT_BUILTIN
}

void
scm_bootstrap_vm (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_vm",
                            (scm_t_extension_init_func)scm_init_vm, NULL);
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_vm_builtins",
                            (scm_t_extension_init_func)scm_init_vm_builtins,
                            NULL);

  page_size = getpagesize ();
  /* page_size should be a power of two.  */
  if (page_size & (page_size - 1))
    abort ();

  sym_vm_run = scm_from_latin1_symbol ("vm-run");
  sym_vm_error = scm_from_latin1_symbol ("vm-error");
  sym_keyword_argument_error = scm_from_latin1_symbol ("keyword-argument-error");
  sym_regular = scm_from_latin1_symbol ("regular");
  sym_debug = scm_from_latin1_symbol ("debug");

  vm_boot_continuation = scm_i_make_program (vm_boot_continuation_code);
  SCM_SET_CELL_WORD_0 (vm_boot_continuation,
                       (SCM_CELL_WORD_0 (vm_boot_continuation)
                        | SCM_F_PROGRAM_IS_BOOT));

#define DEFINE_BUILTIN(builtin, BUILTIN, req, opt, rest)                \
  vm_builtin_##builtin = scm_i_make_program (vm_builtin_##builtin##_code);
  FOR_EACH_VM_BUILTIN (DEFINE_BUILTIN);
#undef DEFINE_BUILTIN
}

void
scm_init_vm (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/vm.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
