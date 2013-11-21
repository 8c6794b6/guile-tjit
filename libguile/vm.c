/* Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

#include "libguile/bdw-gc.h"
#include <gc/gc_mark.h>

#include "_scm.h"
#include "control.h"
#include "frames.h"
#include "instructions.h"
#include "loader.h"
#include "programs.h"
#include "vm.h"
#include "vm-builtins.h"

#include "private-gc.h" /* scm_getenv_int */

static int vm_default_engine = SCM_VM_REGULAR_ENGINE;

/* Unfortunately we can't snarf these: snarfed things are only loaded up from
   (system vm vm), which might not be loaded before an error happens. */
static SCM sym_vm_run;
static SCM sym_vm_error;
static SCM sym_keyword_argument_error;
static SCM sym_regular;
static SCM sym_debug;

/* The VM has a number of internal assertions that shouldn't normally be
   necessary, but might be if you think you found a bug in the VM. */
#define VM_ENABLE_ASSERTIONS

/* #define VM_ENABLE_PARANOID_ASSERTIONS */

/* When defined, arrange so that the GC doesn't scan the VM stack beyond its
   current SP.  This should help avoid excess data retention.  See
   http://thread.gmane.org/gmane.comp.programming.garbage-collection.boehmgc/3001
   for a discussion.  */
#define VM_ENABLE_PRECISE_STACK_GC_SCAN

/* Size in SCM objects of the stack reserve.  The reserve is used to run
   exception handling code in case of a VM stack overflow.  */
#define VM_STACK_RESERVE_SIZE  512



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

/* In theory, a number of vm instances can be active in the call trace, and we
   only want to reify the continuations of those in the current continuation
   root. I don't see a nice way to do this -- ideally it would involve dynwinds,
   and previous values of the *the-vm* fluid within the current continuation
   root. But we don't have access to continuation roots in the dynwind stack.
   So, just punt for now, we just capture the continuation for the current VM.

   While I'm on the topic, ideally we could avoid copying the C stack if the
   continuation root is inside VM code, and call/cc was invoked within that same
   call to vm_run; but that's currently not implemented.
 */
SCM
scm_i_vm_capture_stack (SCM *stack_base, SCM *fp, SCM *sp, scm_t_uint32 *ra,
                        scm_t_dynstack *dynstack, scm_t_uint32 flags)
{
  struct scm_vm_cont *p;

  p = scm_gc_malloc (sizeof (*p), "capture_vm_cont");
  p->stack_size = sp - stack_base + 1;
  p->stack_base = scm_gc_malloc (p->stack_size * sizeof (SCM),
				 "capture_vm_cont");
  p->ra = ra;
  p->sp = sp;
  p->fp = fp;
  memcpy (p->stack_base, stack_base, (sp + 1 - stack_base) * sizeof (SCM));
  p->reloc = p->stack_base - stack_base;
  p->dynstack = dynstack;
  p->flags = flags;
  return scm_cell (scm_tc7_vm_cont, (scm_t_bits)p);
}

static void
vm_return_to_continuation (struct scm_vm *vp, SCM cont, size_t n, SCM *argv)
{
  struct scm_vm_cont *cp;
  SCM *argv_copy;

  argv_copy = alloca (n * sizeof(SCM));
  memcpy (argv_copy, argv, n * sizeof(SCM));

  cp = SCM_VM_CONT_DATA (cont);

  if (vp->stack_size < cp->stack_size + n + 3)
    scm_misc_error ("vm-engine", "not enough space to reinstate continuation",
                    scm_list_1 (cont));

  vp->sp = cp->sp;
  vp->fp = cp->fp;
  memcpy (vp->stack_base, cp->stack_base, cp->stack_size * sizeof (SCM));

  {
    size_t i;

    /* Push on an empty frame, as the continuation expects.  */
    for (i = 0; i < 3; i++)
      {
        vp->sp++;
        *vp->sp = SCM_BOOL_F;
      }

    /* Push the return values.  */
    for (i = 0; i < n; i++)
      {
        vp->sp++;
        *vp->sp = argv_copy[i];
      }
    vp->ip = cp->ra;
  }
}

SCM
scm_i_capture_current_stack (void)
{
  scm_i_thread *thread;
  struct scm_vm *vp;

  thread = SCM_I_CURRENT_THREAD;
  vp = SCM_VM_DATA (scm_the_vm ());

  return scm_i_vm_capture_stack (vp->stack_base, vp->fp, vp->sp, vp->ip,
                                 scm_dynstack_capture_all (&thread->dynstack),
                                 0);
}

static void vm_dispatch_apply_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_push_continuation_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_pop_continuation_hook (struct scm_vm *vp, SCM *old_fp) SCM_NOINLINE;
static void vm_dispatch_next_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_abort_hook (struct scm_vm *vp) SCM_NOINLINE;
static void vm_dispatch_restore_continuation_hook (struct scm_vm *vp) SCM_NOINLINE;

static void
vm_dispatch_hook (struct scm_vm *vp, int hook_num, SCM *argv, int n)
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
  c_frame.fp_offset = vp->fp - vp->stack_base;
  c_frame.sp_offset = vp->sp - vp->stack_base;
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
      args[1] = argv[0];
      scm_c_run_hookn (hook, args, 2);
    }
  else
    {
      SCM args = SCM_EOL;

      while (n--)
        args = scm_cons (argv[n], args);
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
static void vm_dispatch_pop_continuation_hook (struct scm_vm *vp, SCM *old_fp)
{
  return vm_dispatch_hook (vp, SCM_VM_POP_CONTINUATION_HOOK,
                           &SCM_FRAME_LOCAL (old_fp, 1),
                           SCM_FRAME_NUM_LOCALS (old_fp, vp->sp) - 1);
}
static void vm_dispatch_next_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_NEXT_HOOK, NULL, 0);
}
static void vm_dispatch_abort_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_ABORT_CONTINUATION_HOOK,
                           &SCM_FRAME_LOCAL (vp->fp, 1),
                           SCM_FRAME_NUM_LOCALS (vp->fp, vp->sp) - 1);
}
static void vm_dispatch_restore_continuation_hook (struct scm_vm *vp)
{
  return vm_dispatch_hook (vp, SCM_VM_RESTORE_CONTINUATION_HOOK, NULL, 0);
}

static void
vm_abort (struct scm_vm *vp, SCM tag,
          size_t nstack, SCM *stack_args, SCM tail, SCM *sp,
          scm_i_jmp_buf *current_registers) SCM_NORETURN;

static void
vm_abort (struct scm_vm *vp, SCM tag,
          size_t nstack, SCM *stack_args, SCM tail, SCM *sp,
          scm_i_jmp_buf *current_registers)
{
  size_t i;
  ssize_t tail_len;
  SCM *argv;
  
  tail_len = scm_ilength (tail);
  if (tail_len < 0)
    scm_misc_error ("vm-engine", "tail values to abort should be a list",
                    scm_list_1 (tail));

  argv = alloca ((nstack + tail_len) * sizeof (SCM));
  for (i = 0; i < nstack; i++)
    argv[i] = stack_args[i];
  for (; i < nstack + tail_len; i++, tail = scm_cdr (tail))
    argv[i] = scm_car (tail);

  /* FIXME: NULLSTACK (SCM_VM_DATA (vp)->sp - sp) */
  vp->sp = sp;

  scm_c_abort (vp, tag, nstack + tail_len, argv, current_registers);
}

static void
vm_reinstate_partial_continuation (struct scm_vm *vp, SCM cont,
                                   size_t n, SCM *argv,
                                   scm_t_dynstack *dynstack,
                                   scm_i_jmp_buf *registers)
{
  struct scm_vm_cont *cp;
  SCM *argv_copy, *base;
  scm_t_ptrdiff reloc;
  size_t i;

  argv_copy = alloca (n * sizeof(SCM));
  memcpy (argv_copy, argv, n * sizeof(SCM));

  cp = SCM_VM_CONT_DATA (cont);
  base = SCM_FRAME_LOCALS_ADDRESS (vp->fp);
  reloc = cp->reloc + (base - cp->stack_base);

#define RELOC(scm_p)						\
  (((SCM *) (scm_p)) + reloc)

  if ((base - vp->stack_base) + cp->stack_size + n + 1 > vp->stack_size)
    scm_misc_error ("vm-engine",
                    "not enough space to instate partial continuation",
                    scm_list_1 (cont));

  memcpy (base, cp->stack_base, cp->stack_size * sizeof (SCM));

  /* now relocate frame pointers */
  {
    SCM *fp;
    for (fp = RELOC (cp->fp);
         SCM_FRAME_LOWER_ADDRESS (fp) > base;
         fp = SCM_FRAME_DYNAMIC_LINK (fp))
      SCM_FRAME_SET_DYNAMIC_LINK (fp, RELOC (SCM_FRAME_DYNAMIC_LINK (fp)));
  }

  vp->sp = base - 1 + cp->stack_size;
  vp->fp = RELOC (cp->fp);
  vp->ip = cp->ra;

  /* Push the arguments. */
  for (i = 0; i < n; i++)
    {
      vp->sp++;
      *vp->sp = argv_copy[i];
    }

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
#undef RELOC
}


/*
 * VM Internal functions
 */

void
scm_i_vm_print (SCM x, SCM port, scm_print_state *pstate)
{
  const struct scm_vm *vm;

  vm = SCM_VM_DATA (x);

  scm_puts_unlocked ("#<vm ", port);
  switch (vm->engine)
    {
    case SCM_VM_REGULAR_ENGINE:
      scm_puts_unlocked ("regular-engine ", port);
      break;

    case SCM_VM_DEBUG_ENGINE:
      scm_puts_unlocked ("debug-engine ", port);
      break;

    default:
      scm_puts_unlocked ("unknown-engine ", port);
    }
  scm_uintprint (SCM_UNPACK (x), 16, port);
  scm_puts_unlocked (">", port);
}


/*
 * VM Error Handling
 */

static void vm_error (const char *msg, SCM arg) SCM_NORETURN;
static void vm_error_bad_instruction (scm_t_uint32 inst) SCM_NORETURN SCM_NOINLINE;
static void vm_error_unbound (SCM proc, SCM sym) SCM_NORETURN SCM_NOINLINE;
static void vm_error_unbound_fluid (SCM proc, SCM fluid) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_variable (const char *func_name, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_apply_to_non_list (SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_kwargs_length_not_even (SCM proc) SCM_NORETURN SCM_NOINLINE;
static void vm_error_kwargs_invalid_keyword (SCM proc, SCM obj) SCM_NORETURN SCM_NOINLINE;
static void vm_error_kwargs_unrecognized_keyword (SCM proc, SCM kw) SCM_NORETURN SCM_NOINLINE;
static void vm_error_too_many_args (int nargs) SCM_NORETURN SCM_NOINLINE;
static void vm_error_wrong_num_args (SCM proc) SCM_NORETURN SCM_NOINLINE;
static void vm_error_wrong_type_apply (SCM proc) SCM_NORETURN SCM_NOINLINE;
static void vm_error_stack_overflow (struct scm_vm *vp) SCM_NORETURN SCM_NOINLINE;
static void vm_error_stack_underflow (void) SCM_NORETURN SCM_NOINLINE;
static void vm_error_improper_list (SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_pair (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_bytevector (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
static void vm_error_not_a_struct (const char *subr, SCM x) SCM_NORETURN SCM_NOINLINE;
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
vm_error_unbound (SCM proc, SCM sym)
{
  scm_error_scm (scm_misc_error_key, proc,
                 scm_from_latin1_string ("Unbound variable: ~s"),
                 scm_list_1 (sym), SCM_BOOL_F);
}

static void
vm_error_unbound_fluid (SCM proc, SCM fluid)
{
  scm_error_scm (scm_misc_error_key, proc,
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
vm_error_stack_overflow (struct scm_vm *vp)
{
  if (vp->stack_limit < vp->stack_base + vp->stack_size)
    /* There are VM_STACK_RESERVE_SIZE bytes left.  Make them available so
       that `throw' below can run on this VM.  */
    vp->stack_limit = vp->stack_base + vp->stack_size;
  else
    /* There is no space left on the stack.  FIXME: Do something more
       sensible here! */
    abort ();
  vm_error ("VM: Stack overflow", SCM_UNDEFINED);
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
  SCM_PACK_OP_12_12 (mov, 6, 1),
  SCM_PACK_OP_24 (call, 6), SCM_PACK_OP_ARG_8_24 (0, 1),
  SCM_PACK_OP_12_12 (mov, 0, 2),
  SCM_PACK_OP_24 (tail_call_shuffle, 7)
};

static const scm_t_uint32 vm_builtin_call_with_current_continuation_code[] = {
  SCM_PACK_OP_24 (assert_nargs_ee, 2),
  SCM_PACK_OP_24 (call_cc, 0)
};


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

#define VM_MIN_STACK_SIZE	(1024)
#define VM_DEFAULT_STACK_SIZE	(256 * 1024)
static size_t vm_stack_size = VM_DEFAULT_STACK_SIZE;

static void
initialize_default_stack_size (void)
{
  int size = scm_getenv_int ("GUILE_STACK_SIZE", vm_stack_size);
  if (size >= VM_MIN_STACK_SIZE)
    vm_stack_size = size;
}

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

typedef SCM (*scm_t_vm_engine) (struct scm_vm *vp,
                                SCM program, SCM *argv, size_t nargs);

static const scm_t_vm_engine vm_engines[SCM_VM_NUM_ENGINES] =
  { vm_regular_engine, vm_debug_engine };

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN

/* The GC "kind" for the VM stack.  */
static int vm_stack_gc_kind;

#endif

static SCM
make_vm (void)
#define FUNC_NAME "make_vm"
{
  int i;
  struct scm_vm *vp;

  vp = scm_gc_malloc (sizeof (struct scm_vm), "vm");

  vp->stack_size= vm_stack_size;

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN
  vp->stack_base = (SCM *)
    GC_generic_malloc (vp->stack_size * sizeof (SCM), vm_stack_gc_kind);

  /* Keep a pointer to VP so that `vm_stack_mark ()' can know what the stack
     top is.  */
  *vp->stack_base = SCM_PACK_POINTER (vp);
  vp->stack_base++;
  vp->stack_size--;
#else
  vp->stack_base  = scm_gc_malloc (vp->stack_size * sizeof (SCM),
				   "stack-base");
#endif

  vp->stack_limit = vp->stack_base + vp->stack_size - VM_STACK_RESERVE_SIZE;
  vp->ip    	  = NULL;
  vp->sp    	  = vp->stack_base - 1;
  vp->fp    	  = NULL;
  vp->engine      = vm_default_engine;
  vp->trace_level = 0;
  for (i = 0; i < SCM_VM_NUM_HOOKS; i++)
    vp->hooks[i] = SCM_BOOL_F;
  return scm_cell (scm_tc7_vm, (scm_t_bits)vp);
}
#undef FUNC_NAME

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN

/* Mark the VM stack region between its base and its current top.  */
static struct GC_ms_entry *
vm_stack_mark (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
	       struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  GC_word *word;
  const struct scm_vm *vm;

  /* The first word of the VM stack should contain a pointer to the
     corresponding VM.  */
  vm = * ((struct scm_vm **) addr);

  if (vm == NULL
      || (SCM *) addr != vm->stack_base - 1)
    /* ADDR must be a pointer to a free-list element, which we must ignore
       (see warning in <gc/gc_mark.h>).  */
    return mark_stack_ptr;

  for (word = (GC_word *) vm->stack_base; word <= (GC_word *) vm->sp; word++)
    mark_stack_ptr = GC_MARK_AND_PUSH ((* (GC_word **) word),
				       mark_stack_ptr, mark_stack_limit,
				       NULL);

  return mark_stack_ptr;
}

#endif /* VM_ENABLE_PRECISE_STACK_GC_SCAN */


SCM
scm_c_vm_run (SCM vm, SCM program, SCM *argv, int nargs)
{
  struct scm_vm *vp = SCM_VM_DATA (vm);
  SCM_CHECK_STACK;
  return vm_engines[vp->engine](vp, program, argv, nargs);
}

SCM
scm_call_n (SCM proc, SCM *argv, size_t nargs)
{
  return scm_c_vm_run (scm_the_vm (), proc, argv, nargs);
}

SCM
scm_the_vm (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (SCM_UNLIKELY (scm_is_false (t->vm)))
    t->vm = make_vm ();

  return t->vm;
}

/* Scheme interface */

#define VM_DEFINE_HOOK(n)				\
{							\
  struct scm_vm *vp;					\
  vp = SCM_VM_DATA (scm_the_vm ());                     \
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

SCM_DEFINE (scm_vm_restore_continuation_hook, "vm-restore-continuation-hook", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_restore_continuation_hook
{
  VM_DEFINE_HOOK (SCM_VM_RESTORE_CONTINUATION_HOOK);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_trace_level, "vm-trace-level", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_vm_trace_level
{
  return scm_from_int (SCM_VM_DATA (scm_the_vm ())->trace_level);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_vm_trace_level_x, "set-vm-trace-level!", 1, 0, 0,
	    (SCM level),
	    "")
#define FUNC_NAME s_scm_set_vm_trace_level_x
{
  SCM_VM_DATA (scm_the_vm ())->trace_level = scm_to_int (level);
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
  return vm_engine_to_symbol (SCM_VM_DATA (scm_the_vm ())->engine, FUNC_NAME);
}
#undef FUNC_NAME

void
scm_c_set_vm_engine_x (int engine)
#define FUNC_NAME "set-vm-engine!"
{
  if (engine < 0 || engine >= SCM_VM_NUM_ENGINES)
    SCM_MISC_ERROR ("Unknown VM engine: ~a",
                    scm_list_1 (scm_from_int (engine)));
    
  SCM_VM_DATA (scm_the_vm ())->engine = engine;
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

  initialize_default_stack_size ();

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

#ifdef VM_ENABLE_PRECISE_STACK_GC_SCAN
  vm_stack_gc_kind =
    GC_new_kind (GC_new_free_list (),
		 GC_MAKE_PROC (GC_new_proc (vm_stack_mark), 0),
		 0, 1);

#endif
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
