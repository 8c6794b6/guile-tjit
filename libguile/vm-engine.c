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

/* This file is included in vm.c multiple times.  */


/* Virtual Machine

   This file contains two virtual machines.  First, the old one -- the
   one that is currently used, and corresponds to Guile 2.0.  It's a
   stack machine, meaning that most instructions pop their operands from
   the top of the stack, and push results there too.

   Following it is the new virtual machine.  It's a register machine,
   meaning that intructions address their operands by index, and store
   results in indexed slots as well.  Those slots are on the stack.
   It's somewhat confusing to call it a register machine, given that the
   values are on the stack.  Perhaps it needs a new name.

   Anyway, things are in a transitional state.  We're going to try to
   avoid munging the old VM very much while we flesh out the new one.
   We're also going to try to make them interoperable, as much as
   possible -- to have the old VM be able to call procedures for the new
   VM, and vice versa.  This should ease the bootstrapping process.  */


/* The old VM.  */
static SCM VM_NAME (SCM, SCM, SCM*, int);
/* The new VM.  */
static SCM RTL_VM_NAME (SCM, SCM, SCM*, size_t);


#if (VM_ENGINE == SCM_VM_REGULAR_ENGINE)
# define VM_USE_HOOKS		0	/* Various hooks */
#elif (VM_ENGINE == SCM_VM_DEBUG_ENGINE)
# define VM_USE_HOOKS		1
#else
# error unknown debug engine VM_ENGINE
#endif

/* Assign some registers by hand.  There used to be a bigger list here,
   but it was never tested, and in the case of x86-32, was a source of
   compilation failures.  It can be revived if it's useful, but my naive
   hope is that simply annotating the locals with "register" will be a
   sufficient hint to the compiler.  */
#ifdef __GNUC__
# if defined __x86_64__
/* GCC 4.6 chooses %rbp for IP_REG and %rbx for SP_REG, which works
   well.  Tell it to keep the jump table in a r12, which is
   callee-saved.  */
#  define JT_REG asm ("r12")
# endif
#endif

#ifndef IP_REG
# define IP_REG
#endif
#ifndef SP_REG
# define SP_REG
#endif
#ifndef FP_REG
# define FP_REG
#endif
#ifndef JT_REG
# define JT_REG
#endif

#define VM_ASSERT(condition, handler)           \
  do {                                          \
    if (SCM_UNLIKELY (!(condition)))            \
      {                                         \
        SYNC_ALL();                             \
        handler;                                \
      }                                         \
  } while (0)

#ifdef VM_ENABLE_ASSERTIONS
# define ASSERT(condition) VM_ASSERT (condition, abort())
#else
# define ASSERT(condition)
#endif

#if VM_USE_HOOKS
#define RUN_HOOK(h, args, n)                            \
  do {                                                  \
    if (SCM_UNLIKELY (vp->trace_level > 0))             \
      {                                                 \
        SYNC_REGISTER ();				\
        vm_dispatch_hook (vm, h, args, n);              \
      }                                                 \
  } while (0)
#else
#define RUN_HOOK(h, args, n)
#endif
#define RUN_HOOK0(h) RUN_HOOK(h, NULL, 0)

#define APPLY_HOOK()                            \
  RUN_HOOK0 (SCM_VM_APPLY_HOOK)
#define PUSH_CONTINUATION_HOOK()                \
  RUN_HOOK0 (SCM_VM_PUSH_CONTINUATION_HOOK)
#define POP_CONTINUATION_HOOK(vals, n)  \
  RUN_HOOK (SCM_VM_POP_CONTINUATION_HOOK, vals, n)
#define NEXT_HOOK()                             \
  RUN_HOOK0 (SCM_VM_NEXT_HOOK)
#define ABORT_CONTINUATION_HOOK(vals, n)        \
  RUN_HOOK (SCM_VM_ABORT_CONTINUATION_HOOK, vals, n)
#define RESTORE_CONTINUATION_HOOK()            \
  RUN_HOOK0 (SCM_VM_RESTORE_CONTINUATION_HOOK)

#define VM_HANDLE_INTERRUPTS                     \
  SCM_ASYNC_TICK_WITH_CODE (current_thread, SYNC_REGISTER ())




/* Cache the VM's instruction, stack, and frame pointer in local variables.  */
#define CACHE_REGISTER()			\
{						\
  ip = vp->ip;					\
  sp = vp->sp;					\
  fp = vp->fp;					\
}

/* Update the registers in VP, a pointer to the current VM.  This must be done
   at least before any GC invocation so that `vp->sp' is up-to-date and the
   whole stack gets marked.  */
#define SYNC_REGISTER()				\
{						\
  vp->ip = ip;					\
  vp->sp = sp;					\
  vp->fp = fp;					\
}

/* FIXME */
#define ASSERT_VARIABLE(x)                                              \
  VM_ASSERT (SCM_VARIABLEP (x), abort())
#define ASSERT_BOUND_VARIABLE(x)                                        \
  VM_ASSERT (SCM_VARIABLEP (x)                                          \
             && !scm_is_eq (SCM_VARIABLE_REF (x), SCM_UNDEFINED),       \
             abort())

#ifdef VM_ENABLE_PARANOID_ASSERTIONS
#define CHECK_IP() \
  do { if (ip < bp->base || ip - bp->base > bp->len) abort (); } while (0)
#define ASSERT_ALIGNED_PROCEDURE() \
  do { if ((scm_t_bits)bp % 8) abort (); } while (0)
#define ASSERT_BOUND(x) \
  VM_ASSERT (!scm_is_eq ((x), SCM_UNDEFINED), abort())
#else
#define CHECK_IP()
#define ASSERT_ALIGNED_PROCEDURE()
#define ASSERT_BOUND(x)
#endif

/* Cache the object table and free variables.  */
#define CACHE_PROGRAM()							\
{									\
  if (bp != SCM_PROGRAM_DATA (program)) {                               \
    bp = SCM_PROGRAM_DATA (program);					\
    ASSERT_ALIGNED_PROCEDURE ();                                        \
    if (SCM_I_IS_VECTOR (SCM_PROGRAM_OBJTABLE (program))) {             \
      objects = SCM_I_VECTOR_WELTS (SCM_PROGRAM_OBJTABLE (program));    \
    } else {                                                            \
      objects = NULL;                                                   \
    }                                                                   \
  }                                                                     \
}

#define SYNC_BEFORE_GC()			\
{						\
  SYNC_REGISTER ();				\
}

#define SYNC_ALL()				\
{						\
  SYNC_REGISTER ();				\
}


/*
 * Error check
 */

/* Accesses to a program's object table.  */
#define CHECK_OBJECT(_num)
#define CHECK_FREE_VARIABLE(_num)


/*
 * Stack operation
 */

#ifdef VM_ENABLE_STACK_NULLING
# define CHECK_STACK_LEAKN(_n) ASSERT (!sp[_n]);
# define CHECK_STACK_LEAK() CHECK_STACK_LEAKN(1)
# define NULLSTACK(_n) { int __x = _n; CHECK_STACK_LEAKN (_n+1); while (__x > 0) sp[__x--] = NULL; }
/* If you have a nonlocal exit in a pre-wind proc while invoking a continuation
   inside a dynwind (phew!), the stack is fully rewound but vm_reset_stack for
   that continuation doesn't have a chance to run. It's not important on a
   semantic level, but it does mess up our stack nulling -- so this macro is to
   fix that. */
# define NULLSTACK_FOR_NONLOCAL_EXIT() if (vp->sp > sp) NULLSTACK (vp->sp - sp);
#else
# define CHECK_STACK_LEAKN(_n)
# define CHECK_STACK_LEAK()
# define NULLSTACK(_n)
# define NULLSTACK_FOR_NONLOCAL_EXIT()
#endif

/* For this check, we don't use VM_ASSERT, because that leads to a
   per-site SYNC_ALL, which is too much code growth.  The real problem
   of course is having to check for overflow all the time... */
#define CHECK_OVERFLOW()                                                \
  do { if (SCM_UNLIKELY (sp >= stack_limit)) goto handle_overflow; } while (0)

#ifdef VM_CHECK_UNDERFLOW
#define PRE_CHECK_UNDERFLOW(N)                  \
  VM_ASSERT (sp - (N) > SCM_FRAME_UPPER_ADDRESS (fp), vm_error_stack_underflow ())
#define CHECK_UNDERFLOW() PRE_CHECK_UNDERFLOW (0)
#else
#define PRE_CHECK_UNDERFLOW(N) /* nop */
#define CHECK_UNDERFLOW() /* nop */
#endif


#define PUSH(x)	do { sp++; CHECK_OVERFLOW (); *sp = x; } while (0)
#define DROP()	do { sp--; CHECK_UNDERFLOW (); NULLSTACK (1); } while (0)
#define DROPN(_n) do { sp -= (_n); CHECK_UNDERFLOW (); NULLSTACK (_n); } while (0)
#define POP(x)	do { PRE_CHECK_UNDERFLOW (1); x = *sp--; NULLSTACK (1); } while (0)
#define POP2(x,y) do { PRE_CHECK_UNDERFLOW (2); x = *sp--; y = *sp--; NULLSTACK (2); } while (0)
#define POP3(x,y,z) do { PRE_CHECK_UNDERFLOW (3); x = *sp--; y = *sp--; z = *sp--; NULLSTACK (3); } while (0)

/* Pop the N objects on top of the stack and push a list that contains
   them.  */
#define POP_LIST(n)				\
do						\
{						\
  int i;					\
  SCM l = SCM_EOL, x;				\
  SYNC_BEFORE_GC ();                            \
  for (i = n; i; i--)                           \
    {                                           \
      POP (x);                                  \
      l = scm_cons (x, l);                      \
    }                                           \
  PUSH (l);					\
} while (0)

/* The opposite: push all of the elements in L onto the list. */
#define PUSH_LIST(l, NILP)			\
do						\
{						\
  for (; scm_is_pair (l); l = SCM_CDR (l))      \
    PUSH (SCM_CAR (l));                         \
  VM_ASSERT (NILP (l), vm_error_improper_list (l)); \
} while (0)


/*
 * Instruction operation
 */

#define FETCH()		(*ip++)
#define FETCH_LENGTH(len) do { len=*ip++; len<<=8; len+=*ip++; len<<=8; len+=*ip++; } while (0)

#undef NEXT_JUMP
#ifdef HAVE_LABELS_AS_VALUES
# define NEXT_JUMP()		goto *jump_table[FETCH () & SCM_VM_INSTRUCTION_MASK]
#else
# define NEXT_JUMP()		goto vm_start
#endif

#define NEXT					\
{						\
  NEXT_HOOK ();					\
  CHECK_STACK_LEAK ();                          \
  NEXT_JUMP ();					\
}


/* See frames.h for the layout of stack frames */
/* When this is called, bp points to the new program data,
   and the arguments are already on the stack */
#define DROP_FRAME()                            \
  {                                             \
    sp -= 3;                                    \
    NULLSTACK (3);                              \
    CHECK_UNDERFLOW ();                         \
  }
    

static SCM
VM_NAME (SCM vm, SCM program, SCM *argv, int nargs)
{
  /* VM registers */
  register scm_t_uint8 *ip IP_REG;	/* instruction pointer */
  register SCM *sp SP_REG;		/* stack pointer */
  register SCM *fp FP_REG;		/* frame pointer */
  struct scm_vm *vp = SCM_VM_DATA (vm);

  /* Cache variables */
  struct scm_objcode *bp = NULL;	/* program base pointer */
  SCM *objects = NULL;			/* constant objects */
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */

  scm_i_thread *current_thread = SCM_I_CURRENT_THREAD;

  /* Internal variables */
  int nvalues = 0;
  scm_i_jmp_buf registers;              /* used for prompts */

#ifdef HAVE_LABELS_AS_VALUES
  static const void **jump_table_pointer = NULL;
#endif

#ifdef HAVE_LABELS_AS_VALUES
  register const void **jump_table JT_REG;

  if (SCM_UNLIKELY (!jump_table_pointer))
    {
      int i;
      jump_table_pointer = malloc (SCM_VM_NUM_INSTRUCTIONS * sizeof (void*));
      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        jump_table_pointer[i] = &&vm_error_bad_instruction;
#define VM_INSTRUCTION_TO_LABEL 1
#define jump_table jump_table_pointer
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef jump_table
#undef VM_INSTRUCTION_TO_LABEL
    }

  /* Attempt to keep JUMP_TABLE_POINTER in a register.  This saves one
     load instruction at each instruction dispatch.  */
  jump_table = jump_table_pointer;
#endif

  if (SCM_I_SETJMP (registers))
    {
      /* Non-local return.  Cache the VM registers back from the vp, and
         go to the handler.

         Note, at this point, we must assume that any variable local to
         vm_engine that can be assigned *has* been assigned. So we need to pull
         all our state back from the ip/fp/sp.
      */
      CACHE_REGISTER ();
      program = SCM_FRAME_PROGRAM (fp);
      CACHE_PROGRAM ();
      /* The stack contains the values returned to this continuation,
         along with a number-of-values marker -- like an MV return. */
      ABORT_CONTINUATION_HOOK (sp - SCM_I_INUM (*sp), SCM_I_INUM (*sp));
      NEXT;
    }

  CACHE_REGISTER ();

  /* Since it's possible to receive the arguments on the stack itself,
     and indeed the RTL VM invokes us that way, shuffle up the
     arguments first.  */
  VM_ASSERT (sp + 8 + nargs < stack_limit, vm_error_too_many_args (nargs));
  {
    int i;
    for (i = nargs - 1; i >= 0; i--)
      sp[9 + i] = argv[i];
  }

  /* Initial frame */
  PUSH (SCM_PACK (fp)); /* dynamic link */
  PUSH (SCM_PACK (0)); /* mvra */
  PUSH (SCM_PACK (ip)); /* ra */
  PUSH (boot_continuation);
  fp = sp + 1;
  ip = SCM_C_OBJCODE_BASE (SCM_PROGRAM_DATA (boot_continuation));

  /* MV-call frame, function & arguments */
  PUSH (SCM_PACK (fp)); /* dynamic link */
  PUSH (SCM_PACK (ip + 1)); /* mvra */
  PUSH (SCM_PACK (ip)); /* ra */
  PUSH (program);
  fp = sp + 1;
  sp += nargs;

  PUSH_CONTINUATION_HOOK ();

 apply:
  program = fp[-1];
  if (!SCM_PROGRAM_P (program))
    {
      if (SCM_STRUCTP (program) && SCM_STRUCT_APPLICABLE_P (program))
        fp[-1] = SCM_STRUCT_PROCEDURE (program);
      else if (SCM_HAS_TYP7 (program, scm_tc7_rtl_program))
        {
          SCM ret;
          SYNC_ALL ();

          ret = RTL_VM_NAME (vm, program, fp, sp - fp + 1);

          NULLSTACK_FOR_NONLOCAL_EXIT ();

          if (SCM_UNLIKELY (SCM_VALUESP (ret)))
            {
              /* multiple values returned to continuation */
              ret = scm_struct_ref (ret, SCM_INUM0);
              nvalues = scm_ilength (ret);
              PUSH_LIST (ret, scm_is_null);
              goto vm_return_values;
            }
          else
            {
              PUSH (ret);
              goto vm_return;
            }
        }
      else if (SCM_HAS_TYP7 (program, scm_tc7_smob)
               && SCM_SMOB_APPLICABLE_P (program))
        {
          /* (smob arg0 ... argN) => (apply-smob smob arg0 ... argN) */
          int i;
          PUSH (SCM_BOOL_F);
          for (i = sp - fp; i >= 0; i--)
            fp[i] = fp[i - 1];
          fp[-1] = SCM_SMOB_DESCRIPTOR (program).apply_trampoline;
        }
      else
        {
          SYNC_ALL();
          vm_error_wrong_type_apply (program);
        }
      goto apply;
    }

  CACHE_PROGRAM ();
  ip = SCM_C_OBJCODE_BASE (bp);

  APPLY_HOOK ();

  /* Let's go! */
  NEXT;

#ifndef HAVE_LABELS_AS_VALUES
 vm_start:
  switch ((*ip++) & SCM_VM_INSTRUCTION_MASK) {
#endif

#include "vm-expand.h"
#include "vm-i-system.c"
#include "vm-i-scheme.c"
#include "vm-i-loader.c"

#ifndef HAVE_LABELS_AS_VALUES
  default:
    goto vm_error_bad_instruction;
  }
#endif

  abort (); /* never reached */

 vm_error_bad_instruction:
  vm_error_bad_instruction (ip[-1]);
  abort (); /* never reached */

 handle_overflow:
  SYNC_ALL ();
  vm_error_stack_overflow (vp);
  abort (); /* never reached */
}

#undef ALIGNED_P
#undef CACHE_REGISTER
#undef CHECK_OVERFLOW
#undef FREE_VARIABLE_REF
#undef FUNC2
#undef INIT
#undef INUM_MAX
#undef INUM_MIN
#undef jump_table
#undef LOCAL_REF
#undef LOCAL_SET
#undef NEXT
#undef NEXT_JUMP
#undef REL
#undef RETURN
#undef RETURN_ONE_VALUE
#undef RETURN_VALUE_LIST
#undef SYNC_ALL
#undef SYNC_BEFORE_GC
#undef SYNC_IP
#undef SYNC_REGISTER
#undef VARIABLE_BOUNDP
#undef VARIABLE_REF
#undef VARIABLE_SET
#undef VM_DEFINE_OP
#undef VM_INSTRUCTION_TO_LABEL




/* Virtual Machine

   This is Guile's new virtual machine.  When I say "new", I mean
   relative to the current virtual machine.  At some point it will
   become "the" virtual machine, and we'll delete this paragraph.  As
   such, the rest of the comments speak as if there's only one VM.

   <more overview here>
 */


/* The VM has three state bits: the instruction pointer (IP), the frame
   pointer (FP), and the top-of-stack pointer (SP).  We cache the first
   two of these in machine registers, local to the VM, because they are
   used extensively by the VM.  As the SP is used more by code outside
   the VM than by the VM itself, we don't bother caching it locally.

   Since the FP changes infrequently, relative to the IP, we keep vp->fp
   in sync with the local FP.  This would be a big lose for the IP,
   though, so instead of updating vp->ip all the time, we call SYNC_IP
   whenever we would need to know the IP of the top frame.  In practice,
   we need to SYNC_IP whenever we call out of the VM to a function that
   would like to walk the stack, perhaps as the result of an
   exception.  */

#define SYNC_IP() \
  vp->ip = (scm_t_uint8 *) (ip)

#define SYNC_REGISTER() \
  SYNC_IP()
#define SYNC_BEFORE_GC() /* Only SP and FP needed to trace GC */
#define SYNC_ALL() /* FP already saved */ \
  SYNC_IP()

#define CHECK_OVERFLOW(sp)                      \
  do {                                          \
    if (SCM_UNLIKELY ((sp) >= stack_limit))     \
      vm_error_stack_overflow (vp);             \
  } while (0)

/* Reserve stack space for a frame.  Will check that there is sufficient
   stack space for N locals, not including the procedure, in addition to
   4 words to set up the next frame.  Invoke after preparing the new
   frame and setting the fp and ip.  */
#define ALLOC_FRAME(n)                                              \
  do {                                                              \
    SCM *new_sp = vp->sp = fp - 1 + n;                              \
    CHECK_OVERFLOW (new_sp + 4);                                    \
  } while (0)

/* Reset the current frame to hold N locals.  Used when we know that no
   stack expansion is needed.  */
#define RESET_FRAME(n)                                              \
  do {                                                              \
    vp->sp = fp - 1 + n;                                            \
  } while (0)

/* Compute the number of locals in the frame.  This is equal to the
   number of actual arguments when a function is first called.  */
#define FRAME_LOCALS_COUNT()                                        \
  (vp->sp + 1 - fp)

/* Restore registers after returning from a frame.  */
#define RESTORE_FRAME()                                             \
  do {                                                              \
  } while (0)


#define CACHE_REGISTER()                        \
  do {                                          \
    ip = (scm_t_uint32 *) vp->ip;               \
    fp = vp->fp;                                \
  } while (0)

#ifdef HAVE_LABELS_AS_VALUES
# define BEGIN_DISPATCH_SWITCH /* */
# define END_DISPATCH_SWITCH /* */
# define NEXT(n)                                \
  do                                            \
    {                                           \
      ip += n;                                  \
      NEXT_HOOK ();                             \
      op = *ip;                                 \
      goto *jump_table[op & 0xff];              \
    }                                           \
  while (0)
# define VM_DEFINE_OP(opcode, tag, name, meta)  \
  op_##tag:
#else
# define BEGIN_DISPATCH_SWITCH                  \
  vm_start:                                     \
    NEXT_HOOK ();                               \
    op = *ip;                                   \
  switch (op & 0xff)                            \
    {
# define END_DISPATCH_SWITCH                    \
  default:                                      \
    goto vm_error_bad_instruction;              \
    }
# define NEXT(n)                                \
  do                                            \
    {                                           \
      ip += n;                                  \
      goto vm_start;                            \
    }                                           \
  while (0)
# define VM_DEFINE_OP(opcode, tag, name, meta)  \
  op_##tag:                                     \
  case opcode:
#endif

#define LOCAL_REF(i)		SCM_FRAME_VARIABLE (fp, i)
#define LOCAL_SET(i,o)		SCM_FRAME_VARIABLE (fp, i) = o

#define VARIABLE_REF(v)		SCM_VARIABLE_REF (v)
#define VARIABLE_SET(v,o)	SCM_VARIABLE_SET (v, o)
#define VARIABLE_BOUNDP(v)      (!scm_is_eq (VARIABLE_REF (v), SCM_UNDEFINED))
#define FREE_VARIABLE_REF(i)	SCM_RTL_PROGRAM_FREE_VARIABLE_REF (SCM_FRAME_PROGRAM (fp), i)

#define RETURN_ONE_VALUE(ret)                           \
  do {                                                  \
    SCM val = ret;                                      \
    SCM *sp = SCM_FRAME_LOWER_ADDRESS (fp);             \
    VM_HANDLE_INTERRUPTS;                               \
    ip = SCM_FRAME_RTL_RETURN_ADDRESS (fp);             \
    vp->sp = sp;                                        \
    fp = vp->fp = SCM_FRAME_DYNAMIC_LINK (fp);          \
    *sp = val;                                          \
    POP_CONTINUATION_HOOK (sp, 1);                      \
    NEXT (0);                                           \
  } while (0)

/* While we could generate the list-unrolling code here, it's fine for
   now to just tail-call (apply values vals).  */
#define RETURN_VALUE_LIST(vals_)                        \
  do {                                                  \
    SCM vals = vals_;                                   \
    VM_HANDLE_INTERRUPTS;                               \
    fp[-1] = rtl_apply;                                 \
    fp[0] = rtl_values;                                 \
    fp[1] = vals;                                       \
    RESET_FRAME (2);                                    \
    ip = (scm_t_uint32 *) rtl_apply_code;               \
    goto op_apply;                                      \
  } while (0)

#define BR_NARGS(rel)                           \
  scm_t_uint16 expected;                        \
  SCM_UNPACK_RTL_24 (op, expected);             \
  if (FRAME_LOCALS_COUNT() rel expected)        \
    {                                           \
      scm_t_int32 offset = ip[1];               \
      offset >>= 8; /* Sign-extending shift. */ \
      NEXT (offset);                            \
    }                                           \
  NEXT (2)

#define BR_UNARY(x, exp)                        \
  scm_t_uint32 test;                            \
  SCM x;                                        \
  SCM_UNPACK_RTL_24 (op, test);                 \
  x = LOCAL_REF (test);                         \
  if ((ip[1] & 0x1) ? !(exp) : (exp))           \
    {                                           \
      scm_t_int32 offset = ip[1];               \
      offset >>= 8; /* Sign-extending shift. */ \
      if (offset < 0)                           \
        VM_HANDLE_INTERRUPTS;                   \
      NEXT (offset);                            \
    }                                           \
  NEXT (2)

#define BR_BINARY(x, y, exp)                    \
  scm_t_uint16 a, b;                            \
  SCM x, y;                                     \
  SCM_UNPACK_RTL_12_12 (op, a, b);              \
  x = LOCAL_REF (a);                            \
  y = LOCAL_REF (b);                            \
  if ((ip[1] & 0x1) ? !(exp) : (exp))           \
    {                                           \
      scm_t_int32 offset = ip[1];               \
      offset >>= 8; /* Sign-extending shift. */ \
      if (offset < 0)                           \
        VM_HANDLE_INTERRUPTS;                   \
      NEXT (offset);                            \
    }                                           \
  NEXT (2)

#define BR_ARITHMETIC(crel,srel)                                        \
  {                                                                     \
    scm_t_uint16 a, b;                                                  \
    SCM x, y;                                                           \
    SCM_UNPACK_RTL_12_12 (op, a, b);                                    \
    x = LOCAL_REF (a);                                                  \
    y = LOCAL_REF (b);                                                  \
    if (SCM_I_INUMP (x) && SCM_I_INUMP (y))                             \
      {                                                                 \
        scm_t_signed_bits x_bits = SCM_UNPACK (x);                      \
        scm_t_signed_bits y_bits = SCM_UNPACK (y);                      \
        if (x_bits crel y_bits)                                         \
          {                                                             \
            scm_t_int32 offset = ip[1];                                 \
            offset >>= 8; /* Sign-extending shift. */                   \
            if (offset < 0)                                             \
              VM_HANDLE_INTERRUPTS;                                     \
            NEXT (offset);                                              \
          }                                                             \
        NEXT (2);                                                       \
      }                                                                 \
    else                                                                \
      {                                                                 \
        SYNC_IP ();                                                     \
        if (scm_is_true (srel (x, y)))                                  \
          {                                                             \
            scm_t_int32 offset = ip[1];                                 \
            offset >>= 8; /* Sign-extending shift. */                   \
            if (offset < 0)                                             \
              VM_HANDLE_INTERRUPTS;                                     \
            NEXT (offset);                                              \
          }                                                             \
        NEXT (2);                                                       \
      }                                                                 \
  }

#define ARGS1(a1)                               \
  scm_t_uint16 dst, src;                        \
  SCM a1;                                       \
  SCM_UNPACK_RTL_12_12 (op, dst, src);          \
  a1 = LOCAL_REF (src)
#define ARGS2(a1, a2)                           \
  scm_t_uint8 dst, src1, src2;                  \
  SCM a1, a2;                                   \
  SCM_UNPACK_RTL_8_8_8 (op, dst, src1, src2);   \
  a1 = LOCAL_REF (src1);                        \
  a2 = LOCAL_REF (src2)
#define RETURN(x)                               \
  do { LOCAL_SET (dst, x); NEXT (1); } while (0)

/* The maximum/minimum tagged integers.  */
#define INUM_MAX (INTPTR_MAX - 1)
#define INUM_MIN (INTPTR_MIN + scm_tc2_int)

#define BINARY_INTEGER_OP(CFUNC,SFUNC)                                      \
  {                                                             \
    ARGS2 (x, y);						\
    if (SCM_I_INUMP (x) && SCM_I_INUMP (y))                     \
      {                                                         \
        scm_t_int64 n = SCM_I_INUM (x) CFUNC SCM_I_INUM (y);    \
        if (SCM_FIXABLE (n))                                    \
          RETURN (SCM_I_MAKINUM (n));                           \
      }                                                         \
    SYNC_IP ();                                                 \
    RETURN (SFUNC (x, y));                                      \
  }

#define VM_VALIDATE_PAIR(x, proc)		\
  VM_ASSERT (scm_is_pair (x), vm_error_not_a_pair (proc, x))
  
#define VM_VALIDATE_STRUCT(obj, proc)           \
  VM_ASSERT (SCM_STRUCTP (obj), vm_error_not_a_pair (proc, obj))

#define VM_VALIDATE_BYTEVECTOR(x, proc)		\
  VM_ASSERT (SCM_BYTEVECTOR_P (x), vm_error_not_a_bytevector (proc, x))

/* Return true (non-zero) if PTR has suitable alignment for TYPE.  */
#define ALIGNED_P(ptr, type)			\
  ((scm_t_uintptr) (ptr) % alignof_type (type) == 0)

static SCM
RTL_VM_NAME (SCM vm, SCM program, SCM *argv, size_t nargs_)
{
  /* Instruction pointer: A pointer to the opcode that is currently
     running.  */
  register scm_t_uint32 *ip IP_REG;

  /* Frame pointer: A pointer into the stack, off of which we index
     arguments and local variables.  Pushed at function calls, popped on
     returns.  */
  register SCM *fp FP_REG;

  /* Current opcode: A cache of *ip.  */
  register scm_t_uint32 op;

  /* Cached variables. */
  struct scm_vm *vp = SCM_VM_DATA (vm);
  SCM *stack_limit = vp->stack_limit;	/* stack limit address */
  scm_i_thread *current_thread = SCM_I_CURRENT_THREAD;
  scm_i_jmp_buf registers;              /* used for prompts */

#ifdef HAVE_LABELS_AS_VALUES
  static const void **jump_table_pointer = NULL;
  register const void **jump_table JT_REG;

  if (SCM_UNLIKELY (!jump_table_pointer))
    {
      int i;
      jump_table_pointer = malloc (SCM_VM_NUM_INSTRUCTIONS * sizeof (void*));
      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        jump_table_pointer[i] = &&vm_error_bad_instruction;
#define INIT(opcode, tag, name, meta) jump_table_pointer[opcode] = &&op_##tag;
      FOR_EACH_VM_OPERATION(INIT);
#undef INIT
    }

  /* Attempt to keep JUMP_TABLE_POINTER in a register.  This saves one
     load instruction at each instruction dispatch.  */
  jump_table = jump_table_pointer;
#endif

  if (SCM_I_SETJMP (registers))
    {
      /* Non-local return.  The values are on the stack, on a new frame
         set up to call `values' to return the values to the handler.
         Cache the VM registers back from the vp, and dispatch to the
         body of `values'.

         Note, at this point, we must assume that any variable local to
         vm_engine that can be assigned *has* been assigned. So we need
         to pull all our state back from the ip/fp/sp.
      */
      CACHE_REGISTER ();
      ABORT_CONTINUATION_HOOK (fp, FRAME_LOCALS_COUNT());
      NEXT (0);
    }

  /* Load previous VM registers. */
  CACHE_REGISTER ();

  VM_HANDLE_INTERRUPTS;

  /* Initialization */
  {
    SCM *base;

    /* Check that we have enough space: 4 words for the boot
       continuation, 4 + nargs for the procedure application, and 4 for
       setting up a new frame.  */
    base = vp->sp + 1;
    CHECK_OVERFLOW (vp->sp + 4 + 4 + nargs_ + 4);

    /* Since it's possible to receive the arguments on the stack itself,
       and indeed the regular VM invokes us that way, shuffle up the
       arguments first.  */
    {
      int i;
      for (i = nargs_ - 1; i >= 0; i--)
        base[8 + i] = argv[i];
    }

    /* Initial frame, saving previous fp and ip, with the boot
       continuation.  */
    base[0] = SCM_PACK (fp); /* dynamic link */
    base[1] = SCM_PACK (0); /* the boot continuation does not return to scheme */
    base[2] = SCM_PACK (ip); /* ra */
    base[3] = rtl_boot_continuation;
    fp = &base[4];
    ip = rtl_boot_single_value_continuation_code;
    if (ip - 1 != rtl_boot_multiple_value_continuation_code)
      abort();

    /* MV-call frame, function & arguments */
    base[4] = SCM_PACK (fp); /* dynamic link */
    base[5] = SCM_PACK (ip - 1); /* in RTL programs, MVRA precedes RA by one */
    base[6] = SCM_PACK (ip); /* ra */
    base[7] = program;
    fp = vp->fp = &base[8];
    RESET_FRAME (nargs_);
  }

 apply:
  while (!SCM_RTL_PROGRAM_P (SCM_FRAME_PROGRAM (fp)))
    {
#if 0
      SCM proc = SCM_FRAME_PROGRAM (fp);

      if (SCM_STRUCTP (proc) && SCM_STRUCT_APPLICABLE_P (proc))
        {
          fp[-1] = SCM_STRUCT_PROCEDURE (proc);
          continue;
        }
      if (SCM_HAS_TYP7 (proc, scm_tc7_smob) && SCM_SMOB_APPLICABLE_P (proc))
        {
          scm_t_uint32 n = FRAME_LOCALS_COUNT();

          /* Shuffle args up, place smob in local 0. */
          CHECK_OVERFLOW (vp->sp + 1);
          vp->sp++;
          while (n--)
            LOCAL_SET (n + 1, LOCAL_REF (n));
          LOCAL_SET (0, proc);

          fp[-1] = SCM_SMOB_DESCRIPTOR (proc).apply_trampoline;
          continue;
        }

      SYNC_IP();
      vm_error_wrong_type_apply (proc);
#else
      SCM ret;
      SYNC_ALL ();

      ret = VM_NAME (vm, fp[-1], fp, FRAME_LOCALS_COUNT ());

      if (SCM_UNLIKELY (SCM_VALUESP (ret)))
        RETURN_VALUE_LIST (scm_struct_ref (ret, SCM_INUM0));
      else
        RETURN_ONE_VALUE (ret);
#endif
    }

  /* Let's go! */
  ip = SCM_RTL_PROGRAM_CODE (SCM_FRAME_PROGRAM (fp));
  NEXT (0);

  BEGIN_DISPATCH_SWITCH;
  

  

  /*
   * Call and return
   */

  /* halt _:24
   *
   * Bring the VM to a halt, returning the single value from r0.
   */
  VM_DEFINE_OP (0, halt, "halt", OP1 (U8_X24))
    {
      SCM ret = LOCAL_REF (0);

      vp->ip = SCM_FRAME_RETURN_ADDRESS (fp);
      vp->sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      vp->fp = SCM_FRAME_DYNAMIC_LINK (fp);

      return ret;
    }

  /* halt/values _:24
   *
   * Bring the VM to a halt, returning all the values on the stack.
   */
  VM_DEFINE_OP (1, halt_values, "halt/values", OP1 (U8_X24))
    {
      scm_t_ptrdiff n;
      SCM *base;
      SCM ret = SCM_EOL;

      SYNC_BEFORE_GC();

      base = fp + 4;
      n = FRAME_LOCALS_COUNT ();
      while (n--)
        ret = scm_cons (base[n], ret);

      vp->ip = SCM_FRAME_RETURN_ADDRESS (fp);
      vp->sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      vp->fp = SCM_FRAME_DYNAMIC_LINK (fp);

      return scm_values (ret);
    }

  /* call from:24 _:8 proc:24 _:8 nargs:24 arg0:24 0:8 ...
   *
   * Call a procedure.  Push a call frame on at FROM, saving the return
   * address and the fp.  Parse out NARGS, and push the procedure and
   * arguments.  All arguments except for RETURN-LOC are 24-bit values.
   * FROM, PROC, and NARGS are in the upper 24 bits of the words.  The
   * ARGN... are in the lower 24 bits, with the upper 8 bits being 0.
   *
   * The MVRA of the new frame is set to point to the next instruction
   * after the end of the `call' instruction.  The word following that
   * is the RA.
   */
  VM_DEFINE_OP (2, call, "call", OP3 (U8_U24, X8_U24, X8_R24))
    {
      scm_t_uint32 from, proc, nargs, n;
      SCM *old_fp = fp;

      SCM_UNPACK_RTL_24 (op, from);
      SCM_UNPACK_RTL_24 (ip[1], proc);
      SCM_UNPACK_RTL_24 (ip[2], nargs);

      VM_HANDLE_INTERRUPTS;

      fp = vp->fp = old_fp + from + 4;
      SCM_FRAME_SET_DYNAMIC_LINK (fp, old_fp);
      SCM_FRAME_SET_RTL_MV_RETURN_ADDRESS (fp, ip + 3 + nargs);
      SCM_FRAME_SET_RTL_RETURN_ADDRESS (fp, ip + 4 + nargs);
      fp[-1] = old_fp[proc];
      ALLOC_FRAME (nargs);

      for (n = 0; n < nargs; n++)
        LOCAL_SET (n, old_fp[ip[3 + n]]);

      PUSH_CONTINUATION_HOOK ();
      APPLY_HOOK ();

      if (SCM_UNLIKELY (!SCM_RTL_PROGRAM_P (SCM_FRAME_PROGRAM (fp))))
        goto apply;

      ip = SCM_RTL_PROGRAM_CODE (SCM_FRAME_PROGRAM (fp));
      NEXT (0);
    }

  /* call/values from:24 _:8 proc:24
   *
   * Call a procedure, with the values already pushed above a call frame
   * at FROM.  This instruction is used to handle MV returns in the case
   * that we can't inline the handler.
   *
   * As with `call', the next instruction after the call/values will be
   * the MVRA, and the word after that instruction is the RA.
   */
  VM_DEFINE_OP (3, call_values, "call/values", OP2 (U8_U24, X8_U24))
    {
      scm_t_uint32 from, proc;
      SCM *old_fp = fp;

      SCM_UNPACK_RTL_24 (op, from);
      SCM_UNPACK_RTL_24 (ip[1], proc);

      VM_HANDLE_INTERRUPTS;

      fp = vp->fp = old_fp + from + 4;
      SCM_FRAME_SET_DYNAMIC_LINK (fp, old_fp);
      SCM_FRAME_SET_RTL_MV_RETURN_ADDRESS (fp, ip + 2);
      SCM_FRAME_SET_RTL_RETURN_ADDRESS (fp, ip + 3);
      fp[-1] = old_fp[proc];

      PUSH_CONTINUATION_HOOK ();
      APPLY_HOOK ();

      if (SCM_UNLIKELY (!SCM_RTL_PROGRAM_P (SCM_FRAME_PROGRAM (fp))))
        goto apply;

      ip = SCM_RTL_PROGRAM_CODE (SCM_FRAME_PROGRAM (fp));
      NEXT (0);
    }

  /* tail-call nargs:24 _:8 proc:24
   *
   * Tail-call a procedure.  Requires that all of the arguments have
   * already been shuffled into position.
   */
  VM_DEFINE_OP (4, tail_call, "tail-call", OP2 (U8_U24, X8_U24))
    {
      scm_t_uint32 nargs, proc;

      SCM_UNPACK_RTL_24 (op, nargs);
      SCM_UNPACK_RTL_24 (ip[1], proc);

      VM_HANDLE_INTERRUPTS;

      fp[-1] = LOCAL_REF (proc);
      /* No need to check for overflow, as the compiler has already
         ensured that this frame has enough space.  */
      RESET_FRAME (nargs);

      APPLY_HOOK ();

      if (SCM_UNLIKELY (!SCM_RTL_PROGRAM_P (SCM_FRAME_PROGRAM (fp))))
        goto apply;

      ip = SCM_RTL_PROGRAM_CODE (SCM_FRAME_PROGRAM (fp));
      NEXT (0);
    }

  /* return src:24
   *
   * Return a value.
   */
  VM_DEFINE_OP (5, return, "return", OP1 (U8_U24))
    {
      scm_t_uint32 src;
      SCM_UNPACK_RTL_24 (op, src);
      RETURN_ONE_VALUE (LOCAL_REF (src));
    }

  /* return-values nvalues:24
   *
   * Return a number of values from a call frame.  This opcode
   * corresponds to an application of `values' in tail position.  As
   * with tail calls, we expect that the NVALUES values have already
   * been shuffled down to a contiguous array starting at slot 0.
   */
  VM_DEFINE_OP (6, return_values, "return/values", OP1 (U8_U24))
    {
      scm_t_uint32 nargs;
      SCM_UNPACK_RTL_24 (op, nargs);
      RESET_FRAME (nargs);
      fp[-1] = rtl_values;
      goto op_values;
    }


  

  /*
   * Specialized call stubs
   */

  /* subr-call ptr-idx:24
   *
   * Call a subr, passing all locals in this frame as arguments.  Fetch
   * the foreign pointer from PTR-IDX, a free variable.  Return from the
   * calling frame.  This instruction is part of the trampolines
   * created in gsubr.c, and is not generated by the compiler.
   */
  VM_DEFINE_OP (7, subr_call, "subr-call", OP1 (U8_U24))
    {
      scm_t_uint32 ptr_idx;
      SCM pointer, ret;
      SCM (*subr)();

      SCM_UNPACK_RTL_24 (op, ptr_idx);

      pointer = FREE_VARIABLE_REF (ptr_idx);
      subr = SCM_POINTER_VALUE (pointer);

      VM_HANDLE_INTERRUPTS;
      SYNC_IP ();

      switch (FRAME_LOCALS_COUNT ())
        {
        case 0:
          ret = subr ();
          break;
        case 1:
          ret = subr (fp[0]);
          break;
        case 2:
          ret = subr (fp[0], fp[1]);
          break;
        case 3:
          ret = subr (fp[0], fp[1], fp[2]);
          break;
        case 4:
          ret = subr (fp[0], fp[1], fp[2], fp[3]);
          break;
        case 5:
          ret = subr (fp[0], fp[1], fp[2], fp[3], fp[4]);
          break;
        case 6:
          ret = subr (fp[0], fp[1], fp[2], fp[3], fp[4], fp[5]);
          break;
        case 7:
          ret = subr (fp[0], fp[1], fp[2], fp[3], fp[4], fp[5], fp[6]);
          break;
        case 8:
          ret = subr (fp[0], fp[1], fp[2], fp[3], fp[4], fp[5], fp[6], fp[7]);
          break;
        case 9:
          ret = subr (fp[0], fp[1], fp[2], fp[3], fp[4], fp[5], fp[6], fp[7], fp[8]);
          break;
        case 10:
          ret = subr (fp[0], fp[1], fp[2], fp[3], fp[4], fp[5], fp[6], fp[7], fp[8], fp[9]);
          break;
        default:
          abort ();
        }

      // NULLSTACK_FOR_NONLOCAL_EXIT ();

      if (SCM_UNLIKELY (SCM_VALUESP (ret)))
        /* multiple values returned to continuation */
        RETURN_VALUE_LIST (scm_struct_ref (ret, SCM_INUM0));
      else
        RETURN_ONE_VALUE (ret);
    }

  /* foreign-call cif-idx:12 ptr-idx:12
   *
   * Call a foreign function.  Fetch the CIF and foreign pointer from
   * CIF-IDX and PTR-IDX, both free variables.  Return from the calling
   * frame.  Arguments are taken from the stack.  This instruction is
   * part of the trampolines created by the FFI, and is not generated by
   * the compiler.
   */
  VM_DEFINE_OP (8, foreign_call, "foreign-call", OP1 (U8_U12_U12))
    {
      scm_t_uint16 cif_idx, ptr_idx;
      SCM cif, pointer, ret;

      SCM_UNPACK_RTL_12_12 (op, cif_idx, ptr_idx);

      cif = FREE_VARIABLE_REF (cif_idx);
      pointer = FREE_VARIABLE_REF (ptr_idx);

      SYNC_IP ();
      VM_HANDLE_INTERRUPTS;

      // FIXME: separate args
      ret = scm_i_foreign_call (scm_cons (cif, pointer), fp);

      // NULLSTACK_FOR_NONLOCAL_EXIT ();

      if (SCM_UNLIKELY (SCM_VALUESP (ret)))
        /* multiple values returned to continuation */
        RETURN_VALUE_LIST (scm_struct_ref (ret, SCM_INUM0));
      else
        RETURN_ONE_VALUE (ret);
    }

  /* continuation-call contregs:24
   *
   * Return to a continuation, nonlocally.  The arguments to the
   * continuation are taken from the stack.  CONTREGS is a free variable
   * containing the reified continuation.  This instruction is part of
   * the implementation of undelimited continuations, and is not
   * generated by the compiler.
   */
  VM_DEFINE_OP (9, continuation_call, "continuation-call", OP1 (U8_U24))
    {
      SCM contregs;
      scm_t_uint32 contregs_idx;

      SCM_UNPACK_RTL_24 (op, contregs_idx);

      contregs = FREE_VARIABLE_REF (contregs_idx);

      SYNC_IP ();
      scm_i_check_continuation (contregs);
      vm_return_to_continuation (scm_i_contregs_vm (contregs),
                                 scm_i_contregs_vm_cont (contregs),
                                 FRAME_LOCALS_COUNT (), fp);
      scm_i_reinstate_continuation (contregs);

      /* no NEXT */
      abort ();
    }

  /* compose-continuation cont:24
   *
   * Compose a partial continution with the current continuation.  The
   * arguments to the continuation are taken from the stack.  CONT is a
   * free variable containing the reified continuation.  This
   * instruction is part of the implementation of partial continuations,
   * and is not generated by the compiler.
   */
  VM_DEFINE_OP (10, compose_continuation, "compose-continuation", OP1 (U8_U24))
    {
      SCM vmcont;
      scm_t_uint32 cont_idx;

      SCM_UNPACK_RTL_24 (op, cont_idx);
      vmcont = LOCAL_REF (cont_idx);

      SYNC_IP ();
      VM_ASSERT (SCM_VM_CONT_REWINDABLE_P (vmcont),
                 vm_error_continuation_not_rewindable (vmcont));
      vm_reinstate_partial_continuation (vm, vmcont, FRAME_LOCALS_COUNT (), fp,
                                         &current_thread->dynstack,
                                         &registers);
      CACHE_REGISTER ();
      NEXT (0);
    }

  /* apply _:24
   *
   * Tail-apply the procedure in local slot 0 to the rest of the
   * arguments.  This instruction is part of the implementation of
   * `apply', and is not generated by the compiler.
   */
  VM_DEFINE_OP (11, apply, "apply", OP1 (U8_X24))
    {
      int i, list_idx, list_len, nargs;
      SCM list;

      VM_HANDLE_INTERRUPTS;

      VM_ASSERT (FRAME_LOCALS_COUNT () >= 2, abort ());
      nargs = FRAME_LOCALS_COUNT ();
      list_idx = nargs - 1;
      list = LOCAL_REF (list_idx);
      list_len = scm_ilength (list);

      VM_ASSERT (list_len >= 0, vm_error_apply_to_non_list (list));

      nargs = nargs - 2 + list_len;
      ALLOC_FRAME (nargs);

      for (i = 0; i < list_idx; i++)
        fp[i - 1] = fp[i];

      /* Null out these slots, just in case there are less than 2 elements
         in the list. */
      fp[list_idx - 1] = SCM_UNDEFINED;
      fp[list_idx] = SCM_UNDEFINED;

      for (i = 0; i < list_len; i++, list = SCM_CDR (list))
        fp[list_idx - 1 + i] = SCM_CAR (list);

      APPLY_HOOK ();

      if (SCM_UNLIKELY (!SCM_RTL_PROGRAM_P (SCM_FRAME_PROGRAM (fp))))
        goto apply;

      ip = SCM_RTL_PROGRAM_CODE (SCM_FRAME_PROGRAM (fp));
      NEXT (0);
    }

  /* call/cc _:24
   *
   * Capture the current continuation, and tail-apply the procedure in
   * local slot 0 to it.  This instruction is part of the implementation
   * of `call/cc', and is not generated by the compiler.
   */
  VM_DEFINE_OP (12, call_cc, "call/cc", OP1 (U8_X24))
#if 0
    {
      SCM vm_cont, cont;
      scm_t_dynstack *dynstack;

      VM_HANDLE_INTERRUPTS;

      SYNC_IP ();
      dynstack = scm_dynstack_capture_all (&current_thread->dynstack);
      vm_cont = scm_i_vm_capture_stack (vp->stack_base,
                                        SCM_FRAME_DYNAMIC_LINK (fp),
                                        SCM_FRAME_LOWER_ADDRESS (fp) - 1,
                                        SCM_FRAME_RETURN_ADDRESS (fp),
                                        SCM_FRAME_MV_RETURN_ADDRESS (fp),
                                        dynstack,
                                        0);
      cont = scm_i_make_continuation (&registers, vm, vm_cont);

      fp[-1] = fp[0];
      fp[0] = cont;
      RESET_FRAME (1);

      APPLY_HOOK ();

      if (SCM_UNLIKELY (!SCM_RTL_PROGRAM_P (SCM_FRAME_PROGRAM (fp))))
        goto apply;

      ip = SCM_RTL_PROGRAM_CODE (SCM_FRAME_PROGRAM (fp));
      NEXT (0);
    }
#else
  abort();
#endif

  /* values _:24
   *
   * Return all values on the stack to the current continuation.
   * This instruction is part of the implementation of
   * `values', and is not generated by the compiler.
   */
  VM_DEFINE_OP (13, values, "values", OP1 (U8_X24))
    {
      SCM *base = fp;
#if VM_USE_HOOKS
      int nargs = FRAME_LOCALS_COUNT ();
#endif

      /* We don't do much; it's the caller that's responsible for
         shuffling values and resetting the stack.  */

      VM_HANDLE_INTERRUPTS;
      ip = SCM_FRAME_RTL_MV_RETURN_ADDRESS (fp);
      fp = vp->fp = SCM_FRAME_DYNAMIC_LINK (fp);

      /* Clear stack frame.  */
      base[-1] = SCM_BOOL_F;
      base[-2] = SCM_BOOL_F;
      base[-3] = SCM_BOOL_F;
      base[-4] = SCM_BOOL_F;

      POP_CONTINUATION_HOOK (base, nargs);

      NEXT (0);
    }


  

  /*
   * Function prologues
   */

  /* br-if-nargs-ne expected:24 _:8 offset:24
   * br-if-nargs-lt expected:24 _:8 offset:24
   * br-if-nargs-gt expected:24 _:8 offset:24
   *
   * If the number of actual arguments is not equal, less than, or greater
   * than EXPECTED, respectively, add OFFSET, a signed 24-bit number, to
   * the current instruction pointer.
   */
  VM_DEFINE_OP (14, br_if_nargs_ne, "br-if-nargs-ne", OP2 (U8_U24, X8_L24))
    {
      BR_NARGS (!=);
    }
  VM_DEFINE_OP (15, br_if_nargs_lt, "br-if-nargs-lt", OP2 (U8_U24, X8_L24))
    {
      BR_NARGS (<);
    }
  VM_DEFINE_OP (16, br_if_nargs_gt, "br-if-nargs-gt", OP2 (U8_U24, X8_L24))
    {
      BR_NARGS (>);
    }

  /* assert-nargs-ee expected:24
   * assert-nargs-ge expected:24
   * assert-nargs-le expected:24
   *
   * If the number of actual arguments is not ==, >=, or <= EXPECTED,
   * respectively, signal an error.
   */
  VM_DEFINE_OP (17, assert_nargs_ee, "assert-nargs-ee", OP1 (U8_U24))
    {
      scm_t_uint32 expected;
      SCM_UNPACK_RTL_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () == expected,
                 vm_error_wrong_num_args (SCM_FRAME_PROGRAM (fp)));
      NEXT (1);
    }
  VM_DEFINE_OP (18, assert_nargs_ge, "assert-nargs-ge", OP1 (U8_U24))
    {
      scm_t_uint32 expected;
      SCM_UNPACK_RTL_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () >= expected,
                 vm_error_wrong_num_args (SCM_FRAME_PROGRAM (fp)));
      NEXT (1);
    }
  VM_DEFINE_OP (19, assert_nargs_le, "assert-nargs-le", OP1 (U8_U24))
    {
      scm_t_uint32 expected;
      SCM_UNPACK_RTL_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () <= expected,
                 vm_error_wrong_num_args (SCM_FRAME_PROGRAM (fp)));
      NEXT (1);
    }

  /* reserve-locals nlocals:24
   *
   * Ensure that there is space on the stack for NLOCALS local variables,
   * setting them all to SCM_UNDEFINED, except those nargs values that
   * were passed as arguments.
   */
  VM_DEFINE_OP (20, reserve_locals, "reserve-locals", OP1 (U8_U24))
    {
      scm_t_uint32 nlocals, nargs;
      SCM_UNPACK_RTL_24 (op, nlocals);

      nargs = FRAME_LOCALS_COUNT ();
      ALLOC_FRAME (nlocals);
      while (nlocals-- > nargs)
        LOCAL_SET (nlocals, SCM_UNDEFINED);

      NEXT (1);
    }

  /* assert-nargs-ee/locals expected:12 nlocals:12
   *
   * Equivalent to a sequence of assert-nargs-ee and reserve-locals.  The
   * number of locals reserved is EXPECTED + NLOCALS.
   */
  VM_DEFINE_OP (21, assert_nargs_ee_locals, "assert-nargs-ee/locals", OP1 (U8_U12_U12))
    {
      scm_t_uint16 expected, nlocals;
      SCM_UNPACK_RTL_12_12 (op, expected, nlocals);
      VM_ASSERT (FRAME_LOCALS_COUNT () == expected,
                 vm_error_wrong_num_args (SCM_FRAME_PROGRAM (fp)));
      ALLOC_FRAME (expected + nlocals);
      while (nlocals--)
        LOCAL_SET (expected + nlocals, SCM_UNDEFINED);

      NEXT (1);
    }

  /* bind-kwargs nreq:24 allow-other-keys:1 has-rest:1 _:6 nreq-and-opt:24
   * _:8 ntotal:24 kw-offset:32
   *
   * Find the last positional argument, and shuffle all the rest above
   * NTOTAL.  Initialize the intervening locals to SCM_UNDEFINED.  Then
   * load the constant at KW-OFFSET words from the current IP, and use it
   * to bind keyword arguments.  If HAS-REST, collect all shuffled
   * arguments into a list, and store it in NREQ-AND-OPT.  Finally, clear
   * the arguments that we shuffled up.
   *
   * A macro-mega-instruction.
   */
  VM_DEFINE_OP (22, bind_kwargs, "bind-kwargs", OP4 (U8_U24, U8_U24, X8_U24, N32))
    {
      scm_t_uint32 nreq, nreq_and_opt, ntotal, npositional, nkw, n, nargs;
      scm_t_int32 kw_offset;
      scm_t_bits kw_bits;
      SCM kw;
      char allow_other_keys, has_rest;

      SCM_UNPACK_RTL_24 (op, nreq);
      allow_other_keys = ip[1] & 0x1;
      has_rest = ip[1] & 0x2;
      SCM_UNPACK_RTL_24 (ip[1], nreq_and_opt);
      SCM_UNPACK_RTL_24 (ip[2], ntotal);
      kw_offset = ip[3];
      kw_bits = (scm_t_bits) (ip + kw_offset);
      VM_ASSERT (!(kw_bits & 0x7), abort());
      kw = SCM_PACK (kw_bits);

      nargs = FRAME_LOCALS_COUNT ();

      /* look in optionals for first keyword or last positional */
      /* starting after the last required positional arg */
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
      ALLOC_FRAME (ntotal + nkw);
      n = nkw;
      while (n--)
        LOCAL_SET (ntotal + n, LOCAL_REF (npositional + n));
      /* and fill optionals & keyword args with SCM_UNDEFINED */
      n = npositional;
      while (n < ntotal)
        LOCAL_SET (n++, SCM_UNDEFINED);

      VM_ASSERT (has_rest || (nkw % 2) == 0,
                 vm_error_kwargs_length_not_even (SCM_FRAME_PROGRAM (fp)));

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
                       vm_error_kwargs_unrecognized_keyword (SCM_FRAME_PROGRAM (fp)));
            n++;
          }
        else
          VM_ASSERT (has_rest, vm_error_kwargs_invalid_keyword (SCM_FRAME_PROGRAM (fp)));

      if (has_rest)
        {
          SCM rest = SCM_EOL;
          n = nkw;
          while (n--)
            rest = scm_cons (LOCAL_REF (ntotal + n), rest);
          LOCAL_SET (nreq_and_opt, rest);
        }

      RESET_FRAME (ntotal);

      NEXT (4);
    }

  /* bind-rest dst:24
   *
   * Collect any arguments at or above DST into a list, and store that
   * list at DST.
   */
  VM_DEFINE_OP (23, bind_rest, "bind-rest", OP1 (U8_U24) | OP_DST)
    {
      scm_t_uint32 dst, nargs;
      SCM rest = SCM_EOL;

      SCM_UNPACK_RTL_24 (op, dst);
      nargs = FRAME_LOCALS_COUNT ();

      while (nargs-- > dst)
        {
          rest = scm_cons (LOCAL_REF (nargs), rest);
          LOCAL_SET (nargs, SCM_UNDEFINED);
        }

      LOCAL_SET (dst, rest);

      RESET_FRAME (dst + 1);

      NEXT (1);
    }

  /* drop-values nlocals:24
   *
   * Reset the stack pointer to only have space for NLOCALS values.
   * Used after extracting values from an MV return.
   */
  VM_DEFINE_OP (24, drop_values, "drop-values", OP1 (U8_U24))
    {
      scm_t_bits nlocals;

      SCM_UNPACK_RTL_24 (op, nlocals);

      RESET_FRAME (nlocals);

      NEXT (1);
    }


  

  /*
   * Branching instructions
   */

  /* br offset:24
   *
   * Add OFFSET, a signed 24-bit number, to the current instruction
   * pointer.
   */
  VM_DEFINE_OP (25, br, "br", OP1 (U8_L24))
    {
      scm_t_int32 offset = op;
      offset >>= 8; /* Sign-extending shift. */
      NEXT (offset);
    }

  /* br-if-true test:24 invert:1 _:7 offset:24
   *
   * If the value in TEST is true for the purposes of Scheme, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (26, br_if_true, "br-if-true", OP2 (U8_U24, B1_X7_L24))
    {
      BR_UNARY (x, scm_is_true (x));
    }

  /* br-if-null test:24 invert:1 _:7 offset:24
   *
   * If the value in TEST is the end-of-list or Lisp nil, add OFFSET, a
   * signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (27, br_if_null, "br-if-null", OP2 (U8_U24, B1_X7_L24))
    {
      BR_UNARY (x, scm_is_null (x));
    }

  /* br-if-nil test:24 invert:1 _:7 offset:24
   *
   * If the value in TEST is false to Lisp, add OFFSET, a signed 24-bit
   * number, to the current instruction pointer.
   */
  VM_DEFINE_OP (28, br_if_nil, "br-if-nil", OP2 (U8_U24, B1_X7_L24))
    {
      BR_UNARY (x, scm_is_lisp_false (x));
    }

  /* br-if-pair test:24 invert:1 _:7 offset:24
   *
   * If the value in TEST is a pair, add OFFSET, a signed 24-bit number,
   * to the current instruction pointer.
   */
  VM_DEFINE_OP (29, br_if_pair, "br-if-pair", OP2 (U8_U24, B1_X7_L24))
    {
      BR_UNARY (x, scm_is_pair (x));
    }

  /* br-if-struct test:24 invert:1 _:7 offset:24
   *
   * If the value in TEST is a struct, add OFFSET, a signed 24-bit
   * number, to the current instruction pointer.
   */
  VM_DEFINE_OP (30, br_if_struct, "br-if-struct", OP2 (U8_U24, B1_X7_L24))
    {
      BR_UNARY (x, SCM_STRUCTP (x));
    }

  /* br-if-char test:24 invert:1 _:7 offset:24
   *
   * If the value in TEST is a char, add OFFSET, a signed 24-bit number,
   * to the current instruction pointer.
   */
  VM_DEFINE_OP (31, br_if_char, "br-if-char", OP2 (U8_U24, B1_X7_L24))
    {
      BR_UNARY (x, SCM_CHARP (x));
    }

  /* br-if-tc7 test:24 invert:1 tc7:7 offset:24
   *
   * If the value in TEST has the TC7 given in the second word, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (32, br_if_tc7, "br-if-tc7", OP2 (U8_U24, B1_U7_L24))
    {
      BR_UNARY (x, SCM_HAS_TYP7 (x, (ip[1] >> 1) & 0x7f));
    }

  /* br-if-eq a:12 b:12 invert:1 _:7 offset:24
   *
   * If the value in A is eq? to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (33, br_if_eq, "br-if-eq", OP2 (U8_U12_U12, B1_X7_L24))
    {
      BR_BINARY (x, y, scm_is_eq (x, y));
    }

  /* br-if-eqv a:12 b:12 invert:1 _:7 offset:24
   *
   * If the value in A is eqv? to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (34, br_if_eqv, "br-if-eqv", OP2 (U8_U12_U12, B1_X7_L24))
    {
      BR_BINARY (x, y,
                 scm_is_eq (x, y)
                 || (SCM_NIMP (x) && SCM_NIMP (y)
                     && scm_is_true (scm_eqv_p (x, y))));
    }

  /* br-if-equal a:12 b:12 invert:1 _:7 offset:24
   *
   * If the value in A is equal? to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  // FIXME: should sync_ip before calling out?
  VM_DEFINE_OP (35, br_if_equal, "br-if-equal", OP2 (U8_U12_U12, B1_X7_L24))
    {
      BR_BINARY (x, y,
                 scm_is_eq (x, y)
                 || (SCM_NIMP (x) && SCM_NIMP (y)
                     && scm_is_true (scm_equal_p (x, y))));
    }

  /* br-if-= a:12 b:12 _:8 offset:24
   *
   * If the value in A is = to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (36, br_if_ee, "br-if-=", OP2 (U8_U12_U12, X8_L24))
    {
      BR_ARITHMETIC (==, scm_num_eq_p);
    }

  /* br-if-< a:12 b:12 _:8 offset:24
   *
   * If the value in A is < to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (37, br_if_lt, "br-if-<", OP2 (U8_U12_U12, X8_L24))
    {
      BR_ARITHMETIC (<, scm_less_p);
    }

  /* br-if-<= a:12 b:12 _:8 offset:24
   *
   * If the value in A is <= to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (38, br_if_le, "br-if-<=", OP2 (U8_U12_U12, X8_L24))
    {
      BR_ARITHMETIC (<=, scm_leq_p);
    }

  /* br-if-> a:12 b:12 _:8 offset:24
   *
   * If the value in A is > to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (39, br_if_gt, "br-if->", OP2 (U8_U12_U12, X8_L24))
    {
      BR_ARITHMETIC (>, scm_gr_p);
    }

  /* br-if->= a:12 b:12 _:8 offset:24
   *
   * If the value in A is >= to the value in B, add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (40, br_if_ge, "br-if->=", OP2 (U8_U12_U12, X8_L24))
    {
      BR_ARITHMETIC (>=, scm_geq_p);
    }


  

  /*
   * Lexical binding instructions
   */

  /* mov dst:12 src:12
   *
   * Copy a value from one local slot to another.
   */
  VM_DEFINE_OP (41, mov, "mov", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst;
      scm_t_uint16 src;

      SCM_UNPACK_RTL_12_12 (op, dst, src);
      LOCAL_SET (dst, LOCAL_REF (src));

      NEXT (1);
    }

  /* long-mov dst:24 _:8 src:24
   *
   * Copy a value from one local slot to another.
   */
  VM_DEFINE_OP (42, long_mov, "long-mov", OP2 (U8_U24, X8_U24) | OP_DST)
    {
      scm_t_uint32 dst;
      scm_t_uint32 src;

      SCM_UNPACK_RTL_24 (op, dst);
      SCM_UNPACK_RTL_24 (ip[1], src);
      LOCAL_SET (dst, LOCAL_REF (src));

      NEXT (2);
    }

  /* box dst:12 src:12
   *
   * Create a new variable holding SRC, and place it in DST.
   */
  VM_DEFINE_OP (43, box, "box", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;
      SCM_UNPACK_RTL_12_12 (op, dst, src);
      LOCAL_SET (dst, scm_cell (scm_tc7_variable, SCM_UNPACK (LOCAL_REF (src))));
      NEXT (1);
    }

  /* empty-box dst:24
   *
   * Create a new unbound variable, and place it in DST.  Used in the
   * general implementation of `letrec', in those cases that fix-letrec
   * fails to fix.
   */
  VM_DEFINE_OP (44, empty_box, "empty-box", OP1 (U8_U24) | OP_DST)
    {
      scm_t_uint32 dst;
      SCM_UNPACK_RTL_24 (op, dst);
      LOCAL_SET (dst, scm_cell (scm_tc7_variable, SCM_UNPACK (SCM_UNDEFINED)));
      NEXT (1);
    }

  /* box-ref dst:12 src:12
   *
   * Unpack the variable at SRC into DST, asserting that the variable is
   * actually bound.
   */
  VM_DEFINE_OP (45, box_ref, "box-ref", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;
      SCM var;
      SCM_UNPACK_RTL_12_12 (op, dst, src);
      var = LOCAL_REF (src);
      VM_ASSERT (SCM_VARIABLEP (var), abort ());
      if (SCM_UNLIKELY (!VARIABLE_BOUNDP (var)))
        {
          SCM var_name;
          /* Attempt to provide the variable name in the error message.  */
          SYNC_IP ();
          var_name = scm_module_reverse_lookup (scm_current_module (), var);
          vm_error_unbound (SCM_FRAME_PROGRAM (fp), scm_is_true (var_name) ? var_name : var);
        }
      LOCAL_SET (dst, VARIABLE_REF (var));
      NEXT (1);
    }

  /* box-set! dst:12 src:12
   *
   * Set the contents of the variable at DST to SET.
   */
  VM_DEFINE_OP (46, box_set, "box-set!", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;
      SCM var;
      SCM_UNPACK_RTL_12_12 (op, dst, src);
      var = LOCAL_REF (dst);
      VM_ASSERT (SCM_VARIABLEP (var), abort ());
      VARIABLE_SET (var, LOCAL_REF (src));
      NEXT (1);
    }

  /* free-ref dst:12 src:12
   *
   * Load free variable SRC into local slot DST.
   */
  VM_DEFINE_OP (47, free_ref, "free-ref", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;
      SCM_UNPACK_RTL_12_12 (op, dst, src);
      CHECK_FREE_VARIABLE (src);
      LOCAL_SET (dst, FREE_VARIABLE_REF (src));
      NEXT (1);
    }

  /* make-closure dst:24 offset:32 _:8 nfree:24 free0:24 0:8 ...
   *
   * Make a new closure, and write it to DST.  The code for the closure
   * will be found at OFFSET words from the current IP.  OFFSET is a
   * signed 32-bit integer.  The registers for the NFREE free variables
   * follow.
   */
  VM_DEFINE_OP (48, make_closure, "make-closure", OP3 (U8_U24, L32, X8_R24) | OP_DST)
    {
      scm_t_uint32 dst, nfree, n;
      scm_t_int32 offset;
      SCM closure;

      SCM_UNPACK_RTL_24 (op, dst);
      offset = ip[1];
      SCM_UNPACK_RTL_24 (ip[2], nfree);

      // FIXME: Assert range of nfree?
      closure = scm_words (scm_tc7_rtl_program | (nfree << 16), nfree + 2);
      SCM_SET_CELL_WORD_1 (closure, ip + offset);
      for (n = 0; n < nfree; n++)
        SCM_RTL_PROGRAM_FREE_VARIABLE_SET (closure, n, LOCAL_REF (ip[n + 3]));
      LOCAL_SET (dst, closure);
      NEXT (nfree + 3);
    }

  /* fix-closure dst:24 _:8 nfree:24 free0:24 0:8 ...
   *
   * "Fix" a closure.  This is used for lambda expressions bound in a
   * <fix>, but which are not always called in tail position.  In that
   * case we allocate the closures first, then destructively update their
   * free variables to point to each other.  NFREE and the locals FREE0...
   * are as in make-closure.
   */
  VM_DEFINE_OP (49, fix_closure, "fix-closure", OP2 (U8_U24, X8_R24))
    {
      scm_t_uint32 dst, nfree, n;
      SCM closure;

      SCM_UNPACK_RTL_24 (op, dst);
      SCM_UNPACK_RTL_24 (ip[1], nfree);
      closure = LOCAL_REF (dst);
      for (n = 0; n < nfree; n++)
        SCM_RTL_PROGRAM_FREE_VARIABLE_SET (closure, n, LOCAL_REF (ip[n + 2]));
      NEXT (nfree + 2);
    }


  

  /*
   * Immediates and statically allocated non-immediates
   */

  /* make-short-immediate dst:8 low-bits:16
   *
   * Make an immediate whose low bits are LOW-BITS, and whose top bits are
   * 0.
   */
  VM_DEFINE_OP (50, make_short_immediate, "make-short-immediate", OP1 (U8_U8_I16) | OP_DST)
    {
      scm_t_uint8 dst;
      scm_t_bits val;

      SCM_UNPACK_RTL_8_16 (op, dst, val);
      LOCAL_SET (dst, SCM_PACK (val));
      NEXT (1);
    }

  /* make-long-immediate dst:24 low-bits:32
   *
   * Make an immediate whose low bits are LOW-BITS, and whose top bits are
   * 0.
   */
  VM_DEFINE_OP (51, make_long_immediate, "make-long-immediate", OP2 (U8_U24, I32))
    {
      scm_t_uint8 dst;
      scm_t_bits val;

      SCM_UNPACK_RTL_24 (op, dst);
      val = ip[1];
      LOCAL_SET (dst, SCM_PACK (val));
      NEXT (2);
    }

  /* make-long-long-immediate dst:24 high-bits:32 low-bits:32
   *
   * Make an immediate with HIGH-BITS and LOW-BITS.
   */
  VM_DEFINE_OP (52, make_long_long_immediate, "make-long-long-immediate", OP3 (U8_U24, A32, B32) | OP_DST)
    {
      scm_t_uint8 dst;
      scm_t_bits val;

      SCM_UNPACK_RTL_24 (op, dst);
#if SIZEOF_SCM_T_BITS > 4
      val = ip[1];
      val <<= 32;
      val |= ip[2];
#else
      ASSERT (ip[1] == 0);
      val = ip[2];
#endif
      LOCAL_SET (dst, SCM_PACK (val));
      NEXT (3);
    }

  /* make-non-immediate dst:24 offset:32
   *
   * Load a pointer to statically allocated memory into DST.  The
   * object's memory is will be found OFFSET 32-bit words away from the
   * current instruction pointer.  OFFSET is a signed value.  The
   * intention here is that the compiler would produce an object file
   * containing the words of a non-immediate object, and this
   * instruction creates a pointer to that memory, effectively
   * resurrecting that object.
   *
   * Whether the object is mutable or immutable depends on where it was
   * allocated by the compiler, and loaded by the loader.
   */
  VM_DEFINE_OP (53, make_non_immediate, "make-non-immediate", OP2 (U8_U24, N32) | OP_DST)
    {
      scm_t_uint32 dst;
      scm_t_int32 offset;
      scm_t_uint32* loc;
      scm_t_bits unpacked;

      SCM_UNPACK_RTL_24 (op, dst);
      offset = ip[1];
      loc = ip + offset;
      unpacked = (scm_t_bits) loc;

      VM_ASSERT (!(unpacked & 0x7), abort());

      LOCAL_SET (dst, SCM_PACK (unpacked));

      NEXT (2);
    }

  /* static-ref dst:24 offset:32
   *
   * Load a SCM value into DST.  The SCM value will be fetched from
   * memory, OFFSET 32-bit words away from the current instruction
   * pointer.  OFFSET is a signed value.
   *
   * The intention is for this instruction to be used to load constants
   * that the compiler is unable to statically allocate, like symbols.
   * These values would be initialized when the object file loads.
   */
  VM_DEFINE_OP (54, static_ref, "static-ref", OP2 (U8_U24, S32))
    {
      scm_t_uint32 dst;
      scm_t_int32 offset;
      scm_t_uint32* loc;
      scm_t_uintptr loc_bits;

      SCM_UNPACK_RTL_24 (op, dst);
      offset = ip[1];
      loc = ip + offset;
      loc_bits = (scm_t_uintptr) loc;
      VM_ASSERT (ALIGNED_P (loc, SCM), abort());

      LOCAL_SET (dst, *((SCM *) loc_bits));

      NEXT (2);
    }

  /* static-set! src:24 offset:32
   *
   * Store a SCM value into memory, OFFSET 32-bit words away from the
   * current instruction pointer.  OFFSET is a signed value.
   */
  VM_DEFINE_OP (55, static_set, "static-set!", OP2 (U8_U24, LO32))
    {
      scm_t_uint32 src;
      scm_t_int32 offset;
      scm_t_uint32* loc;

      SCM_UNPACK_RTL_24 (op, src);
      offset = ip[1];
      loc = ip + offset;
      VM_ASSERT (ALIGNED_P (loc, SCM), abort());

      *((SCM *) loc) = LOCAL_REF (src);

      NEXT (2);
    }

  /* link-procedure! src:24 offset:32
   *
   * Set the code pointer of the procedure in SRC to point OFFSET 32-bit
   * words away from the current instruction pointer.  OFFSET is a
   * signed value.
   */
  VM_DEFINE_OP (56, link_procedure, "link-procedure!", OP2 (U8_U24, L32))
    {
      scm_t_uint32 src;
      scm_t_int32 offset;
      scm_t_uint32* loc;

      SCM_UNPACK_RTL_24 (op, src);
      offset = ip[1];
      loc = ip + offset;

      SCM_SET_CELL_WORD_1 (LOCAL_REF (src), (scm_t_bits) loc);

      NEXT (2);
    }

  

  /*
   * Mutable top-level bindings
   */

  /* There are three slightly different ways to resolve toplevel
     variables.

     1. A toplevel reference outside of a function.  These need to be
        looked up when the expression is evaluated -- no later, and no
        before.  They are looked up relative to the module that is
        current when the expression is evaluated.  For example:

          (if (foo) a b)

        The "resolve" instruction resolves the variable (box), and then
        access is via box-ref or box-set!.

     2. A toplevel reference inside a function.  These are looked up
        relative to the module that was current when the function was
        defined.  Unlike code at the toplevel, which is usually run only
        once, these bindings benefit from memoized lookup, in which the
        variable resulting from the lookup is cached in the function.

          (lambda () (if (foo) a b))

        Although one can use resolve and box-ref, the toplevel-ref and
        toplevel-set! instructions are better for references.

     3. A reference to an identifier with respect to a particular
        module.  This can happen for primitive references, and
        references residualized by macro expansions.  These can be
        cached or not, depending on whether they are in a lambda or not.

          (@ (foo bar) a)
          (@@ (foo bar) a)

        For these, one can use resolve-module, resolve, and the box
        interface, though there is also module-ref as a shortcut.
     */

  /* current-module dst:24
   *
   * Store the current module in DST.
   */
  VM_DEFINE_OP (57, current_module, "current-module", OP1 (U8_U24) | OP_DST)
    {
      scm_t_uint32 dst;

      SCM_UNPACK_RTL_24 (op, dst);

      SYNC_IP ();
      LOCAL_SET (dst, scm_current_module ());

      NEXT (1);
    }

  /* resolve dst:8 mod:8 sym:8
   *
   * Resolve SYM in MOD, and place the resulting variable in DST.
   */
  VM_DEFINE_OP (58, resolve, "resolve", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      scm_t_uint8 dst, mod, sym;

      SCM_UNPACK_RTL_8_8_8 (op, dst, mod, sym);

      SYNC_IP ();
      LOCAL_SET (dst, scm_module_lookup (LOCAL_REF (mod), LOCAL_REF (sym)));

      NEXT (1);
    }

  /* resolve-module dst:8 name:8 public:8
   *
   * Resolve a module with name NAME, placing it in DST.  If PUBLIC is
   * nonzero, resolve the public interface, otherwise use the private
   * interface.
   */
  VM_DEFINE_OP (59, resolve_module, "resolve-module", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      scm_t_uint8 dst, name, public;
      SCM mod;

      SCM_UNPACK_RTL_8_8_8 (op, dst, name, public);

      SYNC_IP ();
      mod = scm_resolve_module (LOCAL_REF (name));
      if (public)
        mod = scm_module_public_interface (mod);
      LOCAL_SET (dst, mod);

      NEXT (1);
    }

  /* define sym:12 val:12
   *
   * Look up a binding for SYM in the current module, creating it if
   * necessary.  Set its value to VAL.
   */
  VM_DEFINE_OP (60, define, "define", OP1 (U8_U12_U12))
    {
      scm_t_uint16 sym, val;
      SCM_UNPACK_RTL_12_12 (op, sym, val);
      SYNC_IP ();
      scm_define (LOCAL_REF (sym), LOCAL_REF (val));
      NEXT (1);
    }

  /* toplevel-ref dst:24 var-offset:32 mod-offset:32 sym-offset:32
   *
   * Load a SCM value.  The SCM value will be fetched from memory,
   * VAR-OFFSET 32-bit words away from the current instruction pointer.
   * VAR-OFFSET is a signed value.  Up to here, toplevel-ref is like
   * static-ref.
   *
   * Then, if the loaded value is a variable, the value of the variable
   * is placed in DST, and control flow continues.
   *
   * Otherwise, we have to resolve the variable.  In that case we load
   * the module from MOD-OFFSET, just as we loaded the variable.
   * Usually the module gets set when the closure is created.  The name
   * is an offset to a symbol.
   *
   * We use the module and the string to resolve the variable, raising
   * an error if it is unbound, unbox it into DST, and cache the
   * resolved variable so that we will hit the cache next time.
   */
  VM_DEFINE_OP (61, toplevel_ref, "toplevel-ref", OP4 (U8_U24, S32, S32, N32) | OP_DST)
    {
      scm_t_uint32 dst;
      scm_t_int32 var_offset;
      scm_t_uint32* var_loc_u32;
      SCM *var_loc;
      SCM var;

      SCM_UNPACK_RTL_24 (op, dst);
      var_offset = ip[1];
      var_loc_u32 = ip + var_offset;
      VM_ASSERT (ALIGNED_P (var_loc_u32, SCM), abort());
      var_loc = (SCM *) var_loc_u32;
      var = *var_loc;

      if (SCM_UNLIKELY (!SCM_VARIABLEP (var)))
        {
          SCM mod, sym;
          scm_t_int32 mod_offset = ip[2]; /* signed */
          scm_t_int32 sym_offset = ip[3]; /* signed */
          scm_t_uint32 *mod_loc = ip + mod_offset;
          scm_t_uint32 *sym_loc = ip + sym_offset;
          
          SYNC_IP ();

          VM_ASSERT (ALIGNED_P (mod_loc, SCM), abort());
          VM_ASSERT (ALIGNED_P (sym_loc, SCM), abort());

          mod = *((SCM *) mod_loc);
          sym = *((SCM *) sym_loc);

          var = scm_module_lookup (mod, sym);
          VM_ASSERT (VARIABLE_BOUNDP (var), vm_error_unbound (fp[-1], sym));

          *var_loc = var;
        }

      LOCAL_SET (dst, VARIABLE_REF (var));
      NEXT (4);
    }

  /* toplevel-set! src:24 var-offset:32 mod-offset:32 sym-offset:32
   *
   * Set a top-level variable from a variable cache cell.  The variable
   * is resolved as in toplevel-ref.
   */
  VM_DEFINE_OP (62, toplevel_set, "toplevel-set!", OP4 (U8_U24, S32, S32, N32))
    {
      scm_t_uint32 src;
      scm_t_int32 var_offset;
      scm_t_uint32* var_loc_u32;
      SCM *var_loc;
      SCM var;

      SCM_UNPACK_RTL_24 (op, src);
      var_offset = ip[1];
      var_loc_u32 = ip + var_offset;
      VM_ASSERT (ALIGNED_P (var_loc_u32, SCM), abort());
      var_loc = (SCM *) var_loc_u32;
      var = *var_loc;

      if (SCM_UNLIKELY (!SCM_VARIABLEP (var)))
        {
          SCM mod, sym;
          scm_t_int32 mod_offset = ip[2]; /* signed */
          scm_t_int32 sym_offset = ip[3]; /* signed */
          scm_t_uint32 *mod_loc = ip + mod_offset;
          scm_t_uint32 *sym_loc = ip + sym_offset;
          
          SYNC_IP ();

          VM_ASSERT (ALIGNED_P (mod_loc, SCM), abort());
          VM_ASSERT (ALIGNED_P (sym_loc, SCM), abort());

          mod = *((SCM *) mod_loc);
          sym = *((SCM *) sym_loc);

          var = scm_module_lookup (mod, sym);

          *var_loc = var;
        }

      VARIABLE_SET (var, LOCAL_REF (src));
      NEXT (4);
    }

  /* module-ref dst:24 var-offset:32 mod-offset:32 sym-offset:32
   *
   * Like toplevel-ref, except MOD-OFFSET points at the name of a module
   * instead of the module itself.
   */
  VM_DEFINE_OP (63, module_ref, "module-ref", OP4 (U8_U24, S32, N32, N32) | OP_DST)
    {
      scm_t_uint32 dst;
      scm_t_int32 var_offset;
      scm_t_uint32* var_loc_u32;
      SCM *var_loc;
      SCM var;

      SCM_UNPACK_RTL_24 (op, dst);
      var_offset = ip[1];
      var_loc_u32 = ip + var_offset;
      VM_ASSERT (ALIGNED_P (var_loc_u32, SCM), abort());
      var_loc = (SCM *) var_loc_u32;
      var = *var_loc;

      if (SCM_UNLIKELY (!SCM_VARIABLEP (var)))
        {
          SCM modname, sym;
          scm_t_int32 modname_offset = ip[2]; /* signed */
          scm_t_int32 sym_offset = ip[3]; /* signed */
          scm_t_uint32 *modname_words = ip + modname_offset;
          scm_t_uint32 *sym_loc = ip + sym_offset;

          SYNC_IP ();

          VM_ASSERT (!(((scm_t_uintptr) modname_words) & 0x7), abort());
          VM_ASSERT (ALIGNED_P (sym_loc, SCM), abort());

          modname = SCM_PACK ((scm_t_bits) modname_words);
          sym = *((SCM *) sym_loc);

          if (scm_is_true (SCM_CAR (modname)))
            var = scm_public_lookup (SCM_CDR (modname), sym);
          else
            var = scm_private_lookup (SCM_CDR (modname), sym);

          VM_ASSERT (VARIABLE_BOUNDP (var), vm_error_unbound (fp[-1], sym));

          *var_loc = var;
        }

      LOCAL_SET (dst, VARIABLE_REF (var));
      NEXT (4);
    }

  /* module-set! src:24 var-offset:32 mod-offset:32 sym-offset:32
   *
   * Like toplevel-set!, except MOD-OFFSET points at the name of a module
   * instead of the module itself.
   */
  VM_DEFINE_OP (64, module_set, "module-set!", OP4 (U8_U24, S32, N32, N32))
    {
      scm_t_uint32 src;
      scm_t_int32 var_offset;
      scm_t_uint32* var_loc_u32;
      SCM *var_loc;
      SCM var;

      SCM_UNPACK_RTL_24 (op, src);
      var_offset = ip[1];
      var_loc_u32 = ip + var_offset;
      VM_ASSERT (ALIGNED_P (var_loc_u32, SCM), abort());
      var_loc = (SCM *) var_loc_u32;
      var = *var_loc;

      if (SCM_UNLIKELY (!SCM_VARIABLEP (var)))
        {
          SCM modname, sym;
          scm_t_int32 modname_offset = ip[2]; /* signed */
          scm_t_int32 sym_offset = ip[3]; /* signed */
          scm_t_uint32 *modname_words = ip + modname_offset;
          scm_t_uint32 *sym_loc = ip + sym_offset;

          SYNC_IP ();

          VM_ASSERT (!(((scm_t_uintptr) modname_words) & 0x7), abort());
          VM_ASSERT (ALIGNED_P (sym_loc, SCM), abort());

          modname = SCM_PACK ((scm_t_bits) modname_words);
          sym = *((SCM *) sym_loc);

          if (scm_is_true (SCM_CAR (modname)))
            var = scm_public_lookup (SCM_CDR (modname), sym);
          else
            var = scm_private_lookup (SCM_CDR (modname), sym);

          *var_loc = var;
        }

      VARIABLE_SET (var, LOCAL_REF (src));
      NEXT (4);
    }

  

  /*
   * The dynamic environment
   */

  /* prompt tag:24 flags:8 handler-offset:24
   *
   * Push a new prompt on the dynamic stack, with a tag from TAG and a
   * handler at HANDLER-OFFSET words from the current IP.  The handler
   * will expect a multiple-value return.
   */
  VM_DEFINE_OP (65, prompt, "prompt", OP2 (U8_U24, U8_L24))
#if 0
    {
      scm_t_uint32 tag;
      scm_t_int32 offset;
      scm_t_uint8 escape_only_p;
      scm_t_dynstack_prompt_flags flags;

      SCM_UNPACK_RTL_24 (op, tag);
      escape_only_p = ip[1] & 0xff;
      offset = ip[1];
      offset >>= 8; /* Sign extension */
  
      /* Push the prompt onto the dynamic stack. */
      flags = escape_only_p ? SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY : 0;
      scm_dynstack_push_prompt (&current_thread->dynstack, flags,
                                LOCAL_REF (tag),
                                fp, vp->sp, ip + offset, &registers);
      NEXT (2);
    }
#else
  abort();
#endif

  /* wind winder:12 unwinder:12
   *
   * Push wind and unwind procedures onto the dynamic stack. Note that
   * neither are actually called; the compiler should emit calls to wind
   * and unwind for the normal dynamic-wind control flow.  Also note that
   * the compiler should have inserted checks that they wind and unwind
   * procs are thunks, if it could not prove that to be the case.
   */
  VM_DEFINE_OP (66, wind, "wind", OP1 (U8_U12_U12))
    {
      scm_t_uint16 winder, unwinder;
      SCM_UNPACK_RTL_12_12 (op, winder, unwinder);
      scm_dynstack_push_dynwind (&current_thread->dynstack,
                                 LOCAL_REF (winder), LOCAL_REF (unwinder));
      NEXT (1);
    }

  /* abort tag:24 _:8 nvalues:24 val0:24 0:8 val1:24 0:8 ...
   *
   * Return a number of values to a prompt handler.  The values VAL0,
   * VAL1, etc are 24-bit values, in the lower 24 bits of their words.
   * The upper 8 bits are 0.
   */
  VM_DEFINE_OP (67, abort, "abort", OP2 (U8_U24, X8_R24))
#if 0
    {
      scm_t_uint32 tag, nvalues;

      SCM_UNPACK_RTL_24 (op, tag);
      SCM_UNPACK_RTL_24 (ip[1], nvalues);

      SYNC_IP ();
      vm_abort (vm, LOCAL_REF (tag), nvalues, &ip[2], &registers);

      /* vm_abort should not return */
      abort ();
    }
#else
  abort();
#endif

  /* unwind _:24
   *
   * A normal exit from the dynamic extent of an expression. Pop the top
   * entry off of the dynamic stack.
   */
  VM_DEFINE_OP (68, unwind, "unwind", OP1 (U8_X24))
    {
      scm_dynstack_pop (&current_thread->dynstack);
      NEXT (1);
    }

  /* wind-fluids fluid-base:24 _:8 n:24 value0:24 0:8 ...
   *
   * Dynamically bind N fluids to values.  The fluids are expected to be
   * allocated in a continguous range on the stack, starting from
   * FLUID-BASE.  The values do not have this restriction.
   */
  VM_DEFINE_OP (69, wind_fluids, "wind-fluids", OP2 (U8_U24, X8_R24))
#if 0
    {
      scm_t_uint32 fluid_base, n;

      SCM_UNPACK_RTL_24 (op, fluid_base);
      SCM_UNPACK_RTL_24 (ip[1], n);

      scm_dynstack_push_fluids_shuffled (&current_thread->dynstack, n,
                                         &fp[fluid_base], fp, &ip[2],
                                         current_thread->dynamic_state);
      NEXT (n + 2);
    }
#else
  abort();
#endif

  /* unwind-fluids _:24
   *
   * Leave the dynamic extent of a with-fluids expression, restoring the
   * fluids to their previous values.
   */
  VM_DEFINE_OP (70, unwind_fluids, "unwind-fluids", OP1 (U8_X24))
    {
      /* This function must not allocate.  */
      scm_dynstack_unwind_fluids (&current_thread->dynstack,
                                  current_thread->dynamic_state);
      NEXT (1);
    }

  /* fluid-ref dst:12 src:12
   *
   * Reference the fluid in SRC, and place the value in DST.
   */
  VM_DEFINE_OP (71, fluid_ref, "fluid-ref", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;
      size_t num;
      SCM fluid, fluids;

      SCM_UNPACK_RTL_12_12 (op, dst, src);
      fluid = LOCAL_REF (src);
      fluids = SCM_I_DYNAMIC_STATE_FLUIDS (current_thread->dynamic_state);
      if (SCM_UNLIKELY (!SCM_FLUID_P (fluid))
          || ((num = SCM_I_FLUID_NUM (fluid)) >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
        {
          /* Punt dynstate expansion and error handling to the C proc. */
          SYNC_IP ();
          LOCAL_SET (dst, scm_fluid_ref (fluid));
        }
      else
        {
          SCM val = SCM_SIMPLE_VECTOR_REF (fluids, num);
          if (scm_is_eq (val, SCM_UNDEFINED))
            val = SCM_I_FLUID_DEFAULT (fluid);
          VM_ASSERT (!scm_is_eq (val, SCM_UNDEFINED),
                     vm_error_unbound_fluid (program, fluid));
          LOCAL_SET (dst, val);
        }

      NEXT (1);
    }

  /* fluid-set fluid:12 val:12
   *
   * Set the value of the fluid in DST to the value in SRC.
   */
  VM_DEFINE_OP (72, fluid_set, "fluid-set", OP1 (U8_U12_U12))
    {
      scm_t_uint16 a, b;
      size_t num;
      SCM fluid, fluids;

      SCM_UNPACK_RTL_12_12 (op, a, b);
      fluid = LOCAL_REF (a);
      fluids = SCM_I_DYNAMIC_STATE_FLUIDS (current_thread->dynamic_state);
      if (SCM_UNLIKELY (!SCM_FLUID_P (fluid))
          || ((num = SCM_I_FLUID_NUM (fluid)) >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
        {
          /* Punt dynstate expansion and error handling to the C proc. */
          SYNC_IP ();
          scm_fluid_set_x (fluid, LOCAL_REF (b));
        }
      else
        SCM_SIMPLE_VECTOR_SET (fluids, num, LOCAL_REF (b));

      NEXT (1);
    }


  

  /*
   * Strings, symbols, and keywords
   */

  /* string-length dst:12 src:12
   *
   * Store the length of the string in SRC in DST.
   */
  VM_DEFINE_OP (73, string_length, "string-length", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (str);
      if (SCM_LIKELY (scm_is_string (str)))
        RETURN (SCM_I_MAKINUM (scm_i_string_length (str)));
      else
        {
          SYNC_IP ();
          RETURN (scm_string_length (str));
        }
    }

  /* string-ref dst:8 src:8 idx:8
   *
   * Fetch the character at position IDX in the string in SRC, and store
   * it in DST.
   */
  VM_DEFINE_OP (74, string_ref, "string-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      scm_t_signed_bits i = 0;
      ARGS2 (str, idx);
      if (SCM_LIKELY (scm_is_string (str)
                      && SCM_I_INUMP (idx)
                      && ((i = SCM_I_INUM (idx)) >= 0)
                      && i < scm_i_string_length (str)))
        RETURN (SCM_MAKE_CHAR (scm_i_string_ref (str, i)));
      else
        {
          SYNC_IP ();
          RETURN (scm_string_ref (str, idx));
        }
    }

  /* No string-set! instruction, as there is no good fast path there.  */

  /* string-to-number dst:12 src:12
   *
   * Parse a string in SRC to a number, and store in DST.
   */
  VM_DEFINE_OP (75, string_to_number, "string->number", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;

      SCM_UNPACK_RTL_12_12 (op, dst, src);
      SYNC_IP ();
      LOCAL_SET (dst,
                 scm_string_to_number (LOCAL_REF (src),
                                       SCM_UNDEFINED /* radix = 10 */));
      NEXT (1);
    }

  /* string-to-symbol dst:12 src:12
   *
   * Parse a string in SRC to a symbol, and store in DST.
   */
  VM_DEFINE_OP (76, string_to_symbol, "string->symbol", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;

      SCM_UNPACK_RTL_12_12 (op, dst, src);
      SYNC_IP ();
      LOCAL_SET (dst, scm_string_to_symbol (LOCAL_REF (src)));
      NEXT (1);
    }

  /* symbol->keyword dst:12 src:12
   *
   * Make a keyword from the symbol in SRC, and store it in DST.
   */
  VM_DEFINE_OP (77, symbol_to_keyword, "symbol->keyword", OP1 (U8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, src;
      SCM_UNPACK_RTL_12_12 (op, dst, src);
      SYNC_IP ();
      LOCAL_SET (dst, scm_symbol_to_keyword (LOCAL_REF (src)));
      NEXT (1);
    }

  

  /*
   * Pairs
   */

  /* cons dst:8 car:8 cdr:8
   *
   * Cons CAR and CDR, and store the result in DST.
   */
  VM_DEFINE_OP (78, cons, "cons", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      RETURN (scm_cons (x, y));
    }

  /* car dst:12 src:12
   *
   * Place the car of SRC in DST.
   */
  VM_DEFINE_OP (79, car, "car", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (x);
      VM_VALIDATE_PAIR (x, "car");
      RETURN (SCM_CAR (x));
    }

  /* cdr dst:12 src:12
   *
   * Place the cdr of SRC in DST.
   */
  VM_DEFINE_OP (80, cdr, "cdr", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (x);
      VM_VALIDATE_PAIR (x, "cdr");
      RETURN (SCM_CDR (x));
    }

  /* set-car! pair:12 car:12
   *
   * Set the car of DST to SRC.
   */
  VM_DEFINE_OP (81, set_car, "set-car!", OP1 (U8_U12_U12))
    {
      scm_t_uint16 a, b;
      SCM x, y;
      SCM_UNPACK_RTL_12_12 (op, a, b);
      x = LOCAL_REF (a);
      y = LOCAL_REF (b);
      VM_VALIDATE_PAIR (x, "set-car!");
      SCM_SETCAR (x, y);
      NEXT (1);
    }

  /* set-cdr! pair:12 cdr:12
   *
   * Set the cdr of DST to SRC.
   */
  VM_DEFINE_OP (82, set_cdr, "set-cdr!", OP1 (U8_U12_U12))
    {
      scm_t_uint16 a, b;
      SCM x, y;
      SCM_UNPACK_RTL_12_12 (op, a, b);
      x = LOCAL_REF (a);
      y = LOCAL_REF (b);
      VM_VALIDATE_PAIR (x, "set-car!");
      SCM_SETCDR (x, y);
      NEXT (1);
    }


  

  /*
   * Numeric operations
   */

  /* add dst:8 a:8 b:8
   *
   * Add A to B, and place the result in DST.
   */
  VM_DEFINE_OP (83, add, "add", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      BINARY_INTEGER_OP (+, scm_sum);
    }

  /* add1 dst:12 src:12
   *
   * Add 1 to the value in SRC, and place the result in DST.
   */
  VM_DEFINE_OP (84, add1, "add1", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (x);

      /* Check for overflow.  */
      if (SCM_LIKELY ((scm_t_intptr) SCM_UNPACK (x) < INUM_MAX))
        {
          SCM result;

          /* Add the integers without untagging.  */
          result = SCM_PACK ((scm_t_intptr) SCM_UNPACK (x)
                             + (scm_t_intptr) SCM_UNPACK (SCM_I_MAKINUM (1))
                             - scm_tc2_int);

          if (SCM_LIKELY (SCM_I_INUMP (result)))
            RETURN (result);
        }

      SYNC_IP ();
      RETURN (scm_sum (x, SCM_I_MAKINUM (1)));
    }

  /* sub dst:8 a:8 b:8
   *
   * Subtract B from A, and place the result in DST.
   */
  VM_DEFINE_OP (85, sub, "sub", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      BINARY_INTEGER_OP (-, scm_difference);
    }

  /* sub1 dst:12 src:12
   *
   * Subtract 1 from SRC, and place the result in DST.
   */
  VM_DEFINE_OP (86, sub1, "sub1", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (x);

      /* Check for underflow.  */
      if (SCM_LIKELY ((scm_t_intptr) SCM_UNPACK (x) > INUM_MIN))
        {
          SCM result;

          /* Substract the integers without untagging.  */
          result = SCM_PACK ((scm_t_intptr) SCM_UNPACK (x)
                             - (scm_t_intptr) SCM_UNPACK (SCM_I_MAKINUM (1))
                             + scm_tc2_int);

          if (SCM_LIKELY (SCM_I_INUMP (result)))
            RETURN (result);
        }

      SYNC_IP ();
      RETURN (scm_difference (x, SCM_I_MAKINUM (1)));
    }

  /* mul dst:8 a:8 b:8
   *
   * Multiply A and B, and place the result in DST.
   */
  VM_DEFINE_OP (87, mul, "mul", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      SYNC_IP ();
      RETURN (scm_product (x, y));
    }

  /* div dst:8 a:8 b:8
   *
   * Divide A by B, and place the result in DST.
   */
  VM_DEFINE_OP (88, div, "div", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      SYNC_IP ();
      RETURN (scm_divide (x, y));
    }

  /* quo dst:8 a:8 b:8
   *
   * Divide A by B, and place the quotient in DST.
   */
  VM_DEFINE_OP (89, quo, "quo", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      SYNC_IP ();
      RETURN (scm_quotient (x, y));
    }

  /* rem dst:8 a:8 b:8
   *
   * Divide A by B, and place the remainder in DST.
   */
  VM_DEFINE_OP (90, rem, "rem", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      SYNC_IP ();
      RETURN (scm_remainder (x, y));
    }

  /* mod dst:8 a:8 b:8
   *
   * Place the modulo of A by B in DST.
   */
  VM_DEFINE_OP (91, mod, "mod", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      SYNC_IP ();
      RETURN (scm_modulo (x, y));
    }

  /* ash dst:8 a:8 b:8
   *
   * Shift A arithmetically by B bits, and place the result in DST.
   */
  VM_DEFINE_OP (92, ash, "ash", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
        {
          if (SCM_I_INUM (y) < 0)
            /* Right shift, will be a fixnum. */
            RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) >> -SCM_I_INUM (y)));
          else
            /* Left shift. See comments in scm_ash. */
            {
              scm_t_signed_bits nn, bits_to_shift;

              nn = SCM_I_INUM (x);
              bits_to_shift = SCM_I_INUM (y);

              if (bits_to_shift < SCM_I_FIXNUM_BIT-1
                  && ((scm_t_bits)
                      (SCM_SRS (nn, (SCM_I_FIXNUM_BIT-1 - bits_to_shift)) + 1)
                      <= 1))
                RETURN (SCM_I_MAKINUM (nn << bits_to_shift));
              /* fall through */
            }
          /* fall through */
        }
      SYNC_IP ();
      RETURN (scm_ash (x, y));
    }

  /* logand dst:8 a:8 b:8
   *
   * Place the bitwise AND of A and B into DST.
   */
  VM_DEFINE_OP (93, logand, "logand", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
        RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) & SCM_I_INUM (y)));
      SYNC_IP ();
      RETURN (scm_logand (x, y));
    }

  /* logior dst:8 a:8 b:8
   *
   * Place the bitwise inclusive OR of A with B in DST.
   */
  VM_DEFINE_OP (94, logior, "logior", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
        RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) | SCM_I_INUM (y)));
      SYNC_IP ();
      RETURN (scm_logior (x, y));
    }

  /* logxor dst:8 a:8 b:8
   *
   * Place the bitwise exclusive OR of A with B in DST.
   */
  VM_DEFINE_OP (95, logxor, "logxor", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (x, y);
      if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
        RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) ^ SCM_I_INUM (y)));
      SYNC_IP ();
      RETURN (scm_logxor (x, y));
    }

  /* vector-length dst:12 src:12
   *
   * Store the length of the vector in SRC in DST.
   */
  VM_DEFINE_OP (96, vector_length, "vector-length", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (vect);
      if (SCM_LIKELY (SCM_I_IS_VECTOR (vect)))
        RETURN (SCM_I_MAKINUM (SCM_I_VECTOR_LENGTH (vect)));
      else
        {
          SYNC_IP ();
          RETURN (scm_vector_length (vect));
        }
    }

  /* vector-ref dst:8 src:8 idx:8
   *
   * Fetch the item at position IDX in the vector in SRC, and store it
   * in DST.
   */
  VM_DEFINE_OP (97, vector_ref, "vector-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      scm_t_signed_bits i = 0;
      ARGS2 (vect, idx);
      if (SCM_LIKELY (SCM_I_IS_NONWEAK_VECTOR (vect)
                      && SCM_I_INUMP (idx)
                      && ((i = SCM_I_INUM (idx)) >= 0)
                      && i < SCM_I_VECTOR_LENGTH (vect)))
        RETURN (SCM_I_VECTOR_ELTS (vect)[i]);
      else
        {
          SYNC_IP ();
          RETURN (scm_vector_ref (vect, idx));
        }
    }

  /* constant-vector-ref dst:8 src:8 idx:8
   *
   * Fill DST with the item IDX elements into the vector at SRC.  Useful
   * for building data types using vectors.
   */
  VM_DEFINE_OP (98, constant_vector_ref, "constant-vector-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      scm_t_uint8 dst, src, idx;
      SCM v;
      
      SCM_UNPACK_RTL_8_8_8 (op, dst, src, idx);
      v = LOCAL_REF (src);
      if (SCM_LIKELY (SCM_I_IS_NONWEAK_VECTOR (v)
                      && idx < SCM_I_VECTOR_LENGTH (v)))
        LOCAL_SET (dst, SCM_I_VECTOR_ELTS (LOCAL_REF (src))[idx]);
      else
        LOCAL_SET (dst, scm_c_vector_ref (v, idx));
      NEXT (1);
    }

  /* vector-set! dst:8 idx:8 src:8
   *
   * Store SRC into the vector DST at index IDX.
   */
  VM_DEFINE_OP (99, vector_set, "vector-set", OP1 (U8_U8_U8_U8))
    {
      scm_t_uint8 dst, idx_var, src;
      SCM vect, idx, val;
      scm_t_signed_bits i = 0;

      SCM_UNPACK_RTL_8_8_8 (op, dst, idx_var, src);
      vect = LOCAL_REF (dst);
      idx = LOCAL_REF (idx_var);
      val = LOCAL_REF (src);

      if (SCM_LIKELY (SCM_I_IS_NONWEAK_VECTOR (vect)
                      && SCM_I_INUMP (idx)
                      && ((i = SCM_I_INUM (idx)) >= 0)
                      && i < SCM_I_VECTOR_LENGTH (vect)))
        SCM_I_VECTOR_WELTS (vect)[i] = val;
      else
        {
          SYNC_IP ();
          scm_vector_set_x (vect, idx, val);
        }
      NEXT (1);
    }


  

  /*
   * Structs and GOOPS
   */

  /* struct-vtable dst:12 src:12
   *
   * Store the vtable of SRC into DST.
   */
  VM_DEFINE_OP (100, struct_vtable, "struct-vtable", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (obj);
      VM_VALIDATE_STRUCT (obj, "struct_vtable");
      RETURN (SCM_STRUCT_VTABLE (obj));
    }

  /* make-struct dst:12 vtable:12 _:8 n-init:24 init0:24 0:8 ...
   *
   * Make a new struct with VTABLE, and place it in DST.  The struct
   * will be constructed with N-INIT initializers, which are located in
   * the locals given by INIT0....  The format of INIT0... is as in the
   * "call" opcode: unsigned 24-bit values, with 0 in the high byte.
   */
  VM_DEFINE_OP (101, make_struct, "make-struct", OP2 (U8_U12_U12, X8_R24))
#if 0
    {
      scm_t_uint16 dst, vtable_r;
      scm_t_uint32 n_init, n;
      SCM vtable, ret;

      SCM_UNPACK_RTL_12_12 (op, dst, vtable_r);
      vtable = LOCAL_REF (vtable_r);
      SCM_UNPACK_RTL_24 (ip[1], n_init);

      SYNC_IP ();

      if (SCM_LIKELY (SCM_STRUCTP (vtable)
                      && SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SIMPLE)
                      && (SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size)
                          == n_init)
                      && !SCM_VTABLE_INSTANCE_FINALIZER (vtable)))
        {
          /* Verily, we are making a simple struct with the right number of
             initializers, and no finalizer. */
          ret = scm_words ((scm_t_bits)SCM_STRUCT_DATA (vtable) | scm_tc3_struct,
                           n_init + 2);
          SCM_SET_CELL_WORD_1 (ret, (scm_t_bits)SCM_CELL_OBJECT_LOC (ret, 2));
          
          for (n = 0; n < n_init; n++)
            SCM_STRUCT_DATA (ret)[n] = SCM_UNPACK (LOCAL_REF (ip[n + 1]));
        }
      else
        ret = scm_c_make_structvs (vtable, fp, &ip[1], n_init);

      LOCAL_SET (dst, ret);
      NEXT (n_init + 1);
    }
#else
  abort ();
#endif

  /* struct-ref dst:8 src:8 idx:8
   *
   * Fetch the item at slot IDX in the struct in SRC, and store it
   * in DST.
   */
  VM_DEFINE_OP (102, struct_ref, "struct-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      ARGS2 (obj, pos);

      if (SCM_LIKELY (SCM_STRUCTP (obj)
                      && SCM_STRUCT_VTABLE_FLAG_IS_SET (obj,
                                                        SCM_VTABLE_FLAG_SIMPLE)
                      && SCM_I_INUMP (pos)))
        {
          SCM vtable;
          scm_t_bits index, len;

          /* True, an inum is a signed value, but cast to unsigned it will
             certainly be more than the length, so we will fall through if
             index is negative. */
          index = SCM_I_INUM (pos);
          vtable = SCM_STRUCT_VTABLE (obj);
          len = SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size);

          if (SCM_LIKELY (index < len))
            {
              scm_t_bits *data = SCM_STRUCT_DATA (obj);
              RETURN (SCM_PACK (data[index]));
            }
        }

      SYNC_IP ();
      RETURN (scm_struct_ref (obj, pos));
    }

  /* struct-set! dst:8 idx:8 src:8
   *
   * Store SRC into the struct DST at slot IDX.
   */
  VM_DEFINE_OP (103, struct_set, "struct-set!", OP1 (U8_U8_U8_U8))
    {
      scm_t_uint8 dst, idx, src;
      SCM obj, pos, val;
      
      SCM_UNPACK_RTL_8_8_8 (op, dst, idx, src);
      obj = LOCAL_REF (dst);
      pos = LOCAL_REF (idx);
      val = LOCAL_REF (src);
      
      if (SCM_LIKELY (SCM_STRUCTP (obj)
                      && SCM_STRUCT_VTABLE_FLAG_IS_SET (obj,
                                                        SCM_VTABLE_FLAG_SIMPLE)
                      && SCM_STRUCT_VTABLE_FLAG_IS_SET (obj,
                                                        SCM_VTABLE_FLAG_SIMPLE_RW)
                      && SCM_I_INUMP (pos)))
        {
          SCM vtable;
          scm_t_bits index, len;

          /* See above regarding index being >= 0. */
          index = SCM_I_INUM (pos);
          vtable = SCM_STRUCT_VTABLE (obj);
          len = SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size);
          if (SCM_LIKELY (index < len))
            {
              scm_t_bits *data = SCM_STRUCT_DATA (obj);
              data[index] = SCM_UNPACK (val);
              NEXT (1);
            }
        }

      SYNC_IP ();
      scm_struct_set_x (obj, pos, val);
      NEXT (1);
    }

  /* class-of dst:12 type:12
   *
   * Store the vtable of SRC into DST.
   */
  VM_DEFINE_OP (104, class_of, "class-of", OP1 (U8_U12_U12) | OP_DST)
    {
      ARGS1 (obj);
      if (SCM_INSTANCEP (obj))
        RETURN (SCM_CLASS_OF (obj));
      SYNC_IP ();
      RETURN (scm_class_of (obj));
    }

  /* slot-ref dst:8 src:8 idx:8
   *
   * Fetch the item at slot IDX in the struct in SRC, and store it in
   * DST.  Unlike struct-ref, IDX is an 8-bit immediate value, not an
   * index into the stack.
   */
  VM_DEFINE_OP (105, slot_ref, "slot-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    {
      scm_t_uint8 dst, src, idx;
      SCM_UNPACK_RTL_8_8_8 (op, dst, src, idx);
      LOCAL_SET (dst,
                 SCM_PACK (SCM_STRUCT_DATA (LOCAL_REF (src))[idx]));
      NEXT (1);
    }

  /* slot-set! dst:8 idx:8 src:8
   *
   * Store SRC into slot IDX of the struct in DST.  Unlike struct-set!,
   * IDX is an 8-bit immediate value, not an index into the stack.
   */
  VM_DEFINE_OP (106, slot_set, "slot-set!", OP1 (U8_U8_U8_U8))
    {
      scm_t_uint8 dst, idx, src;
      SCM_UNPACK_RTL_8_8_8 (op, dst, idx, src);
      SCM_STRUCT_DATA (LOCAL_REF (dst))[idx] = SCM_UNPACK (LOCAL_REF (src));
      NEXT (1);
    }


  

  /*
   * Arrays, packed uniform arrays, and bytevectors.
   */

  /* load-typed-array dst:8 type:8 shape:8 offset:32 len:32
   *
   * Load the contiguous typed array located at OFFSET 32-bit words away
   * from the instruction pointer, and store into DST.  LEN is a byte
   * length.  OFFSET is signed.
   */
  VM_DEFINE_OP (107, load_typed_array, "load-typed-array", OP3 (U8_U8_U8_U8, N32, U32) | OP_DST)
    {
      scm_t_uint8 dst, type, shape;
      scm_t_int32 offset;
      scm_t_uint32 len;

      SCM_UNPACK_RTL_8_8_8 (op, dst, type, shape);
      offset = ip[1];
      len = ip[2];
      SYNC_IP ();
      LOCAL_SET (dst, scm_from_contiguous_typed_array (LOCAL_REF (type),
                                                       LOCAL_REF (shape),
                                                       ip + offset, len));
      NEXT (3);
    }

  /* make-array dst:12 type:12 _:8 fill:12 bounds:12
   *
   * Make a new array with TYPE, FILL, and BOUNDS, storing it in DST.
   */
  VM_DEFINE_OP (108, make_array, "make-array", OP2 (U8_U12_U12, X8_U12_U12) | OP_DST)
    {
      scm_t_uint16 dst, type, fill, bounds;
      SCM_UNPACK_RTL_12_12 (op, dst, type);
      SCM_UNPACK_RTL_12_12 (ip[1], fill, bounds);
      SYNC_IP ();
      LOCAL_SET (dst, scm_make_typed_array (LOCAL_REF (type), LOCAL_REF (fill),
                                            LOCAL_REF (bounds)));
      NEXT (2);
    }

  /* bv-u8-ref dst:8 src:8 idx:8
   * bv-s8-ref dst:8 src:8 idx:8
   * bv-u16-ref dst:8 src:8 idx:8
   * bv-s16-ref dst:8 src:8 idx:8
   * bv-u32-ref dst:8 src:8 idx:8
   * bv-s32-ref dst:8 src:8 idx:8
   * bv-u64-ref dst:8 src:8 idx:8
   * bv-s64-ref dst:8 src:8 idx:8
   * bv-f32-ref dst:8 src:8 idx:8
   * bv-f64-ref dst:8 src:8 idx:8
   *
   * Fetch the item at byte offset IDX in the bytevector SRC, and store
   * it in DST.  All accesses use native endianness.
   */
#define BV_FIXABLE_INT_REF(stem, fn_stem, type, size)			\
  do {									\
    scm_t_signed_bits i;                                                \
    const scm_t_ ## type *int_ptr;					\
    ARGS2 (bv, idx);							\
									\
    VM_VALIDATE_BYTEVECTOR (bv, "bv-" #stem "-ref");                    \
    i = SCM_I_INUM (idx);                                               \
    int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
    if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                    && (i >= 0)						\
                    && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                    && (ALIGNED_P (int_ptr, scm_t_ ## type))))		\
      RETURN (SCM_I_MAKINUM (*int_ptr));                                \
    else                                                                \
      {									\
        SYNC_IP ();							\
        RETURN (scm_bytevector_ ## fn_stem ## _ref (bv, idx));		\
      }									\
  } while (0)

#define BV_INT_REF(stem, type, size)					\
  do {									\
    scm_t_signed_bits i;                                                \
    const scm_t_ ## type *int_ptr;					\
    ARGS2 (bv, idx);							\
									\
    VM_VALIDATE_BYTEVECTOR (bv, "bv-" #stem "-ref");                    \
    i = SCM_I_INUM (idx);                                               \
    int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
    if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                    && (i >= 0)						\
                    && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                    && (ALIGNED_P (int_ptr, scm_t_ ## type))))		\
      {									\
        scm_t_ ## type x = *int_ptr;					\
        if (SCM_FIXABLE (x))						\
          RETURN (SCM_I_MAKINUM (x));					\
        else								\
          {								\
            SYNC_IP ();                                                 \
            RETURN (scm_from_ ## type (x));				\
          }								\
      }									\
    else                                                                \
      {									\
        SYNC_IP ();							\
        RETURN (scm_bytevector_ ## stem ## _native_ref (bv, idx));	\
      }									\
  } while (0)

#define BV_FLOAT_REF(stem, fn_stem, type, size)				\
  do {									\
    scm_t_signed_bits i;                                                \
    const type *float_ptr;						\
    ARGS2 (bv, idx);							\
									\
    VM_VALIDATE_BYTEVECTOR (bv, "bv-" #stem "-ref");                    \
    i = SCM_I_INUM (idx);                                               \
    float_ptr = (type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);		\
									\
    SYNC_IP ();                                                         \
    if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                    && (i >= 0)						\
                    && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                    && (ALIGNED_P (float_ptr, type))))			\
      RETURN (scm_from_double (*float_ptr));				\
    else                                                                \
      RETURN (scm_bytevector_ ## fn_stem ## _native_ref (bv, idx));	\
  } while (0)

  VM_DEFINE_OP (109, bv_u8_ref, "bv-u8-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_FIXABLE_INT_REF (u8, u8, uint8, 1);

  VM_DEFINE_OP (110, bv_s8_ref, "bv-s8-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_FIXABLE_INT_REF (s8, s8, int8, 1);

  VM_DEFINE_OP (111, bv_u16_ref, "bv-u16-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_FIXABLE_INT_REF (u16, u16_native, uint16, 2);

  VM_DEFINE_OP (112, bv_s16_ref, "bv-s16-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_FIXABLE_INT_REF (s16, s16_native, int16, 2);

  VM_DEFINE_OP (113, bv_u32_ref, "bv-u32-ref", OP1 (U8_U8_U8_U8) | OP_DST)
#if SIZEOF_VOID_P > 4
    BV_FIXABLE_INT_REF (u32, u32_native, uint32, 4);
#else
    BV_INT_REF (u32, uint32, 4);
#endif

  VM_DEFINE_OP (114, bv_s32_ref, "bv-s32-ref", OP1 (U8_U8_U8_U8) | OP_DST)
#if SIZEOF_VOID_P > 4
    BV_FIXABLE_INT_REF (s32, s32_native, int32, 4);
#else
    BV_INT_REF (s32, int32, 4);
#endif

  VM_DEFINE_OP (115, bv_u64_ref, "bv-u64-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_INT_REF (u64, uint64, 8);

  VM_DEFINE_OP (116, bv_s64_ref, "bv-s64-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_INT_REF (s64, int64, 8);

  VM_DEFINE_OP (117, bv_f32_ref, "bv-f32-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_FLOAT_REF (f32, ieee_single, float, 4);

  VM_DEFINE_OP (118, bv_f64_ref, "bv-f64-ref", OP1 (U8_U8_U8_U8) | OP_DST)
    BV_FLOAT_REF (f64, ieee_double, double, 8);

  /* bv-u8-set! dst:8 idx:8 src:8
   * bv-s8-set! dst:8 idx:8 src:8
   * bv-u16-set! dst:8 idx:8 src:8
   * bv-s16-set! dst:8 idx:8 src:8
   * bv-u32-set! dst:8 idx:8 src:8
   * bv-s32-set! dst:8 idx:8 src:8
   * bv-u64-set! dst:8 idx:8 src:8
   * bv-s64-set! dst:8 idx:8 src:8
   * bv-f32-set! dst:8 idx:8 src:8
   * bv-f64-set! dst:8 idx:8 src:8
   *
   * Store SRC into the bytevector DST at byte offset IDX.  Multibyte
   * values are written using native endianness.
   */
#define BV_FIXABLE_INT_SET(stem, fn_stem, type, min, max, size)		\
  do {									\
    scm_t_uint8 dst, idx, src;                                          \
    scm_t_signed_bits i, j = 0;                                         \
    SCM bv, scm_idx, val;                                               \
    scm_t_ ## type *int_ptr;						\
									\
    SCM_UNPACK_RTL_8_8_8 (op, dst, idx, src);                           \
    bv = LOCAL_REF (dst);                                               \
    scm_idx = LOCAL_REF (idx);                                          \
    val = LOCAL_REF (src);                                              \
    VM_VALIDATE_BYTEVECTOR (bv, "bv-" #stem "-set");                    \
    i = SCM_I_INUM (scm_idx);                                           \
    int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
    if (SCM_LIKELY (SCM_I_INUMP (scm_idx)                               \
                    && (i >= 0)                                         \
                    && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))         \
                    && (ALIGNED_P (int_ptr, scm_t_ ## type))		\
                    && (SCM_I_INUMP (val))				\
                    && ((j = SCM_I_INUM (val)) >= min)                  \
                    && (j <= max)))					\
      *int_ptr = (scm_t_ ## type) j;					\
    else                                                                \
      {                                                                 \
        SYNC_IP ();                                                     \
        scm_bytevector_ ## fn_stem ## _set_x (bv, scm_idx, val);        \
      }                                                                 \
    NEXT (1);                                                           \
  } while (0)

#define BV_INT_SET(stem, type, size)					\
  do {									\
    scm_t_uint8 dst, idx, src;                                          \
    scm_t_signed_bits i;                                                \
    SCM bv, scm_idx, val;                                               \
    scm_t_ ## type *int_ptr;						\
									\
    SCM_UNPACK_RTL_8_8_8 (op, dst, idx, src);                           \
    bv = LOCAL_REF (dst);                                               \
    scm_idx = LOCAL_REF (idx);                                          \
    val = LOCAL_REF (src);                                              \
    VM_VALIDATE_BYTEVECTOR (bv, "bv-" #stem "-set");                    \
    i = SCM_I_INUM (scm_idx);                                           \
    int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
    if (SCM_LIKELY (SCM_I_INUMP (scm_idx)                               \
                    && (i >= 0)                                         \
                    && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))         \
                    && (ALIGNED_P (int_ptr, scm_t_ ## type))))          \
      *int_ptr = scm_to_ ## type (val);                                 \
    else                                                                \
      {                                                                 \
        SYNC_IP ();                                                     \
        scm_bytevector_ ## stem ## _native_set_x (bv, scm_idx, val);    \
      }                                                                 \
    NEXT (1);                                                           \
  } while (0)

#define BV_FLOAT_SET(stem, fn_stem, type, size)                         \
  do {                                                                  \
    scm_t_uint8 dst, idx, src;                                          \
    scm_t_signed_bits i;                                                \
    SCM bv, scm_idx, val;                                               \
    type *float_ptr;                                                    \
									\
    SCM_UNPACK_RTL_8_8_8 (op, dst, idx, src);                           \
    bv = LOCAL_REF (dst);                                               \
    scm_idx = LOCAL_REF (idx);                                          \
    val = LOCAL_REF (src);                                              \
    VM_VALIDATE_BYTEVECTOR (bv, "bv-" #stem "-set");                    \
    i = SCM_I_INUM (scm_idx);                                           \
    float_ptr = (type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);            \
                                                                        \
    if (SCM_LIKELY (SCM_I_INUMP (scm_idx)                               \
                    && (i >= 0)                                         \
                    && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))         \
                    && (ALIGNED_P (float_ptr, type))))                  \
      *float_ptr = scm_to_double (val);                                 \
    else                                                                \
      {                                                                 \
        SYNC_IP ();                                                     \
        scm_bytevector_ ## fn_stem ## _native_set_x (bv, scm_idx, val); \
      }                                                                 \
    NEXT (1);                                                           \
  } while (0)

  VM_DEFINE_OP (119, bv_u8_set, "bv-u8-set!", OP1 (U8_U8_U8_U8))
    BV_FIXABLE_INT_SET (u8, u8, uint8, 0, SCM_T_UINT8_MAX, 1);

  VM_DEFINE_OP (120, bv_s8_set, "bv-s8-set!", OP1 (U8_U8_U8_U8))
    BV_FIXABLE_INT_SET (s8, s8, int8, SCM_T_INT8_MIN, SCM_T_INT8_MAX, 1);

  VM_DEFINE_OP (121, bv_u16_set, "bv-u16-set!", OP1 (U8_U8_U8_U8))
    BV_FIXABLE_INT_SET (u16, u16_native, uint16, 0, SCM_T_UINT16_MAX, 2);

  VM_DEFINE_OP (122, bv_s16_set, "bv-s16-set!", OP1 (U8_U8_U8_U8))
    BV_FIXABLE_INT_SET (s16, s16_native, int16, SCM_T_INT16_MIN, SCM_T_INT16_MAX, 2);

  VM_DEFINE_OP (123, bv_u32_set, "bv-u32-set!", OP1 (U8_U8_U8_U8))
#if SIZEOF_VOID_P > 4
    BV_FIXABLE_INT_SET (u32, u32_native, uint32, 0, SCM_T_UINT32_MAX, 4);
#else
    BV_INT_SET (u32, uint32, 4);
#endif

  VM_DEFINE_OP (124, bv_s32_set, "bv-s32-set!", OP1 (U8_U8_U8_U8))
#if SIZEOF_VOID_P > 4
    BV_FIXABLE_INT_SET (s32, s32_native, int32, SCM_T_INT32_MIN, SCM_T_INT32_MAX, 4);
#else
    BV_INT_SET (s32, int32, 4);
#endif

  VM_DEFINE_OP (125, bv_u64_set, "bv-u64-set!", OP1 (U8_U8_U8_U8))
    BV_INT_SET (u64, uint64, 8);

  VM_DEFINE_OP (126, bv_s64_set, "bv-s64-set!", OP1 (U8_U8_U8_U8))
    BV_INT_SET (s64, int64, 8);

  VM_DEFINE_OP (127, bv_f32_set, "bv-f32-set!", OP1 (U8_U8_U8_U8))
    BV_FLOAT_SET (f32, ieee_single, float, 4);

  VM_DEFINE_OP (128, bv_f64_set, "bv-f64-set!", OP1 (U8_U8_U8_U8))
    BV_FLOAT_SET (f64, ieee_double, double, 8);

  END_DISPATCH_SWITCH;

 vm_error_bad_instruction:
  vm_error_bad_instruction (op);

  abort (); /* never reached */
}


#undef ABORT_CONTINUATION_HOOK
#undef ALIGNED_P
#undef APPLY_HOOK
#undef ARGS1
#undef ARGS2
#undef BEGIN_DISPATCH_SWITCH
#undef BINARY_INTEGER_OP
#undef BR_ARITHMETIC
#undef BR_BINARY
#undef BR_NARGS
#undef BR_UNARY
#undef BV_FIXABLE_INT_REF
#undef BV_FIXABLE_INT_SET
#undef BV_FLOAT_REF
#undef BV_FLOAT_SET
#undef BV_INT_REF
#undef BV_INT_SET
#undef CACHE_REGISTER
#undef CHECK_OVERFLOW
#undef END_DISPATCH_SWITCH
#undef FREE_VARIABLE_REF
#undef INIT
#undef INUM_MAX
#undef INUM_MIN
#undef LOCAL_REF
#undef LOCAL_SET
#undef NEXT
#undef NEXT_HOOK
#undef NEXT_JUMP
#undef POP_CONTINUATION_HOOK
#undef PUSH_CONTINUATION_HOOK
#undef RESTORE_CONTINUATION_HOOK
#undef RETURN
#undef RETURN_ONE_VALUE
#undef RETURN_VALUE_LIST
#undef RUN_HOOK
#undef RUN_HOOK0
#undef SYNC_ALL
#undef SYNC_BEFORE_GC
#undef SYNC_IP
#undef SYNC_REGISTER
#undef VARIABLE_BOUNDP
#undef VARIABLE_REF
#undef VARIABLE_SET
#undef VM_CHECK_FREE_VARIABLE
#undef VM_CHECK_OBJECT
#undef VM_CHECK_UNDERFLOW
#undef VM_DEFINE_OP
#undef VM_INSTRUCTION_TO_LABEL
#undef VM_USE_HOOKS
#undef VM_VALIDATE_BYTEVECTOR
#undef VM_VALIDATE_PAIR
#undef VM_VALIDATE_STRUCT

/*
(defun renumber-ops ()
  "start from top of buffer and renumber 'VM_DEFINE_FOO (\n' sequences"
  (interactive "")
  (save-excursion
    (let ((counter -1)) (goto-char (point-min))
      (while (re-search-forward "^ *VM_DEFINE_[^ ]+ (\\([^,]+\\)," (point-max) t)
        (replace-match
         (number-to-string (setq counter (1+ counter)))
          t t nil 1)))))
(renumber-ops)
*/
/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
