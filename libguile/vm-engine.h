/* Copyright (C) 2001, 2009 Free Software Foundation, Inc.
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

/* This file is included in vm_engine.c */


/*
 * Registers
 */

/* Register optimization. [ stolen from librep/src/lispmach.h,v 1.3 ]

   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#ifdef __GNUC__
#ifdef __mips__
#define IP_REG asm("$16")
#define SP_REG asm("$17")
#define FP_REG asm("$18")
#endif
#ifdef __sparc__
#define IP_REG asm("%l0")
#define SP_REG asm("%l1")
#define FP_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define IP_REG asm("r9")
#define SP_REG asm("r10")
#define FP_REG asm("r11")
#else
#define IP_REG asm("$9")
#define SP_REG asm("$10")
#define FP_REG asm("$11")
#endif
#endif
#ifdef __i386__
/* too few registers! because of register allocation errors with various gcs,
   just punt on explicit assignments on i386, hoping that the "register"
   declaration will be sufficient. */
#endif
#if defined(PPC) || defined(_POWER) || defined(_IBMR2)
#define IP_REG asm("26")
#define SP_REG asm("27")
#define FP_REG asm("28")
#endif
#ifdef __hppa__
#define IP_REG asm("%r18")
#define SP_REG asm("%r17")
#define FP_REG asm("%r16")
#endif
#ifdef __mc68000__
#define IP_REG asm("a5")
#define SP_REG asm("a4")
#define FP_REG
#endif
#ifdef __arm__
#define IP_REG asm("r9")
#define SP_REG asm("r8")
#define FP_REG asm("r7")
#endif
#endif

#ifndef IP_REG
#define IP_REG
#endif
#ifndef SP_REG
#define SP_REG
#endif
#ifndef FP_REG
#define FP_REG
#endif


/*
 * Cache/Sync
 */

#ifdef VM_ENABLE_ASSERTIONS
# define ASSERT(condition) if (SCM_UNLIKELY (!(condition))) abort()
#else
# define ASSERT(condition)
#endif


#define CACHE_REGISTER()			\
{						\
  ip = vp->ip;					\
  sp = vp->sp;					\
  fp = vp->fp;					\
}

#define SYNC_REGISTER()				\
{						\
  vp->ip = ip;					\
  vp->sp = sp;					\
  vp->fp = fp;					\
}

/* FIXME */
#define ASSERT_VARIABLE(x)                                              \
  do { if (!SCM_VARIABLEP (x)) { SYNC_REGISTER (); abort(); }           \
  } while (0)
#define ASSERT_BOUND_VARIABLE(x)                                        \
  do { ASSERT_VARIABLE (x);                                             \
    if (SCM_VARIABLE_REF (x) == SCM_UNDEFINED)                          \
      { SYNC_REGISTER (); abort(); }                                    \
  } while (0)

#ifdef VM_ENABLE_PARANOID_ASSERTIONS
#define CHECK_IP() \
  do { if (ip < bp->base || ip - bp->base > bp->len) abort (); } while (0)
#define ASSERT_ALIGNED_PROCEDURE() \
  do { if ((scm_t_bits)bp % 8) abort (); } while (0)
#define ASSERT_BOUND(x) \
  do { if ((x) == SCM_UNDEFINED) { SYNC_REGISTER (); abort(); } \
  } while (0)
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
      object_count = SCM_I_VECTOR_LENGTH (SCM_PROGRAM_OBJTABLE (program)); \
    } else {                                                            \
      objects = NULL;                                                   \
      object_count = 0;                                                 \
    }                                                                   \
  }                                                                     \
  {                                                                     \
    SCM c = SCM_PROGRAM_FREE_VARIABLES (program);                       \
    if (SCM_I_IS_VECTOR (c))                                            \
      {                                                                 \
        free_vars = SCM_I_VECTOR_WELTS (c);                             \
        free_vars_count = SCM_I_VECTOR_LENGTH (c);                      \
      }                                                                 \
    else                                                                \
      {                                                                 \
        free_vars = NULL;                                               \
        free_vars_count = 0;                                            \
      }                                                                 \
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
#if VM_CHECK_OBJECT
#define CHECK_OBJECT(_num) \
  do { if (SCM_UNLIKELY ((_num) >= object_count)) goto vm_error_object; } while (0)
#else
#define CHECK_OBJECT(_num)
#endif

#if VM_CHECK_FREE_VARIABLES
#define CHECK_FREE_VARIABLE(_num) \
  do { if (SCM_UNLIKELY ((_num) >= free_vars_count)) goto vm_error_free_variable; } while (0)
#else
#define CHECK_FREE_VARIABLE(_num)
#endif


/*
 * Hooks
 */

#undef RUN_HOOK
#if VM_USE_HOOKS
#define RUN_HOOK(h)				\
{						\
  if (SCM_UNLIKELY (!SCM_FALSEP (vp->hooks[h])))\
    {						\
      SYNC_REGISTER ();				\
      vm_dispatch_hook (vp, vp->hooks[h], hook_args);      \
      CACHE_REGISTER ();			\
    }						\
}
#else
#define RUN_HOOK(h)
#endif

#define BOOT_HOOK()	RUN_HOOK (SCM_VM_BOOT_HOOK)
#define HALT_HOOK()	RUN_HOOK (SCM_VM_HALT_HOOK)
#define NEXT_HOOK()	RUN_HOOK (SCM_VM_NEXT_HOOK)
#define BREAK_HOOK()	RUN_HOOK (SCM_VM_BREAK_HOOK)
#define ENTER_HOOK()	RUN_HOOK (SCM_VM_ENTER_HOOK)
#define APPLY_HOOK()	RUN_HOOK (SCM_VM_APPLY_HOOK)
#define EXIT_HOOK()	RUN_HOOK (SCM_VM_EXIT_HOOK)
#define RETURN_HOOK()	RUN_HOOK (SCM_VM_RETURN_HOOK)


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

#define CHECK_OVERFLOW()			\
  if (sp >= stack_limit)			\
    goto vm_error_stack_overflow

#define CHECK_UNDERFLOW()                       \
  if (sp < SCM_FRAME_UPPER_ADDRESS (fp))        \
    goto vm_error_stack_underflow;

#define PUSH(x)	do { sp++; CHECK_OVERFLOW (); *sp = x; } while (0)
#define DROP()	do { sp--; CHECK_UNDERFLOW (); NULLSTACK (1); } while (0)
#define DROPN(_n)	do { sp -= (_n); CHECK_UNDERFLOW (); NULLSTACK (_n); } while (0)
#define POP(x)	do { x = *sp; DROP (); } while (0)

/* A fast CONS.  This has to be fast since its used, for instance, by
   POP_LIST when fetching a function's argument list.  Note: `scm_cell' is an
   inlined function in Guile 1.7.  Unfortunately, it calls
   `scm_gc_for_newcell ()' which is _not_ inlined and allocated cells on the
   heap.  XXX  */
#define CONS(x,y,z)					\
{							\
  SYNC_BEFORE_GC ();					\
  x = scm_cell (SCM_UNPACK (y), SCM_UNPACK (z));	\
}

/* Pop the N objects on top of the stack and push a list that contains
   them.  */
#define POP_LIST(n)				\
do						\
{						\
  int i;					\
  SCM l = SCM_EOL, x;				\
  for (i = n; i; i--)                           \
    {                                           \
      POP (x);                                  \
      CONS (l, x, l);                           \
    }                                           \
  PUSH (l);					\
} while (0)

/* The opposite: push all of the elements in L onto the list. */
#define PUSH_LIST(l, NILP)			\
do						\
{						\
  for (; scm_is_pair (l); l = SCM_CDR (l))      \
    PUSH (SCM_CAR (l));                         \
  if (SCM_UNLIKELY (!NILP (l))) {               \
    finish_args = scm_list_1 (l);               \
    goto vm_error_improper_list;                \
  }                                             \
} while (0)


#define POP_LIST_MARK()				\
do {						\
  SCM o;					\
  SCM l = SCM_EOL;				\
  POP (o);					\
  while (!SCM_UNBNDP (o))			\
    {						\
      CONS (l, o, l);				\
      POP (o);					\
    }						\
  PUSH (l);					\
} while (0)

#define POP_CONS_MARK()				\
do {						\
  SCM o, l;					\
  POP (l);                                      \
  POP (o);					\
  while (!SCM_UNBNDP (o))			\
    {						\
      CONS (l, o, l);				\
      POP (o);					\
    }						\
  PUSH (l);					\
} while (0)


/*
 * Instruction operation
 */

#define FETCH()		(*ip++)
#define FETCH_LENGTH(len) do { len=*ip++; len<<=8; len+=*ip++; len<<=8; len+=*ip++; } while (0)
#define FETCH_WIDTH(width) do { width=*ip++; } while (0)

#undef CLOCK
#if VM_USE_CLOCK
#define CLOCK(n)	vp->clock += n
#else
#define CLOCK(n)
#endif

#undef NEXT_JUMP
#ifdef HAVE_LABELS_AS_VALUES
#define NEXT_JUMP()		goto *jump_table[FETCH () & SCM_VM_INSTRUCTION_MASK]
#else
#define NEXT_JUMP()		goto vm_start
#endif

#define NEXT					\
{						\
  CLOCK (1);					\
  NEXT_HOOK ();					\
  CHECK_STACK_LEAK ();                          \
  NEXT_JUMP ();					\
}


/*
 * Stack frame
 */

#define INIT_ARGS()				\
{						\
  if (SCM_UNLIKELY (bp->nrest))                 \
    {						\
      int n = nargs - (bp->nargs - 1);		\
      if (n < 0)				\
	goto vm_error_wrong_num_args;		\
      /* NB, can cause GC while setting up the  \
         stack frame */                         \
      POP_LIST (n);				\
    }						\
  else						\
    {						\
      if (SCM_UNLIKELY (nargs != bp->nargs))    \
	goto vm_error_wrong_num_args;		\
    }						\
}

/* See frames.h for the layout of stack frames */
/* When this is called, bp points to the new program data,
   and the arguments are already on the stack */
#define INIT_FRAME()				\
{						\
  int i;					\
						\
  /* New registers */                           \
  sp += bp->nlocs;                              \
  CHECK_OVERFLOW ();				\
  ip = bp->base;				\
						\
  /* Init local variables */			\
  for (i=bp->nlocs; i;)                         \
    sp[-(--i)] = SCM_UNDEFINED;                 \
}

#define DROP_FRAME()                            \
  {                                             \
    sp -= 3;                                    \
    NULLSTACK (3);                              \
    CHECK_UNDERFLOW ();                         \
  }
    

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
