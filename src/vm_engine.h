/* Copyright (C) 2001 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

/* This file is included in vm_engine.c */

/*
 * Options
 */

#define VM_USE_HOOKS		1	/* Various hooks */
#define VM_USE_CLOCK		1	/* Bogoclock */
#define VM_CHECK_EXTERNAL	1	/* Check external link */
#define VM_CHECK_OBJECT         1       /* Check object table */


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
#define IP_REG asm("%esi")
#define SP_REG asm("%edi")
#define FP_REG
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


/*
 * Cache/Sync
 */

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

/* Get a local copy of the program's "object table" (i.e. the vector of
   external bindings that are referenced by the program), initialized by
   `load-program'.  */
#define CACHE_PROGRAM()						\
{								\
  ssize_t _vincr;						\
  scm_t_array_handle _vhandle;					\
								\
  bp = SCM_PROGRAM_DATA (program);				\
  /* Was: objects = SCM_VELTS (bp->objs); */			\
  objects = scm_vector_elements (bp->objs, &_vhandle,		\
				 &object_count, &_vincr);	\
  scm_array_handle_release (&_vhandle);				\
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

#undef CHECK_EXTERNAL
#if VM_CHECK_EXTERNAL
#define CHECK_EXTERNAL(e) \
  do { if (!SCM_CONSP (e)) goto vm_error_external; } while (0)
#else
#define CHECK_EXTERNAL(e)
#endif

/* Accesses to a program's object table.  */
#if VM_CHECK_OBJECT
#define CHECK_OBJECT(_num) \
  do { if ((_num) >= object_count) goto vm_error_object; } while (0)
#else
#define CHECK_OBJECT(_num)
#endif


/*
 * Hooks
 */

#undef RUN_HOOK
#if VM_USE_HOOKS
#define RUN_HOOK(h)				\
{						\
  if (!SCM_FALSEP (vp->hooks[h]))		\
    {						\
      SYNC_REGISTER ();				\
      vm_heapify_frames (vm);			\
      scm_c_run_hook (vp->hooks[h], hook_args);	\
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

#define CHECK_OVERFLOW()			\
  if (sp > stack_limit)				\
    goto vm_error_stack_overflow

#define CHECK_UNDERFLOW()			\
  if (sp < stack_base)				\
    goto vm_error_stack_underflow

#define PUSH(x)	do { sp++; CHECK_OVERFLOW (); *sp = x; } while (0)
#define DROP()	do { CHECK_UNDERFLOW (); sp--; } while (0)
#define DROPN(_n)	do { CHECK_UNDERFLOW (); sp -= (_n); } while (0)
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
  SCM l = SCM_EOL;				\
  sp -= n;					\
  for (i = n; i; i--)				\
    CONS (l, sp[i], l);				\
  PUSH (l);					\
} while (0)


/* Below is a (slightly broken) experiment to avoid calling `scm_cell' and to
   allocate cells on the stack.  This is a significant improvement for
   programs which call a lot of procedures, since the procedure call
   mechanism uses POP_LIST which normally uses `scm_cons'.

   What it does is that it creates a list whose cells are allocated on the
   VM's stack instead of being allocated on the heap via `scm_cell'.  This is
   much faster.  However, if the callee does something like:

     (lambda (. args)
       (set! the-args args))

   then terrible things may happen since the list of arguments may be
   overwritten later on.  */


/* Awful hack that aligns PTR so that it can be considered as a non-immediate
   value by Guile.  */
#define ALIGN_AS_NON_IMMEDIATE(_ptr)		\
{						\
  if ((scm_t_bits)(_ptr) & 6)			\
    {						\
      size_t _incr;				\
						\
      _incr = (scm_t_bits)(_ptr) & 6;		\
      _incr = (~_incr) & 7;			\
      (_ptr) += _incr;				\
    }						\
}

#define POP_LIST_ON_STACK(n)			\
do						\
{						\
  int i;					\
  if (n == 0)					\
    {						\
      sp -= n;					\
      PUSH (SCM_EOL);				\
    }						\
  else						\
    {						\
      SCM *list_head, *list;			\
						\
      list_head = sp + 1;			\
      ALIGN_AS_NON_IMMEDIATE (list_head);	\
      list = list_head;				\
						\
      sp -= n;					\
      for (i = 1; i <= n; i++)			\
	{					\
	  /* The cell's car and cdr.  */	\
	  *(list) = sp[i];			\
	  *(list + 1) = PTR2SCM (list + 2);	\
	  list += 2;				\
	}					\
						\
      /* The last pair's cdr is '().  */	\
      list--;					\
      *list = SCM_EOL;				\
      /* Push the SCM object that points */	\
      /* to the first cell.  */			\
      PUSH (PTR2SCM (list_head));		\
    }						\
}						\
while (0)

/* end of the experiment */


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


/*
 * Instruction operation
 */

#define FETCH()		(*ip++)
#define FETCH_LENGTH(len) do { ip = vm_fetch_length (ip, &len); } while (0)

#undef CLOCK
#if VM_USE_CLOCK
#define CLOCK(n)	vp->clock += n
#else
#define CLOCK(n)
#endif

#undef NEXT_JUMP
#ifdef HAVE_LABELS_AS_VALUES
#define NEXT_JUMP()		goto *jump_table[FETCH ()]
#else
#define NEXT_JUMP()		goto vm_start
#endif

#define NEXT					\
{						\
  CLOCK (1);					\
  NEXT_HOOK ();					\
  NEXT_JUMP ();					\
}


/*
 * Stack frame
 */

#define INIT_ARGS()				\
{						\
  if (bp->nrest)				\
    {						\
      int n = nargs - (bp->nargs - 1);		\
      if (n < 0)				\
	goto vm_error_wrong_num_args;		\
      POP_LIST (n);				\
    }						\
  else						\
    {						\
      if (nargs != bp->nargs)			\
	goto vm_error_wrong_num_args;		\
    }						\
}

/* See frames.h for the layout of stack frames */

#define NEW_FRAME()				\
{						\
  int i;					\
  SCM ra = SCM_PACK (ip);			\
  SCM dl = SCM_PACK (fp);			\
  SCM *p = sp + 1;				\
  SCM *q = p + bp->nlocs;			\
						\
  /* New pointers */				\
  ip = bp->base;				\
  fp = p - bp->nargs;				\
  sp = q + 3;					\
  CHECK_OVERFLOW ();				\
						\
  /* Init local variables */			\
  for (; p < q; p++)				\
    *p = SCM_UNDEFINED;				\
						\
  /* Create external variables */		\
  external = bp->external;			\
  for (i = 0; i < bp->nexts; i++)		\
    CONS (external, SCM_UNDEFINED, external);	\
						\
  /* Set frame data */				\
  p[3] = ra;					\
  p[2] = dl;					\
  p[1] = SCM_BOOL_F;				\
  p[0] = external;				\
}

#define FREE_FRAME()				\
{						\
  SCM *last_sp = sp;				\
  SCM *last_fp = fp;				\
  SCM *p = fp + bp->nargs + bp->nlocs;		\
						\
  /* Restore pointers */			\
  ip = SCM_FRAME_BYTE_CAST (p[3]);		\
  fp = SCM_FRAME_STACK_CAST (p[2]);		\
						\
  if (!SCM_FALSEP (p[1]))			\
    {						\
      /* Unlink the heap stack */		\
      vp->this_frame = p[1];			\
    }						\
  else						\
    {						\
      /* Move stack items */			\
      p += 4;					\
      sp = SCM_FRAME_LOWER_ADDRESS (last_fp);	\
      while (p <= last_sp)			\
	*sp++ = *p++;				\
      sp--;					\
    }						\
}

#define CACHE_EXTERNAL() external = fp[bp->nargs + bp->nlocs]


/*
 * Function support
 */

#define ARGS1(a1)	SCM a1 = sp[0];
#define ARGS2(a1,a2)	SCM a1 = sp[-1], a2 = sp[0]; sp--;
#define ARGS3(a1,a2,a3)	SCM a1 = sp[-2], a2 = sp[-1], a3 = sp[0]; sp -= 2;

#define RETURN(x)	do { *sp = x; NEXT; } while (0)

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
