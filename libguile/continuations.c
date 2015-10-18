/* Copyright (C) 1995,1996,1998,2000,2001,2004, 2006, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "libguile/async.h"
#include "libguile/debug.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#include "libguile/smob.h"
#include "libguile/ports.h"
#include "libguile/dynstack.h"
#include "libguile/eval.h"
#include "libguile/vm.h"
#include "libguile/instructions.h"

#include "libguile/validate.h"
#include "libguile/continuations.h"



static scm_t_bits tc16_continuation;
#define SCM_CONTREGSP(x)	SCM_TYP16_PREDICATE (tc16_continuation, x)

#define SCM_CONTREGS(x)		((scm_t_contregs *) SCM_SMOB_DATA_1 (x))

#define SCM_CONTINUATION_LENGTH(x) (SCM_CONTREGS (x)->num_stack_items)
#define SCM_SET_CONTINUATION_LENGTH(x, n)\
   (SCM_CONTREGS (x)->num_stack_items = (n))
#define SCM_JMPBUF(x)		 ((SCM_CONTREGS (x))->jmpbuf)
#define SCM_CONTINUATION_ROOT(x) ((SCM_CONTREGS (x))->root)   
#define SCM_DFRAME(x)		 ((SCM_CONTREGS (x))->dframe)



/* scm_i_make_continuation will return a procedure whose code will
   reinstate the continuation. Here, as in gsubr.c, we define the form
   of that trampoline function.
 */

static const scm_t_uint32 continuation_stub_code[] =
  {
    SCM_PACK_OP_24 (continuation_call, 0)
  };

static SCM
make_continuation_trampoline (SCM contregs)
{
  SCM ret;
  scm_t_bits nfree = 1;
  scm_t_bits flags = SCM_F_PROGRAM_IS_CONTINUATION;

  ret = scm_words (scm_tc7_program | (nfree << 16) | flags, nfree + 2);
  SCM_SET_CELL_WORD_1 (ret, continuation_stub_code);
  SCM_PROGRAM_FREE_VARIABLE_SET (ret, 0, contregs);

  return ret;
}
  

/* {Continuations}
 */


static int
continuation_print (SCM obj, SCM port, scm_print_state *state SCM_UNUSED)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);

  scm_puts_unlocked ("#<continuation ", port);
  scm_intprint (continuation->num_stack_items, 10, port);
  scm_puts_unlocked (" @ ", port);
  scm_uintprint (SCM_SMOB_DATA_1 (obj), 16, port);
  scm_putc_unlocked ('>', port);
  return 1;
}

/* James Clark came up with this neat one instruction fix for
 * continuations on the SPARC.  It flushes the register windows so
 * that all the state of the process is contained in the stack.
 */

#if defined (sparc) || defined (__sparc__) || defined (__sparc)
# define SCM_FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
# define SCM_FLUSH_REGISTER_WINDOWS /* empty */
#endif

/* this may return more than once: the first time with the escape
   procedure, then subsequently with SCM_UNDEFINED (the vals already having been
   placed on the VM stack). */
#define FUNC_NAME "scm_i_make_continuation"
SCM 
scm_i_make_continuation (int *first, struct scm_vm *vp, SCM vm_cont)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  SCM cont;
  scm_t_contregs *continuation;
  long stack_size;
  SCM_STACKITEM * src;

  SCM_FLUSH_REGISTER_WINDOWS;
  stack_size = scm_stack_size (thread->continuation_base);
  continuation = scm_gc_malloc (sizeof (scm_t_contregs)
				+ (stack_size - 1) * sizeof (SCM_STACKITEM),
				"continuation");
  continuation->num_stack_items = stack_size;
  continuation->root = thread->continuation_root;
  src = thread->continuation_base;
#if ! SCM_STACK_GROWS_UP
  src -= stack_size;
#endif
  continuation->offset = continuation->stack - src;
  memcpy (continuation->stack, src, sizeof (SCM_STACKITEM) * stack_size);
  continuation->vp = vp;
  continuation->vm_cont = vm_cont;

  SCM_NEWSMOB (cont, tc16_continuation, continuation);

  *first = !SCM_I_SETJMP (continuation->jmpbuf);
  if (*first)
    {
#ifdef __ia64__
      continuation->backing_store_size =
	(char *) scm_ia64_ar_bsp(&continuation->jmpbuf.ctx)
	-
	(char *) thread->register_backing_store_base;
      continuation->backing_store = NULL;
      continuation->backing_store = 
        scm_gc_malloc (continuation->backing_store_size,
		       "continuation backing store");
      memcpy (continuation->backing_store, 
              (void *) thread->register_backing_store_base, 
              continuation->backing_store_size);
#endif /* __ia64__ */
      return make_continuation_trampoline (cont);
    }
  else
    {
      scm_gc_after_nonlocal_exit ();
      return SCM_UNDEFINED;
    }
}
#undef FUNC_NAME

int
scm_i_continuation_to_frame (SCM continuation, struct scm_frame *frame)
{
  SCM contregs;
  scm_t_contregs *cont;

  contregs = SCM_PROGRAM_FREE_VARIABLE_REF (continuation, 0);
  cont = SCM_CONTREGS (contregs);

  if (scm_is_true (cont->vm_cont))
    {
      struct scm_vm_cont *data = SCM_VM_CONT_DATA (cont->vm_cont);
      union scm_vm_stack_element *stack_top;

      /* FIXME vm_cont should hold fp/sp offsets */
      stack_top = data->stack_bottom + data->stack_size;
      frame->stack_holder = data;
      frame->fp_offset = stack_top - (data->fp + data->reloc);
      frame->sp_offset = data->stack_size;
      frame->ip = data->ra;

      return 1;
    }
  else
    return 0;
}

struct scm_vm *
scm_i_contregs_vp (SCM contregs)
{
  return SCM_CONTREGS (contregs)->vp;
}

SCM
scm_i_contregs_vm_cont (SCM contregs)
{
  return SCM_CONTREGS (contregs)->vm_cont;
}


/* {Apply}
 */

/* Invoking a continuation proceeds as follows:
 *
 * - the stack is made large enough for the called continuation
 * - the old windchain is unwound down to the branching point
 * - the continuation stack is copied into place
 * - the windchain is rewound up to the continuation's context
 * - the continuation is invoked via longjmp (or setcontext)
 *
 * This order is important so that unwind and rewind handlers are run
 * with their correct stack.
 */

static void scm_dynthrow (SCM);

/* Grow the stack by a fixed amount to provide space to copy in the
 * continuation.  Possibly this function has to be called several times
 * recursively before enough space is available.  Make sure the compiler does
 * not optimize the growth array away by storing it's address into a global
 * variable.
 */

static scm_t_bits scm_i_dummy;

static void 
grow_stack (SCM cont)
{
  scm_t_bits growth[100];

  scm_i_dummy = (scm_t_bits) growth;
  scm_dynthrow (cont);
}


/* Copy the continuation stack into the current stack.  Calling functions from
 * within this function is safe, since only stack frames below this function's
 * own frame are overwritten.  Thus, memcpy can be used for best performance.
 */

static void
copy_stack_and_call (scm_t_contregs *continuation,
		     SCM_STACKITEM * dst)
{
  scm_t_dynstack *dynstack;
  scm_t_bits *joint;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;

  dynstack = SCM_VM_CONT_DATA (continuation->vm_cont)->dynstack;

  joint = scm_dynstack_unwind_fork (&thread->dynstack, dynstack);

  memcpy (dst, continuation->stack,
	  sizeof (SCM_STACKITEM) * continuation->num_stack_items);
#ifdef __ia64__
  thread->pending_rbs_continuation = continuation;
#endif

  scm_dynstack_wind (&thread->dynstack, joint);

  SCM_I_LONGJMP (continuation->jmpbuf, 1);
}

#ifdef __ia64__
void
scm_ia64_longjmp (scm_i_jmp_buf *JB, int VAL)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (t->pending_rbs_continuation)
    {
      memcpy (t->register_backing_store_base,
	      t->pending_rbs_continuation->backing_store,
	      t->pending_rbs_continuation->backing_store_size);
      t->pending_rbs_continuation = NULL;
    }
  setcontext (&JB->ctx);
}
#endif

/* Call grow_stack until the stack space is large enough, then, as the current
 * stack frame might get overwritten, let copy_stack_and_call perform the
 * actual copying and continuation calling.
 */
static void 
scm_dynthrow (SCM cont)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_contregs *continuation = SCM_CONTREGS (cont);
  SCM_STACKITEM *dst = thread->continuation_base;
  SCM_STACKITEM stack_top_element;

  if (thread->critical_section_level)
    {
      fprintf (stderr, "continuation invoked from within critical section.\n");
      abort ();
    }

#if SCM_STACK_GROWS_UP
  if (dst + continuation->num_stack_items >= &stack_top_element)
    grow_stack (cont);
#else
  dst -= continuation->num_stack_items;
  if (dst <= &stack_top_element)
    grow_stack (cont);
#endif /* def SCM_STACK_GROWS_UP */

  SCM_FLUSH_REGISTER_WINDOWS;
  copy_stack_and_call (continuation, dst);
}


void
scm_i_check_continuation (SCM cont)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_contregs *continuation = SCM_CONTREGS (cont);

  if (!scm_is_eq (continuation->root, thread->continuation_root))
    scm_misc_error
      ("%continuation-call", 
       "invoking continuation would cross continuation barrier: ~A",
       scm_list_1 (cont));
}

void
scm_i_reinstate_continuation (SCM cont)
{
  scm_dynthrow (cont);
}

SCM
scm_i_with_continuation_barrier (scm_t_catch_body body,
				 void *body_data,
				 scm_t_catch_handler handler,
				 void *handler_data,
				 scm_t_catch_handler pre_unwind_handler,
				 void *pre_unwind_handler_data)
{
  SCM_STACKITEM stack_item;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  SCM old_controot;
  SCM_STACKITEM *old_contbase;
  SCM result;

  /* Establish a fresh continuation root.  
   */
  old_controot = thread->continuation_root;
  old_contbase = thread->continuation_base;
  thread->continuation_root = scm_cons (thread->handle, old_controot);
  thread->continuation_base = &stack_item;

  /* Call FUNC inside a catch all.  This is now guaranteed to return
     directly and exactly once.
  */
  result = scm_c_catch (SCM_BOOL_T,
			body, body_data,
			handler, handler_data,
			pre_unwind_handler, pre_unwind_handler_data);

  /* Return to old continuation root.
   */
  thread->continuation_base = old_contbase;
  thread->continuation_root = old_controot;

  return result;
}



static int
should_print_backtrace (SCM tag, SCM stack)
{
  return SCM_BACKTRACE_P
    && scm_is_true (stack)
    && scm_initialized_p
    /* It's generally not useful to print backtraces for errors reading
       or expanding code in these fallback catch statements. */
    && !scm_is_eq (tag, scm_from_latin1_symbol ("read-error"))
    && !scm_is_eq (tag, scm_from_latin1_symbol ("syntax-error"));
}

static void
print_exception_and_backtrace (SCM port, SCM tag, SCM args)
{
  SCM stack, frame;

  /* We get here via a throw to a catch-all.  In that case there is the
     throw frame active, and this catch closure, so narrow by two
     frames.  */
  stack = scm_make_stack (SCM_BOOL_T, scm_list_1 (scm_from_int (2)));
  frame = scm_is_true (stack) ? scm_stack_ref (stack, SCM_INUM0) : SCM_BOOL_F;

  if (should_print_backtrace (tag, stack))
    {
      scm_puts_unlocked ("Backtrace:\n", port);
      scm_display_backtrace_with_highlights (stack, port,
                                             SCM_BOOL_F, SCM_BOOL_F,
                                             SCM_EOL);
      scm_newline (port);
    }

  scm_print_exception (port, frame, tag, args);
}



struct c_data {
  void *(*func) (void *);
  void *data;
  void *result;
};

static SCM
c_body (void *d)
{
  struct c_data *data = (struct c_data *)d;
  data->result = data->func (data->data);
  return SCM_UNSPECIFIED;
}

static SCM
c_handler (void *d, SCM tag, SCM args)
{
  struct c_data *data;

  /* If TAG is `quit', exit() the process.  */
  if (scm_is_eq (tag, scm_from_latin1_symbol ("quit")))
    exit (scm_exit_status (args));

  data = (struct c_data *)d;
  data->result = NULL;
  return SCM_UNSPECIFIED;
}

static SCM
pre_unwind_handler (void *error_port, SCM tag, SCM args)
{
  /* Print the exception unless TAG is  `quit'.  */
  if (!scm_is_eq (tag, scm_from_latin1_symbol ("quit")))
    print_exception_and_backtrace (SCM_PACK_POINTER (error_port), tag, args);

  return SCM_UNSPECIFIED;
}

void *
scm_c_with_continuation_barrier (void *(*func) (void *), void *data)
{
  struct c_data c_data;
  c_data.func = func;
  c_data.data = data;
  scm_i_with_continuation_barrier (c_body, &c_data,
				   c_handler, &c_data,
				   pre_unwind_handler,
                                   SCM_UNPACK_POINTER (scm_current_error_port ()));
  return c_data.result;
}

struct scm_data {
  SCM proc;
};

static SCM
scm_body (void *d)
{
  struct scm_data *data = (struct scm_data *)d;
  return scm_call_0 (data->proc);
}

static SCM
scm_handler (void *d, SCM tag, SCM args)
{
  /* Print a message.  Note that if TAG is `quit', this will exit() the
     process.  */
  scm_handle_by_message_noexit (NULL, tag, args);

  return SCM_BOOL_F;
}

SCM_DEFINE (scm_with_continuation_barrier, "with-continuation-barrier", 1,0,0,
	    (SCM proc),
"Call @var{proc} and return its result.  Do not allow the invocation of\n"
"continuations that would leave or enter the dynamic extent of the call\n"
"to @code{with-continuation-barrier}.  Such an attempt causes an error\n"
"to be signaled.\n"
"\n"
"Throws (such as errors) that are not caught from within @var{proc} are\n"
"caught by @code{with-continuation-barrier}.  In that case, a short\n"
"message is printed to the current error port and @code{#f} is returned.\n"
"\n"
"Thus, @code{with-continuation-barrier} returns exactly once.\n")
#define FUNC_NAME s_scm_with_continuation_barrier
{
  struct scm_data scm_data;
  scm_data.proc = proc;
  return scm_i_with_continuation_barrier (scm_body, &scm_data,
					  scm_handler, &scm_data,
					  pre_unwind_handler,
                                          SCM_UNPACK_POINTER (scm_current_error_port ()));
}
#undef FUNC_NAME

void
scm_init_continuations ()
{
  tc16_continuation = scm_make_smob_type ("continuation", 0);
  scm_set_smob_print (tc16_continuation, continuation_print);
#include "libguile/continuations.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
