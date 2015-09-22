/* A stack holds a frame chain
 * Copyright (C) 1996,1997,2000,2001, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation
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
#include "libguile/control.h"
#include "libguile/eval.h"
#include "libguile/debug.h"
#include "libguile/continuations.h"
#include "libguile/struct.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/modules.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vm.h" /* to capture vm stacks */
#include "libguile/frames.h" /* vm frames */

#include "libguile/validate.h"
#include "libguile/stacks.h"
#include "libguile/private-options.h"


static SCM scm_sys_stacks;


/* {Stacks}
 *
 * The stack is represented as a struct that holds a frame. The frame itself is
 * linked to the next frame, or #f.
 *
 * Stacks
 *   Constructor
 *     make-stack
 *   Selectors
 *     stack-id
 *     stack-ref
 *   Inspector
 *     stack-length
 */



/* Count number of debug info frames on a stack, beginning with FRAME.
 */
static long
stack_depth (enum scm_vm_frame_kind kind, const struct scm_frame *frame)
{
  struct scm_frame tmp;
  long n = 1;
  memcpy (&tmp, frame, sizeof tmp);
  while (scm_c_frame_previous (kind, &tmp))
    ++n;
  return n;
}

/* Narrow STACK by cutting away stackframes (mutatingly).
 *
 * Inner frames (most recent) are cut by advancing the frames pointer.
 * Outer frames are cut by decreasing the recorded length.
 *
 * Cut maximally INNER inner frames and OUTER outer frames using
 * the keys INNER_KEY and OUTER_KEY.
 *
 * Frames are cut away starting at the end points and moving towards
 * the center of the stack.  The key is normally compared to the
 * operator in application frames.  Frames up to and including the key
 * are cut.
 *
 * If INNER_KEY is #t a different scheme is used for inner frames:
 *
 * Frames up to but excluding the first source frame originating from
 * a user module are cut, except for possible application frames
 * between the user frame and the last system frame previously
 * encountered.
 */

static scm_t_ptrdiff
find_prompt (SCM key)
{
  scm_t_ptrdiff fp_offset;

  if (!scm_dynstack_find_prompt (&SCM_I_CURRENT_THREAD->dynstack, key,
                                 NULL, &fp_offset, NULL, NULL, NULL))
    scm_misc_error ("make-stack", "Prompt tag not found while narrowing stack",
                    scm_list_1 (key));

  return fp_offset;
}

static long
narrow_stack (long len, enum scm_vm_frame_kind kind, struct scm_frame *frame,
              SCM inner_cut, SCM outer_cut)
{
  /* Resolve procedure cuts to address ranges, if possible.  If the
     debug information has been stripped, this might not be
     possible.  */
  if (scm_is_true (scm_program_p (inner_cut)))
    {
      SCM addr_range = scm_program_address_range (inner_cut);
      if (scm_is_pair (addr_range))
        inner_cut = addr_range;
    }
  if (scm_is_true (scm_program_p (outer_cut)))
    {
      SCM addr_range = scm_program_address_range (outer_cut);
      if (scm_is_pair (addr_range))
        outer_cut = addr_range;
    }

  /* Cut inner part. */
  if (scm_is_true (scm_procedure_p (inner_cut)))
    {
      /* Cut until the given procedure is seen. */
      for (; len ;)
        {
          SCM proc = scm_c_frame_closure (kind, frame);
          len--;
          scm_c_frame_previous (kind, frame);
          if (scm_is_eq (proc, inner_cut))
            break;
        }
    }
  else if (scm_is_pair (inner_cut)
           && scm_is_integer (scm_car (inner_cut))
           && scm_is_integer (scm_cdr (inner_cut)))
    {
      /* Cut until an IP within the given range is found.  */
      scm_t_uintptr low_pc, high_pc, pc;

      low_pc = scm_to_uintptr_t (scm_car (inner_cut));
      high_pc = scm_to_uintptr_t (scm_cdr (inner_cut));

      for (; len ;)
        {
          pc = (scm_t_uintptr) frame->ip;
          len--;
          scm_c_frame_previous (kind, frame);
          if (low_pc <= pc && pc < high_pc)
            break;
        }
    }
  else if (scm_is_integer (inner_cut))
    {
      /* Cut specified number of frames. */
      long inner = scm_to_int (inner_cut);
      
      for (; inner && len; --inner)
        {
          len--;
          scm_c_frame_previous (kind, frame);
        }
    }
  else
    {
      /* Cut until the given prompt tag is seen. */
      scm_t_ptrdiff fp_offset = find_prompt (inner_cut);
      for (; len; len--, scm_c_frame_previous (kind, frame))
        if (fp_offset == frame->fp_offset)
          break;
    }

  /* Cut outer part. */
  if (scm_is_true (scm_procedure_p (outer_cut)))
    {
      long i, new_len;
      struct scm_frame tmp;

      memcpy (&tmp, frame, sizeof tmp);

      /* Cut until the given procedure is seen. */
      for (new_len = i = 0; i < len; i++, scm_c_frame_previous (kind, &tmp))
        if (scm_is_eq (scm_c_frame_closure (kind, &tmp), outer_cut))
          new_len = i;

      len = new_len;
    }
  else if (scm_is_pair (outer_cut)
           && scm_is_integer (scm_car (outer_cut))
           && scm_is_integer (scm_cdr (outer_cut)))
    {
      /* Cut until an IP within the given range is found.  */
      scm_t_uintptr low_pc, high_pc, pc;
      long i, new_len;
      struct scm_frame tmp;

      low_pc = scm_to_uintptr_t (scm_car (outer_cut));
      high_pc = scm_to_uintptr_t (scm_cdr (outer_cut));

      memcpy (&tmp, frame, sizeof tmp);

      /* Cut until the given procedure is seen. */
      for (new_len = i = 0; i < len; i++, scm_c_frame_previous (kind, &tmp))
        {
          pc = (scm_t_uintptr) tmp.ip;
          if (low_pc <= pc && pc < high_pc)
            new_len = i;
        }

      len = new_len;
    }
  else if (scm_is_integer (outer_cut))
    {
      /* Cut specified number of frames. */
      long outer = scm_to_int (outer_cut);
      
      if (outer < len)
        len -= outer;
      else
        len = 0;
    }
  else
    {
      /* Cut until the given prompt tag is seen. */
      long i;
      struct scm_frame tmp;
      scm_t_ptrdiff fp_offset = find_prompt (outer_cut);

      memcpy (&tmp, frame, sizeof tmp);

      for (i = 0; i < len; i++, scm_c_frame_previous (kind, &tmp))
        if (tmp.fp_offset == fp_offset)
          break;

      if (i < len)
        len = i;
      else
        len = 0;
    }

  return len;
}



/* Stacks
 */

SCM scm_stack_type;

SCM_DEFINE (scm_stack_p, "stack?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a calling stack.")
#define FUNC_NAME s_scm_stack_p
{
  return scm_from_bool(SCM_STACKP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_stack, "make-stack", 1, 0, 1, 
            (SCM obj, SCM args),
	    "Create a new stack. If @var{obj} is @code{#t}, the current\n"
	    "evaluation stack is used for creating the stack frames,\n"
	    "otherwise the frames are taken from @var{obj} (which must be\n"
	    "a continuation or a frame object).\n"
            "\n"
	    "@var{args} should be a list containing any combination of\n"
	    "integer, procedure, address range, prompt tag and @code{#t}\n"
            "values.\n"
            "\n"
	    "These values specify various ways of cutting away uninteresting\n"
	    "stack frames from the top and bottom of the stack that\n"
	    "@code{make-stack} returns.  They come in pairs like this:\n"
	    "@code{(@var{inner_cut_1} @var{outer_cut_1} @var{inner_cut_2}\n"
	    "@var{outer_cut_2} @dots{})}.\n"
            "\n"
	    "Each @var{inner_cut_i} can be an integer, a procedure, an\n"
            "address range, or a prompt tag.  An integer means to cut away\n"
            "exactly that number of frames.  A procedure means to cut\n"
            "away all frames up to but excluding the frame whose procedure\n"
            "matches the specified one.  An address range is a pair of\n"
            "integers indicating the low and high addresses of a procedure's\n"
            "code, and is the same as cutting away to a procedure (though\n"
            "with less work).  Anything else is interpreted as a prompt tag\n"
            "which cuts away all frames that are inside a prompt with the\n"
            "given tag.\n"
            "\n"
	    "Each @var{outer_cut_i} can be an integer, a procedure, an\n"
            "address range, or a prompt tag.  An integer means to cut away\n"
            "that number of frames.  A procedure means to cut away frames\n"
            "down to but excluding the frame whose procedure matches the\n"
            "specified one.  An address range is the same, but with the\n"
            "procedure's code specified as an address range.  Anything else\n"
            "is taken to be a prompt tag, which cuts away all frames that are\n"
            "outside a prompt with the given tag.\n"
            "\n"
            "If the @var{outer_cut_i} of the last pair is missing, it is\n"
            "taken as 0.")
#define FUNC_NAME s_scm_make_stack
{
  long n;
  SCM inner_cut, outer_cut;
  enum scm_vm_frame_kind kind;
  struct scm_frame frame;

  /* Extract a pointer to the innermost frame of whatever object
     scm_make_stack was given.  */
  if (scm_is_eq (obj, SCM_BOOL_T))
    {
      SCM cont;
      struct scm_vm_cont *c;
      union scm_vm_stack_element *stack_top;

      cont = scm_i_capture_current_stack ();
      c = SCM_VM_CONT_DATA (cont);

      /* FIXME vm_cont should hold fp/sp offsets */
      stack_top = c->stack_bottom + c->stack_size;
      kind = SCM_VM_FRAME_KIND_CONT;
      frame.stack_holder = c;
      frame.fp_offset = stack_top - (c->fp + c->reloc);
      frame.sp_offset = stack_top - (c->sp + c->reloc);
      frame.ip = c->ra;
    }
  else if (SCM_VM_FRAME_P (obj))
    {
      kind = SCM_VM_FRAME_KIND (obj);
      memcpy (&frame, SCM_VM_FRAME_DATA (obj), sizeof frame);
    }
  else if (SCM_CONTINUATIONP (obj))
    /* FIXME: Narrowing to prompt tags should narrow with respect to the prompts
       that were in place when the continuation was captured. */
    {
      kind = SCM_VM_FRAME_KIND_CONT;
      if (!scm_i_continuation_to_frame (obj, &frame))
        return SCM_BOOL_F;
    }
  else if (SCM_PROGRAM_P (obj) && SCM_PROGRAM_IS_PARTIAL_CONTINUATION (obj))
    {
      kind = SCM_VM_FRAME_KIND_CONT;
      if (!scm_i_vm_cont_to_frame (SCM_PROGRAM_FREE_VARIABLE_REF (obj, 0),
                                   &frame))
        return SCM_BOOL_F;
    }
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
      /* not reached */
    }

  /* Skip initial boot frame, if any.  This is possible if the frame
     originates from a captured continuation.  */
  if (SCM_PROGRAM_P (scm_c_frame_closure (kind, &frame))
      && SCM_PROGRAM_IS_BOOT (scm_c_frame_closure (kind, &frame))
      && !scm_c_frame_previous (kind, &frame))
    return SCM_BOOL_F;

  /* Count number of frames.  Also get stack id tag and check whether
     there are more stackframes than we want to record
     (SCM_BACKTRACE_MAXDEPTH). */
  n = stack_depth (kind, &frame);

  /* Narrow the stack according to the arguments given to scm_make_stack. */
  SCM_VALIDATE_REST_ARGUMENT (args);
  while (n > 0 && !scm_is_null (args))
    {
      inner_cut = SCM_CAR (args);
      args = SCM_CDR (args);
      if (scm_is_null (args)) 
	{
	  outer_cut = SCM_INUM0;
	} 
      else
	{
	  outer_cut = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      
      n = narrow_stack (n, kind, &frame, inner_cut, outer_cut);
    }
  
  if (n > 0)
    {
      /* Make the stack object. */
      SCM stack = scm_make_struct (scm_stack_type, SCM_INUM0, SCM_EOL);
      SCM_SET_STACK_LENGTH (stack, n);
      SCM_SET_STACK_ID (stack, scm_stack_id (obj));
      SCM_SET_STACK_FRAME (stack, scm_c_make_frame (kind, &frame));
      return stack;
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_id, "stack-id", 1, 0, 0, 
            (SCM stack),
	    "Return the identifier given to @var{stack} by @code{start-stack}.")
#define FUNC_NAME s_scm_stack_id
{
  if (scm_is_eq (stack, SCM_BOOL_T)
      /* FIXME: frame case assumes frame still live on the stack, and no
         intervening start-stack. Hmm... */
      || SCM_VM_FRAME_P (stack))
    {
      /* Fetch most recent start-stack tag. */
      SCM stacks = scm_fluid_ref (scm_sys_stacks);
      return scm_is_pair (stacks) ? scm_caar (stacks) : SCM_BOOL_F;
    }
  else if (SCM_CONTINUATIONP (stack))
    /* FIXME: implement me */
    return SCM_BOOL_F;
  else if (SCM_PROGRAM_P (stack) && SCM_PROGRAM_IS_PARTIAL_CONTINUATION (stack))
    /* FIXME: implement me */
    return SCM_BOOL_F;
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, stack);
      /* not reached */
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_ref, "stack-ref", 2, 0, 0,
            (SCM stack, SCM index),
	    "Return the @var{index}'th frame from @var{stack}.")
#define FUNC_NAME s_scm_stack_ref
{
  unsigned long int c_index;
  SCM frame;

  SCM_VALIDATE_STACK (1, stack);
  c_index = scm_to_unsigned_integer (index, 0, SCM_STACK_LENGTH(stack)-1);
  frame = SCM_STACK_FRAME (stack);
  while (c_index--)
    frame = scm_frame_previous (frame);
  return frame;
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_length, "stack-length", 1, 0, 0, 
	    (SCM stack),
	    "Return the length of @var{stack}.")
#define FUNC_NAME s_scm_stack_length
{
  SCM_VALIDATE_STACK (1, stack);
  return scm_from_long (SCM_STACK_LENGTH (stack));
}
#undef FUNC_NAME



void
scm_init_stacks ()
{
  scm_sys_stacks = scm_make_fluid ();
  scm_c_define ("%stacks", scm_sys_stacks);
  
  scm_stack_type = scm_make_vtable (scm_from_locale_string (SCM_STACK_LAYOUT),
                                    SCM_UNDEFINED);
  scm_set_struct_vtable_name_x (scm_stack_type,
				scm_from_latin1_symbol ("stack"));
#include "libguile/stacks.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
