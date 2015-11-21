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
#include <string.h>
#include "_scm.h"
#include "frames.h"
#include "vm.h"

SCM
scm_c_make_frame (enum scm_vm_frame_kind kind, const struct scm_frame *frame)
{
  struct scm_frame *p = scm_gc_malloc (sizeof (struct scm_frame),
                                       "vmframe");
  p->stack_holder = frame->stack_holder;
  p->fp_offset = frame->fp_offset;
  p->sp_offset = frame->sp_offset;
  p->ip = frame->ip;
  return scm_cell (scm_tc7_frame | (kind << 8), (scm_t_bits)p);
}

void
scm_i_frame_print (SCM frame, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<frame ", port);
  scm_uintprint (SCM_UNPACK (frame), 16, port);
  if (scm_module_system_booted_p)
    {
      SCM name = scm_frame_procedure_name (frame);

      if (scm_is_true (name))
        {
          scm_putc_unlocked (' ', port);
          scm_write (name, port);
        }
    }
  /* Don't write args, they can be ridiculously long. */
  scm_puts_unlocked (">", port);
}

static union scm_vm_stack_element*
frame_stack_top (enum scm_vm_frame_kind kind, const struct scm_frame *frame)
{
  switch (kind)
    {
      case SCM_VM_FRAME_KIND_CONT: 
        {
          struct scm_vm_cont *cont = frame->stack_holder;
          return cont->stack_bottom + cont->stack_size;
        }

      case SCM_VM_FRAME_KIND_VM:
        return ((struct scm_vm *) frame->stack_holder)->stack_top;

      default:
        abort ();
    }
}

static scm_t_ptrdiff
frame_offset (enum scm_vm_frame_kind kind, const struct scm_frame *frame)
{
  switch (kind)
    {
    case SCM_VM_FRAME_KIND_CONT:
      return ((struct scm_vm_cont *) frame->stack_holder)->reloc;

    case SCM_VM_FRAME_KIND_VM:
      return 0;

    default:
      abort ();
    }
}

union scm_vm_stack_element*
scm_i_frame_stack_top (SCM frame)
#define FUNC_NAME "frame-stack-top"
{
  SCM_VALIDATE_VM_FRAME (1, frame);

  return frame_stack_top (SCM_VM_FRAME_KIND (frame),
                          SCM_VM_FRAME_DATA (frame));
}
#undef FUNC_NAME

scm_t_ptrdiff
scm_i_frame_offset (SCM frame)
#define FUNC_NAME "frame-offset"
{
  SCM_VALIDATE_VM_FRAME (1, frame);

  return frame_offset (SCM_VM_FRAME_KIND (frame),
                       SCM_VM_FRAME_DATA (frame));

}
#undef FUNC_NAME


/* Scheme interface */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_frame_p
{
  return scm_from_bool (SCM_VM_FRAME_P (obj));
}
#undef FUNC_NAME

/* Retrieve the local in slot 0, which may or may not actually be a
   procedure, and may or may not actually be the procedure being
   applied.  If you want the procedure, look it up from the IP.  */
SCM
scm_c_frame_closure (enum scm_vm_frame_kind kind, const struct scm_frame *frame)
{
  union scm_vm_stack_element *fp, *sp;

  fp = frame_stack_top (kind, frame) - frame->fp_offset;
  sp = frame_stack_top (kind, frame) - frame->sp_offset;

  if (SCM_FRAME_NUM_LOCALS (fp, sp) > 0)
    return SCM_FRAME_LOCAL (fp, 0);

  return SCM_BOOL_F;
}

static SCM frame_procedure_name_var;

static void
init_frame_procedure_name_var (void)
{
  frame_procedure_name_var
    = scm_c_private_lookup ("system vm frame", "frame-procedure-name");
}

SCM_DEFINE (scm_frame_procedure_name, "frame-procedure-name", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_procedure_name
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_frame_procedure_name_var);

  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_call_1 (scm_variable_ref (frame_procedure_name_var), frame);
}
#undef FUNC_NAME

static SCM frame_arguments_var;

static void
init_frame_arguments_var (void)
{
  frame_arguments_var
    = scm_c_private_lookup ("system vm frame", "frame-arguments");
}

SCM_DEFINE (scm_frame_arguments, "frame-arguments", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_arguments
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_frame_arguments_var);

  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_call_1 (scm_variable_ref (frame_arguments_var), frame);
}
#undef FUNC_NAME

static SCM frame_call_representation_var;

static void
init_frame_call_representation_var (void)
{
  frame_call_representation_var
    = scm_c_private_lookup ("system vm frame", "frame-call-representation");
}

SCM scm_frame_call_representation (SCM frame)
#define FUNC_NAME "frame-call-representation"
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_frame_call_representation_var);

  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_call_1 (scm_variable_ref (frame_call_representation_var), frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_source, "frame-source", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_source
{
  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_find_source_for_addr (scm_frame_instruction_pointer (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_num_locals, "frame-num-locals", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_num_locals
{
  union scm_vm_stack_element *fp, *sp;

  SCM_VALIDATE_VM_FRAME (1, frame);

  fp = SCM_VM_FRAME_FP (frame);
  sp = SCM_VM_FRAME_SP (frame);

  return scm_from_ptrdiff_t (SCM_FRAME_NUM_LOCALS (fp, sp));
}
#undef FUNC_NAME

enum stack_item_representation
  {
    STACK_ITEM_SCM = 0,
    STACK_ITEM_F64 = 1,
    STACK_ITEM_U64 = 2,
    STACK_ITEM_S64 = 3
  };

static enum stack_item_representation
scm_to_stack_item_representation (SCM x, const char *subr, int pos)
{
  if (scm_is_eq (x, scm_from_latin1_symbol ("scm")))
    return STACK_ITEM_SCM;
  if (scm_is_eq (x, scm_from_latin1_symbol ("f64")))
    return STACK_ITEM_F64;
  if (scm_is_eq (x, scm_from_latin1_symbol ("u64")))
    return STACK_ITEM_U64;
  if (scm_is_eq (x, scm_from_latin1_symbol ("s64")))
    return STACK_ITEM_S64;

  scm_wrong_type_arg (subr, pos, x);
  return 0;  /* Not reached.  */
}

SCM_DEFINE (scm_frame_local_ref, "frame-local-ref", 3, 0, 0,
	    (SCM frame, SCM index, SCM representation),
	    "")
#define FUNC_NAME s_scm_frame_local_ref
{
  union scm_vm_stack_element *fp, *sp;
  unsigned int i;
  enum stack_item_representation repr;

  SCM_VALIDATE_VM_FRAME (1, frame);
  SCM_VALIDATE_UINT_COPY (2, index, i);
  repr = scm_to_stack_item_representation (representation, FUNC_NAME, SCM_ARG3);

  fp = SCM_VM_FRAME_FP (frame);
  sp = SCM_VM_FRAME_SP (frame);

  if (i < SCM_FRAME_NUM_LOCALS (fp, sp))
    {
      union scm_vm_stack_element *item = SCM_FRAME_SLOT (fp, i);
      switch (repr)
        {
          case STACK_ITEM_SCM:
            return item->as_scm;
          case STACK_ITEM_F64:
            return scm_from_double (item->as_f64);
          case STACK_ITEM_U64:
            return scm_from_uint64 (item->as_u64);
          case STACK_ITEM_S64:
            return scm_from_int64 (item->as_s64);
          default:
            abort();
        }
    }

  SCM_OUT_OF_RANGE (SCM_ARG2, index);
}
#undef FUNC_NAME

/* Need same not-yet-active frame logic here as in frame-num-locals */
SCM_DEFINE (scm_frame_local_set_x, "frame-local-set!", 4, 0, 0,
	    (SCM frame, SCM index, SCM val, SCM representation),
	    "")
#define FUNC_NAME s_scm_frame_local_set_x
{
  union scm_vm_stack_element *fp, *sp;
  unsigned int i;
  enum stack_item_representation repr;

  SCM_VALIDATE_VM_FRAME (1, frame);
  SCM_VALIDATE_UINT_COPY (2, index, i);
  repr = scm_to_stack_item_representation (representation, FUNC_NAME, SCM_ARG3);

  fp = SCM_VM_FRAME_FP (frame);
  sp = SCM_VM_FRAME_SP (frame);

  if (i < SCM_FRAME_NUM_LOCALS (fp, sp))
    {
      union scm_vm_stack_element *item = SCM_FRAME_SLOT (fp, i);
      switch (repr)
        {
          case STACK_ITEM_SCM:
            item->as_scm = val;
            break;
          case STACK_ITEM_F64:
            item->as_f64 = scm_to_double (val);
            break;
          case STACK_ITEM_U64:
            item->as_u64 = scm_to_uint64 (val);
            break;
          case STACK_ITEM_S64:
            item->as_s64 = scm_to_int64 (val);
            break;
          default:
            abort();
        }
      return SCM_UNSPECIFIED;
    }

  SCM_OUT_OF_RANGE (SCM_ARG2, index);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_address, "frame-address", 1, 0, 0,
	    (SCM frame),
	    "Return the frame pointer for @var{frame}.")
#define FUNC_NAME s_scm_frame_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_ptrdiff_t (SCM_VM_FRAME_FP_OFFSET (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_stack_pointer, "frame-stack-pointer", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_stack_pointer
{
  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_from_ptrdiff_t (SCM_VM_FRAME_SP_OFFSET (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_instruction_pointer, "frame-instruction-pointer", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_instruction_pointer
{
  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_from_uintptr_t ((scm_t_uintptr) SCM_VM_FRAME_IP (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_return_address, "frame-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_return_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_uintptr_t ((scm_t_uintptr) (SCM_FRAME_RETURN_ADDRESS
                                              (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_dynamic_link, "frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_dynamic_link
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  /* fixme: munge fp if holder is a continuation */
  return scm_from_uintptr_t
    ((scm_t_uintptr)
     SCM_FRAME_DYNAMIC_LINK (SCM_VM_FRAME_FP (frame)));
}
#undef FUNC_NAME

int
scm_c_frame_previous (enum scm_vm_frame_kind kind, struct scm_frame *frame)
{
  union scm_vm_stack_element *this_fp, *new_fp, *new_sp;
  union scm_vm_stack_element *stack_top = frame_stack_top (kind, frame);

 again:
  this_fp = stack_top - frame->fp_offset;

  if (this_fp == stack_top)
    return 0;

  new_fp = SCM_FRAME_DYNAMIC_LINK (this_fp);

  if (new_fp >= stack_top)
    return 0;

  new_sp = SCM_FRAME_PREVIOUS_SP (this_fp);
  frame->fp_offset = stack_top - new_fp;
  frame->sp_offset = stack_top - new_sp;
  frame->ip = SCM_FRAME_RETURN_ADDRESS (this_fp);

  if (scm_i_vm_is_boot_continuation_code (frame->ip))
    goto again;

  return 1;
}

SCM_DEFINE (scm_frame_previous, "frame-previous", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_previous
{
  enum scm_vm_frame_kind kind;
  struct scm_frame tmp;

  SCM_VALIDATE_VM_FRAME (1, frame);

  kind = SCM_VM_FRAME_KIND (frame);
  memcpy (&tmp, SCM_VM_FRAME_DATA (frame), sizeof tmp);

  if (!scm_c_frame_previous (SCM_VM_FRAME_KIND (frame), &tmp))
    return SCM_BOOL_F;

  return scm_c_make_frame (kind, &tmp);
}
#undef FUNC_NAME


void
scm_init_frames (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/frames.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
