/* Copyright (C) 1998,1999,2000,2001,2002,2003,2004,2008,2009,2010,2011,2012,2013,2014,2015
 * Free Software Foundation, Inc.
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


/* This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 * This file is based upon stklos.c from the STk distribution by
 * Erick Gallesio <eg@unice.fr>.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/dynwind.h"
#include "libguile/eval.h"
#include "libguile/gsubr.h"
#include "libguile/hashtab.h"
#include "libguile/keywords.h"
#include "libguile/macros.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/procprop.h"
#include "libguile/programs.h"
#include "libguile/smob.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/goops.h"

/* Port classes */
#define SCM_IN_PCLASS_INDEX       0
#define SCM_OUT_PCLASS_INDEX      SCM_I_MAX_PORT_TYPE_COUNT
#define SCM_INOUT_PCLASS_INDEX    (2 * SCM_I_MAX_PORT_TYPE_COUNT)

#define SCM_GOOPS_UNBOUND SCM_UNBOUND
#define SCM_GOOPS_UNBOUNDP(x) (scm_is_eq (x, SCM_GOOPS_UNBOUND))

/* Objects have identity, so references to classes and instances are by
   value, not by reference.  Redefinition of a class or modification of
   an instance causes in-place update; you can think of GOOPS as
   building in its own indirection, and for that reason referring to
   GOOPS values by variable reference is unnecessary.

   References to ordinary procedures is by reference (by variable),
   though, as in the rest of Guile.  */

SCM_KEYWORD (k_name, "name");
SCM_KEYWORD (k_setter, "setter");
SCM_SYMBOL (sym_redefined, "redefined");
SCM_GLOBAL_SYMBOL (scm_sym_args, "args");

static int goops_loaded_p = 0;

static SCM var_make_standard_class = SCM_BOOL_F;
static SCM var_change_class = SCM_BOOL_F;
static SCM var_make = SCM_BOOL_F;
static SCM var_inherit_applicable = SCM_BOOL_F;
static SCM var_class_name = SCM_BOOL_F;
static SCM var_class_direct_supers = SCM_BOOL_F;
static SCM var_class_direct_slots = SCM_BOOL_F;
static SCM var_class_direct_subclasses = SCM_BOOL_F;
static SCM var_class_direct_methods = SCM_BOOL_F;
static SCM var_class_precedence_list = SCM_BOOL_F;
static SCM var_class_slots = SCM_BOOL_F;

static SCM var_generic_function_methods = SCM_BOOL_F;
static SCM var_method_generic_function = SCM_BOOL_F;
static SCM var_method_specializers = SCM_BOOL_F;
static SCM var_method_procedure = SCM_BOOL_F;

static SCM var_slot_ref_using_class = SCM_BOOL_F;
static SCM var_slot_set_using_class_x = SCM_BOOL_F;
static SCM var_slot_bound_using_class_p = SCM_BOOL_F;
static SCM var_slot_exists_using_class_p = SCM_BOOL_F;

static SCM var_slot_ref = SCM_BOOL_F;
static SCM var_slot_set_x = SCM_BOOL_F;
static SCM var_slot_bound_p = SCM_BOOL_F;
static SCM var_slot_exists_p = SCM_BOOL_F;

/* These variables are filled in by the object system when loaded. */
static SCM class_boolean, class_char, class_pair;
static SCM class_procedure, class_string, class_symbol;
static SCM class_primitive_generic;
static SCM class_vector, class_null;
static SCM class_integer, class_real, class_complex, class_fraction;
static SCM class_unknown;
static SCM class_top, class_object, class_class;
static SCM class_applicable;
static SCM class_applicable_struct, class_applicable_struct_with_setter;
static SCM class_generic, class_generic_with_setter;
static SCM class_accessor;
static SCM class_extended_generic, class_extended_generic_with_setter;
static SCM class_extended_accessor;
static SCM class_method;
static SCM class_accessor_method;
static SCM class_procedure_class;
static SCM class_applicable_struct_class;
static SCM class_applicable_struct_with_setter_class;
static SCM class_number, class_list;
static SCM class_keyword;
static SCM class_port, class_input_output_port;
static SCM class_input_port, class_output_port;
static SCM class_foreign_slot;
static SCM class_self, class_protected;
static SCM class_hidden, class_opaque, class_read_only;
static SCM class_protected_hidden, class_protected_opaque, class_protected_read_only;
static SCM class_scm;
static SCM class_int, class_float, class_double;

static SCM class_foreign;
static SCM class_hashtable;
static SCM class_fluid;
static SCM class_dynamic_state;
static SCM class_frame;
static SCM class_vm_cont;
static SCM class_bytevector;
static SCM class_uvec;
static SCM class_array;
static SCM class_bitvector;

static SCM vtable_class_map = SCM_BOOL_F;

/* Port classes.  Allocate 3 times the maximum number of port types so that
   input ports, output ports, and in/out ports can be stored at different
   offsets.  See `SCM_IN_PCLASS_INDEX' et al.  */
SCM scm_i_port_class[3 * SCM_I_MAX_PORT_TYPE_COUNT];

/* SMOB classes.  */
SCM scm_i_smob_class[SCM_I_MAX_SMOB_TYPE_COUNT];

static SCM scm_make_unbound (void);
static SCM scm_unbound_p (SCM obj);
static SCM scm_class_p (SCM obj);
static SCM scm_sys_bless_applicable_struct_vtables_x (SCM applicable,
                                                      SCM setter);
static SCM scm_sys_make_root_class (SCM layout);
static SCM scm_sys_init_layout_x (SCM class, SCM layout);
static SCM scm_sys_clear_fields_x (SCM obj);
static SCM scm_sys_goops_early_init (void);
static SCM scm_sys_goops_loaded (void);


/* This function is used for efficient type dispatch.  */
SCM_DEFINE (scm_class_of, "class-of", 1, 0, 0,
	    (SCM x),
	    "Return the class of @var{x}.")
#define FUNC_NAME s_scm_class_of
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return class_integer;

    case scm_tc3_imm24:
      if (SCM_CHARP (x))
	return class_char;
      else if (scm_is_bool (x))
        return class_boolean;
      else if (scm_is_null (x))
        return class_null;
      else
        return class_unknown;

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return class_pair;
	case scm_tc7_symbol:
	  return class_symbol;
	case scm_tc7_vector:
	case scm_tc7_wvect:
	  return class_vector;
	case scm_tc7_pointer:
	  return class_foreign;
	case scm_tc7_hashtable:
	  return class_hashtable;
	case scm_tc7_fluid:
	  return class_fluid;
	case scm_tc7_dynamic_state:
	  return class_dynamic_state;
        case scm_tc7_frame:
	  return class_frame;
        case scm_tc7_keyword:
	  return class_keyword;
        case scm_tc7_vm_cont:
	  return class_vm_cont;
	case scm_tc7_bytevector:
          if (SCM_BYTEVECTOR_ELEMENT_TYPE (x) == SCM_ARRAY_ELEMENT_TYPE_VU8)
            return class_bytevector;
          else
            return class_uvec;
	case scm_tc7_array:
          return class_array;
	case scm_tc7_bitvector:
          return class_bitvector;
	case scm_tc7_string:
	  return class_string;
        case scm_tc7_number:
          switch SCM_TYP16 (x) {
          case scm_tc16_big:
            return class_integer;
          case scm_tc16_real:
            return class_real;
          case scm_tc16_complex:
            return class_complex;
	  case scm_tc16_fraction:
	    return class_fraction;
          }
	case scm_tc7_program:
	  if (SCM_PROGRAM_IS_PRIMITIVE_GENERIC (x)
              && SCM_UNPACK (*SCM_SUBR_GENERIC (x)))
	    return class_primitive_generic;
	  else
	    return class_procedure;

	case scm_tc7_smob:
	  {
	    scm_t_bits type = SCM_TYP16 (x);
	    if (type != scm_tc16_port_with_ps)
	      return scm_i_smob_class[SCM_TC2SMOBNUM (type)];
	    x = SCM_PORT_WITH_PS_PORT (x);
	    /* fall through to ports */
	  }
	case scm_tc7_port:
	  return scm_i_port_class[(SCM_WRTNG & SCM_CELL_WORD_0 (x)
                                   ? (SCM_RDNG & SCM_CELL_WORD_0 (x)
                                      ? SCM_INOUT_PCLASS_INDEX | SCM_PTOBNUM (x)
                                      : SCM_OUT_PCLASS_INDEX | SCM_PTOBNUM (x))
                                   : SCM_IN_PCLASS_INDEX | SCM_PTOBNUM (x))];
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS_VALID)
            /* A GOOPS object with a valid class.  */
	    return SCM_CLASS_OF (x);
	  else if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS)
            /* A GOOPS object whose class might have been redefined.  */
	    {
              SCM class = SCM_CLASS_OF (x);
              SCM new_class = scm_slot_ref (class, sym_redefined);
              if (!scm_is_false (new_class))
		scm_change_object_class (x, class, new_class);
              /* Re-load class from instance.  */
	      return SCM_CLASS_OF (x);
	    }
	  else
            return scm_i_define_class_for_vtable (SCM_CLASS_OF (x));
	default:
	  if (scm_is_pair (x))
	    return class_pair;
	  else
	    return class_unknown;
	}

    case scm_tc3_struct:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
      /* case scm_tc3_unused: */
      /* Never reached */
      break;
    }
  return class_unknown;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_init_layout_x, "%init-layout!", 2, 0, 0,
	    (SCM class, SCM layout),
	    "")
#define FUNC_NAME s_scm_sys_init_layout_x
{
  SCM_VALIDATE_INSTANCE (1, class);
  SCM_ASSERT (!scm_is_symbol (SCM_VTABLE_LAYOUT (class)), class, 1, FUNC_NAME);
  SCM_VALIDATE_STRING (2, layout);

  SCM_SET_VTABLE_LAYOUT (class, scm_make_struct_layout (layout));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_inherit_magic_x, "%inherit-magic!", 2, 0, 0,
	    (SCM class, SCM dsupers),
	    "")
#define FUNC_NAME s_scm_sys_inherit_magic_x
{
  SCM_VALIDATE_INSTANCE (1, class);
  scm_i_struct_inherit_vtable_magic (SCM_CLASS_OF (class), class);
  SCM_SET_CLASS_FLAGS (class, SCM_CLASSF_GOOPS_OR_VALID);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/******************************************************************************/

SCM
scm_make_standard_class (SCM meta, SCM name, SCM dsupers, SCM dslots)
{
  return scm_call_4 (scm_variable_ref (var_make_standard_class),
                     meta, name, dsupers, dslots);
}

/******************************************************************************/

SCM_DEFINE (scm_sys_make_root_class, "%make-root-class", 1, 0, 0,
            (SCM layout),
	    "")
#define FUNC_NAME s_scm_sys_make_root_class
{
  SCM z;

  z = scm_i_make_vtable_vtable (layout);
  SCM_SET_CLASS_FLAGS (z, (SCM_CLASSF_GOOPS_OR_VALID | SCM_CLASSF_METACLASS));

  return z;
}
#undef FUNC_NAME

/******************************************************************************/

SCM_DEFINE (scm_instance_p, "instance?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is an instance.")
#define FUNC_NAME s_scm_instance_p
{
  return scm_from_bool (SCM_INSTANCEP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_p, "class?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a class.")
#define FUNC_NAME s_scm_class_p
{
  return scm_from_bool (SCM_CLASSP (obj));
}
#undef FUNC_NAME

int
scm_is_generic (SCM x)
{
  return SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), class_generic);
}

int
scm_is_method (SCM x)
{
  return SCM_INSTANCEP (x) && SCM_SUBCLASSP (SCM_CLASS_OF (x), class_method);
}

/******************************************************************************
 *
 * Meta object accessors
 *
 ******************************************************************************/

SCM
scm_class_name (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_name), obj);
}

SCM
scm_class_direct_supers (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_direct_supers), obj);
}

SCM
scm_class_direct_slots (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_direct_slots), obj);
}

SCM
scm_class_direct_subclasses (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_direct_subclasses), obj);
}

SCM
scm_class_direct_methods (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_direct_methods), obj);
}

SCM
scm_class_precedence_list (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_precedence_list), obj);
}

SCM
scm_class_slots (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_class_slots), obj);
}

SCM_DEFINE (scm_generic_function_name, "generic-function-name", 1, 0, 0,
            (SCM obj),
	    "Return the name of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_name
{
  SCM_VALIDATE_GENERIC (1, obj);
  return scm_procedure_property (obj, scm_sym_name);
}
#undef FUNC_NAME

SCM
scm_generic_function_methods (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_generic_function_methods), obj);
}

SCM
scm_method_generic_function (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_method_generic_function), obj);
}

SCM
scm_method_specializers (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_method_specializers), obj);
}

SCM
scm_method_procedure (SCM obj)
{
  return scm_call_1 (scm_variable_ref (var_method_procedure), obj);
}

/******************************************************************************
 *
 * S l o t   a c c e s s
 *
 ******************************************************************************/

SCM_DEFINE (scm_make_unbound, "make-unbound", 0, 0, 0,
	    (),
	    "Return the unbound value.")
#define FUNC_NAME s_scm_make_unbound
{
  return SCM_GOOPS_UNBOUND;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unbound_p, "unbound?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is unbound.")
#define FUNC_NAME s_scm_unbound_p
{
  return SCM_GOOPS_UNBOUNDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME




SCM
scm_slot_ref_using_class (SCM class, SCM obj, SCM slot_name)
{
  return scm_call_3 (scm_variable_ref (var_slot_ref_using_class),
                     class, obj, slot_name);
}

SCM
scm_slot_set_using_class_x (SCM class, SCM obj, SCM slot_name, SCM value)
{
  return scm_call_4 (scm_variable_ref (var_slot_set_using_class_x),
                     class, obj, slot_name, value);
}

SCM
scm_slot_bound_using_class_p (SCM class, SCM obj, SCM slot_name)
{
  return scm_call_3 (scm_variable_ref (var_slot_bound_using_class_p),
                     class, obj, slot_name);
}

SCM
scm_slot_exists_using_class_p (SCM class, SCM obj, SCM slot_name)
{
  return scm_call_3 (scm_variable_ref (var_slot_exists_using_class_p),
                     class, obj, slot_name);
}

SCM
scm_slot_ref (SCM obj, SCM slot_name)
{
  return scm_call_2 (scm_variable_ref (var_slot_ref), obj, slot_name);
}

SCM
scm_slot_set_x (SCM obj, SCM slot_name, SCM value)
{
  return scm_call_3 (scm_variable_ref (var_slot_set_x), obj, slot_name, value);
}

SCM
scm_slot_bound_p (SCM obj, SCM slot_name)
{
  return scm_call_2 (scm_variable_ref (var_slot_bound_p), obj, slot_name);
}

SCM
scm_slot_exists_p (SCM obj, SCM slot_name)
{
  return scm_call_2 (scm_variable_ref (var_slot_exists_p), obj, slot_name);
}

SCM_DEFINE (scm_sys_clear_fields_x, "%clear-fields!", 1, 0, 0,
	    (SCM obj),
            "")
#define FUNC_NAME s_scm_sys_clear_fields_x
{
  scm_t_signed_bits n, i;
  SCM vtable, layout;

  SCM_VALIDATE_STRUCT (1, obj);
  vtable = SCM_STRUCT_VTABLE (obj);

  n = SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size);
  layout = SCM_VTABLE_LAYOUT (vtable);

  /* Set all SCM-holding slots to the GOOPS unbound value.  */
  for (i = 0; i < n; i++)
    if (scm_i_symbol_ref (layout, i*2) == 'p')
      SCM_STRUCT_SLOT_SET (obj, i, SCM_GOOPS_UNBOUND);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/******************************************************************************
 *
 * %modify-instance (used by change-class to modify in place)
 *
 ******************************************************************************/

SCM_DEFINE (scm_sys_modify_instance, "%modify-instance", 2, 0, 0,
	    (SCM old, SCM new),
	    "")
#define FUNC_NAME s_scm_sys_modify_instance
{
  SCM_VALIDATE_INSTANCE (1, old);
  SCM_VALIDATE_INSTANCE (2, new);

  /* Exchange the data contained in old and new. We exchange rather than
   * scratch the old value with new to be correct with GC.
   * See "Class redefinition protocol above".
   */
  SCM_CRITICAL_SECTION_START;
  {
    scm_t_bits word0, word1;
    word0 = SCM_CELL_WORD_0 (old);
    word1 = SCM_CELL_WORD_1 (old);
    SCM_SET_CELL_WORD_0 (old, SCM_CELL_WORD_0 (new));
    SCM_SET_CELL_WORD_1 (old, SCM_CELL_WORD_1 (new));
    SCM_SET_CELL_WORD_0 (new, word0);
    SCM_SET_CELL_WORD_1 (new, word1);
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_modify_class, "%modify-class", 2, 0, 0,
	    (SCM old, SCM new),
	    "")
#define FUNC_NAME s_scm_sys_modify_class
{
  SCM_VALIDATE_CLASS (1, old);
  SCM_VALIDATE_CLASS (2, new);

  SCM_CRITICAL_SECTION_START;
  {
    scm_t_bits word0, word1;
    word0 = SCM_CELL_WORD_0 (old);
    word1 = SCM_CELL_WORD_1 (old);
    SCM_SET_CELL_WORD_0 (old, SCM_CELL_WORD_0 (new));
    SCM_SET_CELL_WORD_1 (old, SCM_CELL_WORD_1 (new));
    SCM_STRUCT_DATA (old)[scm_vtable_index_self] = SCM_UNPACK (old);
    SCM_SET_CELL_WORD_0 (new, word0);
    SCM_SET_CELL_WORD_1 (new, word1);
    SCM_STRUCT_DATA (new)[scm_vtable_index_self] = SCM_UNPACK (new);
  }
  SCM_CRITICAL_SECTION_END;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_invalidate_class, "%invalidate-class", 1, 0, 0,
	    (SCM class),
	    "")
#define FUNC_NAME s_scm_sys_invalidate_class
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_CLEAR_CLASS_FLAGS (class, SCM_CLASSF_GOOPS_VALID);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* When instances change class, they finally get a new body, but
 * before that, they go through purgatory in hell.  Odd as it may
 * seem, this data structure saves us from eternal suffering in
 * infinite recursions.
 */

static scm_t_bits **hell;
static long n_hell = 1;		/* one place for the evil one himself */
static long hell_size = 4;
static SCM hell_mutex;

static long
burnin (SCM o)
{
  long i;
  for (i = 1; i < n_hell; ++i)
    if (SCM_STRUCT_DATA (o) == hell[i])
      return i;
  return 0;
}

static void
go_to_hell (void *o)
{
  SCM obj = *(SCM*)o;
  scm_lock_mutex (hell_mutex);
  if (n_hell >= hell_size)
    {
      hell_size *= 2;
      hell = scm_realloc (hell, hell_size * sizeof(*hell));
    }
  hell[n_hell++] = SCM_STRUCT_DATA (obj);
  scm_unlock_mutex (hell_mutex);
}

static void
go_to_heaven (void *o)
{
  SCM obj = *(SCM*)o;
  scm_lock_mutex (hell_mutex);
  hell[burnin (obj)] = hell[--n_hell];
  scm_unlock_mutex (hell_mutex);
}


static SCM
purgatory (SCM obj, SCM new_class)
{
  return scm_call_2 (SCM_VARIABLE_REF (var_change_class), obj, new_class);
}

/* This function calls the generic function change-class for all
 * instances which aren't currently undergoing class change.
 */

void
scm_change_object_class (SCM obj, SCM old_class SCM_UNUSED, SCM new_class)
{
  if (!burnin (obj))
    {
      scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
      scm_dynwind_rewind_handler (go_to_hell, &obj, SCM_F_WIND_EXPLICITLY);
      scm_dynwind_unwind_handler (go_to_heaven, &obj, SCM_F_WIND_EXPLICITLY);
      purgatory (obj, new_class);
      scm_dynwind_end ();
    }
}

/******************************************************************************
 *
 *   GGGG                FFFFF
 *  G                    F
 *  G  GG                FFF
 *  G   G                F
 *   GGG  E N E R I C    F    U N C T I O N S
 *
 * This implementation provides
 *	- generic functions (with class specializers)
 *	- multi-methods
 *	- next-method
 *	- a hard-coded MOP for standard gf, which can be overloaded for non-std gf
 *
 ******************************************************************************/

SCM_DEFINE (scm_generic_capability_p, "generic-capability?", 1, 0, 0,
	    (SCM proc),
	    "")
#define FUNC_NAME s_scm_generic_capability_p
{
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)),
	      proc, SCM_ARG1, FUNC_NAME);
  return (SCM_PRIMITIVE_GENERIC_P (proc) ? SCM_BOOL_T : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_enable_primitive_generic_x, "enable-primitive-generic!", 0, 0, 1,
	    (SCM subrs),
	    "")
#define FUNC_NAME s_scm_enable_primitive_generic_x
{
  SCM_VALIDATE_REST_ARGUMENT (subrs);
  while (!scm_is_null (subrs))
    {
      SCM subr = SCM_CAR (subrs);
      SCM_ASSERT (SCM_PRIMITIVE_GENERIC_P (subr), subr, SCM_ARGn, FUNC_NAME);
      SCM_SET_SUBR_GENERIC (subr,
                            scm_make (scm_list_3 (class_generic,
                                                  k_name,
                                                  SCM_SUBR_NAME (subr))));
      subrs = SCM_CDR (subrs);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_primitive_generic_x, "set-primitive-generic!", 2, 0, 0,
	    (SCM subr, SCM generic),
	    "")
#define FUNC_NAME s_scm_set_primitive_generic_x
{
  SCM_ASSERT (SCM_PRIMITIVE_GENERIC_P (subr), subr, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_GENERICP (generic), generic, SCM_ARG2, FUNC_NAME);
  SCM_SET_SUBR_GENERIC (subr, generic);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_primitive_generic_generic, "primitive-generic-generic", 1, 0, 0,
	    (SCM subr),
	    "")
#define FUNC_NAME s_scm_primitive_generic_generic
{
  if (SCM_PRIMITIVE_GENERIC_P (subr))
    {
      if (!SCM_UNPACK (*SCM_SUBR_GENERIC (subr)))
	scm_enable_primitive_generic_x (scm_list_1 (subr));
      return *SCM_SUBR_GENERIC (subr);
    }
  SCM_WRONG_TYPE_ARG (SCM_ARG1, subr);
}
#undef FUNC_NAME

/* Dirk:FIXME:: In all of these scm_wta_dispatch_* routines it is
 * assumed that 'gf' is zero if uninitialized.  It would be cleaner if
 * some valid SCM value like SCM_BOOL_F or SCM_UNDEFINED were chosen.
 */

SCM
scm_wta_dispatch_0 (SCM gf, const char *subr)
{
  if (!SCM_UNPACK (gf))
    scm_error_num_args_subr (subr);

  return scm_call_0 (gf);
}

SCM
scm_wta_dispatch_1 (SCM gf, SCM a1, int pos, const char *subr)
{
  if (!SCM_UNPACK (gf))
    scm_wrong_type_arg (subr, pos, a1);

  return scm_call_1 (gf, a1);
}

SCM
scm_wta_dispatch_2 (SCM gf, SCM a1, SCM a2, int pos, const char *subr)
{
  if (!SCM_UNPACK (gf))
    scm_wrong_type_arg (subr, pos, (pos == SCM_ARG1) ? a1 : a2);

  return scm_call_2 (gf, a1, a2);
}

SCM
scm_wta_dispatch_n (SCM gf, SCM args, int pos, const char *subr)
{
  if (!SCM_UNPACK (gf))
    scm_wrong_type_arg (subr, pos, scm_list_ref (args, scm_from_int (pos)));

  return scm_apply_0 (gf, args);
}

/******************************************************************************
 *
 * Protocol for calling a generic fumction
 * This protocol is roughly equivalent to (parameter are a little bit different
 * for efficiency reasons):
 *
 * 	+ apply-generic (gf args)
 *		+ compute-applicable-methods (gf args ...)
 *			+ sort-applicable-methods (methods args)
 *		+ apply-methods (gf methods args)
 *
 * apply-methods calls make-next-method to build the "continuation" of a a
 * method.  Applying a next-method will call apply-next-method which in
 * turn will call  apply again to call effectively the following method.
 *
 ******************************************************************************/

SCM_DEFINE (scm_make, "make",  0, 0, 1,
	    (SCM args),
	    "Make a new object.  @var{args} must contain the class and\n"
	    "all necessary initialization information.")
#define FUNC_NAME s_scm_make
{
  return scm_apply_0 (scm_variable_ref (var_make), args);
}
#undef FUNC_NAME


/**********************************************************************
 *
 * Smob classes
 *
 **********************************************************************/

static SCM
make_class_name (const char *prefix, const char *type_name, const char *suffix)
{
  if (!type_name)
    type_name = "";
  return scm_string_to_symbol (scm_string_append
                               (scm_list_3 (scm_from_utf8_string (prefix),
                                            scm_from_utf8_string (type_name),
                                            scm_from_utf8_string (suffix))));
}

SCM
scm_make_extended_class (char const *type_name, int applicablep)
{
  SCM name, meta, supers;

  name = make_class_name ("<", type_name, ">");
  meta = class_class;

  if (applicablep)
    supers = scm_list_1 (class_applicable);
  else
    supers = scm_list_1 (class_top);

  return scm_make_standard_class (meta, name, supers, SCM_EOL);
}

void
scm_i_inherit_applicable (SCM c)
{
  scm_call_1 (scm_variable_ref (var_inherit_applicable), c);
}

static void
create_smob_classes (void)
{
  long i;

  for (i = 0; i < SCM_I_MAX_SMOB_TYPE_COUNT; ++i)
    scm_i_smob_class[i] = SCM_BOOL_F;

  for (i = 0; i < scm_numsmob; ++i)
    if (scm_is_false (scm_i_smob_class[i]))
      scm_i_smob_class[i] = scm_make_extended_class (SCM_SMOBNAME (i),
                                                     scm_smobs[i].apply != 0);
}

void
scm_make_port_classes (long ptobnum, char *type_name)
{
  SCM name, meta, super, supers;

  meta = class_class;

  name = make_class_name ("<", type_name, "-port>");
  supers = scm_list_1 (class_port);
  super = scm_make_standard_class (meta, name, supers, SCM_EOL);

  name = make_class_name ("<", type_name, "-input-port>");
  supers = scm_list_2 (super, class_input_port);
  scm_i_port_class[SCM_IN_PCLASS_INDEX + ptobnum]
    = scm_make_standard_class (meta, name, supers, SCM_EOL);

  name = make_class_name ("<", type_name, "-output-port>");
  supers = scm_list_2 (super, class_output_port);
  scm_i_port_class[SCM_OUT_PCLASS_INDEX + ptobnum]
    = scm_make_standard_class (meta, name, supers, SCM_EOL);

  name = make_class_name ("<", type_name, "-input-output-port>");
  supers = scm_list_2 (super, class_input_output_port);
  scm_i_port_class[SCM_INOUT_PCLASS_INDEX + ptobnum]
    = scm_make_standard_class (meta, name, supers, SCM_EOL);
}

static void
create_port_classes (void)
{
  long i;

  for (i = scm_c_num_port_types () - 1; i >= 0; i--)
    scm_make_port_classes (i, SCM_PTOBNAME (i));
}

SCM
scm_i_define_class_for_vtable (SCM vtable)
{
  SCM class;

  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  if (scm_is_false (vtable_class_map))
    vtable_class_map = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  if (scm_is_false (scm_struct_vtable_p (vtable)))
    abort ();

  class = scm_weak_table_refq (vtable_class_map, vtable, SCM_BOOL_F);

  if (scm_is_false (class))
    {
      if (SCM_UNPACK (class_class))
        {
          SCM name, meta, supers;

          name = SCM_VTABLE_NAME (vtable);
          if (scm_is_symbol (name))
            name = scm_string_to_symbol
              (scm_string_append
               (scm_list_3 (scm_from_latin1_string ("<"),
                            scm_symbol_to_string (name),
                            scm_from_latin1_string (">"))));
          else
            name = scm_from_latin1_symbol ("<>");

          if (SCM_STRUCT_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SETTER))
            {
              meta = class_applicable_struct_with_setter_class;
              supers = scm_list_1 (class_applicable_struct_with_setter);
            }
          else if (SCM_STRUCT_VTABLE_FLAG_IS_SET (vtable,
                                                  SCM_VTABLE_FLAG_APPLICABLE))
            {
              meta = class_applicable_struct_class;
              supers = scm_list_1 (class_applicable_struct);
            }
          else
            {
              meta = class_class;
              supers = scm_list_1 (class_top);
            }

          return scm_make_standard_class (meta, name, supers, SCM_EOL);
        }
      else
        /* `create_struct_classes' will fill this in later.  */
        class = SCM_BOOL_F;

      /* Don't worry about races.  This only happens when creating a
         vtable, which happens by definition in one thread.  */
      scm_weak_table_putq_x (vtable_class_map, vtable, class);
    }

  return class;
}

static SCM
make_struct_class (void *closure SCM_UNUSED,
		   SCM vtable, SCM data, SCM prev SCM_UNUSED)
{
  if (scm_is_false (data))
    scm_i_define_class_for_vtable (vtable);
  return SCM_UNSPECIFIED;
}

static void
create_struct_classes (void)
{
  /* FIXME: take the vtable_class_map while initializing goops?  */
  scm_internal_hash_fold (make_struct_class, 0, SCM_BOOL_F,
                          vtable_class_map);
}

/**********************************************************************
 *
 * C interface
 *
 **********************************************************************/

void
scm_load_goops ()
{
  if (!goops_loaded_p)
    scm_c_resolve_module ("oop goops");
}

SCM
scm_ensure_accessor (SCM name)
{
  SCM var, gf;

  var = scm_module_variable (scm_current_module (), name);
  if (SCM_VARIABLEP (var) && !SCM_UNBNDP (SCM_VARIABLE_REF (var)))
    gf = SCM_VARIABLE_REF (var);
  else
    gf = SCM_BOOL_F;

  if (!SCM_IS_A_P (gf, class_accessor))
    {
      gf = scm_make (scm_list_3 (class_generic, k_name, name));
      gf = scm_make (scm_list_5 (class_accessor,
				 k_name, name, k_setter, gf));
    }

  return gf;
}

/*
 * Initialization
 */

SCM_DEFINE (scm_sys_bless_applicable_struct_vtables_x, "%bless-applicable-struct-vtables!", 2, 0, 0,
	    (SCM applicable, SCM setter),
	    "")
#define FUNC_NAME s_scm_sys_bless_applicable_struct_vtables_x
{
  SCM_VALIDATE_CLASS (1, applicable);
  SCM_VALIDATE_CLASS (2, setter);
  SCM_SET_VTABLE_FLAGS (applicable, SCM_VTABLE_FLAG_APPLICABLE_VTABLE);
  SCM_SET_VTABLE_FLAGS (setter, SCM_VTABLE_FLAG_SETTER_VTABLE);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_goops_early_init, "%goops-early-init", 0, 0, 0,
	    (),
	    "")
#define FUNC_NAME s_scm_sys_goops_early_init
{
  var_make_standard_class = scm_c_lookup ("make-standard-class");
  var_make = scm_c_lookup ("make");
  var_inherit_applicable = scm_c_lookup ("inherit-applicable!");

  /* For SCM_SUBCLASSP.  */
  var_class_precedence_list = scm_c_lookup ("class-precedence-list");

  var_slot_ref_using_class = scm_c_lookup ("slot-ref-using-class");
  var_slot_set_using_class_x = scm_c_lookup ("slot-set-using-class!");
  var_slot_bound_using_class_p = scm_c_lookup ("slot-bound-using-class?");
  var_slot_exists_using_class_p = scm_c_lookup ("slot-exists-using-class?");

  var_slot_ref = scm_c_lookup ("slot-ref");
  var_slot_set_x = scm_c_lookup ("slot-set!");
  var_slot_bound_p = scm_c_lookup ("slot-bound?");
  var_slot_exists_p = scm_c_lookup ("slot-exists?");

  class_class = scm_variable_ref (scm_c_lookup ("<class>"));
  class_top = scm_variable_ref (scm_c_lookup ("<top>"));
  class_object = scm_variable_ref (scm_c_lookup ("<object>"));

  class_foreign_slot = scm_variable_ref (scm_c_lookup ("<foreign-slot>"));
  class_protected = scm_variable_ref (scm_c_lookup ("<protected-slot>"));
  class_hidden = scm_variable_ref (scm_c_lookup ("<hidden-slot>"));
  class_opaque = scm_variable_ref (scm_c_lookup ("<opaque-slot>"));
  class_read_only = scm_variable_ref (scm_c_lookup ("<read-only-slot>"));
  class_self = scm_variable_ref (scm_c_lookup ("<self-slot>"));
  class_protected_opaque = scm_variable_ref (scm_c_lookup ("<protected-opaque-slot>"));
  class_protected_hidden = scm_variable_ref (scm_c_lookup ("<protected-hidden-slot>"));
  class_protected_read_only = scm_variable_ref (scm_c_lookup ("<protected-read-only-slot>"));
  class_scm = scm_variable_ref (scm_c_lookup ("<scm-slot>"));
  class_int = scm_variable_ref (scm_c_lookup ("<int-slot>"));
  class_float = scm_variable_ref (scm_c_lookup ("<float-slot>"));
  class_double = scm_variable_ref (scm_c_lookup ("<double-slot>"));

  /* Applicables */
  class_procedure_class = scm_variable_ref (scm_c_lookup ("<procedure-class>"));
  class_applicable_struct_class = scm_variable_ref (scm_c_lookup ("<applicable-struct-class>"));
  class_applicable_struct_with_setter_class =
    scm_variable_ref (scm_c_lookup ("<applicable-struct-with-setter-class>"));

  class_method = scm_variable_ref (scm_c_lookup ("<method>"));
  class_accessor_method = scm_variable_ref (scm_c_lookup ("<accessor-method>"));
  class_applicable = scm_variable_ref (scm_c_lookup ("<applicable>"));
  class_applicable_struct = scm_variable_ref (scm_c_lookup ("<applicable-struct>"));
  class_applicable_struct_with_setter = scm_variable_ref (scm_c_lookup ("<applicable-struct-with-setter>"));
  class_generic = scm_variable_ref (scm_c_lookup ("<generic>"));
  class_extended_generic = scm_variable_ref (scm_c_lookup ("<extended-generic>"));
  class_generic_with_setter = scm_variable_ref (scm_c_lookup ("<generic-with-setter>"));
  class_accessor = scm_variable_ref (scm_c_lookup ("<accessor>"));
  class_extended_generic_with_setter = scm_variable_ref (scm_c_lookup ("<extended-generic-with-setter>"));
  class_extended_accessor = scm_variable_ref (scm_c_lookup ("<extended-accessor>"));

  /* Primitive types classes */
  class_boolean = scm_variable_ref (scm_c_lookup ("<boolean>"));
  class_char = scm_variable_ref (scm_c_lookup ("<char>"));
  class_list = scm_variable_ref (scm_c_lookup ("<list>"));
  class_pair = scm_variable_ref (scm_c_lookup ("<pair>"));
  class_null = scm_variable_ref (scm_c_lookup ("<null>"));
  class_string = scm_variable_ref (scm_c_lookup ("<string>"));
  class_symbol = scm_variable_ref (scm_c_lookup ("<symbol>"));
  class_vector = scm_variable_ref (scm_c_lookup ("<vector>"));
  class_foreign = scm_variable_ref (scm_c_lookup ("<foreign>"));
  class_hashtable = scm_variable_ref (scm_c_lookup ("<hashtable>"));
  class_fluid = scm_variable_ref (scm_c_lookup ("<fluid>"));
  class_dynamic_state = scm_variable_ref (scm_c_lookup ("<dynamic-state>"));
  class_frame = scm_variable_ref (scm_c_lookup ("<frame>"));
  class_vm_cont = scm_variable_ref (scm_c_lookup ("<vm-continuation>"));
  class_bytevector = scm_variable_ref (scm_c_lookup ("<bytevector>"));
  class_uvec = scm_variable_ref (scm_c_lookup ("<uvec>"));
  class_array = scm_variable_ref (scm_c_lookup ("<array>"));
  class_bitvector = scm_variable_ref (scm_c_lookup ("<bitvector>"));
  class_number = scm_variable_ref (scm_c_lookup ("<number>"));
  class_complex = scm_variable_ref (scm_c_lookup ("<complex>"));
  class_real = scm_variable_ref (scm_c_lookup ("<real>"));
  class_integer = scm_variable_ref (scm_c_lookup ("<integer>"));
  class_fraction = scm_variable_ref (scm_c_lookup ("<fraction>"));
  class_keyword = scm_variable_ref (scm_c_lookup ("<keyword>"));
  class_unknown = scm_variable_ref (scm_c_lookup ("<unknown>"));
  class_procedure = scm_variable_ref (scm_c_lookup ("<procedure>"));
  class_primitive_generic = scm_variable_ref (scm_c_lookup ("<primitive-generic>"));
  class_port = scm_variable_ref (scm_c_lookup ("<port>"));
  class_input_port = scm_variable_ref (scm_c_lookup ("<input-port>"));
  class_output_port = scm_variable_ref (scm_c_lookup ("<output-port>"));
  class_input_output_port = scm_variable_ref (scm_c_lookup ("<input-output-port>"));

  create_smob_classes ();
  create_struct_classes ();
  create_port_classes ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_goops_loaded, "%goops-loaded", 0, 0, 0,
	    (),
	    "Announce that GOOPS is loaded and perform initialization\n"
	    "on the C level which depends on the loaded GOOPS modules.")
#define FUNC_NAME s_scm_sys_goops_loaded
{
  goops_loaded_p = 1;
  var_class_name = scm_c_lookup ("class-name");
  var_class_direct_supers = scm_c_lookup ("class-direct-supers");
  var_class_direct_slots = scm_c_lookup ("class-direct-slots");
  var_class_direct_subclasses = scm_c_lookup ("class-direct-subclasses");
  var_class_direct_methods = scm_c_lookup ("class-direct-methods");
  var_class_slots = scm_c_lookup ("class-slots");

  var_generic_function_methods = scm_c_lookup ("generic-function-methods");
  var_method_generic_function = scm_c_lookup ("method-generic-function");
  var_method_specializers = scm_c_lookup ("method-specializers");
  var_method_procedure = scm_c_lookup ("method-procedure");

  var_change_class = scm_c_lookup ("change-class");

#if (SCM_ENABLE_DEPRECATED == 1)
  scm_init_deprecated_goops ();
#endif

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM scm_module_goops;

static void
scm_init_goops_builtins (void *unused)
{
  scm_module_goops = scm_current_module ();

  hell = scm_calloc (hell_size * sizeof (*hell));
  hell_mutex = scm_make_mutex ();

#include "libguile/goops.x"
}

void
scm_init_goops ()
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_goops_builtins", scm_init_goops_builtins,
                            NULL);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
