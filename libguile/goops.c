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

#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/debug.h"
#include "libguile/dynl.h"
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
#include "libguile/random.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/vm.h"

#include "libguile/validate.h"
#include "libguile/goops.h"

/* Port classes */
#define SCM_IN_PCLASS_INDEX       0
#define SCM_OUT_PCLASS_INDEX      SCM_I_MAX_PORT_TYPE_COUNT
#define SCM_INOUT_PCLASS_INDEX    (2 * SCM_I_MAX_PORT_TYPE_COUNT)

/* Objects have identity, so references to classes and instances are by
   value, not by reference.  Redefinition of a class or modification of
   an instance causes in-place update; you can think of GOOPS as
   building in its own indirection, and for that reason referring to
   GOOPS values by variable reference is unnecessary.

   References to ordinary procedures is by reference (by variable),
   though, as in the rest of Guile.  */

static SCM var_make_standard_class = SCM_BOOL_F;
static SCM var_slot_unbound = SCM_BOOL_F;
static SCM var_slot_missing = SCM_BOOL_F;
static SCM var_no_applicable_method = SCM_BOOL_F;
static SCM var_change_class = SCM_BOOL_F;
static SCM var_make = SCM_BOOL_F;

SCM_SYMBOL (sym_slot_unbound, "slot-unbound");
SCM_SYMBOL (sym_slot_missing, "slot-missing");
SCM_SYMBOL (sym_no_applicable_method, "no-applicable-method");
SCM_SYMBOL (sym_memoize_method_x, "memoize-method!");
SCM_SYMBOL (sym_change_class, "change-class");

SCM_VARIABLE (scm_var_make_extended_generic, "make-extended-generic");


/* Class redefinition protocol:

   A class is represented by a heap header h1 which points to a
   malloc:ed memory block m1.

   When a new version of a class is created, a new header h2 and
   memory block m2 are allocated.  The headers h1 and h2 then switch
   pointers so that h1 refers to m2 and h2 to m1.  In this way, names
   bound to h1 will point to the new class at the same time as h2 will
   be a handle which the GC will use to free m1.

   The `redefined' slot of m1 will be set to point to h1.  An old
   instance will have its class pointer (the CAR of the heap header)
   pointing to m1.  The non-immediate `redefined'-slot in m1 indicates
   the class modification and the new class pointer can be found via
   h1.
*/

#define TEST_CHANGE_CLASS(obj, class)				       \
	{							       \
	  class = SCM_CLASS_OF (obj);				       \
          if (scm_is_true (SCM_OBJ_CLASS_REDEF (obj)))		       \
	    {							       \
	      scm_change_object_class (obj, class, SCM_OBJ_CLASS_REDEF (obj));\
	      class = SCM_CLASS_OF (obj);			       \
	    }							       \
	}

#define SCM_GOOPS_UNBOUND SCM_UNBOUND
#define SCM_GOOPS_UNBOUNDP(x) (scm_is_eq (x, SCM_GOOPS_UNBOUND))

static int goops_loaded_p = 0;
static scm_t_rstate *goops_rstate;

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
static SCM scm_sys_bless_applicable_struct_vtables_x (SCM applicable,
                                                      SCM setter);
static SCM scm_sys_bless_pure_generic_vtable_x (SCM vtable);
static SCM scm_sys_make_root_class (SCM name, SCM dslots,
                                    SCM getters_n_setters);
static SCM scm_sys_init_layout_x (SCM class, SCM layout);
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
	    return SCM_CLASS_OF (x);
	  else if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS)
	    {
	      /* Goops object */
	      if (! scm_is_false (SCM_OBJ_CLASS_REDEF (x)))
		scm_change_object_class (x,
					 SCM_CLASS_OF (x),         /* old */
					 SCM_OBJ_CLASS_REDEF (x)); /* new */
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

/******************************************************************************
 *
 * initialize-object
 *
 ******************************************************************************/

/*fixme* Manufacture keywords in advance */
SCM
scm_i_get_keyword (SCM key, SCM l, long len, SCM default_value, const char *subr)
{
  long i;

  for (i = 0; i != len; i += 2)
    {
      SCM obj = SCM_CAR (l);

      if (!scm_is_keyword (obj))
	scm_misc_error (subr, "bad keyword: ~S", scm_list_1 (obj));
      else if (scm_is_eq (obj, key))
	return SCM_CADR (l);
      else
	l = SCM_CDDR (l);
    }

  return default_value;
}


SCM_DEFINE (scm_get_keyword, "get-keyword", 3, 0, 0,
	    (SCM key, SCM l, SCM default_value),
	    "Determine an associated value for the keyword @var{key} from\n"
	    "the list @var{l}.  The list @var{l} has to consist of an even\n"
	    "number of elements, where, starting with the first, every\n"
	    "second element is a keyword, followed by its associated value.\n"
	    "If @var{l} does not hold a value for @var{key}, the value\n"
	    "@var{default_value} is returned.")
#define FUNC_NAME s_scm_get_keyword
{
  long len;

  SCM_ASSERT (scm_is_keyword (key), key, SCM_ARG1, FUNC_NAME);
  len = scm_ilength (l);
  if (len < 0 || len % 2 == 1)
    scm_misc_error (FUNC_NAME, "Bad keyword-value list: ~S", scm_list_1 (l));

  return scm_i_get_keyword (key, l, len, default_value, FUNC_NAME);
}
#undef FUNC_NAME


SCM_KEYWORD (k_init_keyword, "init-keyword");

static SCM get_slot_value (SCM class, SCM obj, SCM slotdef);
static SCM set_slot_value (SCM class, SCM obj, SCM slotdef, SCM value);

SCM_DEFINE (scm_sys_initialize_object, "%initialize-object", 2, 0, 0,
	    (SCM obj, SCM initargs),
	    "Initialize the object @var{obj} with the given arguments\n"
	    "@var{initargs}.")
#define FUNC_NAME s_scm_sys_initialize_object
{
  SCM tmp, get_n_set, slots;
  SCM class       = SCM_CLASS_OF (obj);
  long n_initargs;

  SCM_VALIDATE_INSTANCE (1, obj);
  n_initargs = scm_ilength (initargs);
  SCM_ASSERT ((n_initargs & 1) == 0, initargs, SCM_ARG2, FUNC_NAME);

  get_n_set = SCM_SLOT (class, scm_si_getters_n_setters);
  slots     = SCM_SLOT (class, scm_si_slots);

  /* See for each slot how it must be initialized */
  for (;
       !scm_is_null (slots);
       get_n_set = SCM_CDR (get_n_set), slots = SCM_CDR (slots))
    {
      SCM slot_name  = SCM_CAR (slots);
      SCM slot_value = SCM_GOOPS_UNBOUND;

      if (!scm_is_null (SCM_CDR (slot_name)))
	{
	  /* This slot admits (perhaps) to be initialized at creation time */
	  long n = scm_ilength (SCM_CDR (slot_name));
	  if (n & 1) /* odd or -1 */
	    SCM_MISC_ERROR ("class contains bogus slot definition: ~S",
			    scm_list_1 (slot_name));
	  tmp 	= scm_i_get_keyword (k_init_keyword,
				     SCM_CDR (slot_name),
				     n,
				     SCM_PACK (0),
				     FUNC_NAME);
	  slot_name = SCM_CAR (slot_name);
	  if (SCM_UNPACK (tmp))
	    {
	      /* an initarg was provided for this slot */
	      if (!scm_is_keyword (tmp))
		SCM_MISC_ERROR ("initarg must be a keyword. It was ~S",
				scm_list_1 (tmp));
	      slot_value = scm_i_get_keyword (tmp,
					      initargs,
					      n_initargs,
					      SCM_GOOPS_UNBOUND,
					      FUNC_NAME);
	    }
	}

      if (!SCM_GOOPS_UNBOUNDP (slot_value))
	/* set slot to provided value */
	set_slot_value (class, obj, SCM_CAR (get_n_set), slot_value);
      else
	{
	  /* set slot to its :init-form if it exists */
	  tmp = SCM_CADAR (get_n_set);
	  if (scm_is_true (tmp))
            set_slot_value (class,
                            obj,
                            SCM_CAR (get_n_set),
                            scm_call_0 (tmp));
	}
    }

  return obj;
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

SCM_DEFINE (scm_sys_make_root_class, "%make-root-class", 3, 0, 0,
	    (SCM name, SCM dslots, SCM getters_n_setters),
	    "")
#define FUNC_NAME s_scm_sys_make_root_class
{
  SCM cs, z;

  cs = scm_from_locale_string (SCM_CLASS_CLASS_LAYOUT);
  z = scm_i_make_vtable_vtable (cs);
  SCM_SET_CLASS_FLAGS (z, (SCM_CLASSF_GOOPS_OR_VALID
                           | SCM_CLASSF_METACLASS));

  SCM_SET_SLOT (z, scm_vtable_index_name, name);
  SCM_SET_SLOT (z, scm_si_direct_supers, SCM_EOL);  /* will be changed */
  SCM_SET_SLOT (z, scm_si_direct_slots, dslots); /* will be changed */
  SCM_SET_SLOT (z, scm_si_direct_subclasses, SCM_EOL);
  SCM_SET_SLOT (z, scm_si_direct_methods, SCM_EOL);
  SCM_SET_SLOT (z, scm_si_cpl, SCM_EOL);  /* will be changed */
  SCM_SET_SLOT (z, scm_si_slots, dslots); /* will be changed */
  SCM_SET_SLOT (z, scm_si_nfields, scm_from_int (SCM_N_CLASS_SLOTS));
  SCM_SET_SLOT (z, scm_si_getters_n_setters, getters_n_setters); /* will be changed */
  SCM_SET_SLOT (z, scm_si_redefined, SCM_BOOL_F);

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

SCM_SYMBOL (sym_procedure, "procedure");
SCM_SYMBOL (sym_direct_supers, "direct-supers");
SCM_SYMBOL (sym_direct_slots, "direct-slots");
SCM_SYMBOL (sym_direct_subclasses, "direct-subclasses");
SCM_SYMBOL (sym_direct_methods, "direct-methods");
SCM_SYMBOL (sym_cpl, "cpl");
SCM_SYMBOL (sym_slots, "slots");

SCM_DEFINE (scm_class_name, "class-name",  1, 0, 0,
	    (SCM obj),
	    "Return the class name of @var{obj}.")
#define FUNC_NAME s_scm_class_name
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, scm_sym_name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_supers, "class-direct-supers", 1, 0, 0,
	    (SCM obj),
	    "Return the direct superclasses of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_supers
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_supers);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_slots, "class-direct-slots", 1, 0, 0,
	    (SCM obj),
	    "Return the direct slots of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_slots
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_slots);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_subclasses, "class-direct-subclasses", 1, 0, 0,
	    (SCM obj),
	    "Return the direct subclasses of the class @var{obj}.")
#define FUNC_NAME s_scm_class_direct_subclasses
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref(obj, sym_direct_subclasses);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_direct_methods, "class-direct-methods", 1, 0, 0,
	    (SCM obj),
	    "Return the direct methods of the class @var{obj}")
#define FUNC_NAME s_scm_class_direct_methods
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_direct_methods);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_precedence_list, "class-precedence-list", 1, 0, 0,
	    (SCM obj),
	    "Return the class precedence list of the class @var{obj}.")
#define FUNC_NAME s_scm_class_precedence_list
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_cpl);
}
#undef FUNC_NAME

SCM_DEFINE (scm_class_slots, "class-slots", 1, 0, 0,
	    (SCM obj),
	    "Return the slot list of the class @var{obj}.")
#define FUNC_NAME s_scm_class_slots
{
  SCM_VALIDATE_CLASS (1, obj);
  return scm_slot_ref (obj, sym_slots);
}
#undef FUNC_NAME

SCM_DEFINE (scm_generic_function_name, "generic-function-name", 1, 0, 0,
	    (SCM obj),
	    "Return the name of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_name
{
  SCM_VALIDATE_GENERIC (1, obj);
  return scm_procedure_property (obj, scm_sym_name);
}
#undef FUNC_NAME

SCM_SYMBOL (sym_methods, "methods");
SCM_SYMBOL (sym_extended_by, "extended-by");
SCM_SYMBOL (sym_extends, "extends");

static
SCM fold_downward_gf_methods (SCM method_lists, SCM gf)
{
  SCM gfs = scm_slot_ref (gf, sym_extended_by);
  method_lists = scm_cons (scm_slot_ref (gf, sym_methods), method_lists);
  while (!scm_is_null (gfs))
    {
      method_lists = fold_downward_gf_methods (method_lists, SCM_CAR (gfs));
      gfs = SCM_CDR (gfs);
    }
  return method_lists;
}

static
SCM fold_upward_gf_methods (SCM method_lists, SCM gf)
{
  if (SCM_IS_A_P (gf, class_extended_generic))
    {
      SCM gfs = scm_slot_ref (gf, sym_extends);
      while (!scm_is_null (gfs))
	{
	  SCM methods = scm_slot_ref (SCM_CAR (gfs), sym_methods);
	  method_lists = fold_upward_gf_methods (scm_cons (methods,
							   method_lists),
						 SCM_CAR (gfs));
	  gfs = SCM_CDR (gfs);
	}
    }
  return method_lists;
}

SCM_DEFINE (scm_generic_function_methods, "generic-function-methods", 1, 0, 0,
	    (SCM obj),
	    "Return the methods of the generic function @var{obj}.")
#define FUNC_NAME s_scm_generic_function_methods
{
  SCM methods;
  SCM_VALIDATE_GENERIC (1, obj);
  methods = fold_upward_gf_methods (SCM_EOL, obj);
  methods = fold_downward_gf_methods (methods, obj);
  return scm_append (methods);
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_generic_function, "method-generic-function", 1, 0, 0,
	    (SCM obj),
	    "Return the generic function for the method @var{obj}.")
#define FUNC_NAME s_scm_method_generic_function
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, scm_from_latin1_symbol ("generic-function"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_specializers, "method-specializers", 1, 0, 0,
	    (SCM obj),
	    "Return specializers of the method @var{obj}.")
#define FUNC_NAME s_scm_method_specializers
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, scm_from_latin1_symbol ("specializers"));
}
#undef FUNC_NAME

SCM_DEFINE (scm_method_procedure, "method-procedure", 1, 0, 0,
	    (SCM obj),
	    "Return the procedure of the method @var{obj}.")
#define FUNC_NAME s_scm_method_procedure
{
  SCM_VALIDATE_METHOD (1, obj);
  return scm_slot_ref (obj, sym_procedure);
}
#undef FUNC_NAME

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



/** Utilities **/

/* In the future, this function will return the effective slot
 * definition associated with SLOT_NAME.  Now it just returns some of
 * the information which will be stored in the effective slot
 * definition.
 */

static SCM
slot_definition_using_name (SCM class, SCM slot_name)
{
  register SCM slots = SCM_SLOT (class, scm_si_getters_n_setters);
  for (; !scm_is_null (slots); slots = SCM_CDR (slots))
    if (scm_is_eq (SCM_CAAR (slots), slot_name))
      return SCM_CAR (slots);
  return SCM_BOOL_F;
}

static SCM
get_slot_value (SCM class SCM_UNUSED, SCM obj, SCM slotdef)
#define FUNC_NAME "%get-slot-value"
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (car access) is the getter function to apply
   *
   * Instances have never more than SCM_MOST_POSITIVE_FIXNUM slots, so
   * we can just assume fixnums here.
   */
  if (SCM_I_INUMP (access))
    /* Don't poke at the slots directly, because scm_struct_ref handles the
       access bits for us. */
    return scm_struct_ref (obj, access);
  else
    return scm_call_1 (SCM_CAR (access), obj);
}
#undef FUNC_NAME

static SCM
get_slot_value_using_name (SCM class, SCM obj, SCM slot_name)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (scm_is_true (slotdef))
    return get_slot_value (class, obj, slotdef);
  else
    return scm_call_3 (SCM_VARIABLE_REF (var_slot_missing), class, obj, slot_name);
}

static SCM
set_slot_value (SCM class SCM_UNUSED, SCM obj, SCM slotdef, SCM value)
#define FUNC_NAME "%set-slot-value"
{
  SCM access = SCM_CDDR (slotdef);
  /* Two cases here:
   *	- access is an integer (the offset of this slot in the slots vector)
   *	- otherwise (cadr access) is the setter function to apply
   *
   * Instances have never more than SCM_MOST_POSITIVE_FIXNUM slots, so
   * we can just assume fixnums here.
   */
  if (SCM_I_INUMP (access))
    /* obey permissions bits via going through struct-set! */
    scm_struct_set_x (obj, access, value);
  else
    /* ((cadr l) obj value) */
    scm_call_2 (SCM_CADR (access), obj, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
set_slot_value_using_name (SCM class, SCM obj, SCM slot_name, SCM value)
{
  SCM slotdef = slot_definition_using_name (class, slot_name);
  if (scm_is_true (slotdef))
    return set_slot_value (class, obj, slotdef, value);
  else
    return scm_call_4 (SCM_VARIABLE_REF (var_slot_missing), class, obj, slot_name, value);
}

static SCM
test_slot_existence (SCM class SCM_UNUSED, SCM obj, SCM slot_name)
{
  register SCM l;

  for (l = SCM_ACCESSORS_OF (obj); !scm_is_null (l); l = SCM_CDR (l))
    if (scm_is_eq (SCM_CAAR (l), slot_name))
      return SCM_BOOL_T;

  return SCM_BOOL_F;
}

		/* ======================================== */

SCM_DEFINE (scm_slot_ref_using_class, "slot-ref-using-class", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_ref_using_class
{
  SCM res;

  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return scm_call_3 (SCM_VARIABLE_REF (var_slot_unbound), class, obj, slot_name);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_slot_set_using_class_x, "slot-set-using-class!", 4, 0, 0,
	    (SCM class, SCM obj, SCM slot_name, SCM value),
	    "")
#define FUNC_NAME s_scm_slot_set_using_class_x
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  return set_slot_value_using_name (class, obj, slot_name, value);
}
#undef FUNC_NAME


SCM_DEFINE (scm_slot_bound_using_class_p, "slot-bound-using-class?", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_bound_using_class_p
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class, obj, slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_exists_using_class_p, "slot-exists-using-class?", 3, 0, 0,
	    (SCM class, SCM obj, SCM slot_name),
	    "")
#define FUNC_NAME s_scm_slot_exists_using_class_p
{
  SCM_VALIDATE_CLASS (1, class);
  SCM_VALIDATE_INSTANCE (2, obj);
  SCM_VALIDATE_SYMBOL (3, slot_name);
  return test_slot_existence (class, obj, slot_name);
}
#undef FUNC_NAME


		/* ======================================== */

SCM_DEFINE (scm_slot_ref, "slot-ref", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return the value from @var{obj}'s slot with the name\n"
	    "@var{slot_name}.")
#define FUNC_NAME s_scm_slot_ref
{
  SCM res, class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS (obj, class);

  res = get_slot_value_using_name (class, obj, slot_name);
  if (SCM_GOOPS_UNBOUNDP (res))
    return scm_call_3 (SCM_VARIABLE_REF (var_slot_unbound), class, obj, slot_name);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_set_x, "slot-set!", 3, 0, 0,
	    (SCM obj, SCM slot_name, SCM value),
	    "Set the slot named @var{slot_name} of @var{obj} to @var{value}.")
#define FUNC_NAME s_scm_slot_set_x
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS(obj, class);

  return set_slot_value_using_name (class, obj, slot_name, value);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_bound_p, "slot-bound?", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return @code{#t} if the slot named @var{slot_name} of @var{obj}\n"
	    "is bound.")
#define FUNC_NAME s_scm_slot_bound_p
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  TEST_CHANGE_CLASS(obj, class);

  return (SCM_GOOPS_UNBOUNDP (get_slot_value_using_name (class,
							 obj,
							 slot_name))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME

SCM_DEFINE (scm_slot_exists_p, "slot-exists?", 2, 0, 0,
	    (SCM obj, SCM slot_name),
	    "Return @code{#t} if @var{obj} has a slot named @var{slot_name}.")
#define FUNC_NAME s_scm_slot_exists_p
{
  SCM class;

  SCM_VALIDATE_INSTANCE (1, obj);
  SCM_VALIDATE_SYMBOL (2, slot_name);
  TEST_CHANGE_CLASS (obj, class);

  return test_slot_existence (class, obj, slot_name);
}
#undef FUNC_NAME


/******************************************************************************
 *
 * %allocate-instance (the low level instance allocation primitive)
 *
 ******************************************************************************/

SCM_DEFINE (scm_sys_allocate_instance, "%allocate-instance", 2, 0, 0,
	    (SCM class, SCM initargs),
	    "Create a new instance of class @var{class} and initialize it\n"
	    "from the arguments @var{initargs}.")
#define FUNC_NAME s_scm_sys_allocate_instance
{
  SCM obj;
  scm_t_signed_bits n, i;
  SCM layout;

  SCM_VALIDATE_CLASS (1, class);

  /* FIXME: duplicates some of scm_make_struct. */

  n = SCM_I_INUM (SCM_SLOT (class, scm_si_nfields));
  obj = scm_i_alloc_struct (SCM_STRUCT_DATA (class), n);

  layout = SCM_VTABLE_LAYOUT (class);

  /* Set all SCM-holding slots to unbound */
  for (i = 0; i < n; i++)
    {
      scm_t_wchar c = scm_i_symbol_ref (layout, i*2);
      if (c == 'p')
        SCM_STRUCT_DATA (obj)[i] = SCM_UNPACK (SCM_GOOPS_UNBOUND);
      else if (c == 's')
        SCM_STRUCT_DATA (obj)[i] = SCM_UNPACK (obj);
      else
        SCM_STRUCT_DATA (obj)[i] = 0;
    }

  return obj;
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


SCM_SYMBOL (scm_sym_change_class, "change-class");

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

SCM_KEYWORD (k_name, "name");
SCM_GLOBAL_SYMBOL (scm_sym_args, "args");

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
  SCM_ASSERT (SCM_PUREGENERICP (generic), generic, SCM_ARG2, FUNC_NAME);
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

typedef struct t_extension {
  struct t_extension *next;
  SCM extended;
  SCM extension;
} t_extension;


/* Hint for `scm_gc_malloc ()' et al. when allocating `t_extension'
   objects.  */
static const char extension_gc_hint[] = "GOOPS extension";

static t_extension *extensions = 0;

void
scm_c_extend_primitive_generic (SCM extended, SCM extension)
{
  if (goops_loaded_p)
    {
      SCM gf, gext;
      if (!SCM_UNPACK (*SCM_SUBR_GENERIC (extended)))
	scm_enable_primitive_generic_x (scm_list_1 (extended));
      gf = *SCM_SUBR_GENERIC (extended);
      gext = scm_call_2 (SCM_VARIABLE_REF (scm_var_make_extended_generic),
			 gf,
			 SCM_SUBR_NAME (extension));
      SCM_SET_SUBR_GENERIC (extension, gext);
    }
  else
    {
      t_extension *e = scm_gc_malloc (sizeof (t_extension),
				      extension_gc_hint);
      t_extension **loc = &extensions;
      /* Make sure that extensions are placed before their own
       * extensions in the extensions list.  O(N^2) algorithm, but
       * extensions of primitive generics are rare.
       */
      while (*loc && !scm_is_eq (extension, (*loc)->extended))
	loc = &(*loc)->next;
      e->next = *loc;
      e->extended = extended;
      e->extension = extension;
      *loc = e;
    }
}

static void
setup_extended_primitive_generics ()
{
  while (extensions)
    {
      t_extension *e = extensions;
      scm_c_extend_primitive_generic (e->extended, e->extension);
      extensions = e->next;
    }
}

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
make_class_from_template (char const *template, char const *type_name, SCM supers, int applicablep)
{
  SCM meta, name;

  if (type_name)
    {
      char buffer[100];
      sprintf (buffer, template, type_name);
      name = scm_from_utf8_symbol (buffer);
    }
  else
    name = SCM_GOOPS_UNBOUND;

  meta = applicablep ? class_procedure_class : class_class;

  return scm_make_standard_class (meta, name, supers, SCM_EOL);
}

SCM
scm_make_extended_class (char const *type_name, int applicablep)
{
  return make_class_from_template ("<%s>",
				   type_name,
				   scm_list_1 (applicablep
					       ? class_applicable
					       : class_top),
				   applicablep);
}

void
scm_i_inherit_applicable (SCM c)
{
  if (!SCM_SUBCLASSP (c, class_applicable))
    {
      SCM dsupers = SCM_SLOT (c, scm_si_direct_supers);
      SCM cpl = SCM_SLOT (c, scm_si_cpl);
      /* patch class_applicable into direct-supers */
      SCM top = scm_c_memq (class_top, dsupers);
      if (scm_is_false (top))
	dsupers = scm_append (scm_list_2 (dsupers,
					  scm_list_1 (class_applicable)));
      else
	{
	  SCM_SETCAR (top, class_applicable);
	  SCM_SETCDR (top, scm_cons (class_top, SCM_CDR (top)));
	}
      SCM_SET_SLOT (c, scm_si_direct_supers, dsupers);
      /* patch class_applicable into cpl */
      top = scm_c_memq (class_top, cpl);
      if (scm_is_false (top))
	abort ();
      else
	{
	  SCM_SETCAR (top, class_applicable);
	  SCM_SETCDR (top, scm_cons (class_top, SCM_CDR (top)));
	}
      /* add class to direct-subclasses of class_applicable */
      SCM_SET_SLOT (class_applicable,
		    scm_si_direct_subclasses,
		    scm_cons (c, SCM_SLOT (class_applicable,
					   scm_si_direct_subclasses)));
    }
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
  SCM c, class = make_class_from_template ("<%s-port>",
					   type_name,
					   scm_list_1 (class_port),
					   0);
  scm_i_port_class[SCM_IN_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-input-port>",
				type_name,
				scm_list_2 (class, class_input_port),
				0);
  scm_i_port_class[SCM_OUT_PCLASS_INDEX + ptobnum]
    = make_class_from_template ("<%s-output-port>",
				type_name,
				scm_list_2 (class, class_output_port),
				0);
  scm_i_port_class[SCM_INOUT_PCLASS_INDEX + ptobnum]
    = c
    = make_class_from_template ("<%s-input-output-port>",
				type_name,
				scm_list_2 (class, class_input_output_port),
				0);
  /* Patch cpl (since this tree is too complex for the C level compute-cpl) */
  SCM_SET_SLOT (c, scm_si_cpl,
		scm_cons2 (c, class, SCM_SLOT (class_input_output_port, scm_si_cpl)));
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


SCM_KEYWORD (k_setter, "setter");

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

#ifdef GUILE_DEBUG
/*
 * Debugging utilities
 */

SCM_DEFINE (scm_pure_generic_p, "pure-generic?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a pure generic.")
#define FUNC_NAME s_scm_pure_generic_p
{
  return scm_from_bool (SCM_PUREGENERICP (obj));
}
#undef FUNC_NAME

#endif /* GUILE_DEBUG */

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

SCM_DEFINE (scm_sys_bless_pure_generic_vtable_x, "%bless-pure-generic-vtable!", 1, 0, 0,
	    (SCM vtable),
	    "")
#define FUNC_NAME s_scm_sys_bless_pure_generic_vtable_x
{
  SCM_VALIDATE_CLASS (1, vtable);
  SCM_SET_CLASS_FLAGS (vtable, SCM_CLASSF_PURE_GENERIC);
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
  var_slot_unbound =
    scm_module_variable (scm_module_goops, sym_slot_unbound);
  var_slot_missing =
    scm_module_variable (scm_module_goops, sym_slot_missing);
  var_no_applicable_method =
    scm_module_variable (scm_module_goops, sym_no_applicable_method);
  var_change_class =
    scm_module_variable (scm_module_goops, sym_change_class);
  setup_extended_primitive_generics ();

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

  goops_rstate = scm_c_make_rstate ("GOOPS", 5);

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
