/*	Copyright (C) 1995, 1996, 1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



/* This file and objects.h contains those minimal pieces of the Guile
 * Object Oriented Programming System which need to be included in
 * libguile.  See the comments in objects.h.
 */

#include "_scm.h"

#include "struct.h"
#include "procprop.h"
#include "chars.h"
#include "keywords.h"
#include "smob.h"
#include "eval.h"
#include "alist.h"

#include "scm_validate.h"
#include "objects.h"


SCM scm_metaclass_standard;
SCM scm_metaclass_operator;

/* These variables are filled in by the object system when loaded. */
SCM scm_class_boolean, scm_class_char, scm_class_pair;
SCM scm_class_procedure, scm_class_string, scm_class_symbol;
SCM scm_class_procedure_with_setter, scm_class_primitive_generic;
SCM scm_class_vector, scm_class_null;
SCM scm_class_integer, scm_class_real, scm_class_complex;
SCM scm_class_unknown;

SCM *scm_port_class = 0;
SCM *scm_smob_class = 0;

SCM scm_no_applicable_method;

SCM (*scm_make_extended_class) (char *type_name);
void (*scm_make_port_classes) (int ptobnum, char *type_name);
void (*scm_change_object_class) (SCM, SCM, SCM);

/* This function is used for efficient type dispatch.  */
SCM
scm_class_of (SCM x)
{
  switch (SCM_ITAG3 (x))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      return scm_class_integer;

    case scm_tc3_imm24:
      if (SCM_ICHRP (x))
	return scm_class_char;
      else
	{
	  switch (SCM_ISYMNUM (x))
	    {
	    case SCM_ISYMNUM (SCM_BOOL_F):
	    case SCM_ISYMNUM (SCM_BOOL_T):
	      return scm_class_boolean;
	    case SCM_ISYMNUM (SCM_EOL):
	      return scm_class_null;
	    default:
	      return scm_class_unknown;
	    }
	}

    case scm_tc3_cons:
      switch (SCM_TYP7 (x))
	{
	case scm_tcs_cons_nimcar:
	  return scm_class_pair;
	case scm_tcs_closures:
	  return scm_class_procedure;
	case scm_tcs_symbols:
	  return scm_class_symbol;
	case scm_tc7_vector:
	case scm_tc7_wvect:
#ifdef HAVE_ARRAYS
	case scm_tc7_bvect:
	case scm_tc7_byvect:
	case scm_tc7_svect:
	case scm_tc7_ivect:
	case scm_tc7_uvect:
	case scm_tc7_fvect:
	case scm_tc7_dvect:
	case scm_tc7_cvect:
#endif
	  return scm_class_vector;
	case scm_tc7_string:
	case scm_tc7_substring:
	  return scm_class_string;
	case scm_tc7_asubr:
	case scm_tc7_subr_0:
	case scm_tc7_subr_1:
	case scm_tc7_cxr:
	case scm_tc7_subr_3:
	case scm_tc7_subr_2:
	case scm_tc7_rpsubr:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_2o:
	case scm_tc7_lsubr_2:
	case scm_tc7_lsubr:
	  if (SCM_SUBR_GENERIC (x) && *SCM_SUBR_GENERIC (x))
	    return scm_class_primitive_generic;
	  else
	    return scm_class_procedure;
	case scm_tc7_cclo:
	  return scm_class_procedure;
	case scm_tc7_pws:
	  return scm_class_procedure_with_setter;

	case scm_tc7_smob:
	  {
	    SCM type = SCM_TYP16 (x);
	    if (type == scm_tc16_flo)
	      {
		if (SCM_CAR (x) & SCM_IMAG_PART)
		  return scm_class_complex;
		else
		  return scm_class_real;
	      }
	    else if (type != scm_tc16_port_with_ps)
	      return scm_smob_class[SCM_TC2SMOBNUM (type)];
	    x = SCM_PORT_WITH_PS_PORT (x);
	    /* fall through to ports */
	  }
	case scm_tc7_port:
	  return scm_port_class[(SCM_WRTNG & SCM_CAR (x)
				 ? (SCM_RDNG & SCM_CAR (x)
				    ? SCM_INOUT_PCLASS_INDEX | SCM_PTOBNUM (x)
				    : SCM_OUT_PCLASS_INDEX | SCM_PTOBNUM (x))
				 : SCM_IN_PCLASS_INDEX | SCM_PTOBNUM (x))];
	case scm_tcs_cons_gloc:
	  /* must be a struct */
	  if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS_VALID)
	    return SCM_CLASS_OF (x);
	  else if (SCM_OBJ_CLASS_FLAGS (x) & SCM_CLASSF_GOOPS)
	    {
	      /* Goops object */
	      if (SCM_OBJ_CLASS_REDEF (x) != SCM_BOOL_F)
		scm_change_object_class (x,
					 SCM_CLASS_OF (x),         /* old */
					 SCM_OBJ_CLASS_REDEF (x)); /* new */
	      return SCM_CLASS_OF (x);
	    }
	  else
	    {
	      /* ordinary struct */
	      SCM handle = scm_struct_create_handle (SCM_STRUCT_VTABLE (x));
	      if (SCM_NFALSEP (SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle))))
		return SCM_STRUCT_TABLE_CLASS (SCM_CDR (handle));
	      else
		{
		  SCM name = SCM_STRUCT_TABLE_NAME (SCM_CDR (handle));
		  SCM class = scm_make_extended_class (SCM_NFALSEP (name)
						       ? SCM_ROCHARS (name)
						       : 0);
		  SCM_SET_STRUCT_TABLE_CLASS (handle, class);
		  return class;
		}
	    }
	default:
	  if (SCM_CONSP (x))
	    return scm_class_pair;
	  else
	    return scm_class_unknown;
	}

    case scm_tc3_cons_gloc:
    case scm_tc3_tc7_1:
    case scm_tc3_tc7_2:
    case scm_tc3_closure:
      /* Never reached */
      break;
    }
  return scm_class_unknown;
}

/* (SCM_IM_DISPATCH ARGS N-SPECIALIZED
 *   #((TYPE1 ... ENV FORMALS FORM ...) ...)
 *   GF)
 *
 * (SCM_IM_HASH_DISPATCH ARGS N-SPECIALIZED HASHSET MASK
 *   #((TYPE1 ... ENV FORMALS FORM ...) ...)
 *   GF)
 *
 * ARGS is either a list of expressions, in which case they
 * are interpreted as the arguments of an application, or
 * a non-pair, which is interpreted as a single expression
 * yielding all arguments.
 *
 * SCM_IM_DISPATCH expressions in generic functions always
 * have ARGS = the symbol `args' or the iloc #@0-0.
 *
 * Need FORMALS in order to support varying arity.  This
 * also avoids the need for renaming of bindings.
 *
 * We should probably not complicate this mechanism by
 * introducing "optimizations" for getters and setters or
 * primitive methods.  Getters and setter will normally be
 * compiled into @slot-[ref|set!] or a procedure call.
 * They rely on the dispatch performed before executing
 * the code which contains them.
 *
 * We might want to use a more efficient representation of
 * this form in the future, perhaps after we have introduced
 * low-level support for syntax-case macros.
 */

SCM
scm_mcache_lookup_cmethod (SCM cache, SCM args)
{
  int i, n, end, mask;
  SCM ls, methods, z = SCM_CDDR (cache);
  n = SCM_INUM (SCM_CAR (z)); /* maximum number of specializers */
  methods = SCM_CADR (z);

  if (SCM_NIMP (methods))
    {
      /* Prepare for linear search */
      mask = -1;
      i = 0;
      end = SCM_LENGTH (methods);
    }
  else
    {
      /* Compute a hash value */
      int hashset = SCM_INUM (methods);
      int j = n;
      mask = SCM_INUM (SCM_CAR (z = SCM_CDDR (z)));
      methods = SCM_CADR (z);
      i = 0;
      ls = args;
      if (SCM_NIMP (ls))
	do
	  {
	    i += (SCM_STRUCT_DATA (scm_class_of (SCM_CAR (ls)))
		  [scm_si_hashsets + hashset]);
	    ls = SCM_CDR (ls);
	  }
	while (--j && SCM_NIMP (ls));
      i &= mask;
      end = i;
    }

  /* Search for match  */
  do
    {
      int j = n;
      z = SCM_VELTS (methods)[i];
      ls = args; /* list of arguments */
      if (SCM_NIMP (ls))
	do
	  {
	    /* More arguments than specifiers => CLASS != ENV */
	    if (scm_class_of (SCM_CAR (ls)) != SCM_CAR (z))
	      goto next_method;
	    ls = SCM_CDR (ls);
	    z = SCM_CDR (z);
	  }
	while (--j && SCM_NIMP (ls));
      /* Fewer arguments than specifiers => CAR != ENV */
      if (!(SCM_IMP (SCM_CAR (z)) || SCM_CONSP (SCM_CAR (z))))
	goto next_method;
      return z;
    next_method:
      i = (i + 1) & mask;
    } while (i != end);
  return SCM_BOOL_F;
}

SCM
scm_mcache_compute_cmethod (SCM cache, SCM args)
{
  SCM cmethod = scm_mcache_lookup_cmethod (cache, args);
  if (SCM_IMP (cmethod))
    /* No match - memoize */
    return scm_memoize_method (cache, args);
  return cmethod;
}

SCM
scm_apply_generic (SCM gf, SCM args)
{
  SCM cmethod = scm_mcache_compute_cmethod (SCM_ENTITY_PROCEDURE (gf), args);
  return scm_eval_body (SCM_CDR (SCM_CMETHOD_CODE (cmethod)),
			SCM_EXTEND_ENV (SCM_CAR (SCM_CMETHOD_CODE (cmethod)),
					args,
					SCM_CMETHOD_ENV (cmethod)));
}

SCM
scm_call_generic_0 (SCM gf)
{
  return scm_apply_generic (gf, SCM_EOL);
}

SCM
scm_call_generic_1 (SCM gf, SCM a1)
{
  return scm_apply_generic (gf, SCM_LIST1 (a1));
}

SCM
scm_call_generic_2 (SCM gf, SCM a1, SCM a2)
{
  return scm_apply_generic (gf, SCM_LIST2 (a1, a2));
}

SCM
scm_call_generic_3 (SCM gf, SCM a1, SCM a2, SCM a3)
{
  return scm_apply_generic (gf, SCM_LIST3 (a1, a2, a3));
}

GUILE_PROC (scm_entity_p, "entity?", 1, 0, 0, 
            (SCM obj),
            "")
#define FUNC_NAME s_scm_entity_p
{
  return SCM_BOOL(SCM_NIMP (obj) && SCM_STRUCTP (obj) && SCM_I_ENTITYP (obj));
}
#undef FUNC_NAME

GUILE_PROC (scm_operator_p, "operator?", 1, 0, 0, 
            (SCM obj),
            "")
#define FUNC_NAME s_scm_operator_p
{
  return SCM_BOOL(SCM_NIMP (obj)
                  && SCM_STRUCTP (obj)
                  && SCM_I_OPERATORP (obj)
                  && !SCM_I_ENTITYP (obj));
}
#undef FUNC_NAME

GUILE_PROC (scm_set_object_procedure_x, "set-object-procedure!", 2, 0, 0, 
            (SCM obj, SCM proc),
            "")
#define FUNC_NAME s_scm_set_object_procedure_x
{
  SCM_ASSERT (SCM_NIMP (obj) && SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || (SCM_I_ENTITYP (obj)
		      && !(SCM_OBJ_CLASS_FLAGS (obj)
			   & SCM_CLASSF_PURE_GENERIC))),
	      obj,
	      SCM_ARG1,
              FUNC_NAME);
  SCM_VALIDATE_PROC(2,proc);
  if (SCM_I_ENTITYP (obj))
    SCM_ENTITY_PROCEDURE (obj) = proc;
  else
    SCM_OPERATOR_CLASS (obj)->procedure = proc;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef GUILE_DEBUG
GUILE_PROC (scm_object_procedure, "object-procedure", 1, 0, 0, 
            (SCM obj),
            "")
#define FUNC_NAME s_scm_object_procedure
{
  SCM_ASSERT (SCM_NIMP (obj) && SCM_STRUCTP (obj)
	      && ((SCM_CLASS_FLAGS (obj) & SCM_CLASSF_OPERATOR)
		  || SCM_I_ENTITYP (obj)),
	      obj, SCM_ARG1, FUNC_NAME);
  return (SCM_I_ENTITYP (obj)
	  ? SCM_ENTITY_PROCEDURE (obj)
	  : SCM_OPERATOR_CLASS (obj)->procedure);
}
#undef FUNC_NAME
#endif /* GUILE_DEBUG */

/* The following procedures are not a part of Goops but a minimal
 * object system built upon structs.  They are here for those who
 * want to implement their own object system.
 */

SCM
scm_i_make_class_object (SCM meta,
			 SCM layout_string,
			 unsigned long flags)
{
  SCM c;
  SCM layout = scm_make_struct_layout (layout_string);
  c = scm_make_struct (meta,
		       SCM_INUM0,
		       SCM_LIST4 (layout, SCM_BOOL_F, SCM_EOL, SCM_EOL));
  SCM_SET_CLASS_FLAGS (c, flags);
  return c;
}

GUILE_PROC (scm_make_class_object, "make-class-object", 2, 0, 0, 
            (SCM metaclass, SCM layout),
            "")
#define FUNC_NAME s_scm_make_class_object
{
  unsigned long flags = 0;
  SCM_VALIDATE_STRUCT(1,metaclass);
  SCM_VALIDATE_STRING(2,layout);
  if (metaclass == scm_metaclass_operator)
    flags = SCM_CLASSF_OPERATOR;
  return scm_i_make_class_object (metaclass, layout, flags);
}
#undef FUNC_NAME

GUILE_PROC (scm_make_subclass_object, "make-subclass-object", 2, 0, 0, 
            (SCM class, SCM layout),
            "")
#define FUNC_NAME s_scm_make_subclass_object
{
  SCM pl;
  SCM_VALIDATE_STRUCT(1,class);
  SCM_VALIDATE_STRING(2,layout);
  pl = SCM_STRUCT_DATA (class)[scm_vtable_index_layout];
  /* Convert symbol->string */
  pl = scm_makfromstr (SCM_CHARS (pl), (scm_sizet) SCM_LENGTH (pl), 0);
  return scm_i_make_class_object (SCM_STRUCT_VTABLE (class),
				  scm_string_append (SCM_LIST2 (pl, layout)),
				  SCM_CLASS_FLAGS (class));
}
#undef FUNC_NAME

void
scm_init_objects ()
{
  SCM ms = scm_makfrom0str (SCM_METACLASS_STANDARD_LAYOUT);
  SCM ml = scm_make_struct_layout (ms);
  SCM mt = scm_make_vtable_vtable (ml, SCM_INUM0,
				   SCM_LIST3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM os = scm_makfrom0str (SCM_METACLASS_OPERATOR_LAYOUT);
  SCM ol = scm_make_struct_layout (os);
  SCM ot = scm_make_vtable_vtable (ol, SCM_INUM0,
				   SCM_LIST3 (SCM_BOOL_F, SCM_EOL, SCM_EOL));
  
  SCM es = scm_makfrom0str (SCM_ENTITY_LAYOUT);
  SCM el = scm_make_struct_layout (es);
  SCM et = scm_make_struct (mt, SCM_INUM0,
			    SCM_LIST4 (el, SCM_BOOL_F, SCM_EOL, SCM_EOL));

  scm_sysintern ("<class>", mt);
  scm_metaclass_standard = mt;
  scm_sysintern ("<operator-class>", ot);
  scm_metaclass_operator = ot;
  SCM_SET_CLASS_FLAGS (et, SCM_CLASSF_OPERATOR | SCM_CLASSF_ENTITY);
  SCM_SET_CLASS_DESTRUCTOR (et, scm_struct_free_entity);
  scm_sysintern ("<entity>", et);

#include "objects.x"
}
