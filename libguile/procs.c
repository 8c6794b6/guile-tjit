/*	Copyright (C) 1995, 1996, 1997, 1999 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "_scm.h"

#include "objects.h"

#include "scm_validate.h"
#include "procs.h"



/* {Procedures}
 */

scm_subr_entry *scm_subr_table;

/* libguile contained approx. 700 primitive procedures on 24 Aug 1999. */

int scm_subr_table_size = 0;
int scm_subr_table_room = 750;

SCM 
scm_make_subr_opt (const char *name, int type, SCM (*fcn) (), int set)
{
  SCM symcell;
  register SCM z;
  int entry;

  if (scm_subr_table_size == scm_subr_table_room)
    {
      scm_sizet new_size = scm_subr_table_room * 3 / 2;
      void *new_table
	= scm_must_realloc ((char *) scm_subr_table,
			    sizeof (scm_subr_entry) * scm_subr_table_room,
			    sizeof (scm_subr_entry) * new_size, 
			    "scm_make_subr_opt");
      scm_subr_table = new_table;
      scm_subr_table_room = new_size;
    }

  SCM_NEWCELL (z);
  symcell = set ? scm_sysintern0 (name) : scm_intern0 (name);
  
  entry = scm_subr_table_size;
  scm_subr_table[entry].handle = z;
  scm_subr_table[entry].name = SCM_CAR (symcell);
  scm_subr_table[entry].generic = 0;
  scm_subr_table[entry].properties = SCM_EOL;
  scm_subr_table[entry].documentation = SCM_BOOL_F;
  
  SCM_SUBRF (z) = fcn;
  SCM_SETCAR (z, (entry << 8) + type);
  scm_subr_table_size++;
  
  if (set)
    SCM_SETCDR (symcell, z);
  
  return z;
}

/* This function isn't currently used since subrs are never freed. */
/* *fixme* Need mutex here. */
void
scm_free_subr_entry (SCM subr)
{
  int entry = SCM_SUBRNUM (subr);
  /* Move last entry in table to the free position */
  scm_subr_table[entry] = scm_subr_table[scm_subr_table_size - 1];
  SCM_SET_SUBRNUM (scm_subr_table[entry].handle, entry);
  scm_subr_table_size--;
}

SCM 
scm_make_subr (const char *name, int type, SCM (*fcn) ())
{
  return scm_make_subr_opt (name, type, fcn, 1);
}

SCM
scm_make_subr_with_generic (const char *name, int type, SCM (*fcn) (), SCM *gf)
{
  SCM subr = scm_make_subr_opt (name, type, fcn, 1);
  scm_subr_table[scm_subr_table_size - 1].generic = gf;
  return subr;
}

void
scm_mark_subr_table ()
{
  int i;
  for (i = 0; i < scm_subr_table_size; ++i)
    {
      SCM_SETGC8MARK (scm_subr_table[i].name);
      if (scm_subr_table[i].generic && *scm_subr_table[i].generic)
	scm_gc_mark (*scm_subr_table[i].generic);
      if (SCM_NIMP (scm_subr_table[i].properties))
	scm_gc_mark (scm_subr_table[i].properties);
      if (SCM_NIMP (scm_subr_table[i].documentation))
	scm_gc_mark (scm_subr_table[i].documentation);
    }
}


#ifdef CCLO
SCM 
scm_makcclo (SCM proc, long len)
{
  SCM s;
  SCM_NEWCELL (s);
  SCM_DEFER_INTS;
  SCM_SETCHARS (s, scm_must_malloc (len * sizeof (SCM), "compiled-closure"));
  SCM_SETLENGTH (s, len, scm_tc7_cclo);
  while (--len)
    SCM_VELTS (s)[len] = SCM_UNSPECIFIED;
  SCM_CCLO_SUBR (s) = proc;
  SCM_ALLOW_INTS;
  return s;
}

/* Undocumented debugging procedure */
#ifdef GUILE_DEBUG
GUILE_PROC (scm_make_cclo, "make-cclo", 2, 0, 0,
            (SCM proc, SCM len),
"")
#define FUNC_NAME s_scm_make_cclo
{
  return scm_makcclo (proc, SCM_INUM (len));
}
#undef FUNC_NAME
#endif
#endif



GUILE_PROC(scm_procedure_p, "procedure?", 1, 0, 0, 
           (SCM obj),
"")
#define FUNC_NAME s_scm_procedure_p
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_cons_gloc:
	if (!SCM_I_OPERATORP (obj))
	  break;
      case scm_tcs_closures:
      case scm_tc7_contin:
      case scm_tcs_subrs:
#ifdef CCLO
      case scm_tc7_cclo:
#endif
      case scm_tc7_pws:
	return SCM_BOOL_T;
      default:
	return SCM_BOOL_F;
      }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

GUILE_PROC(scm_closure_p, "closure?", 1, 0, 0, 
           (SCM obj),
"")
#define FUNC_NAME s_scm_closure_p
{
  return SCM_BOOL(SCM_NIMP (obj) && SCM_CLOSUREP (obj));
}
#undef FUNC_NAME

GUILE_PROC(scm_thunk_p, "thunk?", 1, 0, 0, 
           (SCM obj),
"")
#define FUNC_NAME s_scm_thunk_p
{
  if (SCM_NIMP (obj))
    {
    again:
      switch (SCM_TYP7 (obj))
	{
	case scm_tcs_closures:
	  if (SCM_NULLP (SCM_CAR (SCM_CODE (obj))))
	    return SCM_BOOL_T;
	case scm_tc7_subr_0:
	case scm_tc7_subr_1o:
	case scm_tc7_lsubr:
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
#ifdef CCLO
	case scm_tc7_cclo:
#endif
	  return SCM_BOOL_T;
	case scm_tc7_pws:
	  obj = SCM_PROCEDURE (obj);
	  goto again;
	default:
	  ;
	}
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Only used internally. */
int
scm_subr_p (SCM obj)
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_subrs:
	return 1;
      default:
	;
      }
  return 0;
}

GUILE_PROC(scm_procedure_documentation, "procedure-documentation", 1, 0, 0, 
           (SCM proc),
"")
#define FUNC_NAME s_scm_procedure_documentation
{
  SCM code;
  SCM_ASSERT (SCM_BOOL_T == scm_procedure_p (proc) && SCM_NIMP (proc) && SCM_TYP7 (proc) != scm_tc7_contin,
	  proc, SCM_ARG1, FUNC_NAME);
  switch (SCM_TYP7 (proc))
    {
    case scm_tcs_closures:
      code = SCM_CDR (SCM_CODE (proc));
      if (SCM_IMP (SCM_CDR (code)))
	return SCM_BOOL_F;
      code = SCM_CAR (code);
      if (SCM_IMP (code))
	return SCM_BOOL_F;
      if (SCM_STRINGP (code))
	return code;
    default:
      return SCM_BOOL_F;
/*
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
*/
    }
}
#undef FUNC_NAME


/* Procedure-with-setter
 */

GUILE_PROC (scm_procedure_with_setter_p, "procedure-with-setter?", 1, 0, 0, 
            (SCM obj),
"")
#define FUNC_NAME s_scm_procedure_with_setter_p
{
  return SCM_BOOL(SCM_NIMP (obj) && SCM_PROCEDURE_WITH_SETTER_P (obj));
}
#undef FUNC_NAME

GUILE_PROC (scm_make_procedure_with_setter, "make-procedure-with-setter", 2, 0, 0, 
            (SCM procedure, SCM setter),
"")
#define FUNC_NAME s_scm_make_procedure_with_setter
{
  SCM z;
  SCM_VALIDATE_PROC(1,procedure);
  SCM_VALIDATE_PROC(2,setter);
  SCM_NEWCELL (z);
  SCM_ENTER_A_SECTION;
  SCM_SETCDR (z, scm_cons (procedure, setter));
  SCM_SETCAR (z, scm_tc7_pws);
  SCM_EXIT_A_SECTION;
  return z;
}
#undef FUNC_NAME

GUILE_PROC (scm_procedure, "procedure", 1, 0, 0, 
            (SCM proc),
"")
#define FUNC_NAME s_scm_procedure
{
  SCM_VALIDATE_NIM (1,proc);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_PROCEDURE (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM_ASSERT (SCM_I_OPERATORP (proc), proc, SCM_ARG1, FUNC_NAME);
      return proc;
    }
  SCM_WRONG_TYPE_ARG (1, proc);
  return 0; /* not reached */
}
#undef FUNC_NAME

SCM_GPROC (s_setter, "setter", 1, 0, 0, scm_setter, g_setter);

SCM
scm_setter (SCM proc)
{
  SCM_GASSERT1 (SCM_NIMP (proc), g_setter, proc, SCM_ARG1, s_setter);
  if (SCM_PROCEDURE_WITH_SETTER_P (proc))
    return SCM_SETTER (proc);
  else if (SCM_STRUCTP (proc))
    {
      SCM setter;
      SCM_GASSERT1 (SCM_I_OPERATORP (proc),
		    g_setter, proc, SCM_ARG1, s_setter);
      setter = (SCM_I_ENTITYP (proc)
		? SCM_ENTITY_SETTER (proc)
		: SCM_OPERATOR_SETTER (proc));
      if (SCM_NIMP (setter))
	return setter;
      /* fall through */
    }
  SCM_WTA_DISPATCH_1 (g_setter, proc, SCM_ARG1, s_setter);
  return 0;
}


void
scm_init_iprocs(const scm_iproc *subra, int type)
{
  for(;subra->scm_string; subra++)
    scm_make_subr(subra->scm_string,
		  type,
		  subra->cproc);
}


void
scm_init_subr_table ()
{
  scm_subr_table
    = ((scm_subr_entry *)
       scm_must_malloc (sizeof (scm_subr_entry) * scm_subr_table_room,
			"scm_subr_table"));
}

void
scm_init_procs ()
{
#include "procs.x"
}
