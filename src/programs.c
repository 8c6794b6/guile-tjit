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

#include <string.h>
#include "instructions.h"
#include "programs.h"
#include "vm.h"


scm_t_bits scm_tc16_program;

static SCM zero_vector;

SCM
scm_c_make_program (void *addr, size_t size, SCM holder)
#define FUNC_NAME "scm_c_make_program"
{
  struct scm_program *p = scm_gc_malloc (sizeof (struct scm_program),
					 "program");
  p->size     = size;
  p->nargs    = 0;
  p->nrest    = 0;
  p->nlocs    = 0;
  p->nexts    = 0;
  p->meta     = SCM_BOOL_F;
  p->objs     = zero_vector;
  p->external = SCM_EOL;
  p->holder   = holder;

  /* If nobody holds bytecode's address, then allocate a new memory */
  if (SCM_FALSEP (holder))
    p->base = scm_gc_malloc (size, "program-base");
  else
    p->base = addr;

  SCM_RETURN_NEWSMOB (scm_tc16_program, p);
}
#undef FUNC_NAME

SCM
scm_c_make_closure (SCM program, SCM external)
{
  SCM prog = scm_c_make_program (0, 0, program);
  *SCM_PROGRAM_DATA (prog) = *SCM_PROGRAM_DATA (program);
  SCM_PROGRAM_DATA (prog)->external = external;
  return prog;
}

static SCM
program_mark (SCM obj)
{
  struct scm_program *p = SCM_PROGRAM_DATA (obj);
  scm_gc_mark (p->meta);
  scm_gc_mark (p->objs);
  scm_gc_mark (p->external);
  return p->holder;
}

static scm_sizet
program_free (SCM obj)
{
  struct scm_program *p = SCM_PROGRAM_DATA (obj);
  scm_sizet size = (sizeof (struct scm_program));

  if (SCM_FALSEP (p->holder))
    scm_gc_free (p->base, p->size, "program-base");

  scm_gc_free (p, size, "program");

  return 0;
}

static SCM
program_apply (SCM program, SCM args)
{
  return scm_vm_apply (scm_the_vm (), program, args);
}


/*
 * Scheme interface
 */

SCM_DEFINE (scm_program_p, "program?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_program_p
{
  return SCM_BOOL (SCM_PROGRAM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_base, "program-base", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_base
{
  SCM_VALIDATE_PROGRAM (1, program);

  return scm_ulong2num ((unsigned long) SCM_PROGRAM_DATA (program)->base);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_arity, "program-arity", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_arity
{
  struct scm_program *p;

  SCM_VALIDATE_PROGRAM (1, program);

  p = SCM_PROGRAM_DATA (program);
  return SCM_LIST4 (scm_from_uchar (p->nargs),
		    scm_from_uchar (p->nrest),
		    scm_from_uchar (p->nlocs),
		    scm_from_uchar (p->nexts));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_meta, "program-meta", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_meta
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_PROGRAM_DATA (program)->meta;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_objects, "program-objects", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_objects
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_PROGRAM_DATA (program)->objs;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_external, "program-external", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_external
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_PROGRAM_DATA (program)->external;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_bytecode, "program-bytecode", 1, 0, 0,
	    (SCM program),
	    "Return a u8vector containing @var{program}'s bytecode.")
#define FUNC_NAME s_scm_program_bytecode
{
  size_t size;
  char *c_bytecode;

  SCM_VALIDATE_PROGRAM (1, program);

  size = SCM_PROGRAM_DATA (program)->size;
  c_bytecode = malloc (size);
  if (!c_bytecode)
    return SCM_BOOL_F;

  memcpy (c_bytecode, SCM_PROGRAM_DATA (program)->base, size);

  return scm_take_u8vector (c_bytecode, size);
}
#undef FUNC_NAME



void
scm_init_programs (void)
{
  zero_vector = scm_permanent_object (scm_c_make_vector (0, SCM_BOOL_F));

  scm_tc16_program = scm_make_smob_type ("program", 0);
  scm_set_smob_mark (scm_tc16_program, program_mark);
  scm_set_smob_free (scm_tc16_program, program_free);
  scm_set_smob_apply (scm_tc16_program, program_apply, 0, 0, 1);

#ifndef SCM_MAGIC_SNARFER
#include "programs.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
