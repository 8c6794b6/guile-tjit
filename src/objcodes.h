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

#ifndef _SCM_OBJCODES_H_
#define _SCM_OBJCODES_H_

#include <libguile.h>

struct scm_objcode {
  size_t size;			/* objcode size */
  char *base;			/* objcode base address */
  int fd;			/* file descriptor when mmap'ed */
};

extern scm_t_bits scm_tc16_objcode;

#define SCM_OBJCODE_P(x)	(SCM_SMOB_PREDICATE (scm_tc16_objcode, x))
#define SCM_OBJCODE_DATA(x)	((struct scm_objcode *) SCM_SMOB_DATA (x))
#define SCM_VALIDATE_OBJCODE(p,x) SCM_MAKE_VALIDATE (p, x, OBJCODE_P)

#define SCM_OBJCODE_SIZE(x)	(SCM_OBJCODE_DATA (x)->size)
#define SCM_OBJCODE_BASE(x)	(SCM_OBJCODE_DATA (x)->base)
#define SCM_OBJCODE_FD(x)	(SCM_OBJCODE_DATA (x)->fd)

extern SCM scm_load_objcode (SCM file);
extern SCM scm_objcode_to_program (SCM objcode);
extern SCM scm_objcode_p (SCM obj);
extern SCM scm_bytecode_to_objcode (SCM bytecode, SCM nlocs, SCM nexts);
extern SCM scm_objcode_to_u8vector (SCM objcode);

extern void scm_bootstrap_objcodes (void);
extern void scm_init_objcodes (void);

#endif /* _SCM_OBJCODES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
