/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"
#include "genio.h"
#include "procprop.h"

#include "gsubr.h"

/*
 * gsubr.c
 * Provide `gsubrs' -- subrs taking a prescribed number of required, optional,
 * and rest arguments.
 */

#include "gsubr.h"

/* #define GSUBR_TEST */

SCM scm_i_name;
SCM scm_i_inner_name;
SCM scm_f_gsubr_apply;

SCM
scm_make_gsubr(name, req, opt, rst, fcn)
     const char *name;
     int req;
     int opt;
     int rst;
     SCM (*fcn)();
{
  switch SCM_GSUBR_MAKTYPE(req, opt, rst) {
  case SCM_GSUBR_MAKTYPE(0, 0, 0): return scm_make_subr(name, scm_tc7_subr_0, fcn);
  case SCM_GSUBR_MAKTYPE(1, 0, 0): return scm_make_subr(name, scm_tc7_subr_1, fcn);
  case SCM_GSUBR_MAKTYPE(0, 1, 0): return scm_make_subr(name, scm_tc7_subr_1o, fcn);
  case SCM_GSUBR_MAKTYPE(1, 1, 0): return scm_make_subr(name, scm_tc7_subr_2o, fcn);
  case SCM_GSUBR_MAKTYPE(2, 0, 0): return scm_make_subr(name, scm_tc7_subr_2, fcn);
  case SCM_GSUBR_MAKTYPE(3, 0, 0): return scm_make_subr(name, scm_tc7_subr_3, fcn);
  case SCM_GSUBR_MAKTYPE(0, 0, 1): return scm_make_subr(name, scm_tc7_lsubr, fcn);
  case SCM_GSUBR_MAKTYPE(2, 0, 1): return scm_make_subr(name, scm_tc7_lsubr_2, fcn);
  default:
    {
      SCM symcell = scm_sysintern(name, SCM_UNDEFINED);
      SCM z, cclo = scm_makcclo(scm_f_gsubr_apply, 3L);
      long tmp = ((((SCM_CELLPTR)(SCM_CAR(symcell)))-scm_heap_org)<<8);
      if (SCM_GSUBR_MAX < req + opt + rst) {
	fputs("ERROR in scm_make_gsubr: too many args\n", stderr);
	exit (1);
      }
      if ((tmp>>8) != ((SCM_CELLPTR)(SCM_CAR(symcell))-scm_heap_org))
	tmp = 0;
      SCM_NEWCELL(z);
      SCM_SUBRF(z) = fcn;
      SCM_SETCAR (z, tmp + scm_tc7_subr_0);
      SCM_GSUBR_PROC(cclo) = z;
      SCM_GSUBR_TYPE(cclo) = SCM_MAKINUM(SCM_GSUBR_MAKTYPE(req, opt, rst));
      SCM_SETCDR (symcell, cclo);
#ifdef DEBUG_EXTENSIONS
      if (SCM_REC_PROCNAMES_P)
	scm_set_procedure_property_x (cclo, scm_i_name, SCM_CAR (symcell));
#endif
      return cclo;
    }
  }
}


SCM_PROC(s_gsubr_apply, "gsubr-apply", 0, 0, 1, scm_gsubr_apply);

SCM
scm_gsubr_apply(args)
     SCM args;
{
  SCM self = SCM_CAR(args);
  SCM (*fcn)() = SCM_SUBRF(SCM_GSUBR_PROC(self));
  SCM v[10];			/* must agree with greatest supported arity */
  int typ = SCM_INUM(SCM_GSUBR_TYPE(self));
  int i, n = SCM_GSUBR_REQ(typ) + SCM_GSUBR_OPT(typ) + SCM_GSUBR_REST(typ);
#if 0
  SCM_ASSERT(n <= sizeof(v)/sizeof(SCM),
	     self, "internal programming error", s_gsubr_apply);
#endif
  args = SCM_CDR(args);
  for (i = 0; i < SCM_GSUBR_REQ(typ); i++) {
#ifndef SCM_RECKLESS
    if (SCM_IMP(args))
      wnargs: scm_wrong_num_args (SCM_SNAME(SCM_GSUBR_PROC(self)));
#endif
    v[i] = SCM_CAR(args);
    args = SCM_CDR(args);
  }
  for (; i < SCM_GSUBR_REQ(typ) + SCM_GSUBR_OPT(typ); i++) {
    if (SCM_NIMP(args)) {
      v[i] = SCM_CAR(args);
      args = SCM_CDR(args);
    }
    else
      v[i] = SCM_UNDEFINED;
  }
  if (SCM_GSUBR_REST(typ))
    v[i] = args;
  else
    SCM_ASRTGO(SCM_NULLP(args), wnargs);
  switch (n) {
  case 2: return (*fcn)(v[0], v[1]);
  case 3: return (*fcn)(v[0], v[1], v[2]);
  case 4: return (*fcn)(v[0], v[1], v[2], v[3]);
  case 5: return (*fcn)(v[0], v[1], v[2], v[3], v[4]);
  case 6: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5]);
  case 7: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
  case 8: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
  case 9: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
  case 10: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
  }
  return 0; /* Never reached. */
}


#ifdef GSUBR_TEST
/* A silly example, taking 2 required args, 1 optional, and
   a scm_list of rest args 
   */
SCM
gsubr_21l(req1, req2, opt, rst)
     SCM req1, req2, opt, rst;
{
  scm_puts ("gsubr-2-1-l:\n req1: ", scm_cur_outp);
  scm_display(req1, scm_cur_outp);
  scm_puts ("\n req2: ", scm_cur_outp);
  scm_display(req2, scm_cur_outp);
  scm_puts ("\n opt: ", scm_cur_outp);
  scm_display(opt, scm_cur_outp);
  scm_puts ("\n rest: ", scm_cur_outp);
  scm_display(rst, scm_cur_outp);
  scm_newline(scm_cur_outp);
  return SCM_UNSPECIFIED;
}
#endif



void
scm_init_gsubr()
{
  scm_f_gsubr_apply = scm_make_subr(s_gsubr_apply, scm_tc7_lsubr, scm_gsubr_apply);
  scm_i_name = SCM_CAR (scm_sysintern ("name", SCM_UNDEFINED));
  scm_permanent_object (scm_i_name);
  scm_i_inner_name = SCM_CAR (scm_sysintern ("inner-name", SCM_UNDEFINED));
  scm_permanent_object (scm_i_inner_name);
#ifdef GSUBR_TEST
  scm_make_gsubr("gsubr-2-1-l", 2, 1, 1, gsubr_21l); /* example */
#endif
}
