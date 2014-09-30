/* Copyright (C) 1995-2001, 2006, 2008-2011, 2013
 *   Free Software Foundation, Inc.
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

#include <stdio.h>
#include <stdarg.h>

#include "libguile/_scm.h"
#include "libguile/gsubr.h"
#include "libguile/foreign.h"
#include "libguile/instructions.h"
#include "libguile/srfi-4.h"
#include "libguile/programs.h"

#include "libguile/private-options.h"

/*
 * gsubr.c
 * Provide `gsubrs' -- subrs taking a prescribed number of required, optional,
 * and rest arguments.
 */



/* OK here goes nothing: we're going to define VM assembly trampolines for
   invoking subrs.  Ready?  Right!  */

/* There's a maximum of 10 args, so the number of possible combinations is:
   (REQ-OPT-REST)
   for 0 args: 1 (000) (1 + 0)
   for 1 arg: 3 (100, 010, 001) (2 + 1)
   for 2 args: 5 (200, 110, 020, 101, 011) (3 + 2)
   for 3 args: 7 (300, 210, 120, 030, 201, 111, 021) (4 + 3)
   for N args: 2N+1

   and the index at which N args starts:
   for 0 args: 0
   for 1 args: 1
   for 2 args: 4
   for 3 args: 9
   for N args: N^2

   One can prove this:

   (1 + 3 + 5 + ... + (2N+1))
     = ((2N+1)+1)/2 * (N+1)
     = 2(N+1)/2 * (N+1)
     = (N+1)^2

   Thus the total sum is 11^2 = 121. Let's just generate all of them as
   read-only data.
*/

/* A: req; B: opt; C: rest */
#define A(nreq)                                                         \
  SCM_PACK_OP_24 (assert_nargs_ee, nreq + 1),                           \
  SCM_PACK_OP_24 (subr_call, 0),                                        \
  0,                                                                    \
  0

#define B(nopt)                                                         \
  SCM_PACK_OP_24 (assert_nargs_le, nopt + 1),                           \
  SCM_PACK_OP_24 (alloc_frame, nopt + 1),                               \
  SCM_PACK_OP_24 (subr_call, 0),                                        \
  0

#define C()                                                             \
  SCM_PACK_OP_24 (bind_rest, 1),                                        \
  SCM_PACK_OP_24 (subr_call, 0),                                        \
  0,                                                                    \
  0

#define AB(nreq, nopt)                                                  \
  SCM_PACK_OP_24 (assert_nargs_ge, nreq + 1),                           \
  SCM_PACK_OP_24 (assert_nargs_le, nreq + nopt + 1),                    \
  SCM_PACK_OP_24 (alloc_frame, nreq + nopt + 1),                        \
  SCM_PACK_OP_24 (subr_call, 0)

#define AC(nreq)                                                        \
  SCM_PACK_OP_24 (assert_nargs_ge, nreq + 1),                           \
  SCM_PACK_OP_24 (bind_rest, nreq + 1),                                 \
  SCM_PACK_OP_24 (subr_call, 0),                                        \
  0

#define BC(nopt)                                                        \
  SCM_PACK_OP_24 (bind_rest, nopt + 1),                                 \
  SCM_PACK_OP_24 (subr_call, 0),                                        \
  0,                                                                    \
  0

#define ABC(nreq, nopt)                                                 \
  SCM_PACK_OP_24 (assert_nargs_ge, nreq + 1),                           \
  SCM_PACK_OP_24 (bind_rest, nreq + nopt + 1),                          \
  SCM_PACK_OP_24 (subr_call, 0),                                        \
  0


/*
 (defun generate-bytecode (n)
   "Generate bytecode for N arguments"
   (interactive "p")
   (insert (format "/\* %d arguments *\/\n " n))
   (let ((nreq n))
     (while (<= 0 nreq)
       (let ((nopt (- n nreq)))
         (insert
          (if (< 0 nreq)
              (if (< 0 nopt)
                  (format " AB(%d,%d)," nreq nopt)
                  (format " A(%d)," nreq))
              (if (< 0 nopt)
                  (format " B(%d)," nopt)
                  (format " A(0),"))))
         (setq nreq (1- nreq))))
     (insert "\n ")
     (setq nreq (1- n))
     (while (<= 0 nreq)
       (let ((nopt (- n nreq 1)))
         (insert
          (if (< 0 nreq)
              (if (< 0 nopt)
                  (format " ABC(%d,%d)," nreq nopt)
                  (format " AC(%d)," nreq))
              (if (< 0 nopt)
                  (format " BC(%d)," nopt)
                  (format " C(),"))))
         (setq nreq (1- nreq))))
     (insert "\n\n  ")))

 (defun generate-bytecodes (n)
   "Generate bytecodes for up to N arguments"
   (interactive "p")
   (let ((i 0))
     (while (<= i n)
       (generate-bytecode i)
       (setq i (1+ i)))))
*/
static const scm_t_uint32 subr_stub_code[] = {
  /* C-u 1 0 M-x generate-bytecodes RET */
  /* 0 arguments */
  A(0),

  /* 1 arguments */
  A(1), B(1),
  C(),

  /* 2 arguments */
  A(2), AB(1,1), B(2),
  AC(1), BC(1),

  /* 3 arguments */
  A(3), AB(2,1), AB(1,2), B(3),
  AC(2), ABC(1,1), BC(2),

  /* 4 arguments */
  A(4), AB(3,1), AB(2,2), AB(1,3), B(4),
  AC(3), ABC(2,1), ABC(1,2), BC(3),

  /* 5 arguments */
  A(5), AB(4,1), AB(3,2), AB(2,3), AB(1,4), B(5),
  AC(4), ABC(3,1), ABC(2,2), ABC(1,3), BC(4),

  /* 6 arguments */
  A(6), AB(5,1), AB(4,2), AB(3,3), AB(2,4), AB(1,5), B(6),
  AC(5), ABC(4,1), ABC(3,2), ABC(2,3), ABC(1,4), BC(5),

  /* 7 arguments */
  A(7), AB(6,1), AB(5,2), AB(4,3), AB(3,4), AB(2,5), AB(1,6), B(7),
  AC(6), ABC(5,1), ABC(4,2), ABC(3,3), ABC(2,4), ABC(1,5), BC(6),

  /* 8 arguments */
  A(8), AB(7,1), AB(6,2), AB(5,3), AB(4,4), AB(3,5), AB(2,6), AB(1,7), B(8),
  AC(7), ABC(6,1), ABC(5,2), ABC(4,3), ABC(3,4), ABC(2,5), ABC(1,6), BC(7),

  /* 9 arguments */
  A(9), AB(8,1), AB(7,2), AB(6,3), AB(5,4), AB(4,5), AB(3,6), AB(2,7), AB(1,8), B(9),
  AC(8), ABC(7,1), ABC(6,2), ABC(5,3), ABC(4,4), ABC(3,5), ABC(2,6), ABC(1,7), BC(8),

  /* 10 arguments */
  A(10), AB(9,1), AB(8,2), AB(7,3), AB(6,4), AB(5,5), AB(4,6), AB(3,7), AB(2,8), AB(1,9), B(10),
  AC(9), ABC(8,1), ABC(7,2), ABC(6,3), ABC(5,4), ABC(4,5), ABC(3,6), ABC(2,7), ABC(1,8), BC(9),
};

#undef A
#undef B
#undef C
#undef AB
#undef AC
#undef BC
#undef ABC

/* (nargs * nargs) + nopt + rest * (nargs + 1) */
#define SUBR_STUB_CODE(nreq,nopt,rest)                                \
  &subr_stub_code[((nreq + nopt + rest) * (nreq + nopt + rest)        \
                   + nopt + rest * (nreq + nopt + rest + 1)) * 4]

static const scm_t_uint32*
get_subr_stub_code (unsigned int nreq, unsigned int nopt, unsigned int rest)
{
  if (SCM_UNLIKELY (rest > 1 || nreq + nopt + rest > 10))
    scm_out_of_range ("make-subr", scm_from_uint (nreq + nopt + rest));
      
  return SUBR_STUB_CODE (nreq, nopt, rest);
}

static SCM
create_subr (int define, const char *name,
             unsigned int nreq, unsigned int nopt, unsigned int rest,
             SCM (*fcn) (), SCM *generic_loc)
{
  SCM ret, sname;
  scm_t_bits flags;
  scm_t_bits nfree = generic_loc ? 3 : 2;

  sname = scm_from_utf8_symbol (name);

  flags = SCM_F_PROGRAM_IS_PRIMITIVE;
  flags |= generic_loc ? SCM_F_PROGRAM_IS_PRIMITIVE_GENERIC : 0;

  ret = scm_words (scm_tc7_program | (nfree << 16) | flags, nfree + 2);
  SCM_SET_CELL_WORD_1 (ret, get_subr_stub_code (nreq, nopt, rest));
  SCM_PROGRAM_FREE_VARIABLE_SET (ret, 0, scm_from_pointer (fcn, NULL));
  SCM_PROGRAM_FREE_VARIABLE_SET (ret, 1, sname);
  if (generic_loc)
    SCM_PROGRAM_FREE_VARIABLE_SET (ret, 2,
                                       scm_from_pointer (generic_loc, NULL));

  if (define)
    scm_define (sname, ret);

  return ret;
}

/* Given a program that is a primitive, determine its minimum arity.
   This is possible because each primitive's code is 4 32-bit words
   long, and they are laid out contiguously in an ordered pattern.  */
int
scm_i_primitive_arity (SCM prim, int *req, int *opt, int *rest)
{
  const scm_t_uint32 *code = SCM_PROGRAM_CODE (prim);
  unsigned idx, nargs, base, next;

  if (code < subr_stub_code)
    return 0;
  if (code > subr_stub_code + (sizeof(subr_stub_code) / sizeof(scm_t_uint32)))
    return 0;

  idx = (code - subr_stub_code) / 4;

  nargs = -1;
  next = 0;
  do
    {
      base = next;
      nargs++;
      next = (nargs + 1) * (nargs + 1);
    }
  while (idx >= next);

  *rest = (next - idx) < (idx - base);
  *req = *rest ? (next - 1) - idx : (base + nargs) - idx;
  *opt = *rest ? idx - (next - nargs) : idx - base;

  return 1;
}

scm_t_uintptr
scm_i_primitive_call_ip (SCM subr)
{
  const scm_t_uint32 *code = SCM_PROGRAM_CODE (subr);

  /* A stub is 4 32-bit words long, or 16 bytes.  The call will be one
     instruction, in either the fourth, third, or second word.  Return a
     byte offset from the entry.  */
  return (scm_t_uintptr)(code + (code[3] ? 3 : code[2] ? 2 : 1));
}

SCM
scm_c_make_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_subr (0, name, req, opt, rst, fcn, NULL);
}

SCM
scm_c_define_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_subr (1, name, req, opt, rst, fcn, NULL);
}

SCM
scm_c_make_gsubr_with_generic (const char *name,
			       int req,
			       int opt,
			       int rst,
			       SCM (*fcn)(),
			       SCM *gf)
{
  return create_subr (0, name, req, opt, rst, fcn, gf);
}

SCM
scm_c_define_gsubr_with_generic (const char *name,
				 int req,
				 int opt,
				 int rst,
				 SCM (*fcn)(),
				 SCM *gf)
{
  return create_subr (1, name, req, opt, rst, fcn, gf);
}

void
scm_init_gsubr()
{
#include "libguile/gsubr.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
