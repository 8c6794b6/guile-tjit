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

#ifndef _SCM_LIGHTNING_BINDING_H_
#define _SCM_LIGHTNING_BINDING_H_

#include "libguile/_scm.h"

SCM_API SCM scm_jit_r (SCM i);
SCM_API SCM scm_jit_v (SCM i);
SCM_API SCM scm_jit_f (SCM i);
SCM_API SCM scm_jit_fp (void);
SCM_API SCM scm_jit_r_num (void);
SCM_API SCM scm_jit_v_num (void);
SCM_API SCM scm_jit_f_num (void);
SCM_API SCM scm_jit_code_size (SCM jit);
SCM_API SCM scm_make_bytevector_executable_x (SCM bv);

SCM_API void scm_init_lightning (void);

#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
