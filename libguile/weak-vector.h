/* classes: h_files */

#ifndef SCM_WEAK_VECTOR_H
#define SCM_WEAK_VECTOR_H

/* Copyright (C) 1995,1996,2000,2001, 2003, 2006, 2008, 2009, 2011 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"


/* Weak vectors.  */

#define SCM_I_WVECTP(x) (SCM_HAS_TYP7 (x, scm_tc7_wvect))

SCM_API SCM scm_make_weak_vector (SCM k, SCM fill);
SCM_API SCM scm_weak_vector (SCM l);
SCM_API SCM scm_weak_vector_p (SCM x);
SCM_INTERNAL SCM scm_c_weak_vector_ref (SCM v, size_t k);
SCM_INTERNAL void scm_c_weak_vector_set_x (SCM v, size_t k, SCM x);

SCM_INTERNAL void scm_init_weak_vectors (void);


#endif  /* SCM_WEAK_VECTOR_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
