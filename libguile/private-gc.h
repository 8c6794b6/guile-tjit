/*
 * private-gc.h - private declarations for garbage collection.
 * 
 * Copyright (C) 2002, 03, 04, 05, 06, 07, 08, 09, 11, 13 Free Software Foundation, Inc.
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

#ifndef SCM_PRIVATE_GC
#define SCM_PRIVATE_GC

#include  "_scm.h"


#define SCM_DOUBLECELL_ALIGNED_P(x)  (((2 * sizeof (scm_t_cell) - 1) & SCM_UNPACK (x)) == 0)


SCM_INTERNAL int scm_getenv_int (const char *var, int def);


#define SCM_MAX(A, B) ((A) > (B) ? (A) : (B))
#define SCM_MIN(A, B) ((A) < (B) ? (A) : (B))

SCM_INTERNAL char const *scm_i_tag_name (scm_t_bits tag); /* MOVEME */

#endif
