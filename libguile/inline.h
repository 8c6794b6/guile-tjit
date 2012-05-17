/* classes: h_files */

#ifndef SCM_INLINE_H
#define SCM_INLINE_H

/* Copyright (C) 2001, 2002, 2003, 2004, 2006, 2008, 2009, 2010,
 *   2011, 2012 Free Software Foundation, Inc.
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

/* This file is for inline functions.  On platforms that don't support
   inlining functions, they are turned into ordinary functions.  On
   platforms that do support inline functions, the definitions are still
   compiled into the library, once, in inline.c.  */

#include "libguile/__scm.h"

#include "libguile/gc.h"
#include "libguile/threads.h"
#include "libguile/array-handle.h"
#include "libguile/ports.h"
#include "libguile/numbers.h"
#include "libguile/error.h"


SCM_INLINE SCM scm_array_handle_ref (scm_t_array_handle *h, ssize_t pos);
SCM_INLINE void scm_array_handle_set (scm_t_array_handle *h, ssize_t pos, SCM val);

SCM_INLINE int scm_is_string (SCM x);

SCM_INLINE SCM scm_cell (scm_t_bits car, scm_t_bits cdr);
SCM_INLINE SCM scm_double_cell (scm_t_bits car, scm_t_bits cbr,
			     scm_t_bits ccr, scm_t_bits cdr);
SCM_INLINE SCM scm_words (scm_t_bits car, scm_t_uint32 n_words);

#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
/* Either inlining, or being included from inline.c.  */

SCM_INLINE_IMPLEMENTATION SCM
scm_array_handle_ref (scm_t_array_handle *h, ssize_t p)
{
  if (SCM_UNLIKELY (p < 0 && ((size_t)-p) > h->base))
    /* catch overflow */
    scm_out_of_range (NULL, scm_from_ssize_t (p));
  /* perhaps should catch overflow here too */
  return h->impl->vref (h, h->base + p);
}

SCM_INLINE_IMPLEMENTATION void
scm_array_handle_set (scm_t_array_handle *h, ssize_t p, SCM v)
{
  if (SCM_UNLIKELY (p < 0 && ((size_t)-p) > h->base))
    /* catch overflow */
    scm_out_of_range (NULL, scm_from_ssize_t (p));
  /* perhaps should catch overflow here too */
  h->impl->vset (h, h->base + p, v);
}

SCM_INLINE_IMPLEMENTATION int
scm_is_string (SCM x)
{
  return SCM_HAS_TYP7 (x, scm_tc7_string);
}

#endif
#endif
