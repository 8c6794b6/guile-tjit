#ifndef SCM_FINALIZERS_H
#define SCM_FINALIZERS_H

/* Copyright (C) 2012, 2013 Free Software Foundation, Inc.
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



typedef void (*scm_t_finalizer_proc) (void *obj, void *data);

SCM_INTERNAL void scm_i_set_finalizer (void *obj, scm_t_finalizer_proc,
                                       void *data);
SCM_INTERNAL void scm_i_add_finalizer (void *obj, scm_t_finalizer_proc,
                                       void *data);
SCM_INTERNAL void scm_i_add_resuscitator (void *obj, scm_t_finalizer_proc,
                                          void *data);

SCM_INTERNAL void scm_i_finalizer_pre_fork (void);

/* CALLBACK will be called on OBJ after each garbage collection, as long
   as OBJ is accessible.  It will be called from a finalizer, which may
   be from an async or from another thread. */
SCM_INTERNAL void scm_i_register_weak_gc_callback (SCM obj, void (*callback) (SCM));

SCM_INTERNAL void scm_init_finalizers (void);
SCM_INTERNAL void scm_init_finalizer_thread (void);

#endif  /* SCM_FINALIZERS_H */
