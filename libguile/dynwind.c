/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003, 2004, 2006, 2008, 2010, 2011, 2012 Free Software Foundation, Inc.
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

#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/dynstack.h"
#include "libguile/eval.h"
#include "libguile/ports.h"

#include "libguile/dynwind.h"




SCM
scm_dynamic_wind (SCM in_guard, SCM thunk, SCM out_guard)
#define FUNC_NAME "dynamic-wind"
{
  SCM ans;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;

  SCM_ASSERT (scm_is_true (scm_thunk_p (out_guard)), out_guard,
	      SCM_ARG3, FUNC_NAME);

  scm_call_0 (in_guard);
  scm_dynstack_push_dynwind (&thread->dynstack, in_guard, out_guard);

  ans = scm_call_0 (thunk);

  scm_dynstack_pop (&thread->dynstack);
  scm_call_0 (out_guard);

  return ans;
}
#undef FUNC_NAME


void
scm_dynwind_begin (scm_t_dynwind_flags flags)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;

  scm_dynstack_push_frame (&thread->dynstack, flags);
}

void
scm_dynwind_end (void)
{
  scm_dynstack_unwind_frame (&SCM_I_CURRENT_THREAD->dynstack);
}

void
scm_dynwind_unwind_handler (void (*proc) (void *), void *data,
			    scm_t_wind_flags flags)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_dynstack *dynstack = &thread->dynstack;

  scm_dynstack_push_unwinder (dynstack, flags, proc, data);
}

void
scm_dynwind_rewind_handler (void (*proc) (void *), void *data,
			    scm_t_wind_flags flags)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_dynstack *dynstack = &thread->dynstack;

  scm_dynstack_push_rewinder (dynstack, 0, proc, data);

  if (flags & SCM_F_WIND_EXPLICITLY)
    proc (data);
}

void
scm_dynwind_unwind_handler_with_scm (void (*proc) (SCM), SCM data,
				     scm_t_wind_flags flags)
{
  /* FIXME: This is not a safe cast.  */
  scm_dynwind_unwind_handler ((scm_t_guard) proc, SCM2PTR (data), flags);
}

void
scm_dynwind_rewind_handler_with_scm (void (*proc) (SCM), SCM data,
				     scm_t_wind_flags flags)
{
  /* FIXME: This is not a safe cast.  */
  scm_dynwind_rewind_handler ((scm_t_guard) proc, SCM2PTR (data), flags);
}

void
scm_dynwind_free (void *mem)
{
  scm_dynwind_unwind_handler (free, mem, SCM_F_WIND_EXPLICITLY);
}

void
scm_swap_bindings (SCM vars, SCM vals)
{
  SCM tmp;
  while (scm_is_pair (vals))
    {
      tmp = SCM_VARIABLE_REF (SCM_CAR (vars));
      SCM_VARIABLE_SET (SCM_CAR (vars), SCM_CAR (vals));
      SCM_SETCAR (vals, tmp);
      vars = SCM_CDR (vars);
      vals = SCM_CDR (vals);
    }
}

void
scm_init_dynwind ()
{
#include "libguile/dynwind.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
