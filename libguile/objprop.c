/*	Copyright (C) 1995,1996, 2000, 2001, 2003, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/hashtab.h"
#include "libguile/alist.h"
#include "libguile/root.h"
#include "libguile/weaks.h"

#include "libguile/objprop.h"


/* {Object Properties}
 */

static SCM object_whash;

SCM_DEFINE (scm_object_properties, "object-properties", 1, 0, 0, 
           (SCM obj),
	    "Return @var{obj}'s property list.")
#define FUNC_NAME s_scm_object_properties
{
  return scm_weak_table_refq (object_whash, obj, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_set_object_properties_x, "set-object-properties!", 2, 0, 0,
	    (SCM obj, SCM alist),
	    "Set @var{obj}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_object_properties_x
{
  scm_weak_table_putq_x (object_whash, obj, alist);

  return alist;
}
#undef FUNC_NAME

SCM_DEFINE (scm_object_property, "object-property", 2, 0, 0,
           (SCM obj, SCM key),
	    "Return the property of @var{obj} with name @var{key}.")
#define FUNC_NAME s_scm_object_property
{
  SCM assoc;
  assoc = scm_assq (key, scm_object_properties (obj));
  return (SCM_NIMP (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_object_property_x, "set-object-property!", 3, 0, 0,
	    (SCM obj, SCM key, SCM value),
	    "In @var{obj}'s property list, set the property named @var{key}\n"
	    "to @var{value}.")
#define FUNC_NAME s_scm_set_object_property_x
{
  SCM alist;
  SCM assoc;

  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  alist = scm_weak_table_refq (object_whash, obj, SCM_EOL);
  assoc = scm_assq (key, alist);
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, value);
  else
    scm_weak_table_putq_x (object_whash, obj, scm_acons (key, value, alist));
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  return value;
}
#undef FUNC_NAME


void
scm_init_objprop ()
{
  object_whash = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
#include "libguile/objprop.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
