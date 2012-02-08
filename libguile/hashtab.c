/* Copyright (C) 1995, 1996, 1998, 1999, 2000, 2001, 2003, 2004, 2006,
 *   2008, 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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

#include <alloca.h>
#include <stdio.h>
#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/hash.h"
#include "libguile/eval.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/ports.h"
#include "libguile/bdw-gc.h"

#include "libguile/validate.h"
#include "libguile/hashtab.h"




/* A hash table is a cell containing a vector of association lists.
 *
 * Growing or shrinking, with following rehashing, is triggered when
 * the load factor
 *
 *   L = N / S    (N: number of items in table, S: bucket vector length)
 *
 * passes an upper limit of 0.9 or a lower limit of 0.25.
 *
 * The implementation stores the upper and lower number of items which
 * trigger a resize in the hashtable object.
 *
 * Possible hash table sizes (primes) are stored in the array
 * hashtable_size.
 */

static unsigned long hashtable_size[] = {
  31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363,
  224717, 449419, 898823, 1797641, 3595271, 7190537, 14381041
#if SIZEOF_SCM_T_BITS > 4
  /* vector lengths are stored in the first word of vectors, shifted by
     8 bits for the tc8, so for 32-bit we only get 2^24-1 = 16777215
     elements.  But we allow a few more sizes for 64-bit. */
  , 28762081, 57524111, 115048217, 230096423, 460192829
#endif
};

#define HASHTABLE_SIZE_N (sizeof(hashtable_size)/sizeof(unsigned long))

static char *s_hashtable = "hashtable";

static SCM
make_hash_table (unsigned long k, const char *func_name) 
{
  SCM vector;
  scm_t_hashtable *t;
  int i = 0, n = k ? k : 31;
  while (i + 1 < HASHTABLE_SIZE_N && n > hashtable_size[i])
    ++i;
  n = hashtable_size[i];

  vector = scm_c_make_vector (n, SCM_EOL);

  t = scm_gc_malloc_pointerless (sizeof (*t), s_hashtable);
  t->min_size_index = t->size_index = i;
  t->n_items = 0;
  t->lower = 0;
  t->upper = 9 * n / 10;

  /* FIXME: we just need two words of storage, not three */
  return scm_double_cell (scm_tc7_hashtable, SCM_UNPACK (vector),
                          (scm_t_bits)t, 0);
}

void
scm_i_rehash (SCM table,
	      scm_t_hash_fn hash_fn,
	      void *closure,
	      const char* func_name)
{
  SCM buckets, new_buckets;
  int i;
  unsigned long old_size;
  unsigned long new_size;

  if (SCM_HASHTABLE_N_ITEMS (table) < SCM_HASHTABLE_LOWER (table))
    {
      /* rehashing is not triggered when i <= min_size */
      i = SCM_HASHTABLE (table)->size_index;
      do
	--i;
      while (i > SCM_HASHTABLE (table)->min_size_index
	     && SCM_HASHTABLE_N_ITEMS (table) < hashtable_size[i] / 4);
    }
  else
    {
      i = SCM_HASHTABLE (table)->size_index + 1;
      if (i >= HASHTABLE_SIZE_N)
	/* don't rehash */
	return;
    }
  SCM_HASHTABLE (table)->size_index = i;
  
  new_size = hashtable_size[i];
  if (i <= SCM_HASHTABLE (table)->min_size_index)
    SCM_HASHTABLE (table)->lower = 0;
  else
    SCM_HASHTABLE (table)->lower = new_size / 4;
  SCM_HASHTABLE (table)->upper = 9 * new_size / 10;
  buckets = SCM_HASHTABLE_VECTOR (table);

  new_buckets = scm_c_make_vector (new_size, SCM_EOL);

  SCM_SET_HASHTABLE_VECTOR (table, new_buckets);
  SCM_SET_HASHTABLE_N_ITEMS (table, 0);

  old_size = SCM_SIMPLE_VECTOR_LENGTH (buckets);
  for (i = 0; i < old_size; ++i)
    {
      SCM ls, cell, handle;

      ls = SCM_SIMPLE_VECTOR_REF (buckets, i);
      SCM_SIMPLE_VECTOR_SET (buckets, i, SCM_EOL);

      while (scm_is_pair (ls))
	{
	  unsigned long h;

	  cell = ls;
	  handle = SCM_CAR (cell);
	  ls = SCM_CDR (ls);

	  h = hash_fn (SCM_CAR (handle), new_size, closure);
	  if (h >= new_size)
	    scm_out_of_range (func_name, scm_from_ulong (h));
	  SCM_SETCDR (cell, SCM_SIMPLE_VECTOR_REF (new_buckets, h));
	  SCM_SIMPLE_VECTOR_SET (new_buckets, h, cell);
	  SCM_HASHTABLE_INCREMENT (table);
	}
    }
}


void
scm_i_hashtable_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<hash-table ", port);
  scm_uintprint (SCM_UNPACK (exp), 16, port);
  scm_putc (' ', port);
  scm_uintprint (SCM_HASHTABLE_N_ITEMS (exp), 10, port);
  scm_putc_unlocked ('/', port);
  scm_uintprint (SCM_SIMPLE_VECTOR_LENGTH (SCM_HASHTABLE_VECTOR (exp)),
		 10, port);
  scm_puts_unlocked (">", port);
}


SCM
scm_c_make_hash_table (unsigned long k)
{
  return make_hash_table (k, "scm_c_make_hash_table");
}

SCM_DEFINE (scm_make_hash_table, "make-hash-table", 0, 1, 0,
	    (SCM n),
	    "Make a new abstract hash table object with minimum number of buckets @var{n}\n")
#define FUNC_NAME s_scm_make_hash_table
{
  return make_hash_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n), FUNC_NAME);
}
#undef FUNC_NAME

#define SCM_WEAK_TABLE_P(x) (scm_is_true (scm_weak_table_p (x)))

SCM_DEFINE (scm_hash_table_p, "hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is an abstract hash table object.")
#define FUNC_NAME s_scm_hash_table_p
{
  return scm_from_bool (SCM_HASHTABLE_P (obj) || SCM_WEAK_TABLE_P (obj));
}
#undef FUNC_NAME


/* Accessing hash table entries.  */

SCM
scm_hash_fn_get_handle (SCM table, SCM obj,
			scm_t_hash_fn hash_fn, scm_t_assoc_fn assoc_fn,
			void * closure)
#define FUNC_NAME "scm_hash_fn_get_handle"
{
  unsigned long k;
  SCM buckets, h;

  SCM_VALIDATE_HASHTABLE (SCM_ARG1, table);
  buckets = SCM_HASHTABLE_VECTOR (table);

  if (SCM_SIMPLE_VECTOR_LENGTH (buckets) == 0)
    return SCM_BOOL_F;
  k = hash_fn (obj, SCM_SIMPLE_VECTOR_LENGTH (buckets), closure);
  if (k >= SCM_SIMPLE_VECTOR_LENGTH (buckets))
    scm_out_of_range (FUNC_NAME, scm_from_ulong (k));

  h = assoc_fn (obj, SCM_SIMPLE_VECTOR_REF (buckets, k), closure);

  return h;
}
#undef FUNC_NAME


SCM
scm_hash_fn_create_handle_x (SCM table, SCM obj, SCM init,
			     scm_t_hash_fn hash_fn, scm_t_assoc_fn assoc_fn,
                             void * closure)
#define FUNC_NAME "scm_hash_fn_create_handle_x"
{
  unsigned long k;
  SCM buckets, it;

  SCM_VALIDATE_HASHTABLE (SCM_ARG1, table);
  buckets = SCM_HASHTABLE_VECTOR (table);

  if (SCM_SIMPLE_VECTOR_LENGTH (buckets) == 0)
    SCM_MISC_ERROR ("void hashtable", SCM_EOL);

  k = hash_fn (obj, SCM_SIMPLE_VECTOR_LENGTH (buckets), closure);
  if (k >= SCM_SIMPLE_VECTOR_LENGTH (buckets))
    scm_out_of_range ("hash_fn_create_handle_x", scm_from_ulong (k));

  it = assoc_fn (obj, SCM_SIMPLE_VECTOR_REF (buckets, k), closure);

  if (scm_is_pair (it))
    return it;
  else if (scm_is_true (it))
    scm_wrong_type_arg_msg (NULL, 0, it, "a pair");
  else
    {
      SCM handle, new_bucket;

      handle = scm_cons (obj, init);
      new_bucket = scm_cons (handle, SCM_EOL);

      if (!scm_is_eq (SCM_HASHTABLE_VECTOR (table), buckets))
	{
	  buckets = SCM_HASHTABLE_VECTOR (table);
	  k = hash_fn (obj, SCM_SIMPLE_VECTOR_LENGTH (buckets), closure);
	  if (k >= SCM_SIMPLE_VECTOR_LENGTH (buckets))
	    scm_out_of_range ("hash_fn_create_handle_x", scm_from_ulong (k));
	}
      SCM_SETCDR (new_bucket, SCM_SIMPLE_VECTOR_REF (buckets, k));
      SCM_SIMPLE_VECTOR_SET (buckets, k, new_bucket);
      SCM_HASHTABLE_INCREMENT (table);

      /* Maybe rehash the table.  */
      if (SCM_HASHTABLE_N_ITEMS (table) < SCM_HASHTABLE_LOWER (table)
          || SCM_HASHTABLE_N_ITEMS (table) > SCM_HASHTABLE_UPPER (table))
        scm_i_rehash (table, hash_fn, closure, FUNC_NAME);
      return SCM_CAR (new_bucket);
    }
}
#undef FUNC_NAME


SCM
scm_hash_fn_ref (SCM table, SCM obj, SCM dflt,
		 scm_t_hash_fn hash_fn, scm_t_assoc_fn assoc_fn,
                 void *closure)
{
  SCM it = scm_hash_fn_get_handle (table, obj, hash_fn, assoc_fn, closure);
  if (scm_is_pair (it))
    return SCM_CDR (it);
  else
    return dflt;
}

SCM
scm_hash_fn_set_x (SCM table, SCM obj, SCM val,
		   scm_t_hash_fn hash_fn, scm_t_assoc_fn assoc_fn,
                   void *closure)
{
  SCM pair;

  pair = scm_hash_fn_create_handle_x (table, obj, val,
                                      hash_fn, assoc_fn, closure);

  if (!scm_is_eq (SCM_CDR (pair), val))
    SCM_SETCDR (pair, val);
  
  return val;
}


SCM
scm_hash_fn_remove_x (SCM table, SCM obj,
		      scm_t_hash_fn hash_fn,
		      scm_t_assoc_fn assoc_fn,
                      void *closure)
#define FUNC_NAME "hash_fn_remove_x"
{
  unsigned long k;
  SCM buckets, h;

  SCM_VALIDATE_HASHTABLE (SCM_ARG1, table);

  buckets = SCM_HASHTABLE_VECTOR (table);

  if (SCM_SIMPLE_VECTOR_LENGTH (buckets) == 0)
    return SCM_EOL;

  k = hash_fn (obj, SCM_SIMPLE_VECTOR_LENGTH (buckets), closure);
  if (k >= SCM_SIMPLE_VECTOR_LENGTH (buckets))
    scm_out_of_range (FUNC_NAME, scm_from_ulong (k));

  h = assoc_fn (obj, SCM_SIMPLE_VECTOR_REF (buckets, k), closure);

  if (scm_is_true (h))
    {
      SCM_SIMPLE_VECTOR_SET 
	(buckets, k, scm_delq_x (h, SCM_SIMPLE_VECTOR_REF (buckets, k)));
      SCM_HASHTABLE_DECREMENT (table);
      if (SCM_HASHTABLE_N_ITEMS (table) < SCM_HASHTABLE_LOWER (table))
        scm_i_rehash (table, hash_fn, closure, FUNC_NAME);
    }
  return h;
}
#undef FUNC_NAME

SCM_DEFINE (scm_hash_clear_x, "hash-clear!", 1, 0, 0,
	    (SCM table),
	    "Remove all items from @var{table} (without triggering a resize).")
#define FUNC_NAME s_scm_hash_clear_x
{
  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_clear_x (table);

  SCM_VALIDATE_HASHTABLE (SCM_ARG1, table);

  scm_vector_fill_x (SCM_HASHTABLE_VECTOR (table), SCM_EOL);
  SCM_SET_HASHTABLE_N_ITEMS (table, 0);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_get_handle, "hashq-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_get_handle
{
  return scm_hash_fn_get_handle (table, key,
				 (scm_t_hash_fn) scm_ihashq,
				 (scm_t_assoc_fn) scm_sloppy_assq,
				 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_create_handle_x, "hashq-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashq_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init,
				      (scm_t_hash_fn) scm_ihashq,
				      (scm_t_assoc_fn) scm_sloppy_assq,
				      0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashq_ref, "hashq-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{dflt} (or @code{#f} if no @var{dflt} argument\n"
	    "is supplied).  Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;

  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_refq (table, key, dflt);

  return scm_hash_fn_ref (table, key, dflt,
			  (scm_t_hash_fn) scm_ihashq,
			  (scm_t_assoc_fn) scm_sloppy_assq,
			  0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_set_x, "hashq-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{val} there. Uses @code{eq?} for equality testing.")
#define FUNC_NAME s_scm_hashq_set_x
{
  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_putq_x (table, key, val);

  return scm_hash_fn_set_x (table, key, val,
			    (scm_t_hash_fn) scm_ihashq,
			    (scm_t_assoc_fn) scm_sloppy_assq,
			    0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashq_remove_x, "hashq-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{eq?} for equality tests.")
#define FUNC_NAME s_scm_hashq_remove_x
{
  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_remq_x (table, key);

  return scm_hash_fn_remove_x (table, key,
			       (scm_t_hash_fn) scm_ihashq,
			       (scm_t_assoc_fn) scm_sloppy_assq,
			       0);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashv_get_handle, "hashv-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_get_handle
{
  return scm_hash_fn_get_handle (table, key,
				 (scm_t_hash_fn) scm_ihashv,
				 (scm_t_assoc_fn) scm_sloppy_assv,
				 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_create_handle_x, "hashv-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hashv_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init,
				      (scm_t_hash_fn) scm_ihashv,
				      (scm_t_assoc_fn) scm_sloppy_assv,
				      0);
}
#undef FUNC_NAME


static int
assv_predicate (SCM k, SCM v, void *closure)
{
  return scm_is_true (scm_eqv_p (k, SCM_PACK_POINTER (closure)));
}

SCM_DEFINE (scm_hashv_ref, "hashv-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{dflt} (or @code{#f} if no @var{dflt} argument\n"
	    "is supplied).  Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;

  if (SCM_WEAK_TABLE_P (table))
    return scm_c_weak_table_ref (table, scm_ihashv (key, -1),
                                 assv_predicate, SCM_PACK (key), dflt);

  return scm_hash_fn_ref (table, key, dflt,
			  (scm_t_hash_fn) scm_ihashv,
			  (scm_t_assoc_fn) scm_sloppy_assv,
			  0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashv_set_x, "hashv-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{value} there. Uses @code{eqv?} for equality testing.")
#define FUNC_NAME s_scm_hashv_set_x
{
  if (SCM_WEAK_TABLE_P (table))
    {
      scm_c_weak_table_put_x (table, scm_ihashv (key, -1),
                              assv_predicate, SCM_PACK (key),
                              key, val);
      return SCM_UNSPECIFIED;
    }

  return scm_hash_fn_set_x (table, key, val,
			    (scm_t_hash_fn) scm_ihashv,
			    (scm_t_assoc_fn) scm_sloppy_assv,
			    0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashv_remove_x, "hashv-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{eqv?} for equality tests.")
#define FUNC_NAME s_scm_hashv_remove_x
{
  if (SCM_WEAK_TABLE_P (table))
    {
      scm_c_weak_table_remove_x (table, scm_ihashv (key, -1),
                                 assv_predicate, SCM_PACK (key));
      return SCM_UNSPECIFIED;
    }

  return scm_hash_fn_remove_x (table, key,
			       (scm_t_hash_fn) scm_ihashv,
			       (scm_t_assoc_fn) scm_sloppy_assv,
			       0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_get_handle, "hash-get-handle", 2, 0, 0,
            (SCM table, SCM key),
	    "This procedure returns the @code{(key . value)} pair from the\n"
	    "hash table @var{table}.  If @var{table} does not hold an\n"
	    "associated value for @var{key}, @code{#f} is returned.\n"
	    "Uses @code{equal?} for equality testing.")
#define FUNC_NAME s_scm_hash_get_handle
{
  return scm_hash_fn_get_handle (table, key,
				 (scm_t_hash_fn) scm_ihash,
				 (scm_t_assoc_fn) scm_sloppy_assoc,
				 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hash_create_handle_x, "hash-create-handle!", 3, 0, 0,
            (SCM table, SCM key, SCM init),
	    "This function looks up @var{key} in @var{table} and returns its handle.\n"
	    "If @var{key} is not already present, a new handle is created which\n"
	    "associates @var{key} with @var{init}.")
#define FUNC_NAME s_scm_hash_create_handle_x
{
  return scm_hash_fn_create_handle_x (table, key, init,
				      (scm_t_hash_fn) scm_ihash,
				      (scm_t_assoc_fn) scm_sloppy_assoc,
				      0);
}
#undef FUNC_NAME


static int
assoc_predicate (SCM k, SCM v, void *closure)
{
  return scm_is_true (scm_equal_p (k, SCM_PACK_POINTER (closure)));
}

SCM_DEFINE (scm_hash_ref, "hash-ref", 2, 1, 0,
            (SCM table, SCM key, SCM dflt),
	    "Look up @var{key} in the hash table @var{table}, and return the\n"
	    "value (if any) associated with it.  If @var{key} is not found,\n"
	    "return @var{dflt} (or @code{#f} if no @var{dflt} argument\n"
	    "is supplied).  Uses @code{equal?} for equality testing.")
#define FUNC_NAME s_scm_hash_ref
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;

  if (SCM_WEAK_TABLE_P (table))
    return scm_c_weak_table_ref (table, scm_ihash (key, -1),
                                 assoc_predicate, SCM_PACK (key), dflt);

  return scm_hash_fn_ref (table, key, dflt,
			  (scm_t_hash_fn) scm_ihash,
			  (scm_t_assoc_fn) scm_sloppy_assoc,
			  0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_set_x, "hash-set!", 3, 0, 0,
            (SCM table, SCM key, SCM val),
	    "Find the entry in @var{table} associated with @var{key}, and\n"
	    "store @var{val} there. Uses @code{equal?} for equality\n"
	    "testing.")
#define FUNC_NAME s_scm_hash_set_x
{
  if (SCM_WEAK_TABLE_P (table))
    {
      scm_c_weak_table_put_x (table, scm_ihash (key, -1),
                              assoc_predicate, SCM_PACK (key),
                              key, val);
      return SCM_UNSPECIFIED;
    }

  return scm_hash_fn_set_x (table, key, val,
			    (scm_t_hash_fn) scm_ihash,
			    (scm_t_assoc_fn) scm_sloppy_assoc,
			    0);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hash_remove_x, "hash-remove!", 2, 0, 0,
            (SCM table, SCM key),
	    "Remove @var{key} (and any value associated with it) from\n"
	    "@var{table}.  Uses @code{equal?} for equality tests.")
#define FUNC_NAME s_scm_hash_remove_x
{
  if (SCM_WEAK_TABLE_P (table))
    {
      scm_c_weak_table_remove_x (table, scm_ihash (key, -1),
                                 assoc_predicate, SCM_PACK (key));
      return SCM_UNSPECIFIED;
    }

  return scm_hash_fn_remove_x (table, key,
			       (scm_t_hash_fn) scm_ihash,
			       (scm_t_assoc_fn) scm_sloppy_assoc,
			       0);
}
#undef FUNC_NAME




typedef struct scm_t_ihashx_closure
{
  SCM hash;
  SCM assoc;
  SCM key;
} scm_t_ihashx_closure;

static unsigned long
scm_ihashx (SCM obj, unsigned long n, void *arg)
{
  SCM answer;
  scm_t_ihashx_closure *closure = (scm_t_ihashx_closure *) arg;
  answer = scm_call_2 (closure->hash, obj, scm_from_ulong (n));
  return scm_to_ulong (answer);
}

static SCM
scm_sloppy_assx (SCM obj, SCM alist, void *arg)
{
  scm_t_ihashx_closure *closure = (scm_t_ihashx_closure *) arg;
  return scm_call_2 (closure->assoc, obj, alist);
}

static int
assx_predicate (SCM k, SCM v, void *closure)
{
  scm_t_ihashx_closure *c = (scm_t_ihashx_closure *) closure;

  /* FIXME: The hashx interface is crazy.  Hash tables have nothing to
     do with alists in principle.  Instead of getting an assoc proc,
     hashx functions should use an equality predicate.  Perhaps we can
     change this before 2.2, but until then, add a terrible, terrible
     hack.  */

  return scm_is_true (scm_call_2 (c->assoc, c->key, scm_acons (k, v, SCM_EOL)));
}


SCM_DEFINE (scm_hashx_get_handle, "hashx-get-handle", 4, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key),
	    "This behaves the same way as the corresponding\n"
	    "@code{-get-handle} function, but uses @var{hash} as a hash\n"
	    "function and @var{assoc} to compare keys.  @code{hash} must be\n"
	    "a function that takes two arguments, a key to be hashed and a\n"
	    "table size.  @code{assoc} must be an associator function, like\n"
	    "@code{assoc}, @code{assq} or @code{assv}.")
#define FUNC_NAME s_scm_hashx_get_handle
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.key = key;

  return scm_hash_fn_get_handle (table, key, scm_ihashx, scm_sloppy_assx,
				 (void *) &closure);
}
#undef FUNC_NAME


SCM_DEFINE (scm_hashx_create_handle_x, "hashx-create-handle!", 5, 0, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key, SCM init),
	    "This behaves the same way as the corresponding\n"
	    "@code{-create-handle} function, but uses @var{hash} as a hash\n"
	    "function and @var{assoc} to compare keys.  @code{hash} must be\n"
	    "a function that takes two arguments, a key to be hashed and a\n"
	    "table size.  @code{assoc} must be an associator function, like\n"
	    "@code{assoc}, @code{assq} or @code{assv}.")
#define FUNC_NAME s_scm_hashx_create_handle_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.key = key;

  return scm_hash_fn_create_handle_x (table, key, init, scm_ihashx,
				      scm_sloppy_assx, (void *)&closure);
}
#undef FUNC_NAME



SCM_DEFINE (scm_hashx_ref, "hashx-ref", 4, 1, 0, 
            (SCM hash, SCM assoc, SCM table, SCM key, SCM dflt),
	    "This behaves the same way as the corresponding @code{ref}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    "By way of illustration, @code{hashq-ref table key} is\n"
	    "equivalent to @code{hashx-ref hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_ref
{
  scm_t_ihashx_closure closure;
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.key = key;

  if (SCM_WEAK_TABLE_P (table))
    {
      unsigned long h = scm_to_ulong (scm_call_2 (hash, key,
                                                  scm_from_ulong (-1)));
      return scm_c_weak_table_ref (table, h, assx_predicate, &closure, dflt);
    }

  return scm_hash_fn_ref (table, key, dflt, scm_ihashx, scm_sloppy_assx,
			  (void *)&closure);
}
#undef FUNC_NAME




SCM_DEFINE (scm_hashx_set_x, "hashx-set!", 5, 0, 0,
            (SCM hash, SCM assoc, SCM table, SCM key, SCM val),
	    "This behaves the same way as the corresponding @code{set!}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    " By way of illustration, @code{hashq-set! table key} is\n"
	    "equivalent to @code{hashx-set!  hashq assq table key}.")
#define FUNC_NAME s_scm_hashx_set_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.key = key;

  if (SCM_WEAK_TABLE_P (table))
    {
      unsigned long h = scm_to_ulong (scm_call_2 (hash, key,
                                                  scm_from_ulong (-1)));
      scm_c_weak_table_put_x (table, h, assx_predicate, &closure, key, val);
      return SCM_UNSPECIFIED;
    }

  return scm_hash_fn_set_x (table, key, val, scm_ihashx, scm_sloppy_assx,
			    (void *)&closure);
}
#undef FUNC_NAME

SCM_DEFINE (scm_hashx_remove_x, "hashx-remove!", 4, 0, 0,
	    (SCM hash, SCM assoc, SCM table, SCM obj),
	    "This behaves the same way as the corresponding @code{remove!}\n"
	    "function, but uses @var{hash} as a hash function and\n"
	    "@var{assoc} to compare keys.  @code{hash} must be a function\n"
	    "that takes two arguments, a key to be hashed and a table size.\n"
	    "@code{assoc} must be an associator function, like @code{assoc},\n"
	    "@code{assq} or @code{assv}.\n"
	    "\n"
	    " By way of illustration, @code{hashq-remove! table key} is\n"
	    "equivalent to @code{hashx-remove!  hashq assq #f table key}.")
#define FUNC_NAME s_scm_hashx_remove_x
{
  scm_t_ihashx_closure closure;
  closure.hash = hash;
  closure.assoc = assoc;
  closure.key = obj;

  if (SCM_WEAK_TABLE_P (table))
    {
      unsigned long h = scm_to_ulong (scm_call_2 (hash, obj,
                                                  scm_from_ulong (-1)));
      scm_c_weak_table_remove_x (table, h, assx_predicate, &closure);
      return SCM_UNSPECIFIED;
    }

  return scm_hash_fn_remove_x (table, obj, scm_ihashx, scm_sloppy_assx,
                               (void *) &closure);
}
#undef FUNC_NAME

/* Hash table iterators */

SCM_DEFINE (scm_hash_fold, "hash-fold", 3, 0, 0, 
            (SCM proc, SCM init, SCM table),
	    "An iterator over hash-table elements.\n"
            "Accumulates and returns a result by applying PROC successively.\n"
            "The arguments to PROC are \"(key value prior-result)\" where key\n"
            "and value are successive pairs from the hash table TABLE, and\n"
            "prior-result is either INIT (for the first application of PROC)\n"
            "or the return value of the previous application of PROC.\n"
            "For example, @code{(hash-fold acons '() tab)} will convert a hash\n"
            "table into an a-list of key-value pairs.")
#define FUNC_NAME s_scm_hash_fold
{
  SCM_VALIDATE_PROC (1, proc);

  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_fold (proc, init, table);

  SCM_VALIDATE_HASHTABLE (3, table);
  return scm_internal_hash_fold ((scm_t_hash_fold_fn) scm_call_3,
				 (void *) SCM_UNPACK (proc), init, table);
}
#undef FUNC_NAME

static SCM
for_each_proc (void *proc, SCM handle)
{
  return scm_call_2 (SCM_PACK (proc), SCM_CAR (handle), SCM_CDR (handle));
}

SCM_DEFINE (scm_hash_for_each, "hash-for-each", 2, 0, 0, 
            (SCM proc, SCM table),
	    "An iterator over hash-table elements.\n"
            "Applies PROC successively on all hash table items.\n"
            "The arguments to PROC are \"(key value)\" where key\n"
            "and value are successive pairs from the hash table TABLE.")
#define FUNC_NAME s_scm_hash_for_each
{
  SCM_VALIDATE_PROC (1, proc);

  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_for_each (proc, table);

  SCM_VALIDATE_HASHTABLE (2, table);
  
  scm_internal_hash_for_each_handle (for_each_proc,
				     (void *) SCM_UNPACK (proc),
				     table);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_hash_for_each_handle, "hash-for-each-handle", 2, 0, 0, 
            (SCM proc, SCM table),
	    "An iterator over hash-table elements.\n"
            "Applies PROC successively on all hash table handles.")
#define FUNC_NAME s_scm_hash_for_each_handle
{
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)), proc, 1, FUNC_NAME);
  SCM_VALIDATE_HASHTABLE (2, table);
  
  scm_internal_hash_for_each_handle ((scm_t_hash_handle_fn) scm_call_1,
				     (void *) SCM_UNPACK (proc),
				     table);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
map_proc (void *proc, SCM key, SCM data, SCM value)
{
  return scm_cons (scm_call_2 (SCM_PACK (proc), key, data), value);
}

SCM_DEFINE (scm_hash_map_to_list, "hash-map->list", 2, 0, 0, 
            (SCM proc, SCM table),
	    "An iterator over hash-table elements.\n"
            "Accumulates and returns as a list the results of applying PROC successively.\n"
            "The arguments to PROC are \"(key value)\" where key\n"
            "and value are successive pairs from the hash table TABLE.")
#define FUNC_NAME s_scm_hash_map_to_list
{
  SCM_VALIDATE_PROC (1, proc);

  if (SCM_WEAK_TABLE_P (table))
    return scm_weak_table_map_to_list (proc, table);

  SCM_VALIDATE_HASHTABLE (2, table);
  return scm_internal_hash_fold (map_proc,
				 (void *) SCM_UNPACK (proc),
				 SCM_EOL,
				 table);
}
#undef FUNC_NAME



SCM
scm_internal_hash_fold (scm_t_hash_fold_fn fn, void *closure,
			SCM init, SCM table)
#define FUNC_NAME s_scm_hash_fold
{
  long i, n;
  SCM buckets, result = init;
  
  if (SCM_WEAK_TABLE_P (table))
    return scm_c_weak_table_fold (fn, closure, init, table);

  SCM_VALIDATE_HASHTABLE (0, table);
  buckets = SCM_HASHTABLE_VECTOR (table);
  
  n = SCM_SIMPLE_VECTOR_LENGTH (buckets);
  for (i = 0; i < n; ++i)
    {
      SCM ls, handle;

      for (ls = SCM_SIMPLE_VECTOR_REF (buckets, i); !scm_is_null (ls);
	   ls = SCM_CDR (ls))
	{
	  handle = SCM_CAR (ls);
          result = fn (closure, SCM_CAR (handle), SCM_CDR (handle), result);
	}
    }

  return result;
}
#undef FUNC_NAME

/* The following redundant code is here in order to be able to support
   hash-for-each-handle.  An alternative would have been to replace
   this code and scm_internal_hash_fold above with a single
   scm_internal_hash_fold_handles, but we don't want to promote such
   an API. */

void
scm_internal_hash_for_each_handle (scm_t_hash_handle_fn fn, void *closure,
				   SCM table)
#define FUNC_NAME s_scm_hash_for_each
{
  long i, n;
  SCM buckets;
  
  SCM_VALIDATE_HASHTABLE (0, table);
  buckets = SCM_HASHTABLE_VECTOR (table);
  n = SCM_SIMPLE_VECTOR_LENGTH (buckets);

  for (i = 0; i < n; ++i)
    {
      SCM ls = SCM_SIMPLE_VECTOR_REF (buckets, i), handle;
      while (!scm_is_null (ls))
	{
	  if (!scm_is_pair (ls))
	    SCM_WRONG_TYPE_ARG (SCM_ARG3, buckets);
	  handle = SCM_CAR (ls);
	  if (!scm_is_pair (handle))
	    SCM_WRONG_TYPE_ARG (SCM_ARG3, buckets);
	  fn (closure, handle);
	  ls = SCM_CDR (ls);
	}
    }
}
#undef FUNC_NAME




void
scm_init_hashtab ()
{
#include "libguile/hashtab.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
