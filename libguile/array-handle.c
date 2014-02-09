/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005,
 * 2006, 2009, 2011, 2013, 2014 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/__scm.h"

#include "libguile/array-handle.h"


SCM scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_LAST + 1];


/* Bytevectors as generalized vectors & arrays.  */

#define DEFINE_BYTEVECTOR_ACCESSORS(type, tag, infix)           \
  static SCM                                                    \
  bytevector_##tag##_ref (SCM bv, size_t pos)                   \
  {                                                             \
    SCM idx = scm_from_size_t (pos * sizeof (type));            \
    return scm_bytevector_##infix##_ref (bv, idx);              \
  }                                                             \
  static void                                                   \
  bytevector_##tag##_set (SCM bv, size_t pos, SCM val)          \
  {                                                             \
    SCM idx = scm_from_size_t (pos * sizeof (type));            \
    scm_bytevector_##infix##_set_x (bv, idx, val);              \
  }

DEFINE_BYTEVECTOR_ACCESSORS (uint8_t, u8, u8);
DEFINE_BYTEVECTOR_ACCESSORS (int8_t, s8, s8);
DEFINE_BYTEVECTOR_ACCESSORS (uint16_t, u16, u16_native);
DEFINE_BYTEVECTOR_ACCESSORS (int16_t, s16, s16_native);
DEFINE_BYTEVECTOR_ACCESSORS (uint32_t, u32, u32_native);
DEFINE_BYTEVECTOR_ACCESSORS (int32_t, s32, s32_native);
DEFINE_BYTEVECTOR_ACCESSORS (uint64_t, u64, u64_native);
DEFINE_BYTEVECTOR_ACCESSORS (int64_t, s64, s64_native);
DEFINE_BYTEVECTOR_ACCESSORS (float, f32, ieee_single_native);
DEFINE_BYTEVECTOR_ACCESSORS (double, f64, ieee_double_native);

/* Since these functions are only called by Guile's C code, we can abort
   instead of throwing if there is an error.  */
static SCM
bytevector_c32_ref (SCM bv, size_t pos)
{
  char *c_bv;
  float real, imag;

  if (!SCM_BYTEVECTOR_P (bv))
    abort ();
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  pos *= 2 * sizeof (float);
  if (pos + 2 * sizeof (float) - 1 >= SCM_BYTEVECTOR_LENGTH (bv))
    abort ();

  memcpy (&real, &c_bv[pos], sizeof (float));
  memcpy (&imag, &c_bv[pos + sizeof (float)], sizeof (float));
  return scm_c_make_rectangular (real, imag);
}

static SCM
bytevector_c64_ref (SCM bv, size_t pos)
{
  char *c_bv;
  double real, imag;

  if (!SCM_BYTEVECTOR_P (bv))
    abort ();
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  pos *= 2 * sizeof (double);
  if (pos + 2 * sizeof (double) - 1 >= SCM_BYTEVECTOR_LENGTH (bv))
    abort ();

  memcpy (&real, &c_bv[pos], sizeof (double));
  memcpy (&imag, &c_bv[pos + sizeof (double)], sizeof (double));
  return scm_c_make_rectangular (real, imag);
}

static void
bytevector_c32_set (SCM bv, size_t pos, SCM val)
{
  char *c_bv;
  float real, imag;

  if (!SCM_BYTEVECTOR_P (bv))
    abort ();
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  pos *= 2 * sizeof (float);
  if (pos + 2 * sizeof (float) - 1 >= SCM_BYTEVECTOR_LENGTH (bv))
    abort ();

  real = scm_c_real_part (val);
  imag = scm_c_imag_part (val);
  memcpy (&c_bv[pos], &real, sizeof (float));
  memcpy (&c_bv[pos + sizeof (float)], &imag, sizeof (float));
}

static void
bytevector_c64_set (SCM bv, size_t pos, SCM val)
{
  char *c_bv;
  double real, imag;

  if (!SCM_BYTEVECTOR_P (bv))
    abort ();
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);
  pos *= 2 * sizeof (double);
  if (pos + 2 * sizeof (double) - 1 >= SCM_BYTEVECTOR_LENGTH (bv))
    abort ();

  real = scm_c_real_part (val);
  imag = scm_c_imag_part (val);
  memcpy (&c_bv[pos], &real, sizeof (double));
  memcpy (&c_bv[pos + sizeof (double)], &imag, sizeof (double));
}

static void
initialize_vector_handle (scm_t_array_handle *h, size_t len,
                          scm_t_array_element_type element_type,
                          scm_t_vector_ref vref, scm_t_vector_set vset,
                          void *writable_elements)
{
  h->base = 0;
  h->ndims = 1;
  h->dims = &h->dim0;
  h->dim0.lbnd = 0;
  h->dim0.ubnd = (ssize_t) (len - 1U);
  h->dim0.inc = 1;
  h->element_type = element_type;
  h->elements = h->writable_elements = writable_elements;
  h->vector = h->array;
  h->vref = vref;
  h->vset = vset;
}

void
scm_array_get_handle (SCM array, scm_t_array_handle *h)
{
  if (!SCM_HEAP_OBJECT_P (array))
    scm_wrong_type_arg_msg (NULL, 0, array, "array");

  h->array = array;

  switch (SCM_TYP7 (array))
    {
    case scm_tc7_string:
      initialize_vector_handle (h, scm_c_string_length (array),
                                SCM_ARRAY_ELEMENT_TYPE_CHAR,
                                scm_c_string_ref, scm_c_string_set_x,
                                NULL);
      break;
    case scm_tc7_vector:
      initialize_vector_handle (h, scm_c_vector_length (array),
                                SCM_ARRAY_ELEMENT_TYPE_SCM,
                                scm_c_vector_ref, scm_c_vector_set_x,
                                SCM_I_VECTOR_WELTS (array));
      break;
    case scm_tc7_bitvector:
      initialize_vector_handle (h, scm_c_bitvector_length (array),
                                SCM_ARRAY_ELEMENT_TYPE_BIT,
                                scm_c_bitvector_ref, scm_c_bitvector_set_x,
                                scm_i_bitvector_bits (array));
      break;
    case scm_tc7_bytevector:
      {
        size_t byte_length, length, element_byte_size;
        scm_t_array_element_type element_type;
        scm_t_vector_ref vref;
        scm_t_vector_set vset;

        byte_length = scm_c_bytevector_length (array);
        element_type = SCM_BYTEVECTOR_ELEMENT_TYPE (array);
        element_byte_size = scm_i_array_element_type_sizes[element_type] / 8;
        length = byte_length / element_byte_size;

        switch (element_type)
          {
#define ACCESSOR_CASE(tag, TAG)                 \
          case SCM_ARRAY_ELEMENT_TYPE_##TAG:    \
            vref = bytevector_##tag##_ref;      \
            vset = bytevector_##tag##_set;      \
            break

          case SCM_ARRAY_ELEMENT_TYPE_VU8:
          ACCESSOR_CASE(u8, U8);
          ACCESSOR_CASE(s8, S8);
          ACCESSOR_CASE(u16, U16);
          ACCESSOR_CASE(s16, S16);
          ACCESSOR_CASE(u32, U32);
          ACCESSOR_CASE(s32, S32);
          ACCESSOR_CASE(u64, U64);
          ACCESSOR_CASE(s64, S64);
          ACCESSOR_CASE(f32, F32);
          ACCESSOR_CASE(f64, F64);
          ACCESSOR_CASE(c32, C32);
          ACCESSOR_CASE(c64, C64);

          case SCM_ARRAY_ELEMENT_TYPE_SCM:
          case SCM_ARRAY_ELEMENT_TYPE_BIT:
          case SCM_ARRAY_ELEMENT_TYPE_CHAR:
          default:
            abort ();

#undef ACCESSOR_CASE
          }

        initialize_vector_handle (h, length, element_type, vref, vset,
                                  SCM_BYTEVECTOR_CONTENTS (array));
      }
      break;
    case scm_tc7_array:
      h->base = SCM_I_ARRAY_BASE (array);
      h->ndims = SCM_I_ARRAY_NDIM (array);
      h->dims = SCM_I_ARRAY_DIMS (array);
      {
        scm_t_array_handle vh;

        scm_array_get_handle (SCM_I_ARRAY_V (array), &vh);
        h->element_type = vh.element_type;
        h->elements = vh.elements;
        h->writable_elements = vh.writable_elements;
        h->vector = vh.vector;
        h->vref = vh.vref;
        h->vset = vh.vset;
        scm_array_handle_release (&vh);
      }
      break;
    default:
      scm_wrong_type_arg_msg (NULL, 0, array, "array");
    }
}

ssize_t
scm_array_handle_pos (scm_t_array_handle *h, SCM indices)
{
  scm_t_array_dim *s = scm_array_handle_dims (h);
  ssize_t pos = 0, i;
  size_t k = scm_array_handle_rank (h);
  
  while (k > 0 && scm_is_pair (indices))
    {
      i = scm_to_signed_integer (SCM_CAR (indices), s->lbnd, s->ubnd);
      pos += (i - s->lbnd) * s->inc;
      k--;
      s++;
      indices = SCM_CDR (indices);
    }
  if (k > 0 || !scm_is_null (indices))
    scm_misc_error (NULL, "wrong number of indices, expecting ~a",
		    scm_list_1 (scm_from_size_t (scm_array_handle_rank (h))));
  return pos;
}

static void
check_array_index_bounds (scm_t_array_dim *dim, ssize_t idx)
{
  if (idx < dim->lbnd || idx > dim->ubnd)
    scm_error (scm_out_of_range_key, NULL, "Value out of range ~S to ~S: ~S",
               scm_list_3 (scm_from_ssize_t (dim->lbnd),
                           scm_from_ssize_t (dim->ubnd),
                           scm_from_ssize_t (idx)),
               scm_list_1 (scm_from_ssize_t (idx)));
}

ssize_t
scm_array_handle_pos_1 (scm_t_array_handle *h, ssize_t idx0)
{
  scm_t_array_dim *dim = scm_array_handle_dims (h);

  if (scm_array_handle_rank (h) != 1)
    scm_misc_error (NULL, "wrong number of indices, expecting ~A",
		    scm_list_1 (scm_from_size_t (scm_array_handle_rank (h))));

  check_array_index_bounds (&dim[0], idx0);

  return (idx0 - dim[0].lbnd) * dim[0].inc;
}

ssize_t
scm_array_handle_pos_2 (scm_t_array_handle *h, ssize_t idx0, ssize_t idx1)
{
  scm_t_array_dim *dim = scm_array_handle_dims (h);

  if (scm_array_handle_rank (h) != 2)
    scm_misc_error (NULL, "wrong number of indices, expecting ~A",
		    scm_list_1 (scm_from_size_t (scm_array_handle_rank (h))));

  check_array_index_bounds (&dim[0], idx0);
  check_array_index_bounds (&dim[1], idx1);

  return ((idx0 - dim[0].lbnd) * dim[0].inc
          + (idx1 - dim[1].lbnd) * dim[1].inc);
}

SCM
scm_array_handle_element_type (scm_t_array_handle *h)
{
  if (h->element_type < 0 || h->element_type > SCM_ARRAY_ELEMENT_TYPE_LAST)
    abort (); /* guile programming error */
  return scm_i_array_element_types[h->element_type];
}

void
scm_array_handle_release (scm_t_array_handle *h)
{
  /* Nothing to do here until arrays need to be reserved for real.
   */
}

const SCM *
scm_array_handle_elements (scm_t_array_handle *h)
{
  if (h->element_type != SCM_ARRAY_ELEMENT_TYPE_SCM)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "non-uniform array");
  return ((const SCM*)h->elements) + h->base;
}

SCM *
scm_array_handle_writable_elements (scm_t_array_handle *h)
{
  if (h->element_type != SCM_ARRAY_ELEMENT_TYPE_SCM)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "non-uniform array");
  return ((SCM*)h->elements) + h->base;
}

void
scm_init_array_handle (void)
{
#define DEFINE_ARRAY_TYPE(tag, TAG)                             \
  scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_##TAG] = scm_from_utf8_symbol (#tag)
  
  scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_SCM] = SCM_BOOL_T;
  DEFINE_ARRAY_TYPE (a, CHAR);
  DEFINE_ARRAY_TYPE (b, BIT);
  DEFINE_ARRAY_TYPE (vu8, VU8);
  DEFINE_ARRAY_TYPE (u8, U8);
  DEFINE_ARRAY_TYPE (s8, S8);
  DEFINE_ARRAY_TYPE (u16, U16);
  DEFINE_ARRAY_TYPE (s16, S16);
  DEFINE_ARRAY_TYPE (u32, U32);
  DEFINE_ARRAY_TYPE (s32, S32);
  DEFINE_ARRAY_TYPE (u64, U64);
  DEFINE_ARRAY_TYPE (s64, S64);
  DEFINE_ARRAY_TYPE (f32, F32);
  DEFINE_ARRAY_TYPE (f64, F64);
  DEFINE_ARRAY_TYPE (c32, C32);
  DEFINE_ARRAY_TYPE (c64, C64);

#include "libguile/array-handle.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
