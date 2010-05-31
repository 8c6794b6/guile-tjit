/* Copyright (C) 2001, 2009, 2010 Free Software Foundation, Inc.
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

/* This file is included in vm_engine.c */


/*
 * Predicates
 */

#define ARGS1(a1)	SCM a1 = sp[0];
#define ARGS2(a1,a2)	SCM a1 = sp[-1], a2 = sp[0]; sp--; NULLSTACK (1);
#define ARGS3(a1,a2,a3)	SCM a1 = sp[-2], a2 = sp[-1], a3 = sp[0]; sp -= 2; NULLSTACK (2);

#define RETURN(x)	do { *sp = x; NEXT; } while (0)

VM_DEFINE_FUNCTION (128, not, "not", 1)
{
  ARGS1 (x);
  RETURN (scm_from_bool (scm_is_false (x)));
}

VM_DEFINE_FUNCTION (129, not_not, "not-not", 1)
{
  ARGS1 (x);
  RETURN (scm_from_bool (!scm_is_false (x)));
}

VM_DEFINE_FUNCTION (130, eq, "eq?", 2)
{
  ARGS2 (x, y);
  RETURN (scm_from_bool (scm_is_eq (x, y)));
}

VM_DEFINE_FUNCTION (131, not_eq, "not-eq?", 2)
{
  ARGS2 (x, y);
  RETURN (scm_from_bool (!scm_is_eq (x, y)));
}

VM_DEFINE_FUNCTION (132, nullp, "null?", 1)
{
  ARGS1 (x);
  RETURN (scm_from_bool (scm_is_null (x)));
}

VM_DEFINE_FUNCTION (133, not_nullp, "not-null?", 1)
{
  ARGS1 (x);
  RETURN (scm_from_bool (!scm_is_null (x)));
}

VM_DEFINE_FUNCTION (134, eqv, "eqv?", 2)
{
  ARGS2 (x, y);
  if (scm_is_eq (x, y))
    RETURN (SCM_BOOL_T);
  if (SCM_IMP (x) || SCM_IMP (y))
    RETURN (SCM_BOOL_F);
  SYNC_REGISTER ();
  RETURN (scm_eqv_p (x, y));
}

VM_DEFINE_FUNCTION (135, equal, "equal?", 2)
{
  ARGS2 (x, y);
  if (scm_is_eq (x, y))
    RETURN (SCM_BOOL_T);
  if (SCM_IMP (x) || SCM_IMP (y))
    RETURN (SCM_BOOL_F);
  SYNC_REGISTER ();
  RETURN (scm_equal_p (x, y));
}

VM_DEFINE_FUNCTION (136, pairp, "pair?", 1)
{
  ARGS1 (x);
  RETURN (scm_from_bool (scm_is_pair (x)));
}

VM_DEFINE_FUNCTION (137, listp, "list?", 1)
{
  ARGS1 (x);
  RETURN (scm_from_bool (scm_ilength (x) >= 0));
}


/*
 * Basic data
 */

VM_DEFINE_FUNCTION (138, cons, "cons", 2)
{
  ARGS2 (x, y);
  CONS (x, x, y);
  RETURN (x);
}

#define VM_VALIDATE_CONS(x)                     \
  if (SCM_UNLIKELY (!scm_is_pair (x)))          \
    { finish_args = x;                          \
      goto vm_error_not_a_pair;                 \
    }
  
VM_DEFINE_FUNCTION (139, car, "car", 1)
{
  ARGS1 (x);
  VM_VALIDATE_CONS (x);
  RETURN (SCM_CAR (x));
}

VM_DEFINE_FUNCTION (140, cdr, "cdr", 1)
{
  ARGS1 (x);
  VM_VALIDATE_CONS (x);
  RETURN (SCM_CDR (x));
}

VM_DEFINE_INSTRUCTION (141, set_car, "set-car!", 0, 2, 0)
{
  SCM x, y;
  POP (y);
  POP (x);
  VM_VALIDATE_CONS (x);
  SCM_SETCAR (x, y);
  NEXT;
}

VM_DEFINE_INSTRUCTION (142, set_cdr, "set-cdr!", 0, 2, 0)
{
  SCM x, y;
  POP (y);
  POP (x);
  VM_VALIDATE_CONS (x);
  SCM_SETCDR (x, y);
  NEXT;
}


/*
 * Numeric relational tests
 */

#undef REL
#define REL(crel,srel)						\
{								\
  ARGS2 (x, y);							\
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))			\
    RETURN (scm_from_bool (SCM_I_INUM (x) crel SCM_I_INUM (y)));  \
  SYNC_REGISTER ();                                             \
  RETURN (srel (x, y));                                         \
}

VM_DEFINE_FUNCTION (143, ee, "ee?", 2)
{
  REL (==, scm_num_eq_p);
}

VM_DEFINE_FUNCTION (144, lt, "lt?", 2)
{
  REL (<, scm_less_p);
}

VM_DEFINE_FUNCTION (145, le, "le?", 2)
{
  REL (<=, scm_leq_p);
}

VM_DEFINE_FUNCTION (146, gt, "gt?", 2)
{
  REL (>, scm_gr_p);
}

VM_DEFINE_FUNCTION (147, ge, "ge?", 2)
{
  REL (>=, scm_geq_p);
}


/*
 * Numeric functions
 */

#undef FUNC2
#define FUNC2(CFUNC,SFUNC)				\
{							\
  ARGS2 (x, y);						\
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))		\
    {							\
      scm_t_int64 n = SCM_I_INUM (x) CFUNC SCM_I_INUM (y);\
      if (SCM_FIXABLE (n))				\
	RETURN (SCM_I_MAKINUM (n));			\
    }							\
  SYNC_REGISTER ();					\
  RETURN (SFUNC (x, y));				\
}

VM_DEFINE_FUNCTION (148, add, "add", 2)
{
  FUNC2 (+, scm_sum);
}

VM_DEFINE_FUNCTION (149, add1, "add1", 1)
{
  ARGS1 (x);
  if (SCM_I_INUMP (x))
    {
      scm_t_int64 n = SCM_I_INUM (x) + 1;
      if (SCM_FIXABLE (n))
	RETURN (SCM_I_MAKINUM (n));
    }
  SYNC_REGISTER ();
  RETURN (scm_sum (x, SCM_I_MAKINUM (1)));
}

VM_DEFINE_FUNCTION (150, sub, "sub", 2)
{
  FUNC2 (-, scm_difference);
}

VM_DEFINE_FUNCTION (151, sub1, "sub1", 1)
{
  ARGS1 (x);
  if (SCM_I_INUMP (x))
    {
      scm_t_int64 n = SCM_I_INUM (x) - 1;
      if (SCM_FIXABLE (n))
	RETURN (SCM_I_MAKINUM (n));
    }
  SYNC_REGISTER ();
  RETURN (scm_difference (x, SCM_I_MAKINUM (1)));
}

VM_DEFINE_FUNCTION (152, mul, "mul", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_product (x, y));
}

VM_DEFINE_FUNCTION (153, div, "div", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_divide (x, y));
}

VM_DEFINE_FUNCTION (154, quo, "quo", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_quotient (x, y));
}

VM_DEFINE_FUNCTION (155, rem, "rem", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_remainder (x, y));
}

VM_DEFINE_FUNCTION (156, mod, "mod", 2)
{
  ARGS2 (x, y);
  SYNC_REGISTER ();
  RETURN (scm_modulo (x, y));
}

VM_DEFINE_FUNCTION (157, ash, "ash", 2)
{
  ARGS2 (x, y);
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
    {
      if (SCM_I_INUM (y) < 0)
        /* Right shift, will be a fixnum. */
        RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) >> -SCM_I_INUM (y)));
      else
        /* Left shift. See comments in scm_ash. */
        {
          long nn, bits_to_shift;

          nn = SCM_I_INUM (x);
          bits_to_shift = SCM_I_INUM (y);

          if (bits_to_shift < SCM_I_FIXNUM_BIT-1
              && ((unsigned long)
                  (SCM_SRS (nn, (SCM_I_FIXNUM_BIT-1 - bits_to_shift)) + 1)
                  <= 1))
            RETURN (SCM_I_MAKINUM (nn << bits_to_shift));
          /* fall through */
        }
      /* fall through */
    }
  SYNC_REGISTER ();
  RETURN (scm_ash (x, y));
}

VM_DEFINE_FUNCTION (158, logand, "logand", 2)
{
  ARGS2 (x, y);
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
    RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) & SCM_I_INUM (y)));
  SYNC_REGISTER ();
  RETURN (scm_logand (x, y));
}

VM_DEFINE_FUNCTION (159, logior, "logior", 2)
{
  ARGS2 (x, y);
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
    RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) | SCM_I_INUM (y)));
  SYNC_REGISTER ();
  RETURN (scm_logior (x, y));
}

VM_DEFINE_FUNCTION (160, logxor, "logxor", 2)
{
  ARGS2 (x, y);
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
    RETURN (SCM_I_MAKINUM (SCM_I_INUM (x) ^ SCM_I_INUM (y)));
  SYNC_REGISTER ();
  RETURN (scm_logxor (x, y));
}


/*
 * Vectors and arrays
 */

VM_DEFINE_FUNCTION (161, vector_ref, "vector-ref", 2)
{
  long i = 0;
  ARGS2 (vect, idx);
  if (SCM_LIKELY (SCM_I_IS_NONWEAK_VECTOR (vect)
                  && SCM_I_INUMP (idx)
                  && ((i = SCM_I_INUM (idx)) >= 0)
                  && i < SCM_I_VECTOR_LENGTH (vect)))
    RETURN (SCM_I_VECTOR_ELTS (vect)[i]);
  else
    {
      SYNC_REGISTER ();
      RETURN (scm_vector_ref (vect, idx));
    }
}

VM_DEFINE_INSTRUCTION (162, vector_set, "vector-set", 0, 3, 0)
{
  long i = 0;
  SCM vect, idx, val;
  POP (val); POP (idx); POP (vect);
  if (SCM_LIKELY (SCM_I_IS_NONWEAK_VECTOR (vect)
                  && SCM_I_INUMP (idx)
                  && ((i = SCM_I_INUM (idx)) >= 0)
                  && i < SCM_I_VECTOR_LENGTH (vect)))
    SCM_I_VECTOR_WELTS (vect)[i] = val;
  else
    {
      SYNC_REGISTER ();
      scm_vector_set_x (vect, idx, val);
    }
  NEXT;
}

VM_DEFINE_INSTRUCTION (163, make_array, "make-array", 3, -1, 1)
{
  scm_t_uint32 len;
  SCM shape, ret;

  len = FETCH ();
  len = (len << 8) + FETCH ();
  len = (len << 8) + FETCH ();
  POP (shape);
  SYNC_REGISTER ();
  PRE_CHECK_UNDERFLOW (len);
  ret = scm_from_contiguous_array (shape, sp - len + 1, len);
  DROPN (len);
  PUSH (ret);
  NEXT;
}


/*
 * Structs
 */
#define VM_VALIDATE_STRUCT(obj)			\
  if (SCM_UNLIKELY (!SCM_STRUCTP (obj)))	\
    {						\
      finish_args = (obj);			\
      goto vm_error_not_a_struct;		\
    }

VM_DEFINE_FUNCTION (164, struct_p, "struct?", 1)
{
  ARGS1 (obj);
  RETURN (scm_from_bool (SCM_STRUCTP (obj)));
}

VM_DEFINE_FUNCTION (165, struct_vtable, "struct-vtable", 1)
{
  ARGS1 (obj);
  VM_VALIDATE_STRUCT (obj);
  RETURN (SCM_STRUCT_VTABLE (obj));
}

VM_DEFINE_INSTRUCTION (166, make_struct, "make-struct", 2, -1, 1)
{
  unsigned h = FETCH ();
  unsigned l = FETCH ();
  scm_t_bits n = ((h << 8U) + l);
  SCM vtable = sp[-(n - 1)];
  const SCM *inits = sp - n + 2;
  SCM ret;

  SYNC_REGISTER ();

  if (SCM_LIKELY (SCM_STRUCTP (vtable)
  		  && SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SIMPLE)
                  && (SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size) + 1
                      == n)
                  && !SCM_VTABLE_INSTANCE_FINALIZER (vtable)))
    {
      /* Verily, we are making a simple struct with the right number of
         initializers, and no finalizer. */
      ret = scm_words ((scm_t_bits)SCM_STRUCT_DATA (vtable) | scm_tc3_struct,
                       n + 1);
      SCM_SET_CELL_WORD_1 (ret, (scm_t_bits)SCM_CELL_OBJECT_LOC (ret, 2));
      memcpy (SCM_STRUCT_DATA (ret), inits, (n - 1) * sizeof (SCM));
    }
  else
    ret = scm_c_make_structv (vtable, 0, n - 1, (scm_t_bits *) inits);

  DROPN (n);
  PUSH (ret);

  NEXT;
}

VM_DEFINE_FUNCTION (167, struct_ref, "struct-ref", 2)
{
  ARGS2 (obj, pos);

  if (SCM_LIKELY (SCM_STRUCTP (obj)
  		  && SCM_STRUCT_VTABLE_FLAG_IS_SET (obj,
  						    SCM_VTABLE_FLAG_SIMPLE)
  		  && SCM_I_INUMP (pos)))
    {
      SCM vtable;
      scm_t_bits index, len;

      index = SCM_I_INUM (pos);
      vtable = SCM_STRUCT_VTABLE (obj);
      len = SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size);

      if (SCM_LIKELY (index < len))
  	{
  	  scm_t_bits *data = SCM_STRUCT_DATA (obj);
  	  RETURN (SCM_PACK (data[index]));
  	}
    }

  SYNC_REGISTER ();
  RETURN (scm_struct_ref (obj, pos));
}

VM_DEFINE_FUNCTION (168, struct_set, "struct-set", 3)
{
  ARGS3 (obj, pos, val);

  if (SCM_LIKELY (SCM_STRUCTP (obj)
  		  && SCM_STRUCT_VTABLE_FLAG_IS_SET (obj,
  						    SCM_VTABLE_FLAG_SIMPLE)
  		  && SCM_STRUCT_VTABLE_FLAG_IS_SET (obj,
  						    SCM_VTABLE_FLAG_SIMPLE_RW)
  		  && SCM_I_INUMP (pos)))
    {
      SCM vtable;
      scm_t_bits index, len;

      index = SCM_I_INUM (pos);
      vtable = SCM_STRUCT_VTABLE (obj);
      len = SCM_STRUCT_DATA_REF (vtable, scm_vtable_index_size);
      if (SCM_LIKELY (index < len))
  	{
  	  scm_t_bits *data = SCM_STRUCT_DATA (obj);
  	  data[index] = SCM_UNPACK (val);
  	  RETURN (val);
  	}
    }

  SYNC_REGISTER ();
  RETURN (scm_struct_set_x (obj, pos, val));
}


/*
 * GOOPS support
 */
VM_DEFINE_FUNCTION (169, class_of, "class-of", 1)
{
  ARGS1 (obj);
  if (SCM_INSTANCEP (obj))
    RETURN (SCM_CLASS_OF (obj));
  SYNC_REGISTER ();
  RETURN (scm_class_of (obj));
}

VM_DEFINE_FUNCTION (170, slot_ref, "slot-ref", 2)
{
  size_t slot;
  ARGS2 (instance, idx);
  slot = SCM_I_INUM (idx);
  RETURN (SCM_PACK (SCM_STRUCT_DATA (instance) [slot]));
}

VM_DEFINE_INSTRUCTION (171, slot_set, "slot-set", 0, 3, 0)
{
  SCM instance, idx, val;
  size_t slot;
  POP (val);
  POP (idx);
  POP (instance);
  slot = SCM_I_INUM (idx);
  SCM_STRUCT_DATA (instance) [slot] = SCM_UNPACK (val);
  NEXT;
}


/*
 * Bytevectors
 */
#define VM_VALIDATE_BYTEVECTOR(x)               \
  if (SCM_UNLIKELY (!SCM_BYTEVECTOR_P (x)))     \
    { finish_args = x;                          \
      goto vm_error_not_a_bytevector;           \
    }

#define BV_REF_WITH_ENDIANNESS(stem, fn_stem)                           \
{                                                                       \
  SCM endianness;                                                       \
  POP (endianness);                                                     \
  if (scm_is_eq (endianness, scm_i_native_endianness))                  \
    goto VM_LABEL (bv_##stem##_native_ref);                             \
  {                                                                     \
    ARGS2 (bv, idx);                                                    \
    SYNC_REGISTER ();							\
    RETURN (scm_bytevector_##fn_stem##_ref (bv, idx, endianness));      \
  }                                                                     \
}

/* Return true (non-zero) if PTR has suitable alignment for TYPE.  */
#define ALIGNED_P(ptr, type)			\
  ((scm_t_uintptr) (ptr) % alignof (type) == 0)

VM_DEFINE_FUNCTION (172, bv_u16_ref, "bv-u16-ref", 3)
BV_REF_WITH_ENDIANNESS (u16, u16)
VM_DEFINE_FUNCTION (173, bv_s16_ref, "bv-s16-ref", 3)
BV_REF_WITH_ENDIANNESS (s16, s16)
VM_DEFINE_FUNCTION (174, bv_u32_ref, "bv-u32-ref", 3)
BV_REF_WITH_ENDIANNESS (u32, u32)
VM_DEFINE_FUNCTION (175, bv_s32_ref, "bv-s32-ref", 3)
BV_REF_WITH_ENDIANNESS (s32, s32)
VM_DEFINE_FUNCTION (176, bv_u64_ref, "bv-u64-ref", 3)
BV_REF_WITH_ENDIANNESS (u64, u64)
VM_DEFINE_FUNCTION (177, bv_s64_ref, "bv-s64-ref", 3)
BV_REF_WITH_ENDIANNESS (s64, s64)
VM_DEFINE_FUNCTION (178, bv_f32_ref, "bv-f32-ref", 3)
BV_REF_WITH_ENDIANNESS (f32, ieee_single)
VM_DEFINE_FUNCTION (179, bv_f64_ref, "bv-f64-ref", 3)
BV_REF_WITH_ENDIANNESS (f64, ieee_double)

#undef BV_REF_WITH_ENDIANNESS

#define BV_FIXABLE_INT_REF(stem, fn_stem, type, size)			\
{									\
  long i;								\
  const scm_t_ ## type *int_ptr;					\
  ARGS2 (bv, idx);							\
									\
  VM_VALIDATE_BYTEVECTOR (bv);						\
  i = SCM_I_INUM (idx);							\
  int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
  if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                  && (i >= 0)						\
                  && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                  && (ALIGNED_P (int_ptr, scm_t_ ## type))))		\
    RETURN (SCM_I_MAKINUM (*int_ptr));					\
  else									\
    {									\
      SYNC_REGISTER ();							\
      RETURN (scm_bytevector_ ## fn_stem ## _ref (bv, idx));		\
    }									\
}

#define BV_INT_REF(stem, type, size)					\
{									\
  long i;								\
  const scm_t_ ## type *int_ptr;					\
  ARGS2 (bv, idx);							\
									\
  VM_VALIDATE_BYTEVECTOR (bv);						\
  i = SCM_I_INUM (idx);							\
  int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
  if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                  && (i >= 0)						\
                  && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                  && (ALIGNED_P (int_ptr, scm_t_ ## type))))		\
    {									\
      scm_t_ ## type x = *int_ptr;					\
      if (SCM_FIXABLE (x))						\
        RETURN (SCM_I_MAKINUM (x));					\
      else								\
	{								\
	  SYNC_REGISTER ();						\
	  RETURN (scm_from_ ## type (x));				\
	}								\
    }									\
  else									\
    {									\
      SYNC_REGISTER ();							\
      RETURN (scm_bytevector_ ## stem ## _native_ref (bv, idx));	\
    }									\
}

#define BV_FLOAT_REF(stem, fn_stem, type, size)				\
{									\
  long i;								\
  const type *float_ptr;						\
  ARGS2 (bv, idx);							\
									\
  VM_VALIDATE_BYTEVECTOR (bv);						\
  i = SCM_I_INUM (idx);							\
  float_ptr = (type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);		\
									\
  SYNC_REGISTER ();							\
  if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                  && (i >= 0)						\
                  && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                  && (ALIGNED_P (float_ptr, type))))			\
    RETURN (scm_from_double (*float_ptr));				\
  else									\
    RETURN (scm_bytevector_ ## fn_stem ## _native_ref (bv, idx));	\
}

VM_DEFINE_FUNCTION (180, bv_u8_ref, "bv-u8-ref", 2)
BV_FIXABLE_INT_REF (u8, u8, uint8, 1)
VM_DEFINE_FUNCTION (181, bv_s8_ref, "bv-s8-ref", 2)
BV_FIXABLE_INT_REF (s8, s8, int8, 1)
VM_DEFINE_FUNCTION (182, bv_u16_native_ref, "bv-u16-native-ref", 2)
BV_FIXABLE_INT_REF (u16, u16_native, uint16, 2)
VM_DEFINE_FUNCTION (183, bv_s16_native_ref, "bv-s16-native-ref", 2)
BV_FIXABLE_INT_REF (s16, s16_native, int16, 2)
VM_DEFINE_FUNCTION (184, bv_u32_native_ref, "bv-u32-native-ref", 2)
#if SIZEOF_VOID_P > 4
BV_FIXABLE_INT_REF (u32, u32_native, uint32, 4)
#else
BV_INT_REF (u32, uint32, 4)
#endif
VM_DEFINE_FUNCTION (185, bv_s32_native_ref, "bv-s32-native-ref", 2)
#if SIZEOF_VOID_P > 4
BV_FIXABLE_INT_REF (s32, s32_native, int32, 4)
#else
BV_INT_REF (s32, int32, 4)
#endif
VM_DEFINE_FUNCTION (186, bv_u64_native_ref, "bv-u64-native-ref", 2)
BV_INT_REF (u64, uint64, 8)
VM_DEFINE_FUNCTION (187, bv_s64_native_ref, "bv-s64-native-ref", 2)
BV_INT_REF (s64, int64, 8)
VM_DEFINE_FUNCTION (188, bv_f32_native_ref, "bv-f32-native-ref", 2)
BV_FLOAT_REF (f32, ieee_single, float, 4)
VM_DEFINE_FUNCTION (189, bv_f64_native_ref, "bv-f64-native-ref", 2)
BV_FLOAT_REF (f64, ieee_double, double, 8)

#undef BV_FIXABLE_INT_REF
#undef BV_INT_REF
#undef BV_FLOAT_REF



#define BV_SET_WITH_ENDIANNESS(stem, fn_stem)                           \
{                                                                       \
  SCM endianness;                                                       \
  POP (endianness);                                                     \
  if (scm_is_eq (endianness, scm_i_native_endianness))                  \
    goto VM_LABEL (bv_##stem##_native_set);                             \
  {                                                                     \
    SCM bv, idx, val; POP (val); POP (idx); POP (bv);                   \
    scm_bytevector_##fn_stem##_set_x (bv, idx, val, endianness);        \
    NEXT;                                                               \
  }                                                                     \
}

VM_DEFINE_INSTRUCTION (190, bv_u16_set, "bv-u16-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (u16, u16)
VM_DEFINE_INSTRUCTION (191, bv_s16_set, "bv-s16-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (s16, s16)
VM_DEFINE_INSTRUCTION (192, bv_u32_set, "bv-u32-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (u32, u32)
VM_DEFINE_INSTRUCTION (193, bv_s32_set, "bv-s32-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (s32, s32)
VM_DEFINE_INSTRUCTION (194, bv_u64_set, "bv-u64-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (u64, u64)
VM_DEFINE_INSTRUCTION (195, bv_s64_set, "bv-s64-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (s64, s64)
VM_DEFINE_INSTRUCTION (196, bv_f32_set, "bv-f32-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (f32, ieee_single)
VM_DEFINE_INSTRUCTION (197, bv_f64_set, "bv-f64-set", 0, 4, 0)
BV_SET_WITH_ENDIANNESS (f64, ieee_double)

#undef BV_SET_WITH_ENDIANNESS

#define BV_FIXABLE_INT_SET(stem, fn_stem, type, min, max, size)		\
{									\
  long i, j = 0;							\
  SCM bv, idx, val;							\
  scm_t_ ## type *int_ptr;						\
									\
  POP (val); POP (idx); POP (bv);					\
  VM_VALIDATE_BYTEVECTOR (bv);						\
  i = SCM_I_INUM (idx);							\
  int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
  if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                  && (i >= 0)						\
                  && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                  && (ALIGNED_P (int_ptr, scm_t_ ## type))		\
                  && (SCM_I_INUMP (val))				\
                  && ((j = SCM_I_INUM (val)) >= min)			\
                  && (j <= max)))					\
    *int_ptr = (scm_t_ ## type) j;					\
  else									\
    scm_bytevector_ ## fn_stem ## _set_x (bv, idx, val);		\
  NEXT;									\
}

#define BV_INT_SET(stem, type, size)					\
{									\
  long i = 0;								\
  SCM bv, idx, val;							\
  scm_t_ ## type *int_ptr;						\
									\
  POP (val); POP (idx); POP (bv);					\
  VM_VALIDATE_BYTEVECTOR (bv);						\
  i = SCM_I_INUM (idx);							\
  int_ptr = (scm_t_ ## type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
									\
  if (SCM_LIKELY (SCM_I_INUMP (idx)					\
                  && (i >= 0)						\
                  && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))		\
                  && (ALIGNED_P (int_ptr, scm_t_ ## type))))		\
    *int_ptr = scm_to_ ## type (val);					\
  else									\
    scm_bytevector_ ## stem ## _native_set_x (bv, idx, val);		\
  NEXT;									\
}

#define BV_FLOAT_SET(stem, fn_stem, type, size)			\
{								\
  long i = 0;							\
  SCM bv, idx, val;						\
  type *float_ptr;						\
								\
  POP (val); POP (idx); POP (bv);				\
  VM_VALIDATE_BYTEVECTOR (bv);					\
  i = SCM_I_INUM (idx);						\
  float_ptr = (type *) (SCM_BYTEVECTOR_CONTENTS (bv) + i);	\
								\
  if (SCM_LIKELY (SCM_I_INUMP (idx)				\
                  && (i >= 0)					\
                  && (i + size <= SCM_BYTEVECTOR_LENGTH (bv))	\
                  && (ALIGNED_P (float_ptr, type))))		\
    *float_ptr = scm_to_double (val);				\
  else								\
    scm_bytevector_ ## fn_stem ## _native_set_x (bv, idx, val);	\
  NEXT;								\
}

VM_DEFINE_INSTRUCTION (198, bv_u8_set, "bv-u8-set", 0, 3, 0)
BV_FIXABLE_INT_SET (u8, u8, uint8, 0, SCM_T_UINT8_MAX, 1)
VM_DEFINE_INSTRUCTION (199, bv_s8_set, "bv-s8-set", 0, 3, 0)
BV_FIXABLE_INT_SET (s8, s8, int8, SCM_T_INT8_MIN, SCM_T_INT8_MAX, 1)
VM_DEFINE_INSTRUCTION (200, bv_u16_native_set, "bv-u16-native-set", 0, 3, 0)
BV_FIXABLE_INT_SET (u16, u16_native, uint16, 0, SCM_T_UINT16_MAX, 2)
VM_DEFINE_INSTRUCTION (201, bv_s16_native_set, "bv-s16-native-set", 0, 3, 0)
BV_FIXABLE_INT_SET (s16, s16_native, int16, SCM_T_INT16_MIN, SCM_T_INT16_MAX, 2)
VM_DEFINE_INSTRUCTION (202, bv_u32_native_set, "bv-u32-native-set", 0, 3, 0)
#if SIZEOF_VOID_P > 4
BV_FIXABLE_INT_SET (u32, u32_native, uint32, 0, SCM_T_UINT32_MAX, 4)
#else
BV_INT_SET (u32, uint32, 4)
#endif
VM_DEFINE_INSTRUCTION (203, bv_s32_native_set, "bv-s32-native-set", 0, 3, 0)
#if SIZEOF_VOID_P > 4
BV_FIXABLE_INT_SET (s32, s32_native, int32, SCM_T_INT32_MIN, SCM_T_INT32_MAX, 4)
#else
BV_INT_SET (s32, int32, 4)
#endif
VM_DEFINE_INSTRUCTION (204, bv_u64_native_set, "bv-u64-native-set", 0, 3, 0)
BV_INT_SET (u64, uint64, 8)
VM_DEFINE_INSTRUCTION (205, bv_s64_native_set, "bv-s64-native-set", 0, 3, 0)
BV_INT_SET (s64, int64, 8)
VM_DEFINE_INSTRUCTION (206, bv_f32_native_set, "bv-f32-native-set", 0, 3, 0)
BV_FLOAT_SET (f32, ieee_single, float, 4)
VM_DEFINE_INSTRUCTION (207, bv_f64_native_set, "bv-f64-native-set", 0, 3, 0)
BV_FLOAT_SET (f64, ieee_double, double, 8)

#undef BV_FIXABLE_INT_SET
#undef BV_INT_SET
#undef BV_FLOAT_SET

/*
(defun renumber-ops ()
  "start from top of buffer and renumber 'VM_DEFINE_FOO (\n' sequences"
  (interactive "")
  (save-excursion
    (let ((counter 127)) (goto-char (point-min))
      (while (re-search-forward "^VM_DEFINE_[^ ]+ (\\([^,]+\\)," (point-max) t)
        (replace-match
         (number-to-string (setq counter (1+ counter)))
          t t nil 1)))))
*/

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
