/* this file is #include'd (x times) by convert.c */

/* Convert a vector, weak vector, (if possible string, substring), list
   or uniform vector into an C array.  If the result array in argument 2 
   is NULL, malloc() a new one.  If out of memory, return NULL.  */
#define FUNC_NAME SCM2CTYPES_FN
CTYPE *
SCM2CTYPES (SCM obj, CTYPE *data)
{
  long i, n;
  SCM val;

  SCM_ASSERT (SCM_NIMP (obj) || scm_is_true (scm_list_p (obj)), 
	      obj, SCM_ARG1, FUNC_NAME);

  /* list conversion */
  if (scm_is_true (scm_list_p (obj)))
    {
      /* traverse the given list and validate the range of each member */
      SCM list = obj;
      for (n = 0; scm_is_true (scm_pair_p (list)); list = SCM_CDR (list), n++)
	{
	  val = SCM_CAR (list);
#if SIZEOF_CTYPE && SIZEOF_CTYPE < SIZEOF_SCM_T_BITS
	  /* check integer ranges */
          if (SCM_I_INUMP (val))
            {
              scm_t_signed_bits v = SCM_I_INUM (val);
	      CTYPE c = (CTYPE) v;
	      SCM_ASSERT_RANGE (SCM_ARG1, val, v != (scm_t_signed_bits) c);
            }
	  /* check big number ranges */
	  else if (SCM_BIGP (val))
	    {
              scm_t_signed_bits v = scm_to_long (val);
	      CTYPE c = (CTYPE) v;
	      SCM_ASSERT_RANGE (SCM_ARG1, val, v != (scm_t_signed_bits) c);
	    }
	  else
	  /* check float types */
#elif defined (FLOATTYPE)
	  /* real values, big numbers and immediate values are valid 
	     for float conversions */
	  if (!SCM_REALP (val) && !SCM_BIGP (val) && !SCM_I_INUMP (val))
#else
	  if (!SCM_BIGP (val) && !SCM_I_INUMP (val))
#endif /* FLOATTYPE */
	    SCM_WRONG_TYPE_ARG (SCM_ARG1, val);
        }

      /* allocate new memory if necessary */
      if (data == NULL)
	if ((data = (CTYPE *) malloc (n * sizeof (CTYPE))) == NULL)
	  return NULL;

      /* traverse the list once more and convert each member */
      list = obj;
      for (i = 0; scm_is_true (scm_pair_p (list)); list = SCM_CDR (list), i++)
	{
          val = SCM_CAR (list);
	  if (SCM_I_INUMP (val))
            data[i] = (CTYPE) SCM_I_INUM (val);
          else if (SCM_BIGP (val))
	    data[i] = (CTYPE) scm_to_long (val);
#if defined (FLOATTYPE)
          else
            data[i] = (CTYPE) SCM_REAL_VALUE (val);
#endif
	}
      return data;
    }

  /* other conversions */
  switch (SCM_TYP7 (obj))
    {
      /* vectors and weak vectors */
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      /* traverse the given vector and validate each member */
      for (i = 0; i < n; i++)
        {
          val = SCM_VELTS (obj)[i];
#if SIZEOF_CTYPE && SIZEOF_CTYPE < SIZEOF_SCM_T_BITS
	  /* check integer ranges */
          if (SCM_I_INUMP (val))
            {
              scm_t_signed_bits v = SCM_I_INUM (val);
	      CTYPE c = (CTYPE) v;
	      SCM_ASSERT_RANGE (SCM_ARG1, val, v != (scm_t_signed_bits) c);
            }
	  /* check big number ranges */
	  else if (SCM_BIGP (val))
	    {
              scm_t_signed_bits v = scm_to_long (val);
	      CTYPE c = (CTYPE) v;
	      SCM_ASSERT_RANGE (SCM_ARG1, val, v != (scm_t_signed_bits) c);
	    }
          else
	  /* check float types */
#elif defined (FLOATTYPE)
	  /* real values, big numbers and immediate values are valid 
	     for float conversions */
	  if (!SCM_REALP (val) && !SCM_BIGP (val) && !SCM_I_INUMP (val))
#else
	  if (!SCM_BIGP (val) && !SCM_I_INUMP (val))
#endif /* FLOATTYPE */
	    SCM_WRONG_TYPE_ARG (SCM_ARG1, val);
        }

      /* allocate new memory if necessary */
      if (data == NULL)
	if ((data = (CTYPE *) malloc (n * sizeof (CTYPE))) == NULL)
	  return NULL;

      /* traverse the vector once more and convert each member */
      for (i = 0; i < n; i++)
	{
          val = SCM_VELTS (obj)[i];
	  if (SCM_I_INUMP (val))
            data[i] = (CTYPE) SCM_I_INUM (val);
          else if (SCM_BIGP (val))
	    data[i] = (CTYPE) scm_to_long (val);
#if defined (FLOATTYPE)
          else
            data[i] = (CTYPE) SCM_REAL_VALUE (val);
#endif
	}
      break;

#if SCM_HAVE_ARRAYS
      /* array conversions (uniform vectors) */
    case ARRAYTYPE:
#ifdef ARRAYTYPE_OPTIONAL
    case ARRAYTYPE_OPTIONAL:
#endif
      n = SCM_UVECTOR_LENGTH (obj);

      /* allocate new memory if necessary */
      if (data == NULL)
	if ((data = (CTYPE *) malloc (n * sizeof (CTYPE))) == NULL)
	  return NULL;

#ifdef FLOATTYPE_OPTIONAL
      /* float <-> double conversions */
      if (SCM_TYP7 (obj) == ARRAYTYPE_OPTIONAL)
	{
	  for (i = 0; i < n; i++)
	    data[i] = ((FLOATTYPE_OPTIONAL *) SCM_UVECTOR_BASE (obj))[i];
	}
      else
#endif
#if SIZEOF_CTYPE != SIZEOF_ARRAYTYPE
      /* copy array element by element */
      for (i = 0; i < n; i++)
	data[i] = (CTYPE) ((ARRAYCTYPE *) SCM_UVECTOR_BASE (obj))[i];
#else
      /* copy whole array */
      memcpy (data, (CTYPE *) SCM_UVECTOR_BASE (obj), n * sizeof (CTYPE));
#endif
      break;
#endif /* SCM_HAVE_ARRAYS */

#if SIZEOF_CTYPE == 1
    case scm_tc7_string:
      n = SCM_STRING_LENGTH (obj);
      if (data == NULL)
        if ((data = (CTYPE *) malloc (n * sizeof (CTYPE))) == NULL)
	  return NULL;
      memcpy (data, SCM_STRING_CHARS (obj), n * sizeof (CTYPE));
      break;
#endif

    default:
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
    }
  return data;
}
#undef FUNC_NAME


#if SCM_HAVE_ARRAYS

/* Converts a C array into a uniform vector, returns SCM_UNDEFINED if out
   of memory. */
#define FUNC_NAME CTYPES2UVECT_FN
SCM
CTYPES2UVECT (const CTYPE *data, long n)
{
#if SIZEOF_CTYPE != SIZEOF_UVECTTYPE
  UVECTCTYPE *v;
  long i;
#else
  char *v;
#endif

  SCM_ASSERT_RANGE (SCM_ARG2, scm_from_long (n),
		    n > 0 && n <= SCM_UVECTOR_MAX_LENGTH);
#if SIZEOF_CTYPE != SIZEOF_UVECTTYPE
  v = scm_gc_malloc (n * SIZEOF_UVECTTYPE, "uvect");
  for (i = 0; i < n; i++)
    v[i] = (UVECTCTYPE) data[i];
#else
  v = scm_gc_malloc (n * sizeof (CTYPE), "uvect");
  memcpy (v, data, n * sizeof (CTYPE));
#endif
  return scm_cell (SCM_MAKE_UVECTOR_TAG (n, UVECTTYPE), (scm_t_bits) v);
}
#undef FUNC_NAME

#ifdef UVECTTYPE_OPTIONAL
#define FUNC_NAME CTYPES2UVECT_FN_OPTIONAL
SCM
CTYPES2UVECT_OPTIONAL (const unsigned CTYPE *data, long n)
{
#if SIZEOF_CTYPE != SIZEOF_UVECTTYPE
  unsigned UVECTCTYPE *v;
  long i;
#else
  char *v;
#endif

  SCM_ASSERT_RANGE (SCM_ARG2, scm_from_long (n), 
		    n > 0 && n <= SCM_UVECTOR_MAX_LENGTH);
#if SIZEOF_CTYPE != SIZEOF_UVECTTYPE
  v = scm_gc_malloc (n * SIZEOF_UVECTTYPE, "uvect");
  for (i = 0; i < n; i++)
    v[i] = (unsigned UVECTCTYPE) data[i];
#else
  v = scm_gc_malloc (n * sizeof (CTYPE), "uvect");
  memcpy (v, data, n * sizeof (CTYPE));
#endif
  return scm_cell (SCM_MAKE_UVECTOR_TAG (n, UVECTTYPE_OPTIONAL), 
		   (scm_t_bits) v);
}
#undef FUNC_NAME
#endif /* UVECTTYPE_OPTIONAL */

#endif /* SCM_HAVE_ARRAYS */


/* Converts a C array into a vector. */
#define FUNC_NAME CTYPES2SCM_FN
SCM
CTYPES2SCM (const CTYPE *data, long n)
{
  long i;
  SCM v;
  
  SCM_ASSERT_RANGE (SCM_ARG2, scm_from_long (n), 
		    n > 0 && n <= SCM_VECTOR_MAX_LENGTH);
  v = scm_c_make_vector (n, SCM_UNSPECIFIED);

  for (i = 0; i < n; i++)
#ifdef FLOATTYPE
    SCM_VECTOR_SET (v, i, scm_make_real ((double) data[i]));
#else
    SCM_VECTOR_SET (v, i, scm_from_signed_integer (data[i]));
#endif
  return v;
}
#undef FUNC_NAME

/* cleanup of conditionals */
#undef SCM2CTYPES
#undef SCM2CTYPES_FN
#undef CTYPES2SCM
#undef CTYPES2SCM_FN
#undef CTYPE
#undef CTYPES2UVECT
#undef CTYPES2UVECT_FN
#undef UVECTTYPE
#ifdef UVECTTYPE_OPTIONAL
#undef CTYPES2UVECT_OPTIONAL
#undef CTYPES2UVECT_FN_OPTIONAL
#undef UVECTTYPE_OPTIONAL
#endif
#undef SIZEOF_CTYPE
#undef SIZEOF_UVECTTYPE
#undef SIZEOF_ARRAYTYPE
#undef ARRAYTYPE
#ifdef ARRAYTYPE_OPTIONAL
#undef ARRAYTYPE_OPTIONAL
#endif
#ifdef FLOATTYPE
#undef FLOATTYPE
#endif
#ifdef FLOATTYPE_OPTIONAL
#undef FLOATTYPE_OPTIONAL
#endif
#ifdef UVECTCTYPE
#undef UVECTCTYPE
#endif
#ifdef ARRAYCTYPE
#undef ARRAYCTYPE
#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
