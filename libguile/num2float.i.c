/* this file is #include'd (several times) by numbers.c */

FTYPE
NUM2FLOAT (SCM num, unsigned long int pos, const char *s_caller)
{
  if (SCM_INUMP (num))
    return SCM_INUM (num);
  else if (SCM_BIGP (num))
    { /* bignum */
      FTYPE res = mpz_get_d (SCM_I_BIG_MPZ (num));
      if (! xisinf (res))
	return res;
      else
	scm_out_of_range (s_caller, num);
    }
  else if (SCM_REALP (num))
    return SCM_REAL_VALUE (num);
  else
    scm_wrong_type_arg (s_caller, pos, num);
}

SCM
FLOAT2NUM (FTYPE n)
{
  SCM z = scm_double_cell (scm_tc16_real, 0, 0, 0);
  SCM_REAL_VALUE (z) = n;
  return z;
}

/* clean up */
#undef FLOAT2NUM
#undef NUM2FLOAT
#undef FTYPE

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
