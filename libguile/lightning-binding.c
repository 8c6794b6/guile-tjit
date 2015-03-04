/* Copyright (C) 2014, 2015  Free Software Foundation, Inc.
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

#ifndef _SCM_LIGHTNING_BINDING_
#define _SCM_LIGHTNING_BINDING_

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "gc-inline.h"
#include "lightning-binding.h"
#include <errno.h>
#include <lightning.h>
#include <sys/mman.h>

#if BUILD_VM_LIGHTNING == 1

SCM_DEFINE (scm_jit_r, "%jit-r", 1, 0, 0, (SCM i), "Get R register.")
#define FUNC_NAME s_scm_jit_r
{
  return scm_from_int (jit_r (scm_to_int (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_v, "%jit-v", 1, 0, 0, (SCM i), "Get V register.")
#define FUNC_NAME s_scm_jit_v
{
  return scm_from_int (jit_v (scm_to_int (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_f, "%jit-f", 1, 0, 0, (SCM i), "Get F register.")
#define FUNC_NAME s_scm_jit_f
{
  return scm_from_int (jit_f (scm_to_int (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_fp, "%jit-fp", 0, 0, 0, (), "Get FP register.")
#define FUNC_NAME s_scm_jit_fp
{
  return scm_from_int (JIT_FP);
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_r_num, "jit-r-num", 0, 0, 0, (),
            "Get the number of general purpose registers.")
#define FUNC_NAME s_scm_jit_r_num
{
  return scm_from_int (jit_r_num());
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_v_num, "jit-v-num", 0, 0, 0, (),
            "Get the number of non-volatile registers.")
#define FUNC_NAME s_scm_jit_v_num
{
  return scm_from_int (jit_v_num());
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_f_num, "jit-f-num", 0, 0, 0, (),
            "Get the number of XMM registers.")
#define FUNC_NAME s_scm_jit_f_num
{
  return scm_from_int (jit_f_num());
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_code_size, "%jit-code-size", 1, 0, 0, (SCM jit),
            "Get code size.")
#define FUNC_NAME s_scm_jit_code_size
{
  jit_word_t code_size;
  _jit_get_code ((jit_state_t *) scm_to_pointer(jit), &code_size);
  return scm_from_int (code_size);
}
#undef FUNC_NAME

#define ERRMSG "could not set execution flag on memory\n"

const int page_size = 4096;

static inline void set_flag (unsigned char *bv, size_t n)
{
  char *start = (char *) ((((scm_t_bits ) bv) / page_size) * page_size);
  int len = (scm_t_bits) bv + n - (scm_t_bits) start;
  if(mprotect (start, len, PROT_READ | PROT_EXEC))
    {
      printf("ERROR: ");
      if(errno == EINVAL)
        printf("%s addr is not a valid pointer or not a multiple of PAGESIZE\n"
               ,ERRMSG);
      if(errno == EFAULT)
        printf("%s memory cannot be accessed\n"
               ,ERRMSG);
      if(errno == EACCES)
        printf("%s the memory cannot be given the spicified access\n"
               ,ERRMSG);
      if(errno == ENOMEM)
        printf("%s internal memory structurs could not be constructed\n"
               ,ERRMSG);
    }
}
#undef ERRMSG

SCM_DEFINE (scm_make_bytevector_executable_x, "make-bytevector-executable!",
           1, 0, 0,
           (SCM bv), "Make bytevector as executable.")
#define FUNC_NAME s_scm_make_bytevector_executable_x
{
  if (scm_is_bytevector(bv))
    {
      set_flag((unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv),
               SCM_BYTEVECTOR_LENGTH (bv));
    }
  return SCM_UNSPECIFIED;
}

void
scm_init_lightning (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "lightning-binding.x"
#endif
}

#endif /* BUILD_VM_LIGHTNING == 1 */

#endif /* _SCM_LIGHTNING_BINDING_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
