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

#include <errno.h>
#include <lightning.h>
#include <sys/mman.h>

#include "libguile/_scm.h"
#include "libguile/lightning-binding.h"

#if BUILD_VM_LIGHTNING == 1

static size_t page_size;

/* Fluid for _jit state. */
static SCM the_jit_state;

#define SCM_JIT_STATE \
  ((jit_state_t *) SCM_POINTER_VALUE (scm_fluid_ref (the_jit_state)))

SCM_DEFINE (scm_init_jit, "init-jit", 1, 0, 0, (SCM arg), "")
#define FUNC_NAME s_scm_init_jit
{
  init_jit (scm_to_latin1_string (arg));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_finish_jit, "finish-jit", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_finish_jit
{
  finish_jit ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_new_state, "jit-new-state", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_jit_new_state
{
  return scm_from_pointer (jit_new_state (), NULL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_state, "%jit-state", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_jit_state
{
  return the_jit_state;
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_code_size, "jit-code-size", 0, 0, 0, (SCM jit), "")
#define FUNC_NAME s_scm_jit_code_size
{
  jit_word_t code_size;
  _jit_get_code (SCM_JIT_STATE, &code_size);
  return scm_from_int (code_size);
}
#undef FUNC_NAME


/*
 * Registers
 */

SCM_DEFINE (scm_jit_r, "jit-r", 1, 0, 0, (SCM i), "")
#define FUNC_NAME s_scm_jit_r
{
  return SCM_I_MAKINUM (jit_r (SCM_I_INUM (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_v, "jit-v", 1, 0, 0, (SCM i), "")
#define FUNC_NAME s_scm_jit_v
{
  return SCM_I_MAKINUM (jit_v (SCM_I_INUM (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_f, "jit-f", 1, 0, 0, (SCM i), "")
#define FUNC_NAME s_scm_jit_f
{
  return SCM_I_MAKINUM (jit_f (SCM_I_INUM (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_fp, "jit-fp", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_jit_fp
{
  return SCM_I_MAKINUM (JIT_FP);
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_r_num, "jit-r-num", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_jit_r_num
{
  return SCM_I_MAKINUM (jit_r_num());
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_v_num, "jit-v-num", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_jit_v_num
{
  return SCM_I_MAKINUM (jit_v_num());
}
#undef FUNC_NAME

SCM_DEFINE (scm_jit_f_num, "jit-f-num", 0, 0, 0, (), "")
#define FUNC_NAME s_scm_jit_f_num
{
  return SCM_I_MAKINUM (jit_f_num());
}
#undef FUNC_NAME


/*
 * Macros for lightning functions taking JIT state
 */

#define VOID_0(cname, sname)                       \
  SCM_DEFINE (scm_##cname, sname, 0, 0, 0, (), "") \
  {                                                \
    _##cname (SCM_JIT_STATE);                      \
    return SCM_UNSPECIFIED;                        \
  }                                                \

#define VOID_1(cname, sname, s1, c1)                     \
  SCM_DEFINE (scm_##cname, sname, 1, 0, 0, (SCM s1), "") \
  {                                                      \
    _##cname (SCM_JIT_STATE, c1);                        \
    return SCM_UNSPECIFIED;                              \
  }                                                      \

#define VOID_2(cname, sname, s1, s2, c1, c2)                     \
  SCM_DEFINE (scm_##cname, sname, 2, 0, 0, (SCM s1, SCM s2), "") \
  {                                                              \
    _##cname (SCM_JIT_STATE, c1, c2);                            \
    return SCM_UNSPECIFIED;                                      \
  }                                                              \

#define VOID_3(cname, sname, s1, s2, s3, c1, c2, c3)    \
  SCM_DEFINE (scm_##cname, sname, 1, 0, 0,              \
              (SCM s1, SCM s2, SCM s3), "")             \
  {                                                     \
    _##cname (SCM_JIT_STATE, c1, c2, c3);               \
    return SCM_UNSPECIFIED;                             \
  } \

#define NODE_0(cname, sname)                                    \
  SCM_DEFINE (scm_##cname, sname, 0, 0, 0, (), "")              \
  {                                                             \
    return SCM_PACK (_##cname (SCM_JIT_STATE)); \
  }                                                             \

#define NODE_1(cname, sname, s1, c1)                                    \
  SCM_DEFINE (scm_##cname, sname, 1, 0, 0, (SCM s1), "")                \
  {                                                                     \
    return SCM_PACK (_##cname (SCM_JIT_STATE, c1));    \
  }                                                                     \

#define NODE_2(cname, sname, s1, s2, c1, c2)                            \
  SCM_DEFINE (scm_##cname, sname, 2, 0, 0, (SCM s1, SCM s2), "")        \
  {                                                                     \
    return SCM_PACK (_##cname (SCM_JIT_STATE, c1, c2)); \
  }                                                                     \

#define NODE_3(cname, sname, s1, s2, s3, c1, c2, c3)                    \
  SCM_DEFINE (scm_##cname, sname, 3, 0, 0,                              \
              (SCM s1, SCM s2, SCM s3), "")                             \
  {                                                                     \
    return SCM_PACK (_##cname (SCM_JIT_STATE, c1, c2, c3)); \
  }                                                                     \

#define NODE_4(cname, sname, s1, s2, s3, s4, c1, c2, c3, c4)            \
  SCM_DEFINE (scm_##cname, sname, 4, 0, 0,                              \
              (SCM s1, SCM s2, SCM s3, SCM s4), "")                     \
  {                                                                     \
    return SCM_PACK (_##cname (SCM_JIT_STATE, c1, c2, c3, c4)); \
  }                                                                     \

#define NODE_5(cname, sname, s1, s2, s3, s4, s5, c1, c2, c3, c4, c5)    \
  SCM_DEFINE (scm_##cname, sname, 5, 0, 0,                              \
              (SCM s1, SCM s2, SCM s3, SCM s4, SCM s5), "")             \
  {                                                                     \
    return SCM_PACK (_##cname (SCM_JIT_STATE,          \
                                       c1, c2, c3, c4, c5));            \
  }                                                                     \

#define I32T_1(cname, sname, s1, c1)                            \
  SCM_DEFINE (scm_##cname, sname, 1, 0, 0, (SCM s1), "")        \
  {                                                             \
    return scm_from_int32 (_##cname (SCM_JIT_STATE, c1));       \
  }                                                             \

#define PTR_0(cname, sname)                                     \
  SCM_DEFINE (scm_##cname, sname, 0, 0, 0, (), "")              \
  {                                                             \
    return SCM_I_MAKINUM ((void *) _##cname (SCM_JIT_STATE));   \
  }                                                             \

#define PTR_1(cname, sname, s1, c1)                                     \
  SCM_DEFINE (scm_##cname, sname, 1, 0, 0, (SCM s1), "")                \
  {                                                                     \
    return SCM_I_MAKINUM ((void *) _##cname (SCM_JIT_STATE, c1));       \
  }                                                                     \

#define BOOL_0(cname, sname)                                     \
  SCM_DEFINE (scm_##cname, sname, 0, 0, 0, (), "")               \
  {                                                              \
    return (_##cname (SCM_JIT_STATE)) ? SCM_BOOL_T : SCM_BOOL_F; \
  }                                                              \

#define BOOL_1(cname, sname, s1, c1)                                 \
  SCM_DEFINE (scm_##cname, sname, 1, 0, 0, (SCM s1), "")             \
  {                                                                  \
    return (_##cname (SCM_JIT_STATE, c1)) ? SCM_BOOL_T : SCM_BOOL_F; \
  }                                                                  \

#define BOOL_2(cname, sname, s1, s2, c1, c2)                            \
  SCM_DEFINE (scm_##cname, sname, 2, 0, 0, (SCM s1, SCM s2), "")        \
  {                                                                     \
    return (_##cname (SCM_JIT_STATE, c1, c2)) ? SCM_BOOL_T : SCM_BOOL_F; \
  }                                                                     \

#define BOOL_4(cname, sname, s1, s2, s3, s4, c1, c2, c3, c4) \
  SCM_DEFINE (scm_##cname, sname, 2, 0, 0,                   \
              (SCM s1, SCM s2, SCM s3, SCM s4), "")          \
  {                                                          \
    return (_##cname (SCM_JIT_STATE, c1, c2, c3, c4)) ?      \
      SCM_BOOL_T : SCM_BOOL_F ;                              \
  }                                                          \


/*
 * Macros for argument in lightning functions
 */

#define JIT_NODE(u) ((jit_node_t *) SCM_UNPACK (u))
#define JIT_GPR(u) ((jit_gpr_t) SCM_I_INUM (u))
#define JIT_FPR(u) ((jit_fpr_t) SCM_I_INUM (u))
#define JIT_PTR(u) ((jit_pointer_t) SCM_I_INUM (u))
#define JIT_INT32(u) ((jit_int32_t) SCM_I_INUM (u))
#define JIT_WORD(u) ((jit_word_t) scm_to_ssize_t (u))
#define JIT_FLOAT32(u) ((jit_float32_t) SCM_REAL_VALUE (u))
#define JIT_FLOAT64(u) ((jit_float64_t) SCM_REAL_VALUE (u))

VOID_0 (jit_clear_state, "jit-clear-state")
VOID_0 (jit_destroy_state, "jit-destroy-state")

PTR_1 (jit_address, "jit-address", arg1, JIT_NODE (arg1))
NODE_2 (jit_note, "jit-note", name, pos,
        scm_to_latin1_string (name), SCM_I_INUM (pos))
NODE_1 (jit_name, "jit-name", name, scm_to_latin1_string (name))
NODE_0 (jit_label, "jit-label")
NODE_0 (jit_forward, "jit-forward")
NODE_0 (jit_indirect, "jit-indirect")
VOID_1 (jit_link, "jit-link", u, JIT_NODE (u))
BOOL_1 (jit_forward_p, "jit-forward-p", u, JIT_NODE (u))
BOOL_1 (jit_indirect_p, "jit-indirect-p", u, JIT_NODE (u))
BOOL_1 (jit_target_p, "jit-target-p", u, JIT_NODE (u))

VOID_0 (jit_prolog, "jit-prolog")

I32T_1 (jit_allocai, "jit-allocai", u, JIT_INT32 (u))
VOID_0 (jit_ellipsis, "jit-ellispsis")

NODE_0 (jit_arg, "jit-arg")
VOID_2 (jit_getarg_c, "jit-getarg-c", u, v, JIT_GPR (u), JIT_NODE (v))
VOID_2 (jit_getarg_uc, "jit-getarg-uc", u, v, JIT_GPR (u), JIT_NODE (v))
VOID_2 (jit_getarg_s, "jit-getarg-s", u, v, JIT_GPR (u), JIT_NODE (v))
VOID_2 (jit_getarg_us, "jit-getarg-us", u, v, JIT_GPR (u), JIT_NODE (v))
VOID_2 (jit_getarg_i, "jit-getarg-i", u, v, JIT_GPR (u), JIT_NODE (v))
VOID_2 (jit_getarg_ui, "jit-getarg-ui", u, v, JIT_GPR (u), JIT_NODE (v))
VOID_2 (jit_getarg_l, "jit-getarg-l", u, v, JIT_GPR (u), JIT_NODE (v))

VOID_0 (jit_prepare, "jit-prepare")
VOID_1 (jit_pushargr, "jit-pushargr", u, JIT_GPR (u))
VOID_1 (jit_pushargi, "jit-pushargi", u, JIT_WORD (u))
VOID_1 (jit_finishr, "jit-finishr", u, JIT_GPR (u))
NODE_1 (jit_finishi, "jit-finishi", u, JIT_PTR (u))
VOID_0 (jit_ret, "jit-ret")
VOID_1 (jit_retr, "jit-retr", u, JIT_GPR (u))
VOID_1 (jit_reti, "jit-reti", u, JIT_WORD (u))
VOID_1 (jit_retval_c, "jit-retval-c", u, JIT_GPR (u))
VOID_1 (jit_retval_uc, "jit-retval-uc", u, JIT_GPR (u))
VOID_1 (jit_retval_s, "jit-retval-s", u, JIT_GPR (u))
VOID_1 (jit_retval_us, "jit-retval-us", u, JIT_GPR (u))
VOID_1 (jit_retval_i, "jit-retval-i", u, JIT_GPR (u))
VOID_1 (jit_retval_ui, "jit-retval-ui", u, JIT_GPR (u))
VOID_1 (jit_retval_l, "jit-retval-l", u, JIT_GPR (u))
VOID_0 (jit_epilog, "jit-epilog")

VOID_1 (jit_patch, "jit-patch", u, JIT_NODE (u))
VOID_2 (jit_patch_at, "jit-patch-at", u, v, JIT_NODE (u), JIT_NODE (v))
VOID_2 (jit_patch_abs, "jit-patch-abs", u, v, JIT_NODE (u), JIT_PTR (v))
VOID_0 (jit_realize, "jit-realize")
NODE_1 (jit_get_code, "jit-get-code", u, JIT_PTR (u))
VOID_2 (jit_set_code, "jit-set-code", u, v, JIT_PTR (u), JIT_WORD (v))
NODE_2 (jit_get_data, "jit-get-data", u, v, JIT_PTR (u), JIT_PTR (v))
VOID_3 (jit_set_data, "jit-set-data", u, v, w,
        JIT_PTR (u), JIT_WORD (v), JIT_WORD (w))
VOID_1 (jit_frame, "jit-frame", u, JIT_INT32 (u))
VOID_1 (jit_tramp, "jit-tramp", u, JIT_INT32 (u))
PTR_0 (jit_emit, "jit-emit")

VOID_0 (jit_print, "jit-print")

NODE_0 (jit_arg_f, "jit-arg-f")
VOID_2 (jit_getarg_f, "jit-getarg-f", u, v, JIT_FPR (u), JIT_NODE (v))
VOID_2 (jit_putargr_f, "jit-putargr-f", u, v, JIT_FPR (u), JIT_NODE (v))
VOID_2 (jit_putargi_f, "jit-putargi-f", u, v, JIT_FLOAT32 (u), JIT_NODE (v))
VOID_1 (jit_pushargr_f, "jit-pushargr-f", u, JIT_FPR (u))
VOID_1 (jit_pushargi_f, "jit-pushargi-f", u, JIT_FLOAT32 (u))
VOID_1 (jit_retr_f, "jit-retr-f", u, JIT_FPR (u))
VOID_1 (jit_reti_f, "jit-reti-f", u, JIT_FLOAT32 (u))
VOID_1 (jit_retval_f, "jit-retval-f", u, JIT_FPR (u))

NODE_0 (jit_arg_d, "jit-arg-d")
VOID_2 (jit_getarg_d, "jit-getarg-d", u, v, JIT_FPR (u), JIT_NODE (v))
VOID_2 (jit_putargr_d, "jit-putargr-d", u, v, JIT_FPR (u), JIT_NODE (v))
VOID_2 (jit_putargi_d, "jit-putargi-d", u, v, JIT_FLOAT64 (u), JIT_NODE (v))
VOID_1 (jit_pushargr_d, "jit-pushargr-d", u, JIT_FPR (u))
VOID_1 (jit_pushargi_d, "jit-pushargi-d", u, JIT_FLOAT64 (u))
VOID_1 (jit_retr_d, "jit-retr-d", u, JIT_FPR (u))
VOID_1 (jit_reti_d, "jit-reti-d", u, JIT_FLOAT64 (u))
VOID_1 (jit_retval_d, "jit-retval-d", u, JIT_FPR (u))


/* The variable `c' in `_jit_new_node_XXX' functions are `jit_code_t'
   enum.  Values of `jit_code_t' are passed as Scheme integer. */

NODE_1 (jit_new_node, "jit-new-node", c, SCM_I_INUM (c))
NODE_2 (jit_new_node_w, "jit-new-node-w", c, u, SCM_I_INUM (c), JIT_WORD (u))
NODE_2 (jit_new_node_p, "jit-new-node-p", c, u, SCM_I_INUM (c), JIT_PTR (u))
NODE_3 (jit_new_node_ww, "jit-new-node-ww", c, u, v,
        SCM_I_INUM (c), JIT_WORD (u), JIT_WORD (v))
NODE_3 (jit_new_node_wp, "jit-new-node-wp", c, u, v,
        SCM_I_INUM (c), JIT_WORD (u), JIT_PTR (v))
NODE_3 (jit_new_node_pw, "jit-new-node-pw", c, u, v,
        SCM_I_INUM (c), JIT_PTR (u), JIT_WORD (v))
NODE_3 (jit_new_node_wf, "jit-new-node-wf", c, u, v,
        SCM_I_INUM (c), JIT_WORD (u), JIT_FLOAT32 (v))
NODE_3 (jit_new_node_wd, "jit-new-node-wd", c, u, v,
        SCM_I_INUM (c), JIT_WORD (u), JIT_FLOAT64 (v))
NODE_4 (jit_new_node_www, "jit-new-node-www", c, u, v, w,
        SCM_I_INUM (c), JIT_WORD (u), JIT_WORD (v), JIT_WORD (w))
NODE_5 (jit_new_node_qww, "jit-new-node-qww", c, l, h, v, w,
        SCM_I_INUM (c), JIT_INT32 (l), JIT_INT32 (h),
        JIT_WORD (v), JIT_WORD (w))
NODE_4 (jit_new_node_wwf, "jit-new-node-wwf", c, u, v, w,
        SCM_I_INUM (c), JIT_WORD (u), JIT_WORD (v), JIT_FLOAT32 (w))
NODE_4 (jit_new_node_wwd, "jit-new-node-wwd", c, u, v, w,
        SCM_I_INUM (c), JIT_WORD (u), JIT_WORD (v), JIT_FLOAT64 (w))
NODE_4 (jit_new_node_pww, "jit-new-node-pww", c, u, v, w,
        SCM_I_INUM (c), JIT_PTR (u), JIT_WORD (v), JIT_WORD (w))
NODE_4 (jit_new_node_pwf, "jit-new-node-pwf", c, u, v, w,
        SCM_I_INUM (c), JIT_PTR (u), JIT_WORD (v), JIT_FLOAT32 (w))
NODE_4 (jit_new_node_pwd, "jit-new-node-pwd", c, u, v, w,
        SCM_I_INUM (c), JIT_PTR (u), JIT_WORD (v), JIT_FLOAT64 (w))

BOOL_1 (jit_arg_register_p, "jit-arg-register-p", u, JIT_NODE (u))
BOOL_1 (jit_callee_save_p, "jit-callee-save-p", u, JIT_INT32 (u))
BOOL_1 (jit_pointer_p, "jit-pointer-p", u, JIT_PTR (u))
BOOL_4 (jit_get_note, "jit-get-note", n, u, v, w,
        JIT_PTR (n), JIT_PTR (u), JIT_PTR (v), JIT_PTR (w))
VOID_0 (jit_disassemble, "jit-disassemble")


/*
 * Executable bytevector
 */

static inline void
set_flag (signed char *bv, size_t n)
#define ERRMSG "could not set execution flag on memory\n"
{
  char *start = (char *) ((((scm_t_bits) bv) / page_size) * page_size);
  int len = (scm_t_bits) bv + n - (scm_t_bits) start;
  if (mprotect (start, len, PROT_WRITE | PROT_READ | PROT_EXEC))
    {
      printf ("ERROR: ");
      if (errno == EINVAL)
        printf ("%s addr is not a valid pointer or not a multiple of PAGESIZE\n"
                ,ERRMSG);
      if (errno == EFAULT)
        printf ("%s memory cannot be accessed\n"
                ,ERRMSG);
      if (errno == EACCES)
        printf ("%s the memory cannot be given the spicified access\n"
                ,ERRMSG);
      if (errno == ENOMEM)
        printf ("%s internal memory structurs could not be constructed\n"
                ,ERRMSG);
    }
}
#undef ERRMSG

SCM_DEFINE (scm_make_bytevector_executable_x, "make-bytevector-executable!",
           1, 0, 0,
           (SCM bv), "Make bytevector as executable.")
#define FUNC_NAME s_scm_make_bytevector_executable_x
{
  if (scm_is_bytevector (bv))
    set_flag (SCM_BYTEVECTOR_CONTENTS (bv), SCM_BYTEVECTOR_LENGTH (bv));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * Initialization
 */

static void* scm_lightning_malloc (size_t size)
{
  return GC_MALLOC (size);
}

static void* scm_lightning_realloc (void* mem, size_t size)
{
  return GC_REALLOC (mem, size);
}

static void scm_lightning_free (void *mem)
{
  GC_FREE (mem);
}

void
scm_init_lightning (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "lightning-binding.x"
#endif
  page_size = getpagesize ();
  jit_set_memory_functions (scm_lightning_malloc,
                            scm_lightning_realloc,
                            scm_lightning_free);

  the_jit_state = scm_make_fluid ();
}

#undef VOID_0
#undef VOID_1
#undef VOID_2
#undef VOID_3
#undef NODE_0
#undef NODE_1
#undef NODE_2
#undef NODE_3
#undef NODE_4
#undef NODE_5
#undef BOOL_0
#undef BOOL_1
#undef BOOL_2

#undef JIT_NODE
#undef JIT_GPR
#undef JIT_PTR
#undef JIT_INT32
#undef JIT_WORD
#undef JIT_FLOAT32
#undef JIT_FLOAT64

#endif /* BUILD_VM_LIGHTNING == 1 */

#endif /* _SCM_LIGHTNING_BINDING_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
