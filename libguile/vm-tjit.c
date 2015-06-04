/* Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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

#include "_scm.h"
#include "vm-tjit.h"
#include "bdw-gc.h"

/*
 * Configurable parameters
 */

/* Number of iterations to decide a hot loop. */
static SCM tjit_hot_count = SCM_I_MAKINUM (1024);

/* Maximum length of traced bytecodes. */
static SCM tjit_max_record = SCM_I_MAKINUM (8000);


/*
 * Constants
 */

#define OP1(a) 1
#define OP2(a, b) 2
#define OP3(a, b, c) 3
#define OP4(a, b, c, d) 4
#define OP5(a, b, c, d, e) 5
#define OP_DST 0
#define NOP 0

static const int op_sizes[256] = {
#define OP_SIZE(opcode, tag, name, meta) meta,
  FOR_EACH_VM_OPERATION (OP_SIZE)
#undef OP_SIZE
};

#undef OP1
#undef OP2
#undef OP3
#undef OP4
#undef OP5
#undef OP_DST
#undef NOP

typedef scm_t_uint32* (*scm_t_native_code) (scm_i_thread *thread,
                                            struct scm_vm *vp,
                                            scm_i_jmp_buf *registers,
                                            int resume);


/*
 *  Internal variables
 */

static SCM ip_counter_table;
static SCM native_code_table;
static SCM compile_tjit_var;


/*
 * Internal functions
 */

static inline SCM
scm_compile_tjit (SCM bc_ptr, SCM bc_len, SCM ip_ptr, SCM ip_len)
{
  SCM result;
  /* scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE); */
  result = scm_call_4 (compile_tjit_var, bc_ptr, bc_len, ip_ptr, ip_len);
  /* scm_c_set_vm_engine_x (SCM_VM_TJIT_ENGINE); */
  return result;
}

static inline scm_t_uint32*
scm_tjit_enter (scm_t_uint32 *ip, scm_t_int32 jump, size_t *state,
                scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
                scm_i_thread *thread, struct scm_vm *vp,
                scm_i_jmp_buf *registers, int resume)
{
  SCM scm_ip, current_count, new_count, code;

  scm_ip = SCM_I_MAKINUM (ip + jump);
  current_count = scm_hashq_ref (ip_counter_table, scm_ip, SCM_INUM0);
  new_count = SCM_PACK (SCM_UNPACK (current_count) + INUM_STEP);
  scm_hashq_set_x (ip_counter_table, scm_ip, new_count);
  code = scm_hashq_ref (native_code_table, scm_ip, SCM_BOOL_F);

  if (tjit_hot_count < new_count && scm_is_false (code))
    {
      *state = SCM_TJIT_STATE_TRACE;
      *loop_start = (scm_t_uintptr) (ip + jump);
      *loop_end = (scm_t_uintptr) ip;
      scm_hashq_set_x (native_code_table, scm_ip, SCM_BOOL_T);
    }

  return ip;
}

static inline scm_t_uint32*
scm_tjit_merge (scm_t_uint32 *ip, SCM *fp, size_t *state,
                scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
                scm_t_uint32 *bc_idx, scm_t_uint32 *bytecode,
                scm_t_uint32 *ips_idx, scm_t_uintptr *ips,
                scm_i_thread *thread, struct scm_vm *vp,
                scm_i_jmp_buf *registers, int resume)
{
#define SYNC_IP() vp->ip = (ip)
#define CACHE_FP() fp = (vp->fp)

  int op, op_size, i;

  op = *ip & 0xff;
  op_size = op_sizes[op];

  /* Store current IP. */
  ips[*ips_idx] = (scm_t_uintptr) ip;
  *ips_idx += 1;

  /* Store current bytecode. */
  for (i = 0; i < op_size; ++i)
    bytecode[*bc_idx + i] = ip[i];
  *bc_idx += op_sizes[op];

  if (ip != ((scm_t_uint32 *) *loop_end))
    {
      if (SCM_I_INUM (tjit_max_record) < *bc_idx)
        {
          /* XXX: Log the abort for too long trace. */
          *state = SCM_TJIT_STATE_INTERPRET;
          *bc_idx = 0;
          *ips_idx = 0;

          return ip;
        }
    }
  else
    {
      SCM code;
      scm_t_native_code func;

      /* Compile the traced bytecode. */
      SYNC_IP ();
      code =
        scm_compile_tjit (scm_from_pointer (bytecode, NULL),
                          SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32)),
                          scm_from_pointer (ips, NULL),
                          SCM_I_MAKINUM (*ips_idx));
      CACHE_FP ();

      /* Cache the native code. */
      scm_hashq_set_x (native_code_table, SCM_I_MAKINUM (*loop_start),
                       code);

      /* Run compiled native code. Then update the IP to the place
         where guard failed. */
      func = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
      ip = func (thread, vp, registers, resume);

      /* Cleanup tracing states. */
      *state = SCM_TJIT_STATE_INTERPRET;
      *bc_idx = 0;
      *ips_idx = 0;
    }

  return ip;

}


/*
 * Scheme interfaces
 */

SCM_DEFINE (scm_tjit_ip_counter, "tjit-ip-counter", 0, 0, 0, (void), "")
#define FUNC_NAME s_scm_tjit_ip_counter
{
  return ip_counter_table;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_hot_count, "tjit-hot-count", 0, 0, 0,
            (void), "")
#define FUNC_NAME s_scm_tjit_hot_count
{
  return tjit_hot_count;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_tjit_hot_count_x, "set-tjit-hot-count!", 1, 0, 0,
            (SCM count), "")
#define FUNC_NAME s_scm_set_tjit_hot_count_x
{
  if (SCM_I_NINUMP (count) || count < 0)
    SCM_MISC_ERROR ("Unknown hot count: ~a", scm_list_1 (count));

  tjit_hot_count = count;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* XXX: scm_set_tjit_native_code_x */


/*
 * Initialization
 */

void
scm_init_vm_tjit (void)
{
  ip_counter_table = scm_c_make_hash_table (255);
  native_code_table = scm_c_make_hash_table (255);
  compile_tjit_var = SCM_VARIABLE_REF (scm_c_lookup ("compile-tjit"));

  GC_expand_hp (4 * 1024 * 1024 * SIZEOF_SCM_T_BITS);

#include "libguile/vm-tjit.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
