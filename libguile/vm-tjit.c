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
static SCM code_cache_table;
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
scm_tjit_enter (scm_t_uint32 *ip, size_t *state, scm_t_int32 jump,
                scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
                scm_i_thread *thread, struct scm_vm *vp,
                scm_i_jmp_buf *registers, int resume)
{
  SCM scm_ip, code;

  scm_ip = SCM_I_MAKINUM (ip + jump);
  code = scm_hashq_ref (code_cache_table, scm_ip, SCM_BOOL_F);

  if (scm_is_true (code))
    {
      /* Found compiled native code for this bytecode in cache, run it.
         Set next IP to the one returned from failed guard. */

      scm_t_native_code func;

      func = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
      ip = func (thread, vp, registers, resume);
    }
  else
    {
      SCM current_count, new_count;

      /* Increment the loop counter. */
      current_count = scm_hashq_ref (ip_counter_table, scm_ip, SCM_INUM0);
      new_count = SCM_PACK (SCM_UNPACK (current_count) + INUM_STEP);
      scm_hashq_set_x (ip_counter_table, scm_ip, new_count);

      /* If loop is hot, start tracing the bytecodes from next IP. */
      if (tjit_hot_count < new_count)
        {
          *state = SCM_TJIT_STATE_RECORD;
          *loop_start = (scm_t_uintptr) (ip + jump);
          *loop_end = (scm_t_uintptr) ip;
        }

      /* Next IP is jump destination specified in bytecode. */
      ip += jump;
    }

  return ip;
}

static inline void
scm_tjit_merge (scm_t_uint32 *ip, size_t *state,
                scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
                struct scm_vm *vp, SCM *fp,
                scm_t_uint32 *bc_idx, scm_t_uint32 *bytecode,
                scm_t_uint32 *ips_idx, scm_t_uintptr *ips)
{
#define SYNC_IP() vp->ip = (ip)
#define CACHE_FP() fp = (vp->fp)

#define IP_REACHED_TO(target) ip == ((scm_t_uint32 *) *target)

#define CLEANUP_STATES()                        \
  do {                                          \
    *state = SCM_TJIT_STATE_INTERPRET;          \
    *bc_idx = 0;                                \
    *ips_idx = 0;                               \
  } while (0)

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

  if (IP_REACHED_TO (loop_end))
    {
      SCM code, s_bytecode, s_bc_idx, s_ips, s_ips_idx;

      s_bytecode = scm_from_pointer (bytecode, NULL);
      s_bc_idx = SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32));
      s_ips = scm_from_pointer (ips, NULL);
      s_ips_idx = SCM_I_MAKINUM (*ips_idx);

      /* Compile the traced bytecode. */
      SYNC_IP ();
      code = scm_compile_tjit (s_bytecode, s_bc_idx, s_ips, s_ips_idx);
      CACHE_FP ();

      /* Cache the native code. */
      scm_hashq_set_x (code_cache_table, SCM_I_MAKINUM (*loop_start), code);

      CLEANUP_STATES ();
    }
  else if (SCM_I_INUM (tjit_max_record) < *bc_idx)
    {
      /* XXX: Log the abort for too long trace. */
      CLEANUP_STATES ();
    }

#undef SYNC_IP
#undef CACHE_FP
#undef IP_REACHED_TO
#undef CLEANUP_STATES
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
  code_cache_table = scm_c_make_hash_table (255);
  compile_tjit_var = SCM_VARIABLE_REF (scm_c_lookup ("compile-tjit"));

  GC_expand_hp (4 * 1024 * 1024 * SIZEOF_SCM_T_BITS);

#include "libguile/vm-tjit.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
