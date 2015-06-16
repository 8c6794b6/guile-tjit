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


#define SCM_INCR(n) SCM_PACK (SCM_UNPACK (n) + INUM_STEP)


/*
 * Configurable parameters
 */

/* Number of iterations to decide a hot loop. */
static SCM tjit_hot_count = SCM_I_MAKINUM (60);

/* Maximum length of traced bytecodes. */
static SCM tjit_max_record = SCM_I_MAKINUM (6000);

/* Maximum count of retries for failed compilation. */
static SCM tjit_max_retries = SCM_I_MAKINUM (1);


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


/*
 *  Internal variables
 */

static SCM ip_counter_table;
static SCM code_cache_table;
static SCM failed_ip_table;
static SCM bytecode_buffer_fluid;
static SCM compile_tjit_var;


/*
 * Internal functions
 */

static inline SCM
scm_compile_tjit (SCM bc_ptr, SCM bc_len, SCM ip_ptr)
{
  SCM result;
  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  result = scm_call_3 (compile_tjit_var, bc_ptr, bc_len, ip_ptr);
  scm_c_set_vm_engine_x (SCM_VM_TJIT_ENGINE);
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
      /* Run compiled native code of this bytecode.  Set next IP to the
         one returned from failed guard. */
      scm_t_native_code fn;

      fn = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
      ip = fn (thread, vp, registers, resume);
    }
  else
    {
      SCM current_count;
      current_count = scm_hashq_ref (ip_counter_table, scm_ip, SCM_INUM0);

      if (tjit_hot_count < current_count &&
          scm_hashq_ref (failed_ip_table, scm_ip, SCM_INUM0) < tjit_max_retries)
        {
          /* Loops is hot and could be compiled. Start tracing the
             bytecodes from next IP */
          *state = SCM_TJIT_STATE_RECORD;
          *loop_start = (scm_t_uintptr) (ip + jump);
          *loop_end = (scm_t_uintptr) ip;
        }
      else
        {
          /* Increment the loop counter. */
          SCM new_count;

          new_count = SCM_INCR (current_count);
          scm_hashq_set_x (ip_counter_table, scm_ip, new_count);
        }

      /* Next IP is jump destination specified in bytecode. */
      ip += jump;
    }

  return ip;
}

static inline void
scm_tjit_merge (scm_t_uint32 *ip, size_t *state,
                scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
                struct scm_vm *vp, SCM *fp, scm_i_thread *thread,
                scm_t_uint32 *bc_idx, scm_t_uint32 *bytecode,
                scm_t_uint32 *ips_idx, SCM *ips)
{
#define SYNC_IP() vp->ip = (ip)
#define CACHE_FP() fp = (vp->fp)

#define LOCAL_ADDRESS(i)	(&SCM_FRAME_LOCAL (fp, i))
#define LOCAL_REF(i)            SCM_FRAME_LOCAL (fp, i)

#define IP_REACHED_TO(target) ip == ((scm_t_uint32 *) *target)
#define CLEANUP_STATES()                        \
  do {                                          \
    *state = SCM_TJIT_STATE_INTERPRET;          \
    *ips_idx = 0;                               \
    *ips = SCM_EOL;                             \
    *bc_idx = 0;                                \
  } while (0)

  SCM s_loop_start;
  SCM locals, env;
  SCM code, scm_ip;
  int num_locals;
  int op, op_size, i;

  s_loop_start = SCM_I_MAKINUM (*loop_start);

  scm_ip = SCM_I_MAKINUM (ip);
  code = scm_hashq_ref (code_cache_table, scm_ip, SCM_BOOL_F);
  if (scm_is_true (code))
    printf ("Found native code at %p while recording trace.\n", ip);

  op = *ip & 0xff;
  op_size = op_sizes[op];

  /* Store current IP and frame locals. Copying the local contents to
     vector manually, to get updated information from *fp, not from
     *vp which may not in sync. */
  num_locals = FRAME_LOCALS_COUNT ();
  locals = scm_c_make_vector (num_locals, SCM_UNDEFINED);
  for (i = 0; i < num_locals; ++i)
    scm_c_vector_set_x (locals, i, LOCAL_REF (i));
  env = scm_inline_cons (thread, SCM_I_MAKINUM (ip), locals);

  *ips = scm_inline_cons (thread, env, *ips);
  *ips_idx += 1;

  /* Store current bytecode. */
  for (i = 0; i < op_size; ++i)
    bytecode[*bc_idx + i] = ip[i];

  *bc_idx += op_sizes[op];

  if (SCM_I_INUM (tjit_max_record) < *bc_idx)
    {
      /* XXX: Log the abort for too long trace. */
      scm_hashq_set_x (failed_ip_table, s_loop_start, SCM_INUM1);
      CLEANUP_STATES ();
    }
  else if (IP_REACHED_TO (loop_end))
    {
      SCM code, s_bytecode, s_bc_idx;

      s_bytecode = scm_from_pointer (bytecode, NULL);
      s_bc_idx = SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32));

      /* Compile the traced bytecode. */
      SYNC_IP ();
      code = scm_compile_tjit (s_bytecode, s_bc_idx, *ips);
      CACHE_FP ();

      /* Cache the native code on compilation success. */
      if (scm_is_true (code))
        scm_hashq_set_x (code_cache_table, s_loop_start, code);
      else
        {
          SCM retries;
          retries = scm_hashq_ref (failed_ip_table, s_loop_start, SCM_INUM0);
          scm_hashq_set_x (failed_ip_table, s_loop_start, SCM_INCR (retries));
        }

      CLEANUP_STATES ();
    }

#undef SYNC_IP
#undef CACHE_FP
#undef LOCAL_ADDRESS
#undef LOCAL_REF
#undef IP_REACHED_TO
#undef CLEANUP_STATES
}

static inline
scm_t_uint32* scm_tjit_bytecode_buffer (void)
{
  return (scm_t_uint32 *) SCM_UNPACK (scm_fluid_ref (bytecode_buffer_fluid));
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


/*
 * Initialization
 */

static inline
void scm_init_buffer (void)
{
  void *buffer;
  size_t bytes;

  bytecode_buffer_fluid = scm_make_fluid ();
  bytes = sizeof (scm_t_uint32 *) * SCM_I_INUM (tjit_max_record) * 5;
  buffer = scm_inline_gc_malloc_pointerless (SCM_I_CURRENT_THREAD, bytes);
  scm_gc_protect_object (buffer);
  scm_fluid_set_x (bytecode_buffer_fluid, SCM_PACK (buffer));
}

void
scm_init_vm_tjit (void)
{
  ip_counter_table = scm_c_make_hash_table (31);
  code_cache_table = scm_c_make_hash_table (31);
  failed_ip_table = scm_c_make_hash_table (31);
  compile_tjit_var = SCM_VARIABLE_REF (scm_c_lookup ("compile-tjit"));

  scm_init_buffer ();

  GC_expand_hp (8 * 1024 * 1024 * SIZEOF_SCM_T_BITS);

#include "libguile/vm-tjit.x"
}

#undef SCM_INCR

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
