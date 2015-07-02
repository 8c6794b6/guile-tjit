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
static int trace_id = 1;


/*
 * Internal functions
 */

static inline SCM
scm_compile_tjit (scm_t_uint32 *bytecode, scm_t_uint32 *bc_idx, SCM ip_ptr)
{
  SCM result;
  SCM s_id, s_bytecode, s_bc_idx;

  s_id = SCM_I_MAKINUM (trace_id);
  s_bytecode = scm_from_pointer (bytecode, NULL);
  s_bc_idx = SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32));

  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  result = scm_call_4 (compile_tjit_var, s_id, s_bytecode, s_bc_idx, ip_ptr);
  scm_c_set_vm_engine_x (SCM_VM_TJIT_ENGINE);

  return result;
}

static inline void
cleanup_states (size_t *state, SCM *ips, scm_t_uint32 *bc_idx)
{
  *state = SCM_TJIT_STATE_INTERPRET;
  *ips = SCM_EOL;
  *bc_idx = 0;
}

static inline
scm_t_uint32* scm_tjit_bytecode_buffer (void)
{
  return (scm_t_uint32 *) SCM_UNPACK (scm_fluid_ref (bytecode_buffer_fluid));
}

static inline void
increment_ip_counter (SCM count, SCM ip)
{
  SCM new_count = SCM_PACK (SCM_UNPACK (count) + INUM_STEP);
  scm_hashq_set_x (ip_counter_table, ip, new_count);
}

static inline void
increment_compilation_failure (SCM ip)
{
  SCM retries;
  retries = scm_hashq_ref (failed_ip_table, ip, SCM_INUM0);
  retries = SCM_PACK (SCM_UNPACK (retries) + INUM_STEP);
  scm_hashq_set_x (failed_ip_table, ip, retries);
}

static inline int
ready_to_record_p (SCM ip, SCM current_count)
{
  return (tjit_hot_count < current_count &&
          scm_hashq_ref (failed_ip_table, ip, SCM_INUM0) <
          tjit_max_retries);
}

static inline scm_t_uint32*
call_native (SCM code, scm_i_thread *thread, struct scm_vm *vp,
             scm_i_jmp_buf *registers, int resume)
{
  scm_t_native_code fn;
  fn = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
  return fn (thread, vp, registers, resume);
}


/* C macros for vm-tjit engine

  These two macros were perviously defined as static inline functions.
  Though the static functions had some problems with garbage collector,
  sometimes fp was gabage collected after invoking native function.
  Hence rewritten as C macro to avoid this issue.  This file is included
  by "libguile/vm.c". Following two macros share common variables
  defined in "libguile/vm-engine.h", such as thread, vp, ip, ... etc.
*/

#define SCM_TJIT_ENTER(jump)                                            \
  do {                                                                  \
    SCM s_ip, code;                                                     \
                                                                        \
    s_ip = SCM_I_MAKINUM (ip + jump);                                   \
    code = scm_hashq_ref (code_cache_table, s_ip, SCM_BOOL_F);          \
                                                                        \
    if (scm_is_true (code))                                             \
      ip = call_native (code, thread, vp, registers, resume);           \
    else                                                                \
      {                                                                 \
        SCM current_count =                                             \
          scm_hashq_ref (ip_counter_table, s_ip, SCM_INUM0);            \
                                                                        \
        if (ready_to_record_p (s_ip, current_count))                    \
          {                                                             \
            tjit_state = SCM_TJIT_STATE_RECORD;                         \
            tjit_loop_start = (scm_t_uintptr) (ip + jump);              \
            tjit_loop_end = (scm_t_uintptr) ip;                         \
          }                                                             \
        else                                                            \
          increment_ip_counter (current_count, s_ip);                   \
                                                                        \
        /* Next IP is jump destination specified in bytecode. */        \
        ip += jump;                                                     \
      }                                                                 \
  } while (0)


#define SCM_TJIT_MERGE()                                                \
  do {                                                                  \
    SCM s_ip, s_loop_start;                                             \
    SCM locals, code;                                                   \
    int num_locals;                                                     \
    int opcode, i;                                                      \
    SCM trace = SCM_EOL;                                                \
                                                                        \
    s_ip = SCM_I_MAKINUM (ip);                                          \
    s_loop_start = SCM_I_MAKINUM (tjit_loop_start);                     \
    code = scm_hashq_ref (code_cache_table, s_ip, SCM_BOOL_F);          \
                                                                        \
    if (scm_is_true (code))                                             \
      ip = call_native (code, thread, vp, registers, resume);           \
                                                                        \
    opcode = *ip & 0xff;                                                \
                                                                        \
    /* Store current bytecode and increment bytecode index. */          \
    for (i = 0; i < op_sizes[opcode]; ++i)                              \
      {                                                                 \
        tjit_bytecode[tjit_bc_idx] = ip[i];                             \
        tjit_bc_idx += 1;                                               \
      }                                                                 \
                                                                        \
    /* Store current IP and frame locals. Copying the local    */       \
    /* contents to vector manually, to get updated information */       \
    /* from *fp, not from *vp which may out of sync.           */       \
    num_locals = FRAME_LOCALS_COUNT ();                                 \
    locals = scm_c_make_vector (num_locals, SCM_UNDEFINED);             \
    for (i = 0; i < num_locals; ++i)                                    \
      scm_c_vector_set_x (locals, i, LOCAL_REF (i));                    \
                                                                        \
    trace = scm_inline_cons (thread, locals, trace);                    \
    trace = scm_inline_cons (thread, code, trace);                      \
    trace = scm_inline_cons (thread, SCM_I_MAKINUM (ip), trace);        \
    tjit_traces = scm_inline_cons (thread, trace, tjit_traces);         \
                                                                        \
    if (SCM_I_INUM (tjit_max_record) < tjit_bc_idx)                     \
      {                                                                 \
        /* XXX: Log the abort for too long trace. */                    \
        scm_hashq_set_x (failed_ip_table, s_loop_start, SCM_INUM1);     \
        cleanup_states (&tjit_state, &tjit_traces, &tjit_bc_idx);       \
      }                                                                 \
    else if (ip == ((scm_t_uint32 *) tjit_loop_end))                    \
      {                                                                 \
        SCM code;                                                       \
                                                                        \
        SYNC_IP ();                                                     \
        code =                                                          \
          scm_compile_tjit (tjit_bytecode, &tjit_bc_idx, tjit_traces);  \
        CACHE_FP ();                                                    \
        ++trace_id;                                                     \
                                                                        \
        if (scm_is_true (code))                                         \
          scm_hashq_set_x (code_cache_table, s_loop_start, code);       \
        else                                                            \
          increment_compilation_failure (s_loop_start);                 \
                                                                        \
        cleanup_states (&tjit_state, &tjit_traces, &tjit_bc_idx);       \
      }                                                                 \
  } while (0)


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

void
scm_bootstrap_vm_tjit(void)
{
  void *buffer;
  size_t bytes;

  bytecode_buffer_fluid = scm_make_fluid ();
  bytes = sizeof (scm_t_uint32 *) * SCM_I_INUM (tjit_max_record) * 5;
  buffer = scm_inline_gc_malloc_pointerless (SCM_I_CURRENT_THREAD, bytes);
  scm_fluid_set_x (bytecode_buffer_fluid, SCM_PACK (buffer));

  ip_counter_table = scm_c_make_hash_table (31);
  code_cache_table = scm_c_make_hash_table (31);
  failed_ip_table = scm_c_make_hash_table (31);
  compile_tjit_var = SCM_VARIABLE_REF (scm_c_lookup ("compile-tjit"));

  GC_expand_hp (1024 * 1024 * SIZEOF_SCM_T_BITS);
}

void
scm_init_vm_tjit (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/vm-tjit.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
