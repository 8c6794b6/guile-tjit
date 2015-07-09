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
static SCM tjit_hot_loop = SCM_I_MAKINUM (60);

/* Number of exits to decide a side exit is hot. */
static SCM tjit_hot_exit = SCM_I_MAKINUM (10);

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
static SCM failed_ip_table;
static SCM native_code_table;
static SCM exit_log_table;
static SCM bytecode_buffer_fluid;
static SCM compile_tjit_var;
static int trace_id = 1;

/* struct scm_native_entry */
/* { */
/*   int trace_id; */
/*   SCM code; */
/*   SCM exit_log; */
/* }; */


/*
 * Internal functions
 */

static inline SCM
tjitc (scm_t_uint32 *bytecode, scm_t_uint32 *bc_idx, SCM ip_ptr,
       scm_t_uintptr *parent_ip, int parent_exit_id)
{
  SCM s_id, s_bytecode, s_bc_idx, s_parent_ip, s_parent_exit_id;
  SCM result;

  s_id = SCM_I_MAKINUM (trace_id);
  s_bytecode = scm_from_pointer (bytecode, NULL);
  s_bc_idx = SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32));
  s_parent_ip = SCM_I_MAKINUM (*parent_ip);
  s_parent_exit_id = SCM_I_MAKINUM (parent_exit_id);

  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  result = scm_call_6 (compile_tjit_var, s_id, s_bytecode, s_bc_idx, ip_ptr,
                       s_parent_ip, s_parent_exit_id);
  scm_c_set_vm_engine_x (SCM_VM_TJIT_ENGINE);

  return result;
}

static inline void
add_native_code (scm_i_thread *thread, SCM s_ip, int trace_id, SCM code)
{
  /* Allocating memory and creating struct was showing segfault in
     nested loop example. */

  /* struct scm_native_entry* cache = */
  /*   scm_inline_gc_malloc (thread, sizeof (struct scm_native_code *)); */
  /* cache->trace_id = trace_id; */
  /* cache->code = code; */
  /* scm_hashq_set_x (native_code_table, s_ip, SCM_PACK_POINTER (cache)); */

  scm_hashq_set_x (native_code_table, s_ip, code);
  scm_hashq_set_x (exit_log_table, s_ip, scm_c_make_hash_table (31));
}

static inline void
start_recording (size_t *state, scm_t_uint32 *start, scm_t_uint32 *end,
                 scm_t_uintptr *loop_start, scm_t_uintptr *loop_end)
{
  *state = SCM_TJIT_STATE_RECORD;
  *loop_start = (scm_t_uintptr) start;
  *loop_end = (scm_t_uintptr) end;
}

static inline void
stop_recording (size_t *state, SCM *ips, scm_t_uint32 *bc_idx,
                scm_t_uintptr *parent_ip, int *exit_id)
{
  *state = SCM_TJIT_STATE_INTERPRET;
  *ips = SCM_EOL;
  *bc_idx = 0;
  *parent_ip = 0;
  *exit_id = 0;
}

static inline scm_t_uint32*
call_native (SCM s_ip, SCM code,
             scm_i_thread *thread, SCM *fp, scm_i_jmp_buf *registers,
             size_t *state, scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
             scm_t_uintptr *parent_ip, int *parent_exit_id)
{
  scm_t_native_code fn;
  scm_t_bits ret;
  scm_t_bits high_addr;
  scm_t_uintptr next_ip;
  SCM exit_id, exit_log, count;

  fn = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
  ret = fn (thread, fp, registers);

#if SCM_SIZEOF_UINTPTR_T == 8
 /* Assuming as 64bit architecture. */
  high_addr = SCM_I_INUM (s_ip) & 0xffffffff00000000;
  next_ip = high_addr | (ret >> 32);
#else
 /* XXX: Not tested, assuming as 32bit architecture. */
  high_addr = SCM_I_INUM (s_ip) & 0xffff0000;
  next_ip = high_addr | (ret >> 16);
#endif

  exit_id = SCM_I_MAKINUM (0xffff & ret);
  exit_log = scm_hashq_ref (exit_log_table, s_ip, SCM_BOOL_F);

  count = scm_hashq_ref (exit_log, exit_id, SCM_INUM0);
  count = SCM_PACK (SCM_UNPACK (count) + INUM_STEP);
  scm_hashq_set_x (exit_log, exit_id, count);

  if (tjit_hot_exit < count)
    {
      /* XXX: Detect trace exit to bytecode IP which is same as start
         ip. This could happen when initial arguments to native code had
         different types than at the time recording trace the first
         time, which will make guards in entry clause to fail.  Might
         not need to test for existing native code, because when
         patching went well, different IP should returned. */
      SCM code = scm_hashq_ref (native_code_table, SCM_I_MAKINUM (next_ip),
                                SCM_BOOL_F);

      if (scm_is_false (code))
        {
          scm_t_uint32 *start = (scm_t_uint32 *) next_ip;
          scm_t_uint32 *end = (scm_t_uint32 *) SCM_I_INUM (s_ip);
          SCM s_next_ip = SCM_I_MAKINUM (next_ip);

          if (scm_hashq_ref (failed_ip_table, s_next_ip, SCM_INUM0) <
              tjit_max_retries)
            {
              start_recording (state, start, end, loop_start, loop_end);
              *parent_ip = (scm_t_uintptr) SCM_I_INUM (s_ip);
              *parent_exit_id = (int) SCM_I_INUM (exit_id);
            }
        }
    }

  return (scm_t_uint32 *) next_ip;
}

static inline scm_t_uint32*
tjit_bytecode_buffer (void)
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
hot_loop_p (SCM ip, SCM current_count)
{
  return (tjit_hot_loop < current_count &&
          scm_hashq_ref (failed_ip_table, ip, SCM_INUM0) <
          tjit_max_retries);
}


/* C macros for vm-tjit engine

  These two macros were perviously defined as static inline functions.
  Though the static functions had some problems with garbage collector,
  sometimes fp was gabage collected after invoking native function.
  Hence rewritten as C macro to avoid this issue.  This file is included
  by "libguile/vm.c". Following two macros share common variables
  defined in "libguile/vm-engine.h", such as thread, vp, ip, ... etc. */

#define SCM_TJIT_ENTER(jump)                                            \
  do {                                                                  \
    SCM s_ip, code;                                                     \
                                                                        \
    s_ip = SCM_I_MAKINUM (ip + jump);                                   \
    code = scm_hashq_ref (native_code_table, s_ip, SCM_BOOL_F);         \
                                                                        \
    if (scm_is_true (code))                                             \
      ip = call_native (s_ip, code, thread, fp, registers,              \
                        &tjit_state, &tjit_loop_start, &tjit_loop_end,  \
                        &tjit_parent_ip, &tjit_parent_exit_id);         \
    else                                                                \
      {                                                                 \
        SCM current_count =                                             \
          scm_hashq_ref (ip_counter_table, s_ip, SCM_INUM0);            \
                                                                        \
        if (hot_loop_p (s_ip, current_count))                           \
          start_recording (&tjit_state, ip + jump, ip,                  \
                           &tjit_loop_start, &tjit_loop_end);           \
        else                                                            \
          increment_ip_counter (current_count, s_ip);                   \
                                                                        \
        /* Next IP is jump destination specified in bytecode. */        \
        ip += jump;                                                     \
      }                                                                 \
  } while (0)


#define SCM_TJIT_MERGE()                                                \
  do {                                                                  \
    SCM s_loop_start, locals;                                           \
    int num_locals;                                                     \
    int opcode, i;                                                      \
                                                                        \
    s_loop_start = SCM_I_MAKINUM (tjit_loop_start);                     \
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
    if (SCM_I_INUM (tjit_max_record) < tjit_bc_idx)                     \
      {                                                                 \
        /* XXX: Log the abort for too long trace. */                    \
        scm_hashq_set_x (failed_ip_table, s_loop_start, SCM_INUM1);     \
        stop_recording (&tjit_state, &tjit_traces, &tjit_bc_idx,        \
                        &tjit_parent_ip, &tjit_parent_exit_id);         \
      }                                                                 \
    else if (ip == ((scm_t_uint32 *) tjit_loop_end))                    \
      {                                                                 \
        SCM code;                                                       \
                                                                        \
        SYNC_IP ();                                                     \
        code = tjitc (tjit_bytecode, &tjit_bc_idx, tjit_traces,         \
                      &tjit_parent_ip, tjit_parent_exit_id);            \
        CACHE_FP ();                                                    \
        ++trace_id;                                                     \
                                                                        \
        if (scm_is_true (code))                                         \
          add_native_code (thread, s_loop_start, trace_id, code);       \
        else                                                            \
          increment_compilation_failure (s_loop_start);                 \
                                                                        \
        stop_recording (&tjit_state, &tjit_traces, &tjit_bc_idx,        \
                        &tjit_parent_ip, &tjit_parent_exit_id);         \
      }                                                                 \
    else                                                                \
      {                                                                 \
        SCM trace = SCM_EOL;                                            \
        SCM s_ip = SCM_I_MAKINUM (ip);                                  \
                                                                        \
        trace = scm_inline_cons (thread, locals, trace);                \
        trace = scm_inline_cons (thread, SCM_BOOL_F, trace);            \
        trace = scm_inline_cons (thread, s_ip, trace);                  \
        tjit_traces = scm_inline_cons (thread, trace, tjit_traces);     \
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

SCM_DEFINE (scm_tjit_hot_loop, "tjit-hot-loop", 0, 0, 0, (void),
            "Number of iterations to decide loop is hot.")
#define FUNC_NAME s_scm_tjit_hot_loop
{
  return tjit_hot_loop;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_tjit_hot_loop_x, "set-tjit-hot-loop!", 1, 0, 0,
            (SCM count),
            "Set number of iterations to decide loop is hot")
#define FUNC_NAME s_scm_set_tjit_hot_loop_x
{
  if (SCM_I_NINUMP (count) || count < 0)
    SCM_MISC_ERROR ("Unknown hot loop: ~a", scm_list_1 (count));

  tjit_hot_loop = count;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_hot_exit, "tjit-hot-exit", 0, 0, 0, (void),
            "Number of exits to decide side exit is hot.")
#define FUNC_NAME s_scm_tjit_hot_exit
{
  return tjit_hot_exit;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_tjit_hot_exit_x, "set-tjit-hot-exit!", 1, 0, 0,
            (SCM count),
            "Set number of exits to decide side exit is hot.")
#define FUNC_NAME s_scm_set_tjit_hot_exit_x
{
  if (SCM_I_NINUMP (count) || count < 0)
    SCM_MISC_ERROR ("Unknown hot exit: ~a", scm_list_1 (count));

  tjit_hot_exit = count;
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
  native_code_table = scm_c_make_hash_table (31);
  failed_ip_table = scm_c_make_hash_table (31);
  exit_log_table = scm_c_make_hash_table (31);
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
