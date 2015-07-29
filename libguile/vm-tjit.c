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
static SCM tlog_table;
static SCM bytecode_buffer_fluid;
static SCM compile_tjit_var;
static int trace_id = 1;


/*
 * Internal functions
 */

static inline SCM
tjitc (scm_t_uint32 *bytecode, scm_t_uint32 *bc_idx, SCM traces,
       scm_t_uintptr *parent_ip, int parent_exit_id,
       SCM linked_ip)
{
  SCM s_id, s_bytecode, s_bc_idx;
  SCM s_parent_ip, s_parent_exit_id;
  SCM result;

  if (scm_is_null (traces))
    return SCM_UNSPECIFIED;

  s_id = SCM_I_MAKINUM (trace_id);
  s_bytecode = scm_from_pointer (bytecode, NULL);
  s_bc_idx = SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32));
  s_parent_ip = SCM_I_MAKINUM (*parent_ip);
  s_parent_exit_id = SCM_I_MAKINUM (parent_exit_id);

  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  result = scm_call_7 (compile_tjit_var, s_id, s_bytecode, s_bc_idx,
                       traces, s_parent_ip, s_parent_exit_id, linked_ip);
  scm_c_set_vm_engine_x (SCM_VM_TJIT_ENGINE);

  return result;
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

static inline SCM
to_hex (SCM n)
{
  return scm_number_to_string (n, SCM_I_MAKINUM (16));
}

static inline void
dump_fps(SCM *old_fp, SCM *fp)
{
  SCM port = scm_current_output_port ();
  scm_puts ("old_fp:", port);
  scm_display (scm_from_pointer ((void *) old_fp, NULL), port);
  scm_puts (", fp:", port);
  scm_display (scm_from_pointer ((void *) fp, NULL), port);
  scm_newline (port);
}

static inline void
dump_locals (int n, SCM *fp)
{
  int i;
  SCM port = scm_current_output_port ();
  scm_puts ("locals", port);
  for (i = 0; i < n; ++i)
    {
      scm_puts(" [", port);
      scm_display (SCM_I_MAKINUM (i), port);
      scm_puts("]:", port);
      scm_display (SCM_PACK (fp[i]), port);
    }
  scm_newline (port);
}

static inline scm_t_uint32 *
call_native (SCM s_ip, SCM tlog,
             scm_i_thread *thread, SCM *fp, scm_i_jmp_buf *registers,
             size_t *state, scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
             scm_t_uintptr *parent_ip, int *parent_exit_id,
             scm_t_int32 *fp_offset_out, scm_t_uint32 *nlocals_out)
{
  scm_t_native_code f;
  scm_t_uintptr next_ip;
  scm_t_uint32 nlocals;
  SCM s_next_ip, fp_offset;
  SCM code, ret, exit_id, exit_ip, exit_counts, count;

  code = SCM_TLOG_CODE (tlog);
  f = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
  ret = f (thread, fp, registers);

  next_ip = SCM_TJIT_RETVAL_NEXT_IP (ret);
  exit_id = SCM_TJIT_RETVAL_EXIT_ID (ret);
  exit_ip = SCM_TJIT_RETVAL_EXIT_IP (ret);
  nlocals = SCM_TJIT_RETVAL_NLOCALS (ret);
  fp_offset = SCM_TJIT_RETVAL_LOCAL_OFFSET (ret);

  s_next_ip = SCM_I_MAKINUM (next_ip);
  tlog = scm_hashq_ref (tlog_table, exit_ip, SCM_BOOL_F);

  if (scm_is_true (tlog))
    {
      exit_counts = SCM_TLOG_EXIT_COUNTS (tlog);
      count = scm_hashq_ref (exit_counts, exit_id, SCM_INUM0);
      count = SCM_PACK (SCM_UNPACK (count) + INUM_STEP);
      scm_hashq_set_x (exit_counts, exit_id, count);
    }
  else
    {
      SCM port = scm_current_output_port ();
      scm_puts ("XXX: No trace for exit_ip ", port);
      scm_display (scm_number_to_string (exit_ip, SCM_I_MAKINUM (16)), port);
      scm_newline (port);
      count = SCM_INUM0;
    }

  if (tjit_hot_exit < count &&
      scm_hashq_ref (failed_ip_table, s_next_ip, SCM_INUM0) <
      tjit_max_retries)
    {
      scm_t_uint32 *start = (scm_t_uint32 *) next_ip;
      scm_t_uint32 *end = (scm_t_uint32 *) SCM_I_INUM (s_ip);

      if (start == end)
        {
          /* Deoptimizing root trace. When start and end is the same IP,
             assuming that this happened because of a guard failure in
             entry clause of the native code. Recording starts when VM
             entered the same loop next time. */
          scm_hashq_remove_x (tlog_table, s_ip);
        }
      else if (exit_ip == s_next_ip)
        {
          /* Deoptimizing side trace. When exit IP and next IP is the
             same IP, assuming that the guard failure caused this was at
             the beginning of side trace's native code. */
          scm_hashq_remove_x (tlog_table, exit_ip);

          /* Using exit ID from parent of the side trace, to replace
             the exit code of side trace's parent trace. */
          *parent_ip = (scm_t_uintptr) SCM_I_INUM (s_ip);
          *parent_exit_id = (int) SCM_I_INUM (SCM_TLOG_PARENT_EXIT_ID (tlog));
          start_recording (state, start, end, loop_start, loop_end);
        }
      else
        {
          *parent_ip = (scm_t_uintptr) SCM_I_INUM (exit_ip);
          *parent_exit_id = (int) SCM_I_INUM (exit_id);
          start_recording (state, start, end, loop_start, loop_end);
        }
    }

  *fp_offset_out = SCM_I_INUM (fp_offset);
  *nlocals_out = nlocals;

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

static inline SCM
record (scm_i_thread *thread, SCM s_ip, SCM *fp, SCM locals, SCM traces)
{
  SCM trace = SCM_EOL;
  SCM s_fp = scm_from_pointer ((void *) fp, NULL);

  trace = scm_inline_cons (thread, locals, trace);
  trace = scm_inline_cons (thread, s_fp, trace);
  trace = scm_inline_cons (thread, s_ip, trace);

  return scm_inline_cons (thread, trace, traces);
}

static inline int
is_root_trace (SCM tlog)
{
  return scm_is_true (tlog) && scm_is_true (SCM_TLOG_LOOP_ADDRESS (tlog));
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
    SCM s_ip, tlog;                                                     \
                                                                        \
    s_ip = SCM_I_MAKINUM (ip + jump);                                   \
    tlog = scm_hashq_ref (tlog_table, s_ip, SCM_BOOL_F);                \
                                                                        \
    if (scm_is_true (tlog))                                             \
      {                                                                 \
        scm_t_int32 shift = 0;                                          \
        scm_t_uint32 nlocals = 0;                                       \
                                                                        \
        /* Update `fp' and `ip' in C code.  Variables `fp' and `ip'  */ \
        /* are using registers, see "libguile/vm-engine.h".          */ \
        ip = call_native (s_ip, tlog, thread, fp, registers,            \
                          &tjit_state,                                  \
                          &tjit_loop_start, &tjit_loop_end,             \
                          &tjit_parent_ip, &tjit_parent_exit_id,        \
                          &shift, &nlocals);                            \
                                                                        \
        /* When fp is shifted, setting vp->sp with number of locals  */ \
        /* returned from native code, vp->sp need to be recovered    */ \
        /* after taking side exit.                                   */ \
        if (shift != 0)                                                 \
          {                                                             \
            SCM *old_fp;                                                \
            old_fp = fp;                                                \
            /* XXX: Use SCM_FRAME_DYNAMIC_LINK ? */                     \
            fp = vp->fp = old_fp + shift;                               \
            ALLOC_FRAME (nlocals);                                      \
          }                                                             \
      }                                                                 \
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
    SCM s_ip = SCM_I_MAKINUM (ip);                                      \
    SCM s_loop_start = SCM_I_MAKINUM (tjit_loop_start);                 \
    SCM tlog = scm_hashq_ref (tlog_table, s_ip, SCM_BOOL_F);            \
                                                                        \
    SCM locals;                                                         \
    int opcode, i, num_locals;                                          \
                                                                        \
    /* Record bytecode IP, FP, and locals. When tlog is true,        */ \
    /* current record is side trace, and the current bytecode IP is  */ \
    /* already recorded by root trace.                               */ \
    if (scm_is_false (tlog))                                            \
      {                                                                 \
        opcode = *ip & 0xff;                                            \
                                                                        \
        /* Store current bytecode and increment bytecode index. */      \
        for (i = 0; i < op_sizes[opcode]; ++i)                          \
          {                                                             \
            tjit_bytecode[tjit_bc_idx] = ip[i];                         \
            tjit_bc_idx += 1;                                           \
          }                                                             \
                                                                        \
        /* Copying the local contents to vector manually, to get     */ \
        /* updated information from *fp, not from *vp which may out  */ \
        /* of sync.                                                  */ \
        num_locals = FRAME_LOCALS_COUNT ();                             \
        locals = scm_c_make_vector (num_locals, SCM_UNDEFINED);         \
        for (i = 0; i < num_locals; ++i)                                \
          scm_c_vector_set_x (locals, i, LOCAL_REF (i));                \
                                                                        \
        tjit_traces = record (thread, s_ip, fp, locals, tjit_traces);   \
      }                                                                 \
                                                                        \
    if (ip == ((scm_t_uint32 *) tjit_loop_end) || is_root_trace (tlog)) \
      {                                                                 \
        SCM ret;                                                        \
                                                                        \
        SYNC_IP ();                                                     \
        ret = tjitc (tjit_bytecode, &tjit_bc_idx, tjit_traces,          \
                     &tjit_parent_ip, tjit_parent_exit_id,              \
                     s_ip);                                             \
        CACHE_FP ();                                                    \
        ++trace_id;                                                     \
                                                                        \
        if (scm_is_false (ret))                                         \
          increment_compilation_failure (s_loop_start);                 \
                                                                        \
        stop_recording (&tjit_state, &tjit_traces, &tjit_bc_idx,        \
                        &tjit_parent_ip, &tjit_parent_exit_id);         \
      }                                                                 \
    else if (SCM_I_INUM (tjit_max_record) < tjit_bc_idx)                \
      {                                                                 \
        /* XXX: Log the abort for too long trace. */                    \
        scm_hashq_set_x (failed_ip_table, s_loop_start, SCM_INUM1);     \
        stop_recording (&tjit_state, &tjit_traces, &tjit_bc_idx,        \
                        &tjit_parent_ip, &tjit_parent_exit_id);         \
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

SCM_DEFINE (scm_tlog_table, "tlog-table", 0, 0, 0, (void),
            "Hash table containing tlogs.")
#define FUNC_NAME s_scm_tlog_table
{
  return tlog_table;
}
#undef FUNC_NAME

SCM
scm_make_tjit_retval (scm_i_thread *thread, scm_t_bits next_ip,
                      scm_t_bits exit_id, scm_t_bits exit_ip,
                      scm_t_bits nlocals, scm_t_bits local_offset)
{
  SCM ret = scm_inline_gc_malloc_words (thread, 5);

  SCM_SET_CELL_WORD (ret, 0, next_ip);
  SCM_SET_CELL_WORD (ret, 1, exit_id);
  SCM_SET_CELL_WORD (ret, 2, exit_ip);
  SCM_SET_CELL_WORD (ret, 3, nlocals);
  SCM_SET_CELL_WORD (ret, 4, local_offset);

  return ret;
}


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
  failed_ip_table = scm_c_make_hash_table (31);
  tlog_table = scm_c_make_hash_table (31);
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
