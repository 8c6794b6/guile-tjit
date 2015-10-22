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
static SCM tjit_max_retries = SCM_I_MAKINUM (2);


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

/* Hash table to hold iteration counts for loops. Key is bytecode IP,
   value is current count. */
static SCM tjit_ip_counter_table;

/* Hash table to hold IPs of failed traces. Key is bytecode IP, value is
   number of failed compilation. */
static SCM tjit_failed_ip_table;

/* Hash table to hold all fragments. Key is fragment ID, value is
   fragment data. */
static SCM tjit_fragment_table;

/* Hash table to hold fragment data of root traces. Key is bytecode IP,
   value is fragment data. */
static SCM tjit_root_trace_table;

/* Fluid to hold recorded bytecode. */
static SCM bytecode_buffer_fluid;

/* Variable to hold Scheme procedure tjitc. */
static SCM tjitc_var;

/* Initial trace id, increment after native compilation. */
static int tjit_trace_id = 1;


/*
 * Internal functions
 */

static inline SCM
tjitc (scm_t_uint32 *bytecode, scm_t_uint32 *bc_idx, SCM traces,
       int parent_fragment_id, int parent_exit_id, SCM linked_ip,
       SCM loop_p)
{
  SCM s_id, s_bytecode, s_bc_idx;
  SCM s_parent_fragment_id, s_parent_exit_id;
  SCM result;

  if (scm_is_null (traces))
    return SCM_UNSPECIFIED;

  s_id = SCM_I_MAKINUM (tjit_trace_id);
  s_bytecode = scm_from_pointer (bytecode, NULL);
  s_bc_idx = SCM_I_MAKINUM (*bc_idx * sizeof (scm_t_uint32));
  s_parent_fragment_id = SCM_I_MAKINUM (parent_fragment_id);
  s_parent_exit_id = SCM_I_MAKINUM (parent_exit_id);

  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  result = scm_call_8 (tjitc_var, s_id, s_bytecode, s_bc_idx, traces,
                       s_parent_fragment_id, s_parent_exit_id, linked_ip,
                       loop_p);
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
stop_recording (size_t *state, SCM *traces, scm_t_uint32 *bc_idx,
                int *parent_fragment_id, int *exit_id)
{
  *state = SCM_TJIT_STATE_INTERPRET;
  *traces = SCM_EOL;
  *bc_idx = 0;
  *parent_fragment_id = 0;
  *exit_id = 0;
}

static inline SCM
to_hex (SCM n)
{
  return scm_number_to_string (n, SCM_I_MAKINUM (16));
}

static inline int
is_hot_loop (SCM ip, SCM count)
{
  return (tjit_hot_loop < count &&
          scm_hashq_ref (tjit_failed_ip_table, ip, SCM_INUM0) <
          tjit_max_retries);
}

static inline int
is_hot_exit (SCM ip, SCM count)
{
  return (tjit_hot_exit < count &&
          scm_hashq_ref (tjit_failed_ip_table, ip, SCM_INUM0) < tjit_max_retries);
}

static inline void
call_native (SCM s_ip, scm_t_uint32 *previous_ip, SCM fragment,
             scm_i_thread *thread, struct scm_vm *vp, scm_i_jmp_buf *registers,
             size_t *state, scm_t_uintptr *loop_start, scm_t_uintptr *loop_end,
             int *parent_fragment_id, int *parent_exit_id,
             scm_t_uint32 *nlocals_out)
{
  scm_t_native_code f;
  scm_t_uint32 nlocals;
  SCM s_next_ip;
  SCM code, ret, exit_id, fragment_id, exit_counts, count;

  code = SCM_FRAGMENT_CODE (fragment);
  f = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
  ret = f (thread, vp, registers);

  exit_id = SCM_TJIT_RETVAL_EXIT_ID (ret);
  fragment_id = SCM_TJIT_RETVAL_FRAGMENT_ID (ret);
  nlocals = SCM_TJIT_RETVAL_NLOCALS (ret);

  s_next_ip = SCM_I_MAKINUM ((scm_t_uintptr) vp->ip);
  fragment = scm_hashq_ref (tjit_fragment_table, fragment_id, SCM_BOOL_F);

  if (scm_is_true (fragment))
    {
      exit_counts = SCM_FRAGMENT_EXIT_COUNTS (fragment);
      count = scm_hashq_ref (exit_counts, exit_id, SCM_INUM0);
      count = SCM_PACK (SCM_UNPACK (count) + INUM_STEP);
      scm_hashq_set_x (exit_counts, exit_id, count);
    }
  else
    /* Formerly, this part was displaying debug message to tell that
       required fragment was not found. After migrating from CPS IR to
       ANF IR, the use of SCM port was causing problem, seemed like
       related to garbage collector and GCC's reordering. Thus, removed
       the message. */
    count = SCM_INUM0;

  if (is_hot_exit (s_next_ip, count))
    {
      scm_t_uint32 *start = vp->ip;
      scm_t_uint32 *end = (scm_t_uint32 *) SCM_I_INUM (s_ip);

      /* When start and end is the same IP, assuming as guard failure in
         entry clause of the native code has happened. Recording starts
         when VM entered the same loop next time. To prevent the VM
         interpreter to stop recording immediately without visiting
         bytecode of the loop, setting end to the IP before the backward
         jump. Result of record will be a looping side trace starting
         from entry IP of parent trace. */
      if (start == end)
        end = previous_ip;

      *parent_fragment_id = (int) SCM_I_INUM (fragment_id);
      *parent_exit_id = (int) SCM_I_INUM (exit_id);
      start_recording (state, start, end, loop_start, loop_end);
    }

  *nlocals_out = nlocals;
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
  scm_hashq_set_x (tjit_ip_counter_table, ip, new_count);
}

static inline void
increment_compilation_failure (SCM ip)
{
  SCM retries;
  retries = scm_hashq_ref (tjit_failed_ip_table, ip, SCM_INUM0);
  retries = SCM_PACK (SCM_UNPACK (retries) + INUM_STEP);
  scm_hashq_set_x (tjit_failed_ip_table, ip, retries);
}

static inline SCM
record (scm_i_thread *thread, SCM s_ip, union scm_vm_stack_element *fp,
        SCM locals, SCM traces)
{
  SCM trace = SCM_EOL;
  SCM s_fp = scm_from_pointer ((void *) fp, NULL);
  SCM s_ra = SCM_I_MAKINUM (SCM_FRAME_RETURN_ADDRESS (fp));

  trace = scm_inline_cons (thread, locals, trace);
  trace = scm_inline_cons (thread, s_ra, trace);
  trace = scm_inline_cons (thread, s_fp, trace);
  trace = scm_inline_cons (thread, s_ip, trace);

  return scm_inline_cons (thread, trace, traces);
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
    SCM s_ip, fragment;                                                 \
                                                                        \
    s_ip = SCM_I_MAKINUM (ip + jump);                                   \
    fragment = scm_hashq_ref (tjit_root_trace_table, s_ip, SCM_BOOL_F); \
                                                                        \
    if (scm_is_true (fragment))                                         \
      {                                                                 \
        scm_t_uint32 nlocals = 0;                                       \
                                                                        \
        call_native (s_ip, ip, fragment, thread, vp, registers,         \
                     &tjit_state,                                       \
                     &tjit_loop_start, &tjit_loop_end,                  \
                     &tjit_parent_fragment_id, &tjit_parent_exit_id,    \
                     &nlocals);                                         \
                                                                        \
        /* Update `sp' and `ip' in C code. */                           \
        CACHE_REGISTER ();                                              \
                                                                        \
        /* Setting vp->sp with number of locals returnd from native  */ \
        /* code, vp->sp need to be recovered after taking side exit. */ \
        if (nlocals != FRAME_LOCALS_COUNT ())                           \
          ALLOC_FRAME (nlocals);                                        \
      }                                                                 \
    else                                                                \
      {                                                                 \
        SCM count =                                                     \
          scm_hashq_ref (tjit_ip_counter_table, s_ip, SCM_INUM0);       \
                                                                        \
        if (is_hot_loop (s_ip, count))                                  \
          start_recording (&tjit_state, ip + jump, ip,                  \
                           &tjit_loop_start, &tjit_loop_end);           \
        else                                                            \
          increment_ip_counter (count, s_ip);                           \
                                                                        \
        /* Next IP is jump destination specified in bytecode. */        \
        ip += jump;                                                     \
      }                                                                 \
  } while (0)

#define SCM_TJIT_MERGE()                                                \
  do {                                                                  \
    SCM s_ip = SCM_I_MAKINUM (ip);                                      \
                                                                        \
    scm_t_uint32 *start_ip = (scm_t_uint32 *) tjit_loop_start;          \
    scm_t_uint32 *end_ip = (scm_t_uint32 *) tjit_loop_end;              \
                                                                        \
    /* Avoid looking up fragment of looping-side-trace itself. */       \
    int link_found =                                                    \
      scm_is_true (scm_hashq_ref (tjit_root_trace_table, s_ip,          \
                                  SCM_BOOL_F))                          \
      && ip != start_ip;                                                \
                                                                        \
    /* Record bytecode IP, FP, and locals. When link was found,      */ \
    /* current record is side trace, and the current bytecode IP is  */ \
    /* already recorded by root trace.                               */ \
    if (!link_found)                                                    \
      {                                                                 \
        SCM locals;                                                     \
        int opcode, i, num_locals;                                      \
                                                                        \
        opcode = *ip & 0xff;                                            \
                                                                        \
        /* Store current bytecode. */                                   \
        for (i = 0; i < op_sizes[opcode]; ++i, ++tjit_bc_idx)           \
          tjit_bytecode[tjit_bc_idx] = ip[i];                           \
                                                                        \
        /* Copy local contents to vector. */                            \
        num_locals = FRAME_LOCALS_COUNT ();                             \
        locals = scm_c_make_vector (num_locals, SCM_UNDEFINED);         \
        for (i = 0; i < num_locals; ++i)                                \
          scm_c_vector_set_x (locals, i, SP_REF (i));                   \
                                                                        \
        tjit_traces =                                                   \
          record (thread, s_ip, vp->fp, locals, tjit_traces);           \
      }                                                                 \
                                                                        \
    /* Stop recording when IP reached to the end or found a link. */    \
    if (ip == end_ip || link_found)                                     \
      {                                                                 \
        SYNC_IP ();                                                     \
        tjitc (tjit_bytecode, &tjit_bc_idx, tjit_traces,                \
               tjit_parent_fragment_id, tjit_parent_exit_id, s_ip,      \
               link_found ? SCM_BOOL_F : SCM_BOOL_T);                   \
        CACHE_SP ();                                                    \
        ++tjit_trace_id;                                                \
        stop_recording (&tjit_state, &tjit_traces, &tjit_bc_idx,        \
                        &tjit_parent_fragment_id,                       \
                        &tjit_parent_exit_id);                          \
      }                                                                 \
    else if (SCM_I_INUM (tjit_max_record) < tjit_bc_idx)                \
      {                                                                 \
        /* XXX: Log the abort for too long trace. */                    \
        SCM s_loop_start_ip = SCM_I_MAKINUM (tjit_loop_start);          \
        increment_compilation_failure (s_loop_start_ip);                \
        stop_recording (&tjit_state, &tjit_traces, &tjit_bc_idx,        \
                        &tjit_parent_fragment_id,                       \
                        &tjit_parent_exit_id);                          \
      }                                                                 \
  } while (0)


/*
 * Scheme interfaces
 */

SCM_DEFINE (scm_tjit_ip_counter, "tjit-ip-counter", 0, 0, 0, (void),
            "Hash table to count number of jumped visits, per IP.")
#define FUNC_NAME s_scm_tjit_ip_counter
{
  return tjit_ip_counter_table;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_fragment_table, "tjit-fragment-table", 0, 0, 0, (void),
            "Hash table containing all fragments.")
#define FUNC_NAME s_scm_tjit_fragment_table
{
  return tjit_fragment_table;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_root_trace_table, "tjit-root-trace-table", 0, 0, 0, (void),
            "Hash table containing root trace fragments.")
#define FUNC_NAME s_scm_tjit_root_trace_table
{
  return tjit_root_trace_table;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_failed_ip_table, "tjit-failed-ip-table", 0, 0, 0, (void),
            "Hash table containing failed ips.")
#define FUNC_NAME s_scm_tjit_failed_ip_table
{
  return tjit_failed_ip_table;
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

SCM_DEFINE (scm_tjit_max_retries, "tjit-max-retries", 0, 0, 0, (void),
            "Number of retries for failed IP.")
#define FUNC_NAME s_scm_tjit_max_retries
{
  return tjit_max_retries;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_tjit_max_retries_x, "set-tjit-max-retries!", 1, 0, 0,
            (SCM count),
            "Set number of max retries for failed IP.")
#define FUNC_NAME s_scm_set_tjit_max_retries_x
{
  if (SCM_I_NINUMP (count) || count < 0)
    SCM_MISC_ERROR ("Unknown hot exit: ~a", scm_list_1 (count));

  tjit_max_retries = count;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_tjit_make_retval (scm_i_thread *thread,
                      scm_t_bits exit_id, scm_t_bits exit_ip,
                      scm_t_bits nlocals)
{
  SCM ret = scm_inline_gc_malloc_pointerless (thread, 3 * sizeof (void *));

  SCM_SET_CELL_WORD (ret, 0, exit_id);
  SCM_SET_CELL_WORD (ret, 1, exit_ip);
  SCM_SET_CELL_WORD (ret, 2, nlocals);

  return ret;
}

void
scm_tjit_dump_retval (SCM tjit_retval, struct scm_vm *vp)
{
  SCM port = scm_current_output_port ();

  scm_puts (";;; trace ", port);
  scm_display (SCM_TJIT_RETVAL_FRAGMENT_ID (tjit_retval), port);
  scm_puts (": exit ", port);
  scm_display (SCM_TJIT_RETVAL_EXIT_ID (tjit_retval), port);
  scm_puts (" => ", port);
  scm_display (to_hex (SCM_I_MAKINUM (vp->ip)), port);
  scm_newline (port);
}

static inline void
scm_i_dump_sp (SCM trace_id, union scm_vm_stack_element *sp, SCM port)
{
  /* union scm_vm_stack_element *old_fp = SCM_FRAME_DYNAMIC_LINK (fp); */
  /* scm_t_uint32 *ra = SCM_FRAME_RETURN_ADDRESS (fp); */

  scm_puts (";;; trace ", port);
  scm_display (trace_id, port);
  scm_puts (": sp=", port);
  scm_display (to_hex (SCM_I_MAKINUM (sp)), port);

  /* scm_puts (": fp=", port); */
  /* scm_display (to_hex (SCM_I_MAKINUM (fp.as_uint)), port); */
  /* scm_puts (" dl=", port); */
  /* scm_display (to_hex (SCM_I_MAKINUM (old_fp)), port); */
  /* scm_puts (" ra=", port); */
  /* scm_display (to_hex (SCM_I_MAKINUM (ra)), port); */

  scm_newline (port);
}

void
scm_tjit_dump_locals (SCM trace_id, int n, union scm_vm_stack_element *sp)
{
  int i;
  SCM port = scm_current_output_port ();

  scm_i_dump_sp (trace_id, sp, port);

  scm_puts (";;; trace ", port);
  scm_display (trace_id, port);
  scm_puts (": locals", port);
  for (i = 0; i < n + 2; ++i)
    {
      scm_puts(" [", port);
      scm_display (SCM_I_MAKINUM (i), port);
      scm_puts("]: #x", port);
      scm_display (to_hex (SCM_I_MAKINUM (sp[i].as_ptr)), port);
    }
  scm_newline (port);
}


/*
 * Gluing functions
 */

SCM
scm_do_inline_from_double (scm_i_thread *thread, double val)
{
  SCM z;

  z = SCM_PACK_POINTER
    (scm_inline_gc_malloc_pointerless (thread, sizeof (scm_t_double)));

  SCM_SET_CELL_TYPE (z, scm_tc16_real);
  SCM_REAL_VALUE (z) = val;

  return z;
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

  tjit_ip_counter_table = scm_c_make_hash_table (31);
  tjit_failed_ip_table = scm_c_make_hash_table (31);
  tjit_fragment_table = scm_c_make_hash_table (31);
  tjit_root_trace_table = scm_c_make_hash_table (31);
  tjitc_var = SCM_VARIABLE_REF (scm_c_lookup ("tjitc"));

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
