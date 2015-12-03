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

/* Internal C macros */

#define SCM_TJIT_IS_HOT(ref, ip, count)                            \
  (ref < count                                                     \
   && scm_hashq_ref (tjit_failed_ip_table, ip, SCM_INUM0) <        \
   tjit_max_retries)

#define SCM_TJIT_INCREMENT(table, ip, count)                            \
  do {                                                                  \
    SCM new_count = SCM_PACK (SCM_UNPACK (count) + INUM_STEP);          \
    scm_hashq_set_x (table, ip, new_count);                             \
  } while (0)

#define SCM_TJITC(ip, loop_p)             \
  do {                                    \
    SYNC_IP ();                           \
    tjitc (tj, ip, loop_p);               \
    CACHE_SP ();                          \
    ++tjit_trace_id;                      \
    stop_recording (tj);                  \
  } while (0)

#define SCM_DOWNREC_P(fragmeng)                     \
  scm_is_true (SCM_FRAGMENT_DOWNREC_P (fragment))

#define SCM_UPREC_P(fragment)                   \
  scm_is_true (SCM_FRAGMENT_UPREC_P (fragment))

/*
 * Configurable parameters
 */

/* Number of iterations to decide a hot loop. */
static SCM tjit_hot_loop = SCM_I_MAKINUM (60);

/* Number of calls to decide a hot procedure call. */
static SCM tjit_hot_call = SCM_I_MAKINUM (12);

/* Number of exits to decide a side exit is hot. */
static SCM tjit_hot_exit = SCM_I_MAKINUM (10);

/* Maximum length of traced bytecodes. */
static SCM tjit_max_record = SCM_I_MAKINUM (6000);

/* Maximum count of retries for failed compilation. */
static SCM tjit_max_retries = SCM_I_MAKINUM (2);

/* Number of recursive procedure calls to unroll. */
static SCM tjit_num_unrolls = SCM_I_MAKINUM (3);


/*
 *  Internal variables
 */

/* Hash table to hold iteration counts for loops. Key is bytecode IP,
   value is current count. */
static SCM tjit_jump_counter_table;

/* Hash table to hold counts for call. */
static SCM tjit_call_counter_table;

/* Hash table to hold counts for return. */
static SCM tjit_return_counter_table;

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
to_hex (SCM n)
{
  return scm_number_to_string (n, SCM_I_MAKINUM (16));
}

static inline SCM
tjitc (struct scm_tjit_state *tj, SCM linked_ip, SCM loop_p)
{
  SCM s_id, s_bytecode, s_bytecode_ptr;
  SCM s_parent_fragment_id, s_parent_exit_id;
  SCM result, downrec_p, uprec_p;
  size_t bytecode_len;

  if (scm_is_null (tj->traces))
    return SCM_UNSPECIFIED;

  s_id = SCM_I_MAKINUM (tjit_trace_id);
  s_bytecode_ptr = scm_from_pointer (tj->bytecode, NULL);
  bytecode_len = tj->bc_idx * sizeof (scm_t_uint32);
  s_bytecode = scm_c_take_gc_bytevector ((signed char *) tj->bytecode,
                                         bytecode_len, s_bytecode_ptr);
  s_parent_fragment_id = SCM_I_MAKINUM (tj->parent_fragment_id);
  s_parent_exit_id = SCM_I_MAKINUM (tj->parent_exit_id);
  downrec_p = tj->trace_type == SCM_TJIT_TRACE_CALL ? SCM_BOOL_T : SCM_BOOL_F;
  uprec_p = tj->trace_type == SCM_TJIT_TRACE_RETURN ? SCM_BOOL_T : SCM_BOOL_F;

  scm_c_set_vm_engine_x (SCM_VM_REGULAR_ENGINE);
  result = scm_call_9 (tjitc_var, s_id, s_bytecode, tj->traces,
                       s_parent_fragment_id, s_parent_exit_id, linked_ip,
                       loop_p, downrec_p, uprec_p);
  scm_c_set_vm_engine_x (SCM_VM_TJIT_ENGINE);

  return result;
}

static inline void
start_recording (struct scm_tjit_state *tj,
                 scm_t_uint32 *start, scm_t_uint32 *end,
                 enum scm_tjit_trace_type trace_type)
{
  tj->vm_state = SCM_TJIT_VM_STATE_RECORD;
  tj->trace_type = trace_type;
  tj->loop_start = (scm_t_uintptr) start;
  tj->loop_end = (scm_t_uintptr) end;
}

static inline void
stop_recording (struct scm_tjit_state *tj)
{
  tj->vm_state = SCM_TJIT_VM_STATE_INTERPRET;
  tj->traces = SCM_EOL;
  tj->bc_idx = 0;
  tj->parent_fragment_id = 0;
  tj->parent_exit_id = 0;
  tj->nunrolled = 0;
}

static inline void
abort_recording (struct scm_tjit_state *tj, scm_t_uint32 *ip)
{
  SCM retries, s_ip;
  s_ip = SCM_I_MAKINUM (ip);
  retries = scm_hashq_ref (tjit_failed_ip_table, s_ip, SCM_INUM0);
  retries = SCM_PACK (SCM_UNPACK (retries) + INUM_STEP);
  scm_hashq_set_x (tjit_failed_ip_table, s_ip, retries);
  stop_recording (tj);
}

static inline void
record (struct scm_tjit_state *tj, scm_i_thread *thread, struct scm_vm *vp,
        scm_t_uint32 *ip, union scm_vm_stack_element *sp)
{
  int opcode, i, num_locals;
  SCM locals, trace;
  SCM s_ra = SCM_I_MAKINUM (SCM_FRAME_RETURN_ADDRESS (vp->fp));

  opcode = *ip & 0xff;

  /* Store current bytecode. */
  for (i = 0; i < op_sizes[opcode]; ++i, ++tj->bc_idx)
    tj->bytecode[tj->bc_idx] = ip[i];

  /* Copy local contents to vector. */
  num_locals = FRAME_LOCALS_COUNT ();
  locals = scm_c_make_vector (num_locals, SCM_UNDEFINED);
  for (i = 0; i < num_locals; ++i)
    scm_c_vector_set_x (locals, i, sp[i].as_scm);

  trace = scm_inline_cons (thread, locals, SCM_EOL);
  trace = scm_inline_cons (thread, s_ra, trace);
  trace = scm_inline_cons (thread, SCM_I_MAKINUM (ip), trace);

  tj->traces = scm_inline_cons (thread, trace, tj->traces);
}

static inline void
call_native (SCM s_ip, scm_t_uint32 *previous_ip, SCM fragment,
             scm_i_thread *thread, struct scm_vm *vp, scm_i_jmp_buf *registers,
             struct scm_tjit_state *tj, scm_t_uint32 *nlocals_out)
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

  if (SCM_TJIT_IS_HOT (tjit_hot_exit, s_next_ip, count))
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

      tj->parent_fragment_id = (int) SCM_I_INUM (fragment_id);
      tj->parent_exit_id = (int) SCM_I_INUM (exit_id);
      start_recording (tj, start, end, SCM_TJIT_TRACE_JUMP);
    }

  *nlocals_out = nlocals;
}

static inline struct scm_tjit_state*
scm_make_tjit_state (void)
{
  struct scm_tjit_state *t =
    scm_gc_malloc (sizeof (struct scm_tjit_state), "tjitstate");

  t->vm_state = SCM_TJIT_VM_STATE_INTERPRET;
  t->trace_type = SCM_TJIT_TRACE_JUMP;
  t->loop_start = 0;
  t->loop_end = 0;
  t->bc_idx = 0;
  t->bytecode =
    (scm_t_uint32 *) SCM_UNPACK (scm_fluid_ref (bytecode_buffer_fluid));
  t->traces = SCM_EOL;
  t->parent_fragment_id = 0;
  t->parent_exit_id = 0;
  t->nunrolled = 0;

  return t;
}

/* C macros for vm-tjit engine

  These two macros were perviously defined as static inline functions.
  Though the static functions had some problems with garbage collector,
  sometimes fp was gabage collected after invoking native function.
  Hence rewritten as C macro to avoid this issue.  This file is included
  by "libguile/vm.c". Following two macros share common variables
  defined in "libguile/vm-engine.h", such as thread, vp, ip, ... etc. */

#define SCM_TJIT_ENTER(JUMP, HOT_IP, END_IP, TTYPE, TABLE, REF)         \
  do {                                                                  \
    SCM s_ip, fragment;                                                 \
                                                                        \
    s_ip = SCM_I_MAKINUM (ip + JUMP);                                   \
    fragment = scm_hashq_ref (tjit_root_trace_table, s_ip, SCM_BOOL_F); \
                                                                        \
    if (scm_is_true (fragment))                                         \
      {                                                                 \
        scm_t_uint32 nlocals = 0;                                       \
                                                                        \
        call_native (s_ip, ip, fragment, thread, vp, registers, tj,     \
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
        SCM s_hot_ip = SCM_I_MAKINUM (HOT_IP);                          \
        SCM count = scm_hashq_ref (TABLE, s_hot_ip, SCM_INUM0);         \
                                                                        \
        if (SCM_TJIT_IS_HOT (REF, s_hot_ip, count))                     \
          start_recording (tj, ip + JUMP, END_IP, TTYPE);               \
        else                                                            \
          SCM_TJIT_INCREMENT (TABLE, s_hot_ip, count);                  \
                                                                        \
        /* Next IP is jump destination specified in bytecode. */        \
        ip += JUMP;                                                     \
      }                                                                 \
  } while (0)

#define SCM_TJIT_MERGE()                                                \
  do {                                                                  \
    SCM s_ip = SCM_I_MAKINUM (ip);                                      \
                                                                        \
    scm_t_uint32 *start_ip = (scm_t_uint32 *) tj->loop_start;           \
    scm_t_uint32 *end_ip = (scm_t_uint32 *) tj->loop_end;               \
    SCM fragment = scm_hashq_ref (tjit_root_trace_table, s_ip,          \
                                  SCM_BOOL_F);                          \
                                                                        \
    /* Avoid looking up fragment of looping-side-trace itself. */       \
    int link_found = scm_is_true (fragment) && ip != start_ip;          \
                                                                        \
    switch (tj->trace_type)                                             \
      {                                                                 \
      case SCM_TJIT_TRACE_JUMP:                                         \
        if (link_found)                                                 \
          {                                                             \
            if (SCM_DOWNREC_P (fragment))                               \
              {                                                         \
                tj->trace_type = SCM_TJIT_TRACE_CALL;                   \
                record (tj, thread, vp, ip, sp);                        \
              }                                                         \
            else if (SCM_UPREC_P (fragment))                            \
              {                                                         \
                tj->trace_type = SCM_TJIT_TRACE_RETURN;                 \
                record (tj, thread, vp, ip, sp);                        \
              }                                                         \
            else                                                        \
              SCM_TJITC (s_ip, SCM_BOOL_F);                             \
          }                                                             \
        else if (ip == end_ip)                                          \
          {                                                             \
            record (tj, thread, vp, ip, sp);                            \
            SCM_TJITC (s_ip, SCM_BOOL_T);                               \
          }                                                             \
        else                                                            \
          record (tj, thread, vp, ip, sp);                              \
        break;                                                          \
                                                                        \
      case SCM_TJIT_TRACE_CALL:                                         \
        if (ip == start_ip || (link_found && SCM_DOWNREC_P (fragment))) \
          {                                                             \
            if (tj->nunrolled == SCM_I_INUM (tjit_num_unrolls))         \
              SCM_TJITC (s_ip, link_found ? SCM_BOOL_F : SCM_BOOL_T);   \
            else                                                        \
              {                                                         \
                record (tj, thread, vp, ip, sp);                        \
                ++(tj->nunrolled);                                      \
              }                                                         \
          }                                                             \
        else if (ip == end_ip)                                          \
          /* XXX: Hot non-recursive procedure call. Worth to compile */ \
          /* but currently marked as failure and ignored.            */ \
          abort_recording (tj, start_ip);                               \
        else                                                            \
          record (tj, thread, vp, ip, sp);                              \
        break;                                                          \
                                                                        \
      case SCM_TJIT_TRACE_RETURN:                                       \
        if (ip == start_ip || (link_found && SCM_UPREC_P (fragment)))   \
          {                                                             \
            if (tj->nunrolled == SCM_I_INUM (tjit_num_unrolls))         \
              SCM_TJITC (s_ip, link_found ? SCM_BOOL_F : SCM_BOOL_T);   \
            else                                                        \
              {                                                         \
                record (tj, thread, vp, ip, sp);                        \
                ++(tj->nunrolled);                                      \
              }                                                         \
          }                                                             \
        else                                                            \
          record (tj, thread, vp, ip, sp);                              \
        break;                                                          \
                                                                        \
      default:                                                          \
        break;                                                          \
      }                                                                 \
                                                                        \
    if (SCM_I_INUM (tjit_max_record) < tj->bc_idx)                      \
      abort_recording (tj, start_ip);                                   \
  } while (0)


/*
 * Scheme interfaces
 */

SCM_DEFINE (scm_tjit_ip_counter, "tjit-ip-counter", 0, 0, 0, (void),
            "Hash table to count number of jumped visits, per IP.")
#define FUNC_NAME s_scm_tjit_ip_counter
{
  return tjit_jump_counter_table;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_call_counter, "tjit-call-counter", 0, 0, 0, (void),
            "Hash table to count number of calls, per IP.")
#define FUNC_NAME s_scm_tjit_call_counter
{
  return tjit_call_counter_table;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_return_counter, "tjit-return-counter", 0, 0, 0, (void),
            "Hash table to count number of returns, per IP.")
#define FUNC_NAME s_scm_tjit_return_counter
{
  return tjit_return_counter_table;
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

SCM_DEFINE (scm_tjit_hot_call, "tjit-hot-call", 0, 0, 0, (void),
            "Number of calls to decide procedure is hot.")
#define FUNC_NAME s_scm_tjit_hot_call
{
  return tjit_hot_call;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_tjit_hot_call_x, "set-tjit-hot-call!", 1, 0, 0,
            (SCM count),
            "Set number of calls to decide procedure is hot")
#define FUNC_NAME s_scm_set_tjit_hot_call_x
{
  if (SCM_I_NINUMP (count) || count < 0)
    SCM_MISC_ERROR ("Unknown hot call: ~a", scm_list_1 (count));

  tjit_hot_call = count;
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

void
scm_tjit_dump_locals (SCM trace_id, int n, union scm_vm_stack_element *sp,
                      struct scm_vm *vp)
{
  int i;
  SCM port = scm_current_output_port ();

  scm_puts (";;; trace ", port);
  scm_display (trace_id, port);
  scm_puts (": sp=", port);
  scm_display (to_hex (SCM_I_MAKINUM (sp)), port);
  scm_puts (" fp=", port);
  scm_display (to_hex (SCM_I_MAKINUM (vp->fp)), port);
  scm_puts (" ra=", port);
  scm_display (to_hex (SCM_I_MAKINUM (sp[n].as_ptr)), port);
  scm_puts (" dl=", port);
  scm_display (to_hex (SCM_I_MAKINUM (sp[n + 1].as_ptr)), port);
  scm_newline (port);

  scm_puts (";;; trace ", port);
  scm_display (trace_id, port);
  scm_puts (": locals", port);
  for (i = 0; i < n; ++i)
    {
      scm_puts(" [", port);
      scm_display (SCM_I_MAKINUM (i), port);
      scm_puts ("]:", port);
      scm_display (sp[i].as_scm, port);
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

SCM
scm_do_inline_cons (scm_i_thread *thread, SCM x, SCM y)
{
  return scm_inline_cons (thread, x, y);
}

void
scm_do_vm_expand_stack (struct scm_vm *vp, union scm_vm_stack_element *new_sp)
{
  vm_expand_stack (vp, new_sp);
}


/*
 * Initialization
 */

void
scm_bootstrap_vm_tjit(void)
{
  void *buffer;
  size_t bytes;

  GC_expand_hp (1024 * 1024 * SIZEOF_SCM_T_BITS);

  bytecode_buffer_fluid = scm_make_fluid ();
  bytes = sizeof (scm_t_uint32 *) * SCM_I_INUM (tjit_max_record) * 5;
  buffer = scm_gc_malloc_pointerless (bytes, "tjitbuffer");
  scm_fluid_set_x (bytecode_buffer_fluid, SCM_PACK (buffer));

  tjit_jump_counter_table = scm_c_make_hash_table (31);
  tjit_call_counter_table = scm_c_make_hash_table (31);
  tjit_return_counter_table = scm_c_make_hash_table (31);
  tjit_failed_ip_table = scm_c_make_hash_table (31);
  tjit_fragment_table = scm_c_make_hash_table (31);
  tjit_root_trace_table = scm_c_make_hash_table (31);
  tjitc_var = SCM_VARIABLE_REF (scm_c_lookup ("tjitc"));
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
