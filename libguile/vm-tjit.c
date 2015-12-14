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

/* For bytecode IP hash, used to count hot IP, etc. */
#define TJIT_HASH_MASK 0xffffff
#define TJIT_HASH_SIZE ((TJIT_HASH_MASK + 1) / 4)
#define TJIT_HASH_FUNC(ip) ((TJIT_HASH_MASK & ip) >> 2)

#define SCM_TJIT_HASH(name)                             \
  static scm_t_uint16 name##_hash[TJIT_HASH_SIZE];      \
                                                        \
  static inline scm_t_uint16                            \
  name##_ref (scm_t_uint32 key)                         \
  {                                                     \
    return name##_hash[TJIT_HASH_FUNC (key)];           \
  }                                                     \
                                                        \
  static inline void                                    \
  name##_set (scm_t_uint32 key, scm_t_uint16 val)       \
  {                                                     \
    name##_hash[TJIT_HASH_FUNC (key)] = val;            \
  }

#define SCM_TJIT_PARAM(name, sname, ini)                                \
                                                                        \
  static SCM tjit_##name = SCM_I_MAKINUM (ini);                         \
                                                                        \
  SCM_DEFINE (scm_tjit_##name, "tjit-"#sname, 0, 0, 0, (void), "")      \
  {                                                                     \
    return tjit_##name;                                                 \
  }                                                                     \
                                                                        \
  SCM_DEFINE (scm_set_tjit_##name##_x, "set-tjit-"#sname"!",            \
              1, 0, 0, (SCM count), "")                                 \
  {                                                                     \
    /* XXX: Some params does not require `65536 < count' check. */      \
    if (SCM_I_NINUMP (count)                                            \
        || count < 0                                                    \
        || SCM_I_MAKINUM (65536) < count)                               \
      scm_misc_error ("set-tjit-"#sname"!",                             \
                      "Invalid arg: ~s", scm_list_1 (count));           \
                                                                        \
    tjit_##name = count;                                                \
    return SCM_UNSPECIFIED;                                             \
  }

#define SCM_TJIT_TABLE(name, sname)                                     \
                                                                        \
  static SCM tjit_##name##_table;                                       \
                                                                        \
  SCM_DEFINE (scm_tjit_##name, "tjit-"#sname, 0, 0, 0, (void), "")      \
  {                                                                     \
   return tjit_##name##_table;                                          \
  }

#define SCM_TJIT_IS_HOT(ref, ip, count)                                 \
  (ref < count                                                          \
   && SCM_I_MAKINUM (failed_ip_ref ((scm_t_uintptr) (ip))) <            \
   tjit_max_retries)

#define SCM_TJIT_INCREMENT_HOT_IP(JUMP, END_IP, TTYPE, REF)             \
  do {                                                                  \
      if (SCM_I_MAKINUM (failed_ip_ref ((scm_t_uintptr) (ip + JUMP))) < \
          tjit_max_retries)                                             \
      {                                                                 \
        scm_t_uint16 count = hot_ip_ref ((scm_t_uintptr) (ip + JUMP));  \
                                                                        \
        if (REF < SCM_I_MAKINUM (count))                                \
          start_recording (tj, ip + JUMP, END_IP, TTYPE);               \
        else                                                            \
          hot_ip_set ((scm_t_uintptr) (ip + JUMP), count + 1);          \
      }                                                                 \
                                                                        \
      /* Next IP is jump destination specified in bytecode. */          \
      ip += JUMP;                                                       \
  } while (0);

#define SCM_TJITC(ip, loop_p)             \
  do {                                    \
    SYNC_IP ();                           \
    tjitc (tj, ip, loop_p);               \
    CACHE_SP ();                          \
    stop_recording (tj);                  \
  } while (0)

#define SCM_DOWNREC_P(fragmeng)                         \
  (scm_is_true (fragment) && scm_is_true (SCM_FRAGMENT_DOWNREC_P (fragment)))

#define SCM_UPREC_P(fragment)                   \
  (scm_is_true (fragment) && scm_is_true (SCM_FRAGMENT_UPREC_P (fragment)))


/*
 * Configurable parameters
 */

/* Number of iterations to decide a hot loop. */
SCM_TJIT_PARAM (hot_loop, hot-loop, 60)

/* Number of calls to decide a hot procedure call. */
SCM_TJIT_PARAM (hot_call, hot-call, 12)

/* Number of exits to decide a hot side exit. */
SCM_TJIT_PARAM (hot_exit, hot-exit, 10)

/* Maximum length of traced bytecodes. */
SCM_TJIT_PARAM (max_record, max-record, 6000)

/* Maximum count of retries for failed compilation. */
SCM_TJIT_PARAM (max_retries, max-retries, 2)

/* Number of recursive procedure calls to unroll. */
SCM_TJIT_PARAM (num_unrolls, num-unrolls, 3)


/*
 *  Internal variables
 */

/* Hash array to hold iteration counts for loops. Key is bytecode IP,
   value is current count. */
SCM_TJIT_HASH (hot_ip)

/* Hash array to hold IP of  root traces. Key is bytecode IP,
   value is 0 or 1. */
SCM_TJIT_HASH (root_ip)

/* Hash array to hold IPs of failed traces. Key is bytecode IP, value is
   number of failed compilation. */
SCM_TJIT_HASH (failed_ip)

/* Hash table to hold all fragments. Key is fragment ID, value is
   fragment data. */
SCM_TJIT_TABLE (fragment, fragment);

/* Hash table to hold fragment data of root traces. Key is bytecode IP,
   value is fragment data. */
SCM_TJIT_TABLE (root_trace, root-trace);


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
  scm_t_uint16 retries;
  retries = failed_ip_ref ((scm_t_uintptr) ip);
  failed_ip_set ((scm_t_uintptr) ip, retries + 1);
  stop_recording (tj);
}

static inline void
record (struct scm_tjit_state *tj, scm_i_thread *thread, struct scm_vm *vp,
        scm_t_uint32 *ip, union scm_vm_stack_element *sp)
{
  int opcode, i, num_locals;
  SCM locals, trace;
  SCM s_ra = SCM_I_MAKINUM (SCM_FRAME_RETURN_ADDRESS (vp->fp));
  SCM s_dl = SCM_I_MAKINUM (SCM_FRAME_DYNAMIC_LINK (vp->fp));

  opcode = *ip & 0xff;

  /* Store current bytecode. */
  for (i = 0; i < op_sizes[opcode]; ++i, ++tj->bc_idx)
    tj->bytecode[tj->bc_idx] = ip[i];

  /* Copy local contents to vector. */
  num_locals = FRAME_LOCALS_COUNT ();
  locals = scm_c_make_vector (num_locals, SCM_UNDEFINED);
  for (i = 0; i < num_locals; ++i)
    scm_c_vector_set_x (locals, i, scm_from_pointer (sp[i].as_ptr, NULL));

  trace = scm_inline_cons (thread, locals, SCM_EOL);
  trace = scm_inline_cons (thread, s_dl, trace);
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
  SCM code, exit_id, fragment_id, exit_counts, count;
  struct scm_tjit_retval *ret;

  code = SCM_FRAGMENT_CODE (fragment);
  f = (scm_t_native_code) SCM_BYTEVECTOR_CONTENTS (code);
  ret = f (thread, vp, registers);

  exit_id = SCM_PACK (ret->exit_id);
  fragment_id = SCM_PACK (ret->fragment_id);
  nlocals = SCM_I_INUM (SCM_PACK (ret->nlocals));

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

  if (SCM_TJIT_IS_HOT (tjit_hot_exit, vp->ip, count))
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

#define SCM_TJIT_ENTER(JUMP, END_IP, TTYPE, REF)                        \
  do {                                                                  \
    if (root_ip_ref ((scm_t_uintptr) (ip + JUMP)))                      \
      {                                                                 \
        SCM s_ip, fragment;                                             \
        scm_t_uint32 nlocals = 0;                                       \
                                                                        \
        s_ip = SCM_I_MAKINUM (ip + JUMP);                               \
        fragment = scm_hashq_ref (tjit_root_trace_table, s_ip,          \
                                  SCM_BOOL_F);                          \
                                                                        \
        /* Check that the fragment exists. `root_ip_ref' uses hash   */ \
        /* function, might return incorrect result.                  */ \
        if (scm_is_true (fragment))                                     \
          {                                                             \
            call_native (s_ip, ip, fragment, thread, vp, registers, tj, \
                         &nlocals);                                     \
                                                                        \
            /* Update `sp' and `ip' in C code. */                       \
            CACHE_REGISTER ();                                          \
                                                                        \
            /* Setting vp->sp with number of locals returnd from     */ \
            /* native code, vp->sp need to be recovered after taking */ \
            /* side exit. */                                            \
            if (nlocals != FRAME_LOCALS_COUNT ())                       \
              ALLOC_FRAME (nlocals);                                    \
          }                                                             \
        else                                                            \
          SCM_TJIT_INCREMENT_HOT_IP (JUMP, END_IP, TTYPE, REF);         \
      }                                                                 \
    else                                                                \
      SCM_TJIT_INCREMENT_HOT_IP (JUMP, END_IP, TTYPE, REF);             \
  } while (0)

#define SCM_TJIT_MERGE()                                                \
  do {                                                                  \
    SCM fragment;                                                       \
    SCM s_ip = SCM_I_MAKINUM (ip);                                      \
    scm_t_uint32 *start_ip = (scm_t_uint32 *) tj->loop_start;           \
    scm_t_uint32 *end_ip = (scm_t_uint32 *) tj->loop_end;               \
    int has_root = root_ip_ref ((scm_t_uintptr) ip);                    \
                                                                        \
    /* Avoid looking up fragment of looping-side-trace itself. */       \
    int link_found = has_root && ip != start_ip;                        \
                                                                        \
    if (has_root)                                                       \
      {                                                                 \
        fragment = scm_hashq_ref (tjit_root_trace_table, s_ip,          \
                                  SCM_BOOL_F);                          \
      }                                                                 \
    else                                                                \
      fragment = SCM_BOOL_F;                                            \
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

SCM_DEFINE (scm_tjit_increment_id_x, "tjit-increment-id!", 0, 0, 0,
            (void), "Increment trace ID.")
#define FUNC_NAME s_scm_tjit_increment_id_x
{
  ++tjit_trace_id;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_increment_compilation_failure_x,
            "tjit-increment-compilation-failure!", 1, 0, 0,
            (SCM ip), "Increment compilation failure.")
#define FUNC_NAME s_scm_tjit_increment_compilation_failure_x
{
  scm_t_uint32 key = SCM_I_INUM (ip);
  failed_ip_set (key, failed_ip_ref (key) + 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_tjit_add_root_ip_x, "tjit-add-root-ip!", 1, 0, 0,
            (SCM ip), "Add root trace IP.")
#define FUNC_NAME s_scm_tjit_add_root_ip_x
{
  root_ip_set (SCM_I_INUM (ip), 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * Gluing functions
 */

struct scm_tjit_retval*
scm_make_tjit_retval (scm_i_thread *thread, scm_t_bits exit_id,
                      scm_t_bits fragment_id, scm_t_bits nlocals)
{
  struct scm_tjit_retval *ret =
    scm_inline_gc_malloc_pointerless (thread, sizeof (struct scm_tjit_retval));

  ret->exit_id = exit_id;
  ret->fragment_id = fragment_id;
  ret->nlocals = nlocals;

  return ret;
}

void
scm_tjit_dump_retval (struct scm_tjit_retval *retval, struct scm_vm *vp)
{
  SCM port = scm_current_output_port ();

  scm_puts (";;; trace ", port);
  scm_display (SCM_PACK (retval->fragment_id), port);
  scm_puts (": exit ", port);
  scm_display (SCM_PACK (retval->exit_id), port);
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
      scm_puts ("]: 0x", port);
      scm_display (to_hex (SCM_I_MAKINUM (sp[i].as_uint)), port);
    }

  scm_newline (port);
}

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

static inline void
init_tjit_hash (void)
{
  int i;

  for (i = 0; i < TJIT_HASH_SIZE; ++i)
    {
      hot_ip_hash[i] = 0;
      root_ip_hash[i] = 0;
      failed_ip_hash[i] = 0;
    }
}

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

  init_tjit_hash ();
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
