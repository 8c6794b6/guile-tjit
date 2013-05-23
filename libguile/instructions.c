/* Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>

#include "_scm.h"
#include "threads.h"
#include "instructions.h"


struct scm_instruction {
  enum scm_opcode opcode;	/* opcode */
  const char *name;		/* instruction name */
  signed char len;		/* Instruction length.  This may be -1 for
				   the loader (see the `VM_LOADER'
				   macro).  */
  signed char npop;		/* The number of values popped.  This may be
				   -1 for insns like `call' which can take
				   any number of arguments.  */
  char npush;			/* the number of values pushed */
  SCM symname;                  /* filled in later */
};


#define OP_HAS_ARITY (1U << 0)

#define FOR_EACH_INSTRUCTION_WORD_TYPE(M)       \
    M(X32)                                      \
    M(U8_X24)                                   \
    M(U8_U24)                                   \
    M(U8_L24)                                   \
    M(U8_R24)                                   \
    M(U8_U8_I16)                                \
    M(U8_U8_U8_U8)                              \
    M(U8_U12_U12)                               \
    M(U32) /* Unsigned. */                      \
    M(I32) /* Immediate. */                     \
    M(A32) /* Immediate, high bits. */          \
    M(B32) /* Immediate, low bits. */           \
    M(N32) /* Non-immediate. */                 \
    M(S32) /* Scheme value (indirected). */     \
    M(L32) /* Label. */                         \
    M(LO32) /* Label with offset. */            \
    M(X8_U24)                                   \
    M(X8_U12_U12)                               \
    M(X8_R24)                                   \
    M(X8_L24)                                   \
    M(B1_X7_L24)                                \
    M(B1_U7_L24)

#define TYPE_WIDTH 5

enum word_type
  {
#define ENUM(type) type,
    FOR_EACH_INSTRUCTION_WORD_TYPE (ENUM)
#undef ENUM
  };

static SCM word_type_symbols[] =
  {
#define FALSE(type) SCM_BOOL_F,
    FOR_EACH_INSTRUCTION_WORD_TYPE (FALSE)
#undef FALSE
  };

#define OP(n,type) ((type) << (n*TYPE_WIDTH))

/* The VM_DEFINE_OP macro uses a CPP-based DSL to describe what kinds of
   arguments each RTL instruction takes.  This piece of code is the only
   bit that actually interprets that language.  These macro definitions
   encode the operand types into bits in a 32-bit integer.

   (rtl-instruction-list) parses those encoded values into lists of
   symbols, one for each 32-bit word that the operator takes.  (system
   vm rtl) uses those word types to generate assemblers and
   disassemblers for the instructions.  */

#define OP1(type0) \
  (OP (0, type0))
#define OP2(type0, type1) \
  (OP (0, type0) | OP (1, type1))
#define OP3(type0, type1, type2) \
  (OP (0, type0) | OP (1, type1) | OP (2, type2))
#define OP4(type0, type1, type2, type3) \
  (OP (0, type0) | OP (1, type1) | OP (2, type2) | OP (3, type3))

#define OP_DST (1 << (TYPE_WIDTH * 5))

#define WORD_TYPE(n, word) \
  (((word) >> ((n) * TYPE_WIDTH)) & ((1 << TYPE_WIDTH) - 1))

struct scm_rtl_instruction {
  enum scm_rtl_opcode opcode;	/* opcode */
  const char *name;		/* instruction name */
  scm_t_uint32 meta;
  SCM symname;                  /* filled in later */
};


#define SCM_VALIDATE_LOOKUP_INSTRUCTION(pos, var, cvar)               \
  do {                                                                \
    cvar = scm_lookup_instruction_by_name (var);                      \
    SCM_ASSERT_TYPE (cvar, var, pos, FUNC_NAME, "INSTRUCTION_P");     \
  } while (0)


static scm_i_pthread_mutex_t itable_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;


static struct scm_instruction*
fetch_instruction_table ()
{
  static struct scm_instruction *table = NULL;

  scm_i_pthread_mutex_lock (&itable_lock);
  if (SCM_UNLIKELY (!table))
    {
      size_t bytes = SCM_VM_NUM_INSTRUCTIONS * sizeof(struct scm_instruction);
      int i;
      table = malloc (bytes);
      memset (table, 0, bytes);
#define VM_INSTRUCTION_TO_TABLE 1
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef VM_INSTRUCTION_TO_TABLE
      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        {
          table[i].opcode = i;
          if (table[i].name)
            table[i].symname = scm_from_utf8_symbol (table[i].name);
          else
            table[i].symname = SCM_BOOL_F;
        }
    }
  scm_i_pthread_mutex_unlock (&itable_lock);

  return table;
}

static struct scm_rtl_instruction*
fetch_rtl_instruction_table ()
{
  static struct scm_rtl_instruction *table = NULL;

  scm_i_pthread_mutex_lock (&itable_lock);
  if (SCM_UNLIKELY (!table))
    {
      size_t bytes = SCM_VM_NUM_INSTRUCTIONS * sizeof(struct scm_rtl_instruction);
      int i;
      table = malloc (bytes);
      memset (table, 0, bytes);

#define INIT(opcode, tag, name_, meta_) table[opcode].name = name_; table[opcode].meta = meta_;
      FOR_EACH_VM_OPERATION (INIT);
#undef INIT

      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        {
          table[i].opcode = i;
          if (table[i].name)
            table[i].symname = scm_from_utf8_symbol (table[i].name);
          else
            table[i].symname = SCM_BOOL_F;
        }
    }
  scm_i_pthread_mutex_unlock (&itable_lock);

  return table;
}

static struct scm_instruction *
scm_lookup_instruction_by_name (SCM name)
{
  static SCM instructions_by_name = SCM_BOOL_F;
  struct scm_instruction *table = fetch_instruction_table ();
  SCM op;

  if (SCM_UNLIKELY (scm_is_false (instructions_by_name)))
    {
      unsigned int i;

      instructions_by_name =
        scm_make_hash_table (SCM_I_MAKINUM (SCM_VM_NUM_INSTRUCTIONS));

      for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
        if (scm_is_true (table[i].symname))
          scm_hashq_set_x (instructions_by_name, table[i].symname,
                           SCM_I_MAKINUM (i));
    }

  op = scm_hashq_ref (instructions_by_name, name, SCM_UNDEFINED);
  if (SCM_I_INUMP (op))
    return &table[SCM_I_INUM (op)];

  return NULL;
}


/* Scheme interface */

SCM_DEFINE (scm_instruction_list, "instruction-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_instruction_list
{
  SCM list = SCM_EOL;
  int i;
  struct scm_instruction *ip = fetch_instruction_table ();
  for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
    if (ip[i].name)
      list = scm_cons (ip[i].symname, list);
  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_rtl_instruction_list, "rtl-instruction-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_rtl_instruction_list
{
  SCM list = SCM_EOL;
  int i;
  struct scm_rtl_instruction *ip = fetch_rtl_instruction_table ();
  for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
    if (ip[i].name)
      {
        scm_t_uint32 meta = ip[i].meta;
        SCM tail = SCM_EOL;
        int len;

        /* Format: (name opcode len rest? out br in) */

        if (WORD_TYPE (3, meta))
          len = 4;
        else if (WORD_TYPE (2, meta))
          len = 3;
        else if (WORD_TYPE (1, meta))
          len = 2;
        else if (WORD_TYPE (0, meta))
          len = 1;
        else
          abort ();

        switch (len)
          {
          case 4:
            tail = scm_cons (word_type_symbols[WORD_TYPE (3, meta)], tail);
          case 3:
            tail = scm_cons (word_type_symbols[WORD_TYPE (2, meta)], tail);
          case 2:
            tail = scm_cons (word_type_symbols[WORD_TYPE (1, meta)], tail);
          case 1:
            tail = scm_cons (word_type_symbols[WORD_TYPE (0, meta)], tail);
          default:
            tail = scm_cons (scm_from_int (ip[i].opcode), tail);
            tail = scm_cons (ip[i].symname, tail);
            break;
          }

        list = scm_cons (tail, list);
      }

  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_p, "instruction?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_instruction_p
{
  return scm_from_bool (scm_lookup_instruction_by_name (obj) != NULL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_length, "instruction-length", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_length
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->len);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_pops, "instruction-pops", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_pops
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->npop);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_pushes, "instruction-pushes", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_pushes
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->npush);
}
#undef FUNC_NAME

SCM_DEFINE (scm_instruction_to_opcode, "instruction->opcode", 1, 0, 0,
	    (SCM inst),
	    "")
#define FUNC_NAME s_scm_instruction_to_opcode
{
  struct scm_instruction *ip;
  SCM_VALIDATE_LOOKUP_INSTRUCTION (1, inst, ip);
  return SCM_I_MAKINUM (ip->opcode);
}
#undef FUNC_NAME

SCM_DEFINE (scm_opcode_to_instruction, "opcode->instruction", 1, 0, 0,
	    (SCM op),
	    "")
#define FUNC_NAME s_scm_opcode_to_instruction
{
  scm_t_signed_bits opcode;
  SCM ret = SCM_BOOL_F;

  SCM_MAKE_VALIDATE (1, op, I_INUMP);
  opcode = SCM_I_INUM (op);

  if (opcode >= 0 && opcode < SCM_VM_NUM_INSTRUCTIONS)
    ret = fetch_instruction_table ()[opcode].symname;

  if (scm_is_false (ret))
    scm_wrong_type_arg_msg (FUNC_NAME, 1, op, "INSTRUCTION_P");

  return ret;
}
#undef FUNC_NAME

void
scm_bootstrap_instructions (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_instructions",
                            (scm_t_extension_init_func)scm_init_instructions,
                            NULL);

#define INIT(type) \
  word_type_symbols[type] = scm_from_utf8_symbol (#type);
    FOR_EACH_INSTRUCTION_WORD_TYPE (INIT)
#undef INIT
}

void
scm_init_instructions (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/instructions.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
