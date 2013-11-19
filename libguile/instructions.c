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


SCM_SYMBOL (sym_left_arrow, "<-");
SCM_SYMBOL (sym_bang, "!");


#define OP_HAS_ARITY (1U << 0)

#define FOR_EACH_INSTRUCTION_WORD_TYPE(M)       \
    M(X32)                                      \
    M(U8_X24)                                   \
    M(U8_U24)                                   \
    M(U8_L24)                                   \
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
    M(X8_L24)                                   \
    M(B1_X7_L24)                                \
    M(B1_U7_L24)                                \
    M(B1_X7_U24)                                \
    M(B1_X31)

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
   arguments each instruction takes.  This piece of code is the only
   bit that actually interprets that language.  These macro definitions
   encode the operand types into bits in a 32-bit integer.

   (instruction-list) parses those encoded values into lists of symbols,
   one for each 32-bit word that the operator takes.  This list is used
   by Scheme to generate assemblers and disassemblers for the
   instructions.  */

#define OP1(type0) \
  (OP (0, type0))
#define OP2(type0, type1) \
  (OP (0, type0) | OP (1, type1))
#define OP3(type0, type1, type2) \
  (OP (0, type0) | OP (1, type1) | OP (2, type2))
#define OP4(type0, type1, type2, type3) \
  (OP (0, type0) | OP (1, type1) | OP (2, type2) | OP (3, type3))
#define OP5(type0, type1, type2, type3, type4) \
  (OP (0, type0) | OP (1, type1) | OP (2, type2) | OP (3, type3) | OP (4, type4))

#define OP_DST (1 << (TYPE_WIDTH * 5))

#define WORD_TYPE(n, word) \
  (((word) >> ((n) * TYPE_WIDTH)) & ((1 << TYPE_WIDTH) - 1))

struct scm_instruction {
  enum scm_rtl_opcode opcode;	/* opcode */
  const char *name;		/* instruction name */
  scm_t_uint32 meta;
  SCM symname;                  /* filled in later */
};


static scm_i_pthread_mutex_t itable_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;


static const struct scm_instruction*
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


/* Scheme interface */

SCM_DEFINE (scm_instruction_list, "instruction-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_instruction_list
{
  SCM list = SCM_EOL;
  int i;
  const struct scm_instruction *ip = fetch_instruction_table ();
  for (i = 0; i < SCM_VM_NUM_INSTRUCTIONS; i++)
    if (ip[i].name)
      {
        scm_t_uint32 meta = ip[i].meta;
        SCM tail = SCM_EOL;
        int len;

        /* Format: (name opcode word0 word1 ...) */

        if (WORD_TYPE (4, meta))
          len = 5;
        else if (WORD_TYPE (3, meta))
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
          case 5:
            tail = scm_cons (word_type_symbols[WORD_TYPE (4, meta)], tail);
          case 4:
            tail = scm_cons (word_type_symbols[WORD_TYPE (3, meta)], tail);
          case 3:
            tail = scm_cons (word_type_symbols[WORD_TYPE (2, meta)], tail);
          case 2:
            tail = scm_cons (word_type_symbols[WORD_TYPE (1, meta)], tail);
          case 1:
            tail = scm_cons (word_type_symbols[WORD_TYPE (0, meta)], tail);
          default:
            tail = scm_cons ((meta & OP_DST) ? sym_left_arrow : sym_bang, tail);
            tail = scm_cons (scm_from_int (ip[i].opcode), tail);
            tail = scm_cons (ip[i].symname, tail);
            break;
          }

        list = scm_cons (tail, list);
      }

  return scm_reverse_x (list, SCM_EOL);
}
#undef FUNC_NAME

void
scm_bootstrap_instructions (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_instructions",
                            (scm_t_extension_init_func)scm_init_instructions,
                            NULL);
}

void
scm_init_instructions (void)
{
#define INIT(type) \
  word_type_symbols[type] = scm_from_utf8_symbol (#type);
    FOR_EACH_INSTRUCTION_WORD_TYPE (INIT)
#undef INIT

#ifndef SCM_MAGIC_SNARFER
#include "libguile/instructions.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
