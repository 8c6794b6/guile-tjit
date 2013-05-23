/* Copyright (C) 2001, 2009, 2012 Free Software Foundation, Inc.
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

#ifndef _SCM_INSTRUCTIONS_H_
#define _SCM_INSTRUCTIONS_H_

#include <libguile.h>
#include <libguile/vm-operations.h>

enum scm_rtl_opcode
  {
#define ENUM(opcode, tag, name, meta) scm_rtl_op_##tag = opcode,
    FOR_EACH_VM_OPERATION(ENUM)
#undef ENUM
  };

#define SCM_PACK_RTL_8_8_8(op,a,b,c) ((op) | ((a) << 8) | ((b) << 16) | ((d) << 24))
#define SCM_PACK_RTL_8_16(op,a,b) ((op) | ((a) << 8) | ((b) << 16))
#define SCM_PACK_RTL_16_8(op,a,b) ((op) | ((a) << 16) | ((b) << 24))
#define SCM_PACK_RTL_24(op,a) ((op) | ((a) << 8))

#define SCM_UNPACK_RTL_8_8_8(op,a,b,c)    \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xff;               \
      b = (op >> 16) & 0xff;              \
      c = op >> 24;                       \
    }                                     \
  while (0)

#define SCM_UNPACK_RTL_8_16(op,a,b)       \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xff;               \
      b = op >> 16;                       \
    }                                     \
  while (0)

#define SCM_UNPACK_RTL_16_8(op,a,b)       \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xffff;             \
      b = op >> 24;                       \
    }                                     \
  while (0)

#define SCM_UNPACK_RTL_12_12(op,a,b)      \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xfff;              \
      b = op >> 20;                       \
    }                                     \
  while (0)

#define SCM_UNPACK_RTL_24(op,a)           \
  do                                      \
    {                                     \
      a = op >> 8;                        \
    }                                     \
  while (0)

#define SCM_VM_NUM_INSTRUCTIONS (1<<8)
#define SCM_VM_INSTRUCTION_MASK (SCM_VM_NUM_INSTRUCTIONS-1)

enum scm_opcode {
#define VM_INSTRUCTION_TO_OPCODE 1
#include <libguile/vm-expand.h>
#include <libguile/vm-i-system.i>
#include <libguile/vm-i-scheme.i>
#include <libguile/vm-i-loader.i>
#undef VM_INSTRUCTION_TO_OPCODE
};

SCM_INTERNAL SCM scm_rtl_instruction_list (void);

SCM_API SCM scm_instruction_list (void);
SCM_API SCM scm_instruction_p (SCM obj);
SCM_API SCM scm_instruction_length (SCM inst);
SCM_API SCM scm_instruction_pops (SCM inst);
SCM_API SCM scm_instruction_pushes (SCM inst);
SCM_API SCM scm_instruction_to_opcode (SCM inst);
SCM_API SCM scm_opcode_to_instruction (SCM op);

SCM_INTERNAL void scm_bootstrap_instructions (void);
SCM_INTERNAL void scm_init_instructions (void);

#endif /* _SCM_INSTRUCTIONS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
