/* classes: h_files */

#ifndef SCM_TAGS_H
#define SCM_TAGS_H

/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



/** This file defines the format of SCM values and cons pairs.
 ** It is here that tag bits are assigned for various purposes.
 **/

/* picks up scmconfig.h too */
#include "libguile/__scm.h"

/* In the beginning was the Word:
 */
#if SCM_SIZEOF_INTPTR_T != 0 && defined(INTPTR_MAX) && defined(INTPTR_MIN)
typedef intptr_t scm_t_signed_bits;
#define SCM_T_SIGNED_BITS_MAX INTPTR_MAX
#define SCM_T_SIGNED_BITS_MIN INTPTR_MIN
#else
typedef signed long scm_t_signed_bits;
#define SCM_T_SIGNED_BITS_MAX LONG_MAX
#define SCM_T_SIGNED_BITS_MIN LONG_MIN
#endif

#if SCM_SIZEOF_UINTPTR_T != 0 && defined(UINTPTR_MAX)
typedef uintptr_t scm_t_bits;
#define SIZEOF_SCM_T_BITS SCM_SIZEOF_UINTPTR_T
#define SCM_T_BITS_MAX UINTPTR_MAX
#else
typedef unsigned long scm_t_bits;
#define SIZEOF_SCM_T_BITS SCM_SIZEOF_UNSIGNED_LONG
#define SCM_T_BITS_MAX ULONG_MAX
#endif

/* But as external interface, we use SCM, which may, according to the desired
 * level of type checking, be defined in several ways:
 */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
    typedef union { struct { scm_t_bits n; } n; } SCM;
    static SCM scm_pack(scm_t_bits b) { SCM s; s.n.n = b; return s; }
#   define SCM_UNPACK(x) ((x).n.n)
#   define SCM_PACK(x) (scm_pack ((scm_t_bits) (x)))
#elif (SCM_DEBUG_TYPING_STRICTNESS == 1)
/* This is the default, which provides an intermediate level of compile time
 * type checking while still resulting in very efficient code.
 */
    typedef struct scm_unused_struct * SCM;
#   define SCM_UNPACK(x) ((scm_t_bits) (x))
#   define SCM_PACK(x) ((SCM) (x))
#else
/* This should be used as a fall back solution for machines on which casting
 * to a pointer may lead to loss of bit information, e. g. in the three least
 * significant bits.
 */
    typedef scm_t_bits SCM;
#   define SCM_UNPACK(x) (x)
#   define SCM_PACK(x) ((scm_t_bits) (x))
#endif


/* SCM values can not be compared by using the operator ==.  Use the following
 * macro instead, which is the equivalent of the scheme predicate 'eq?'.
 */
#define SCM_EQ_P(x, y) (SCM_UNPACK (x) == SCM_UNPACK (y))



/* SCM variables can contain:
 *
 * Non-objects -- meaning that the tag-related macros don't apply to them
 * in the usual way.
 *
 * Immediates -- meaning that the variable contains an entire Scheme object.
 *
 * Non-immediates -- meaning that the variable holds a (possibly
 * tagged) pointer into the cons pair heap.
 *
 * Non-objects are distinguished from other values by careful coding
 * only (i.e., programmers must keep track of any SCM variables they
 * create that don't contain ordinary scheme values).
 *
 * All immediates and pointers to cells of non-immediates have a 0 in
 * bit 0.  All non-immediates that are not pairs have a 1 in bit 0 of
 * the first word of their cell.  This is how pairs are distinguished
 * from other non-immediates; a pair can have a immediate in its car
 * (thus a 0 in bit 0), or a pointer to the cell of a non-immediate
 * (again, this pointer has a 0 in bit 0).
 *
 * Immediates and non-immediates are distinguished by bits 1 and 2.
 * Immediate values must have a 1 in at least one of those bits.
 * Consequently, a pointer to a cell of a non-immediate must have
 * zeros in bits 1 and 2.  Together with the requirement from above
 * that bit 0 must also be zero, this means that pointers to cells of
 * non-immediates must have their three low bits all zero.  This in
 * turn means that cells must be aligned on a 8 byte boundary, which
 * is just right for two 32bit numbers (surprise, surprise).  Does
 * this (or any other detail of tagging) seem arbitrary?  Try changing
 * it!  (Not always impossible but it is fair to say that many details
 * of tags are mutually dependent).  */

#define SCM_IMP(x) 		(6 & SCM_UNPACK (x))
#define SCM_NIMP(x) 		(!SCM_IMP (x))

/* Here is a summary of tagging in SCM values as they might occur in
 * SCM variables or in the heap.
 *
 * low bits    meaning
 *
 *
 * 0		Most objects except...
 * 1 		... structs (this tag is valid only in the header
 *              of a struct's data, as with all odd tags).
 *
 * 00		heap addresses and many immediates (not integers)
 * 01		structs, some tc7_ codes
 * 10		immediate integers
 * 11		various tc7_ codes including, tc16_ codes.
 *
 *
 * 000		heap address
 * 001		structs
 * 010		integer
 * 011		closure
 * 100		immediates
 * 101		tc7_
 * 110 		integer
 * 111		tc7_
 *
 *
 * 100 --- IMMEDIATES
 *
 * Looking at the seven final bits of an immediate:
 *
 * 0000-100	short instruction
 * 0001-100	short instruction
 * 0010-100	short instruction
 * 0011-100	short instruction
 * 0100-100	short instruction
 * 0101-100	short instruction
 * 0110-100	short instruction
 * 0111-100	short instruction
 * 1000-100	short instruction
 * 1001-100	short instruction
 * 1010-100	short instruction
 * 1011-100	short instruction
 * 1100-100	short instruction
 * 1101-100	short instruction
 * 1110-100	immediate characters, various immediates and long instructions
 * 1111-100	ilocs
 *
 * Some of the 1110100 immediates are long instructions (they dispatch in
 * three steps compared to one step for a short instruction).  The three steps
 * are, (1) dispatch on 7 bits to the long instruction handler, (2) check, if
 * the immediate indicates a long instruction (rather than a character or
 * other immediate) (3) dispatch on the additional bits.
 *
 * One way to think of it is that there are 128 short instructions,
 * with the 13 immediates above being some of the most interesting.
 *
 * Also noteworthy are the groups of 16 7-bit instructions implied by
 * some of the 3-bit tags.  For example, closure references consist of
 * an 8-byte aligned address tagged with 011.  There are 16 identical
 * 7-bit instructions, all ending 011, which are invoked by evaluating
 * closures.
 *
 * In other words, if you hand the evaluator a closure, the evaluator
 * treats the closure as a graph of virtual machine instructions.  A
 * closure is a pair with a pointer to the body of the procedure in
 * the CDR and a pointer to the environment of the closure in the CAR.
 * The environment pointer is tagged 011 which implies that the least
 * significant 7 bits of the environment pointer also happen to be a
 * virtual machine instruction we could call "SELF" (for
 * self-evaluating object).
 *
 * A less trivial example are the 16 instructions ending 000.  If
 * those bits tag the CAR of a pair, then evidently the pair is an
 * ordinary cons pair and should be evaluated as a procedure
 * application.  The sixteen, 7-bit 000 instructions are all
 * "NORMAL-APPLY" (Things get trickier.  For example, if the CAR of a
 * procedure application is a symbol, the NORMAL-APPLY instruction
 * will, as a side effect, overwrite that CAR with a new instruction
 * that contains a cached address for the variable named by the
 * symbol.)
 *
 * Here is a summary of tags in the CAR of a non-immediate:
 *
 * cons	   ..........SCM car..............0  ...........SCM cdr.............0
 * struct  ..........void * type........001  ...........void * data.........0
 * closure ..........SCM code...........011  ...........SCM env.............0
 * tc7	   ......24.bits of data...0xxxx1S1  ..........void *data............
 *
 *
 *
 * 101 & 111 --- tc7_ types
 *
 *		tc7_tags are 7 bit tags ending in 1x1.  These tags
 *		occur only in the CAR of heap cells, and have the
 *		handy property that all bits of the CAR above the
 *		bottom eight can be used to store some data, thus
 *		saving a word in the body itself.  Thus, we use them
 *		for strings and vectors (among other things).
 *
 *		TYP7(X) returns bits 0...6 of CELL_TYPE (X)
 *
 *              Sometimes we choose the bottom seven bits carefully,
 *              so that the 2-valued bit (called S bit) can be masked
 *              off to reveal a common type.
 *
 *		TYP7S(X) returns TYP7, but masking out the option bit S.
 *
 *		Some TC7 types are subdivided into 256 subtypes giving
 *		rise to the macros:
 *
 *		TYP16
 *		TYP16S
 *
 *		TYP16S functions similarly wrt to TYP16 as TYP7S to TYP7,
 *		but a different option bit is used (bit 2 for TYP7S,
 *		bit 8 for TYP16S).
 */


/* {Non-immediate values.}
 *
 * If X is non-immediate, it is necessary to look at SCM_CAR (X) to
 * figure out Xs type.  X may be a cons pair, in which case the value
 * SCM_CAR (x) will be either an immediate or non-immediate value.  X
 * may be something other than a cons pair, in which case the value
 * SCM_CAR (x) will be a non-object value.
 *
 * All immediates and non-immediates have a 0 in bit 0.  We
 * additionally preserve the invariant that all non-object values
 * stored in the SCM_CAR of a non-immediate object have a 1 in bit 1:
 */

#define SCM_CONSP(x)  (!SCM_IMP (x) && ((1 & SCM_CELL_TYPE (x)) == 0))
#define SCM_NCONSP(x) (!SCM_CONSP (x))



/* See numbers.h for macros relating to immediate integers.
 */

#define scm_tc2_int              2

#define SCM_ITAG3(x) 		 (7 & SCM_UNPACK (x))
#define SCM_TYP3(x) 		 (7 & SCM_CELL_TYPE (x))
#define scm_tc3_cons	 	 0
#define scm_tc3_struct    	 1
#define scm_tc3_int_1		 (scm_tc2_int + 0)
#define scm_tc3_closure		 3
#define scm_tc3_imm24		 4
#define scm_tc3_tc7_1		 5
#define scm_tc3_int_2		 (scm_tc2_int + 4)
#define scm_tc3_tc7_2		 7


/*
 * Do not change the three bit tags.
 */


#define SCM_ITAG7(x) 		(127 & SCM_UNPACK (x))
#define SCM_TYP7(x) 		(0x7f &        SCM_CELL_TYPE (x))
#define SCM_TYP7S(x) 		((0x7f & ~2) & SCM_CELL_TYPE (x))


#define SCM_TYP16(x) 		(0xffff & SCM_CELL_TYPE (x))
#define SCM_TYP16S(x) 		(0xfeff & SCM_CELL_TYPE (x))

#define SCM_TYP16_PREDICATE(tag, x) (!SCM_IMP (x) && SCM_TYP16 (x) == (tag))



#define scm_tc7_symbol		5
#define scm_tc7_variable        7

/* couple */
#define scm_tc7_vector		13
#define scm_tc7_wvect		15

#define scm_tc7_string		21
/* free                         23 */

/* Many of the following should be turned
 * into structs or smobs.  We need back some
 * of these 7 bit tags!
 */
#define scm_tc7_pws		31

#if SCM_HAVE_ARRAYS
#define scm_tc7_llvect          29
#define scm_tc7_uvect		37
/* free                         39 */
#define scm_tc7_fvect		45
#define scm_tc7_dvect		47
#define scm_tc7_cvect		53
#define scm_tc7_svect		55
#define scm_tc7_bvect		71
#define scm_tc7_byvect		77
#define scm_tc7_ivect		79
#endif

#define scm_tc7_dsubr		61
#define scm_tc7_cclo		63
#define scm_tc7_rpsubr		69
#define scm_tc7_subr_0		85
#define scm_tc7_subr_1		87
#define scm_tc7_cxr		93
#define scm_tc7_subr_3		95
#define scm_tc7_subr_2		101
#define scm_tc7_asubr		103
#define scm_tc7_subr_1o		109
#define scm_tc7_subr_2o		111
#define scm_tc7_lsubr_2		117
#define scm_tc7_lsubr		119


/* There are 256 port subtypes.
 */
#define scm_tc7_port		125


/* There are 256 smob subtypes.  [**] If you change scm_tc7_smob, you must
 * also change the places it is hard coded in this file and possibly others.
 * Dirk:FIXME:: Any hard coded reference to scm_tc7_smob must be replaced by a
 * symbolic reference.
 */
#define scm_tc7_smob		127 /* DO NOT CHANGE [**] */


/* Here are the first four smob subtypes.
 */

/* scm_tc_free_cell is the 0th smob type.  We place this in free cells to tell
 * the conservative marker not to trace it.
 */
#define scm_tc_free_cell	(scm_tc7_smob + 0 * 256L)

/* Smob type 1 to 3 (note the dependency on the predicate SCM_NUMP)
 */
#define scm_tc16_big		(scm_tc7_smob + 1 * 256L)
#define scm_tc16_real           (scm_tc7_smob + 2 * 256L)
#define scm_tc16_complex        (scm_tc7_smob + 3 * 256L)


/* {Immediate Values}
 */

enum scm_tags
{
  scm_tc8_char = 0xf4,
  scm_tc8_iloc = 0xfc
};

#define SCM_ITAG8(X)		(SCM_UNPACK (X) & 0xff)
#define SCM_MAKE_ITAG8(X, TAG)	SCM_PACK (((X) << 8) + TAG)
#define SCM_ITAG8_DATA(X)	(SCM_UNPACK (X) >> 8)



/* Immediate Symbols, Special Symbols, Flags (various constants).
 */

/* SCM_ISYMP tests for ISPCSYM and ISYM */
#define SCM_ISYMP(n) 		((0x187 & SCM_UNPACK (n)) == 4)

/* SCM_IFLAGP tests for ISPCSYM, ISYM and IFLAG */
#define SCM_IFLAGP(n) 		((0x87 & SCM_UNPACK (n)) == 4)
#define SCM_ISYMNUM(n) 		(SCM_UNPACK (n) >> 9)
#define SCM_ISYMCHARS(n) 	(scm_isymnames[SCM_ISYMNUM (n)])
#define SCM_MAKSPCSYM(n) 	SCM_PACK (((n) << 9) + ((n) << 3) + 4L)
#define SCM_MAKISYM(n) 		SCM_PACK (((n) << 9) + 0x74L)
#define SCM_MAKIFLAG(n) 	SCM_PACK (((n) << 9) + 0x174L)

SCM_API char *scm_isymnames[];   /* defined in print.c */

/* This table must agree with the declarations
 * in repl.c: {Names of immediate symbols}.
 *
 * These are used only in eval but their values
 * have to be allocated here.
 */

#define SCM_IM_AND		SCM_MAKSPCSYM (0)
#define SCM_IM_BEGIN		SCM_MAKSPCSYM (1)
#define SCM_IM_CASE		SCM_MAKSPCSYM (2)
#define SCM_IM_COND		SCM_MAKSPCSYM (3)
#define SCM_IM_DO		SCM_MAKSPCSYM (4)
#define SCM_IM_IF		SCM_MAKSPCSYM (5)
#define SCM_IM_LAMBDA		SCM_MAKSPCSYM (6)
#define SCM_IM_LET		SCM_MAKSPCSYM (7)
#define SCM_IM_LETSTAR		SCM_MAKSPCSYM (8)
#define SCM_IM_LETREC		SCM_MAKSPCSYM (9)
#define SCM_IM_OR		SCM_MAKSPCSYM (10)
#define SCM_IM_QUOTE		SCM_MAKSPCSYM (11)
#define SCM_IM_SET_X		SCM_MAKSPCSYM (12)
#define SCM_IM_DEFINE           SCM_MAKSPCSYM (13)
#define SCM_IM_APPLY		SCM_MAKISYM (14)
#define SCM_IM_CONT		SCM_MAKISYM (15)
#define SCM_BOOL_F		SCM_MAKIFLAG (16)
#define SCM_BOOL_T 		SCM_MAKIFLAG (17)
#define SCM_UNDEFINED	 	SCM_MAKIFLAG (18)
#define SCM_EOF_VAL 		SCM_MAKIFLAG (19)
#define SCM_EOL			SCM_MAKIFLAG (20)
#define SCM_UNSPECIFIED		SCM_MAKIFLAG (21)
#define SCM_IM_DISPATCH		SCM_MAKISYM (22)
#define SCM_IM_SLOT_REF		SCM_MAKISYM (23)
#define SCM_IM_SLOT_SET_X	SCM_MAKISYM (24)

/* Multi-language support */

#define SCM_IM_NIL_COND		SCM_MAKISYM (25)
#define SCM_IM_BIND		SCM_MAKISYM (26)

#define SCM_IM_DELAY		SCM_MAKISYM (27)
#define SCM_IM_FUTURE		SCM_MAKISYM (28)
#define SCM_IM_CALL_WITH_VALUES SCM_MAKISYM (29)

/* When a variable is unbound this is marked by the SCM_UNDEFINED
 * value.  The following is an unbound value which can be handled on
 * the Scheme level, i.e., it can be stored in and retrieved from a
 * Scheme variable.  This value is only intended to mark an unbound
 * slot in GOOPS.  It is needed now, but we should probably rewrite
 * the code which handles this value in C so that SCM_UNDEFINED can be
 * used instead.  It is not ideal to let this kind of unique and
 * strange values loose on the Scheme level.
 */
#define SCM_UNBOUND		SCM_MAKIFLAG (30)

#define SCM_UNBNDP(x)		(SCM_EQ_P ((x), SCM_UNDEFINED))

/* The Elisp nil value. */
#define SCM_ELISP_NIL		SCM_MAKIFLAG (31)



/* Dispatching aids:

   When switching on SCM_TYP7 of a SCM value, use these fake case
   labels to catch types that use fewer than 7 bits for tagging.  */

/* For cons pairs with immediate values in the CAR
 */

#define scm_tcs_cons_imcar \
       scm_tc2_int + 0:   case scm_tc2_int + 4:   case scm_tc3_imm24 + 0:\
  case scm_tc2_int + 8:   case scm_tc2_int + 12:  case scm_tc3_imm24 + 8:\
  case scm_tc2_int + 16:  case scm_tc2_int + 20:  case scm_tc3_imm24 + 16:\
  case scm_tc2_int + 24:  case scm_tc2_int + 28:  case scm_tc3_imm24 + 24:\
  case scm_tc2_int + 32:  case scm_tc2_int + 36:  case scm_tc3_imm24 + 32:\
  case scm_tc2_int + 40:  case scm_tc2_int + 44:  case scm_tc3_imm24 + 40:\
  case scm_tc2_int + 48:  case scm_tc2_int + 52:  case scm_tc3_imm24 + 48:\
  case scm_tc2_int + 56:  case scm_tc2_int + 60:  case scm_tc3_imm24 + 56:\
  case scm_tc2_int + 64:  case scm_tc2_int + 68:  case scm_tc3_imm24 + 64:\
  case scm_tc2_int + 72:  case scm_tc2_int + 76:  case scm_tc3_imm24 + 72:\
  case scm_tc2_int + 80:  case scm_tc2_int + 84:  case scm_tc3_imm24 + 80:\
  case scm_tc2_int + 88:  case scm_tc2_int + 92:  case scm_tc3_imm24 + 88:\
  case scm_tc2_int + 96:  case scm_tc2_int + 100: case scm_tc3_imm24 + 96:\
  case scm_tc2_int + 104: case scm_tc2_int + 108: case scm_tc3_imm24 + 104:\
  case scm_tc2_int + 112: case scm_tc2_int + 116: case scm_tc3_imm24 + 112:\
  case scm_tc2_int + 120: case scm_tc2_int + 124: case scm_tc3_imm24 + 120

/* For cons pairs with non-immediate values in the SCM_CAR
 */
#define scm_tcs_cons_nimcar \
       scm_tc3_cons + 0:\
  case scm_tc3_cons + 8:\
  case scm_tc3_cons + 16:\
  case scm_tc3_cons + 24:\
  case scm_tc3_cons + 32:\
  case scm_tc3_cons + 40:\
  case scm_tc3_cons + 48:\
  case scm_tc3_cons + 56:\
  case scm_tc3_cons + 64:\
  case scm_tc3_cons + 72:\
  case scm_tc3_cons + 80:\
  case scm_tc3_cons + 88:\
  case scm_tc3_cons + 96:\
  case scm_tc3_cons + 104:\
  case scm_tc3_cons + 112:\
  case scm_tc3_cons + 120

/* For structs
 */
#define scm_tcs_struct \
       scm_tc3_struct + 0:\
  case scm_tc3_struct + 8:\
  case scm_tc3_struct + 16:\
  case scm_tc3_struct + 24:\
  case scm_tc3_struct + 32:\
  case scm_tc3_struct + 40:\
  case scm_tc3_struct + 48:\
  case scm_tc3_struct + 56:\
  case scm_tc3_struct + 64:\
  case scm_tc3_struct + 72:\
  case scm_tc3_struct + 80:\
  case scm_tc3_struct + 88:\
  case scm_tc3_struct + 96:\
  case scm_tc3_struct + 104:\
  case scm_tc3_struct + 112:\
  case scm_tc3_struct + 120

/* For closures
 */
#define scm_tcs_closures \
       scm_tc3_closure + 0:\
  case scm_tc3_closure + 8:\
  case scm_tc3_closure + 16:\
  case scm_tc3_closure + 24:\
  case scm_tc3_closure + 32:\
  case scm_tc3_closure + 40:\
  case scm_tc3_closure + 48:\
  case scm_tc3_closure + 56:\
  case scm_tc3_closure + 64:\
  case scm_tc3_closure + 72:\
  case scm_tc3_closure + 80:\
  case scm_tc3_closure + 88:\
  case scm_tc3_closure + 96:\
  case scm_tc3_closure + 104:\
  case scm_tc3_closure + 112:\
  case scm_tc3_closure + 120

/* For subrs
 */
#define scm_tcs_subrs \
       scm_tc7_asubr:\
  case scm_tc7_subr_0:\
  case scm_tc7_subr_1:\
  case scm_tc7_dsubr:\
  case scm_tc7_cxr:\
  case scm_tc7_subr_3:\
  case scm_tc7_subr_2:\
  case scm_tc7_rpsubr:\
  case scm_tc7_subr_1o:\
  case scm_tc7_subr_2o:\
  case scm_tc7_lsubr_2:\
  case scm_tc7_lsubr



#if (SCM_ENABLE_DEPRECATED == 1)

#define SCM_CELLP(x) 	(((sizeof (scm_t_cell) - 1) & SCM_UNPACK (x)) == 0)
#define SCM_NCELLP(x) 	(!SCM_CELLP (x))

#endif

#endif  /* SCM_TAGS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
