/* classes: h_files */

#ifndef SCM_GC_H
#define SCM_GC_H
/* Copyright (C) 1995,1996,1998,1999,2000,2001 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */



#include "libguile/__scm.h"

#include "libguile/hooks.h"



typedef struct scm_cell
{
  scm_t_bits word_0;
  scm_t_bits word_1;
} scm_cell;


/* SCM_CELLPTR is a pointer to a cons cell which may be compared or
 * differenced.
 */
typedef scm_cell * SCM_CELLPTR;


/* Cray machines have pointers that are incremented once for each word,
 * rather than each byte, the 3 most significant bits encode the byte
 * within the word.  The following macros deal with this by storing the
 * native Cray pointers like the ones that looks like scm expects.  This
 * is done for any pointers that might appear in the car of a scm_cell,
 * pointers to scm_vector elts, functions, &c are not munged.
 */
#ifdef _UNICOS
#  define SCM2PTR(x) ((SCM_CELLPTR) (SCM_UNPACK (x) >> 3))
#  define PTR2SCM(x) (SCM_PACK (((scm_t_bits) (x)) << 3))
#else
#  define SCM2PTR(x) ((SCM_CELLPTR) (SCM_UNPACK (x)))
#  define PTR2SCM(x) (SCM_PACK ((scm_t_bits) (x)))
#endif /* def _UNICOS */

#define SCM_GC_CARD_N_HEADER_CELLS 1
#define SCM_GC_CARD_N_CELLS        256

#define SCM_GC_CARD_SIZE           (SCM_GC_CARD_N_CELLS * sizeof (scm_cell))
#define SCM_GC_CARD_N_DATA_CELLS   (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS)

#define SCM_GC_CARD_BVEC_SIZE_IN_LIMBS \
    ((SCM_GC_CARD_N_CELLS + SCM_C_BVEC_LIMB_BITS - 1) / SCM_C_BVEC_LIMB_BITS)

#define SCM_GC_IN_CARD_HEADERP(x) \
    SCM_PTR_LT ((scm_cell *) (x), SCM_GC_CELL_CARD (x) + SCM_GC_CARD_N_HEADER_CELLS)

#define SCM_GC_CARD_BVEC(card)  ((scm_t_c_bvec_limb *) ((card)->word_0))
#define SCM_GC_SET_CARD_BVEC(card, bvec) \
    ((card)->word_0 = (scm_t_bits) (bvec))

#define SCM_GC_GET_CARD_FLAGS(card) ((long) ((card)->word_1))
#define SCM_GC_SET_CARD_FLAGS(card, flags) \
    ((card)->word_1 = (scm_t_bits) (flags))
#define SCM_GC_CLR_CARD_FLAGS(card) (SCM_GC_SET_CARD_FLAGS (card, 0L))

#define SCM_GC_GET_CARD_FLAG(card, shift) (SCM_GC_GET_CARD_FLAGS (card) & (1L << (shift)))
#define SCM_GC_SET_CARD_FLAG(card, shift) \
    (SCM_GC_SET_CARD_FLAGS (card, SCM_GC_GET_CARD_FLAGS(card) | (1L << (shift))))
#define SCM_GC_CLR_CARD_FLAG(card, shift) \
    (SCM_GC_SET_CARD_FLAGS (card, SCM_GC_GET_CARD_FLAGS(card) & ~(1L << (shift))))

#define SCM_GC_CARDF_DOUBLECELL 0

#define SCM_GC_CARD_DOUBLECELLP(card)    SCM_GC_GET_CARD_FLAG (card, SCM_GC_CARDF_DOUBLECELL)
#define SCM_GC_SET_CARD_DOUBLECELL(card) SCM_GC_SET_CARD_FLAG (card, SCM_GC_CARDF_DOUBLECELL)

/* card addressing. for efficiency, cards are *always* aligned to
   SCM_GC_CARD_SIZE. */

#define SCM_GC_CARD_SIZE_MASK  (SCM_GC_CARD_SIZE - 1)
#define SCM_GC_CARD_ADDR_MASK  (~SCM_GC_CARD_SIZE_MASK)

#define SCM_GC_CELL_CARD(x)    ((SCM_CELLPTR) ((long) (x) & SCM_GC_CARD_ADDR_MASK))
#define SCM_GC_CELL_SPAN(x)    ((SCM_GC_CARD_DOUBLECELLP (SCM_GC_CELL_CARD (x))) ? 2 : 1)
#define SCM_GC_CELL_OFFSET(x)  (((long) (x) & SCM_GC_CARD_SIZE_MASK) >> SCM_CELL_SIZE_SHIFT)
#define SCM_GC_CELL_BVEC(x)    SCM_GC_CARD_BVEC (SCM_GC_CELL_CARD (x))
#define SCM_GC_CELL_GET_BIT(x) SCM_C_BVEC_GET (SCM_GC_CELL_BVEC (x), SCM_GC_CELL_OFFSET (x))
#define SCM_GC_CELL_SET_BIT(x) SCM_C_BVEC_SET (SCM_GC_CELL_BVEC (x), SCM_GC_CELL_OFFSET (x))
#define SCM_GC_CELL_CLR_BIT(x) SCM_C_BVEC_CLR (SCM_GC_CELL_BVEC (x), SCM_GC_CELL_OFFSET (x))

#define SCM_GC_CARD_UP(x)      SCM_GC_CELL_CARD ((char *) (x) + SCM_GC_CARD_SIZE - 1)
#define SCM_GC_CARD_DOWN       SCM_GC_CELL_CARD

/* low level bit banging aids */

typedef unsigned long scm_t_c_bvec_limb;

#if (SIZEOF_LONG == 8)
#       define SCM_C_BVEC_LIMB_BITS    64
#       define SCM_C_BVEC_OFFSET_SHIFT 6
#       define SCM_C_BVEC_POS_MASK     63
#       define SCM_CELL_SIZE_SHIFT     4
#else
#       define SCM_C_BVEC_LIMB_BITS    32
#       define SCM_C_BVEC_OFFSET_SHIFT 5
#       define SCM_C_BVEC_POS_MASK     31
#       define SCM_CELL_SIZE_SHIFT     3
#endif

#define SCM_C_BVEC_OFFSET(pos) (pos >> SCM_C_BVEC_OFFSET_SHIFT)

#define SCM_C_BVEC_GET(bvec, pos) (bvec[SCM_C_BVEC_OFFSET (pos)] & (1L << (pos & SCM_C_BVEC_POS_MASK)))
#define SCM_C_BVEC_SET(bvec, pos) (bvec[SCM_C_BVEC_OFFSET (pos)] |= (1L << (pos & SCM_C_BVEC_POS_MASK)))
#define SCM_C_BVEC_CLR(bvec, pos) (bvec[SCM_C_BVEC_OFFSET (pos)] &= ~(1L << (pos & SCM_C_BVEC_POS_MASK)))

#define SCM_C_BVEC_BITS2BYTES(bits) \
    (sizeof (scm_t_c_bvec_limb) * ((((bits) & SCM_C_BVEC_POS_MASK) ? 1L : 0L) + SCM_C_BVEC_OFFSET (bits)))

#define SCM_C_BVEC_SET_BYTES(bvec, bytes)   (memset (bvec, 0xff, bytes))
#define SCM_C_BVEC_SET_ALL_BITS(bvec, bits) SCM_C_BVEC_SET_BYTES (bvec, SCM_C_BVEC_BITS2BYTES (bits))

#define SCM_C_BVEC_CLR_BYTES(bvec, bytes)   (memset (bvec, 0, bytes))
#define SCM_C_BVEC_CLR_ALL_BITS(bvec, bits) SCM_C_BVEC_CLR_BYTES (bvec, SCM_C_BVEC_BITS2BYTES (bits))

/* testing and changing GC marks */

#define SCM_GCMARKP(x)   SCM_GC_CELL_GET_BIT (x)
#define SCM_SETGCMARK(x) SCM_GC_CELL_SET_BIT (x)
#define SCM_CLRGCMARK(x) SCM_GC_CELL_CLR_BIT (x)

/* Low level cell data accessing macros:
 */

#if (SCM_DEBUG_CELL_ACCESSES == 1)
#  define SCM_VALIDATE_CELL(cell, expr) (scm_assert_cell_valid (cell), (expr))
#else
#  define SCM_VALIDATE_CELL(cell, expr) (expr)
#endif

#define SCM_CELL_WORD(x, n) \
  SCM_VALIDATE_CELL ((x), ((const scm_t_bits *) SCM2PTR (x)) [n])
#define SCM_CELL_WORD_0(x) SCM_CELL_WORD (x, 0)
#define SCM_CELL_WORD_1(x) SCM_CELL_WORD (x, 1)
#define SCM_CELL_WORD_2(x) SCM_CELL_WORD (x, 2)
#define SCM_CELL_WORD_3(x) SCM_CELL_WORD (x, 3)

#define SCM_CELL_OBJECT(x, n) \
  SCM_VALIDATE_CELL ((x), SCM_PACK (((const scm_t_bits *) SCM2PTR (x)) [n]))
#define SCM_CELL_OBJECT_0(x) SCM_CELL_OBJECT (x, 0)
#define SCM_CELL_OBJECT_1(x) SCM_CELL_OBJECT (x, 1)
#define SCM_CELL_OBJECT_2(x) SCM_CELL_OBJECT (x, 2)
#define SCM_CELL_OBJECT_3(x) SCM_CELL_OBJECT (x, 3)

#define SCM_SET_CELL_WORD(x, n, v) \
  SCM_VALIDATE_CELL ((x), ((scm_t_bits *) SCM2PTR (x)) [n] = (scm_t_bits) (v))
#define SCM_SET_CELL_WORD_0(x, v) SCM_SET_CELL_WORD (x, 0, v)
#define SCM_SET_CELL_WORD_1(x, v) SCM_SET_CELL_WORD (x, 1, v)
#define SCM_SET_CELL_WORD_2(x, v) SCM_SET_CELL_WORD (x, 2, v)
#define SCM_SET_CELL_WORD_3(x, v) SCM_SET_CELL_WORD (x, 3, v)

#define SCM_SET_CELL_OBJECT(x, n, v) \
  SCM_VALIDATE_CELL ((x), ((scm_t_bits *) SCM2PTR (x)) [n] = SCM_UNPACK (v))
#define SCM_SET_CELL_OBJECT_0(x, v) SCM_SET_CELL_OBJECT (x, 0, v)
#define SCM_SET_CELL_OBJECT_1(x, v) SCM_SET_CELL_OBJECT (x, 1, v)
#define SCM_SET_CELL_OBJECT_2(x, v) SCM_SET_CELL_OBJECT (x, 2, v)
#define SCM_SET_CELL_OBJECT_3(x, v) SCM_SET_CELL_OBJECT (x, 3, v)

#define SCM_CELL_TYPE(x) SCM_CELL_WORD_0 (x)
#define SCM_SET_CELL_TYPE(x, t) SCM_SET_CELL_WORD_0 (x, t)


/* Except for the garbage collector, no part of guile should ever run over a
 * free cell.  Thus, in debug mode the above macros report an error if they
 * are applied to a free cell.  Since the garbage collector is allowed to
 * access free cells, it needs its own way to access cells which will not
 * result in errors when in debug mode.  */

#define SCM_GC_CELL_TYPE(x) \
  (((const scm_t_bits *) SCM2PTR (x)) [0])


#define SCM_CELL_WORD_LOC(x, n) ((scm_t_bits *) & SCM_CELL_WORD (x, n))
#define SCM_CARLOC(x) ((SCM *) SCM_CELL_WORD_LOC ((x), 0))
#define SCM_CDRLOC(x) ((SCM *) SCM_CELL_WORD_LOC ((x), 1))


/* SCM_PTR_LT and friends define how to compare two SCM_CELLPTRs (which may
 * point to cells in different heap segments).
 */
#define SCM_PTR_LT(x, y) ((x) < (y))
#define SCM_PTR_GT(x, y) (SCM_PTR_LT (y, x))
#define SCM_PTR_LE(x, y) (!SCM_PTR_GT (x, y))
#define SCM_PTR_GE(x, y) (!SCM_PTR_LT (x, y))


/* Freelists consist of linked cells where the type entry holds the value
 * scm_tc_free_cell and the second entry holds a pointer to the next cell of
 * the freelist.  Due to this structure, freelist cells are not cons cells
 * and thus may not be accessed using SCM_CAR and SCM_CDR.
 */

#define SCM_FREE_CELL_P(x) \
  (!SCM_IMP (x) && (* (const scm_t_bits *) SCM2PTR (x) == scm_tc_free_cell))
#define SCM_FREE_CELL_CDR(x) \
  (SCM_PACK (((const scm_t_bits *) SCM2PTR (x)) [1]))
#define SCM_SET_FREE_CELL_CDR(x, v) \
  (((scm_t_bits *) SCM2PTR (x)) [1] = SCM_UNPACK (v))


#if (SCM_DEBUG_CELL_ACCESSES == 1)
#  define SCM_GC_SET_ALLOCATED(x) \
     (((scm_t_bits *) SCM2PTR (x)) [0] = scm_tc16_allocated)
#else
#  define SCM_GC_SET_ALLOCATED(x)
#endif

#ifdef GUILE_DEBUG_FREELIST
#define SCM_NEWCELL(_into) do { _into = scm_debug_newcell (); } while (0)
#define SCM_NEWCELL2(_into) do { _into = scm_debug_newcell2 (); } while (0)
#else
/* When we introduce POSIX threads support, every thread will have
   a freelist of its own.  */
#define SCM_NEWCELL(_into) \
        do { \
          if (SCM_NULLP (scm_freelist)) \
            { \
             _into = scm_gc_for_newcell (&scm_master_freelist, \
                                         &scm_freelist); \
             SCM_GC_SET_ALLOCATED (_into); \
            } \
          else \
            { \
               _into = scm_freelist; \
               scm_freelist = SCM_FREE_CELL_CDR (scm_freelist); \
               SCM_GC_SET_ALLOCATED (_into); \
            } \
        } while(0)
#define SCM_NEWCELL2(_into) \
        do { \
          if (SCM_NULLP (scm_freelist2)) \
            { \
             _into = scm_gc_for_newcell (&scm_master_freelist2, \
                                         &scm_freelist2); \
             SCM_GC_SET_ALLOCATED (_into); \
            } \
          else \
            { \
               _into = scm_freelist2; \
               scm_freelist2 = SCM_FREE_CELL_CDR (scm_freelist2); \
               SCM_GC_SET_ALLOCATED (_into); \
            } \
        } while(0)
#endif


#define SCM_MARKEDP    SCM_GCMARKP
#define SCM_NMARKEDP(x) (!SCM_MARKEDP (x))

#if (SCM_DEBUG_CELL_ACCESSES == 1)
extern scm_t_bits scm_tc16_allocated;
extern unsigned int scm_debug_cell_accesses_p;
#endif

extern struct scm_t_heap_seg_data *scm_heap_table;
extern size_t scm_n_heap_segs;
extern int scm_block_gc;
extern int scm_gc_heap_lock;
extern unsigned int scm_gc_running_p;


extern size_t scm_default_init_heap_size_1;
extern int scm_default_min_yield_1;
extern size_t scm_default_init_heap_size_2;
extern int scm_default_min_yield_2;
extern size_t scm_default_max_segment_size;

extern size_t scm_max_segment_size;
extern SCM_CELLPTR scm_heap_org;
extern SCM scm_freelist;
extern struct scm_t_freelist scm_master_freelist;
extern SCM scm_freelist2;
extern struct scm_t_freelist scm_master_freelist2;
extern unsigned long scm_gc_cells_collected;
extern unsigned long scm_gc_yield;
extern unsigned long scm_gc_malloc_collected;
extern unsigned long scm_gc_ports_collected;
extern unsigned long scm_cells_allocated;
extern unsigned long scm_mallocated;
extern unsigned long scm_mtrigger;

extern SCM scm_after_gc_hook;

extern scm_t_c_hook scm_before_gc_c_hook;
extern scm_t_c_hook scm_before_mark_c_hook;
extern scm_t_c_hook scm_before_sweep_c_hook;
extern scm_t_c_hook scm_after_sweep_c_hook;
extern scm_t_c_hook scm_after_gc_c_hook;

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)
extern SCM scm_map_free_list (void);
extern SCM scm_free_list_length (void);
#endif
#ifdef GUILE_DEBUG_FREELIST
extern SCM scm_debug_newcell (void);
extern SCM scm_debug_newcell2 (void);
extern SCM scm_gc_set_debug_check_freelist_x (SCM flag);
#endif



#if (SCM_DEBUG_CELL_ACCESSES == 1)
extern void scm_assert_cell_valid (SCM);
extern SCM scm_set_debug_cell_accesses_x (SCM flag);
#endif
extern SCM scm_object_address (SCM obj);
extern SCM scm_unhash_name (SCM name);
extern SCM scm_gc_stats (void);
extern SCM scm_gc (void);
extern void scm_gc_for_alloc (struct scm_t_freelist *freelist);
extern SCM scm_gc_for_newcell (struct scm_t_freelist *master, SCM *freelist);
#if 0
extern void scm_alloc_cluster (struct scm_t_freelist *master);
#endif
extern void scm_igc (const char *what);
extern void scm_gc_mark (SCM p);
extern void scm_gc_mark_dependencies (SCM p);
extern void scm_gc_mark_cell_conservatively (SCM cell);
extern void scm_mark_locations (SCM_STACKITEM x[], unsigned long n);
extern int scm_cellp (SCM value);
extern void scm_gc_sweep (void);
extern void * scm_must_malloc (size_t len, const char *what);
extern void * scm_must_realloc (void *where,
				size_t olen, size_t len,
				const char *what);
extern char *scm_must_strdup (const char *str);
extern char *scm_must_strndup (const char *str, size_t n);
extern void scm_done_malloc (long size);
extern void scm_done_free (long size);
extern void scm_must_free (void *obj);
extern void scm_remember_upto_here_1 (SCM obj);
extern void scm_remember_upto_here_2 (SCM obj1, SCM obj2);
extern void scm_remember_upto_here (SCM obj1, ...);
extern SCM scm_return_first (SCM elt, ...);
extern int scm_return_first_int (int x, ...);
extern SCM scm_permanent_object (SCM obj);
extern SCM scm_gc_protect_object (SCM obj);
extern SCM scm_gc_unprotect_object (SCM obj);
extern void scm_gc_register_root (SCM *p);
extern void scm_gc_unregister_root (SCM *p);
extern void scm_gc_register_roots (SCM *b, unsigned long n);
extern void scm_gc_unregister_roots (SCM *b, unsigned long n);
extern int scm_init_storage (void);
extern void *scm_get_stack_base (void);
extern void scm_init_gc (void);



#if (SCM_DEBUG_DEPRECATED == 0)

extern SCM scm_protect_object (SCM obj);
extern SCM scm_unprotect_object (SCM obj);

#define SCM_SETAND_CAR(x, y) \
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) & (y))))
#define SCM_SETOR_CAR(x, y)\
  (SCM_SETCAR ((x), SCM_PACK (SCM_UNPACK (SCM_CAR (x)) | (y))))
#define SCM_SETAND_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) & (y))))
#define SCM_SETOR_CDR(x, y)\
  (SCM_SETCDR ((x), SCM_PACK (SCM_UNPACK (SCM_CDR (x)) | (y))))
#define SCM_FREEP(x) (SCM_FREE_CELL_P (x))
#define SCM_NFREEP(x) (!SCM_FREE_CELL_P (x))
#define SCM_GC8MARKP(x) SCM_GCMARKP (x)
#define SCM_SETGC8MARK(x) SCM_SETGCMARK (x)
#define SCM_CLRGC8MARK(x) SCM_CLRGCMARK (x)
#define SCM_GCTYP16(x) SCM_TYP16 (x)
#define SCM_GCCDR(x) SCM_CDR (x)
extern void scm_remember (SCM * ptr);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

#endif  /* SCM_GC_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
