/* classes: h_files */

#ifndef SCM_STRUCT_H
#define SCM_STRUCT_H
/* Copyright (C) 1995,1997,1999,2000,2001 Free Software Foundation, Inc.
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
#include "libguile/print.h"



/* Number of words with negative index */
#define scm_struct_n_extra_words 4
#define scm_struct_entity_n_extra_words 6

/* These are how the initial words of a vtable are allocated. */
#define scm_struct_i_setter	-6 /* Setter */
#define scm_struct_i_procedure	-5 /* Optional procedure slot */
#define scm_struct_i_free	-4 /* Destructor */
#define scm_struct_i_ptr	-3 /* Start of block (see alloc_struct) */
#define scm_struct_i_n_words	-2 /* How many words allocated to this struct? */
#define scm_struct_i_size	-1 /* Instance size */
#define scm_struct_i_flags	-1 /* Upper 12 bits used as flags */
#define scm_vtable_index_layout  0 /* A symbol describing the physical arrangement of this type. */
#define scm_vtable_index_vcell   1 /* An opaque word, managed by the garbage collector.  */
#define scm_vtable_index_vtable  2 /* A pointer to the handle for this vtable. */
#define scm_vtable_index_printer 3 /* A printer for this struct type. */
#define scm_vtable_offset_user   4 /* Where do user fields start? */

typedef size_t (*scm_t_struct_free) (scm_t_bits * vtable, scm_t_bits * data);

#define SCM_STRUCTF_MASK   (0xFFF << 20)
#define SCM_STRUCTF_ENTITY (1L << 30) /* Indicates presence of proc slots */
#define SCM_STRUCTF_LIGHT  (1L << 31) /* Light representation
					 (no hidden words) */

/* Dirk:FIXME:: the SCM_STRUCTP predicate is also fulfilled for glocs */
#define SCM_STRUCTP(X)  		(SCM_NIMP(X) && (SCM_TYP3(X) == scm_tc3_cons_gloc))
#define SCM_STRUCT_DATA(X) 		((scm_t_bits *) SCM_CELL_WORD_1 (X))
#define SCM_STRUCT_VTABLE_DATA(X)       ((scm_t_bits *) (SCM_CELL_WORD_0 (X) - scm_tc3_cons_gloc))

#define SCM_STRUCT_LAYOUT(X) 	        (SCM_PACK (SCM_STRUCT_VTABLE_DATA (X) [scm_vtable_index_layout]))
#define SCM_SET_STRUCT_LAYOUT(X, v)     (SCM_STRUCT_VTABLE_DATA (X) [scm_vtable_index_layout] = SCM_UNPACK (v))

#define SCM_STRUCT_VTABLE(X) 	        (SCM_PACK (SCM_STRUCT_VTABLE_DATA (X) [scm_vtable_index_vtable]))
#define SCM_STRUCT_VTABLE_FLAGS(X) \
  (SCM_STRUCT_VTABLE_DATA (X) [scm_struct_i_flags])
#define SCM_STRUCT_PRINTER(X) 	        (SCM_PACK (SCM_STRUCT_VTABLE_DATA (X) [scm_vtable_index_printer]))
#define SCM_SET_STRUCT_PRINTER(x, v)\
   (SCM_STRUCT_VTABLE_DATA (x) [scm_vtable_index_printer] = SCM_UNPACK (v))
#define SCM_SET_VTABLE_DESTRUCTOR(X, D) (SCM_STRUCT_DATA (X) [scm_struct_i_free] = (scm_t_bits) (D))
/* Efficiency is important in the following macro, since it's used in GC */
#define SCM_LAYOUT_TAILP(X)		(((X) & 32) == 0) /* R, W or O */

#define SCM_STRUCT_TABLE_NAME(X) SCM_CAR (X)
#define SCM_SET_STRUCT_TABLE_NAME(X, NAME) SCM_SETCAR (X, NAME)
#define SCM_STRUCT_TABLE_CLASS(X) SCM_CDR (X)
#define SCM_SET_STRUCT_TABLE_CLASS(X, CLASS) SCM_SETCDR (X, CLASS)
extern SCM scm_struct_table;

#define SCM_STRUCT_GC_CHAIN(X) SCM_CELL_OBJECT_3 (X)
#define SCM_SET_STRUCT_GC_CHAIN(X, Y) SCM_SET_CELL_OBJECT_3 (X, Y)
extern SCM scm_structs_to_free;



extern scm_t_bits * scm_alloc_struct (int n_words, int n_extra, char * who);
extern size_t scm_struct_free_0 (scm_t_bits * vtable, scm_t_bits * data);
extern size_t scm_struct_free_light (scm_t_bits * vtable, scm_t_bits * data);
extern size_t scm_struct_free_standard (scm_t_bits * vtable, scm_t_bits * data);
extern size_t scm_struct_free_entity (scm_t_bits * vtable, scm_t_bits * data);
extern SCM scm_make_struct_layout (SCM fields);
extern SCM scm_struct_p (SCM x);
extern SCM scm_struct_vtable_p (SCM x);
extern SCM scm_make_struct (SCM vtable, SCM tail_array_size, SCM init);
extern SCM scm_make_vtable_vtable (SCM extra_fields, SCM tail_array_size, SCM init);
extern SCM scm_struct_ref (SCM handle, SCM pos);
extern SCM scm_struct_set_x (SCM handle, SCM pos, SCM val);
extern SCM scm_struct_vtable (SCM handle);
extern SCM scm_struct_vtable_tag (SCM handle);
extern unsigned long scm_struct_ihashq (SCM obj, unsigned long n);
extern SCM scm_struct_create_handle (SCM obj);
extern SCM scm_struct_vtable_name (SCM vtable);
extern SCM scm_set_struct_vtable_name_x (SCM vtable, SCM name);
extern void scm_print_struct (SCM exp, SCM port, scm_print_state *);
extern void scm_struct_prehistory (void);
extern void scm_init_struct (void);

#endif  /* SCM_STRUCT_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
