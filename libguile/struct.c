/*	Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "genio.h"
#include "eval.h"
#include "alist.h"
#include "weaks.h"
#include "hashtab.h"

#include "scm_validate.h"
#include "struct.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



static SCM required_vtable_fields = SCM_BOOL_F;
SCM scm_struct_table;


SCM_DEFINE (scm_make_struct_layout, "make-struct-layout", 1, 0, 0, 
            (SCM fields),
	    "Return a new structure layout object.\n\n"
	    "@var{fields} must be a read-only string made up of pairs of characters\n"
	    "strung together.  The first character of each pair describes a field\n"
	    "type, the second a field protection.  Allowed types are 'p' for\n"
	    "GC-protected Scheme data, 'u' for unprotected binary data, and 's' for\n"
	    "fields that should point to the structure itself.    Allowed protections\n"
	    "are 'w' for mutable fields, 'r' for read-only fields, and 'o' for opaque \n"
	    "fields.  The last field protection specification may be capitalized to\n"
	    "indicate that the field is a tail-array.")
#define FUNC_NAME s_scm_make_struct_layout
{
  SCM new_sym;
  SCM_VALIDATE_ROSTRING (1,fields);
  { /* scope */
    char * field_desc;
    int len;
    int x;

    len = SCM_ROLENGTH (fields);
    field_desc = SCM_ROCHARS (fields);
    SCM_ASSERT (!(len & 1), fields, "odd length field specification", FUNC_NAME);

    for (x = 0; x < len; x += 2)
      {
	switch (field_desc[x])
	  {
	  case 'u':
	  case 'p':
#if 0
	  case 'i':
	  case 'd':
#endif
	  case 's':
	    break;
	  default:
	    SCM_ASSERT (0, SCM_MAKE_CHAR (field_desc[x]) , "unrecognized field type", FUNC_NAME);
	  }

	switch (field_desc[x + 1])
	  {
	  case 'w':
	    SCM_ASSERT (field_desc[x] != 's', SCM_MAKE_CHAR (field_desc[x + 1]),
			"self fields not writable", FUNC_NAME);
	      
	  case 'r':
	  case 'o':
	    break;
	  case 'R':
	  case 'W':
	  case 'O':
	    SCM_ASSERT (field_desc[x] != 's', SCM_MAKE_CHAR (field_desc[x + 1]),
			"self fields not allowed in tail array",
                        FUNC_NAME);
	    SCM_ASSERT (x == len - 2, SCM_MAKE_CHAR (field_desc[x + 1]),
			"tail array field must be last field in layout",
                        FUNC_NAME);
	    break;
	  default:
	    SCM_ASSERT (0, SCM_MAKE_CHAR (field_desc[x]) , "unrecognized ref specification", FUNC_NAME);
	  }
#if 0
	if (field_desc[x] == 'd')
	  {
	    SCM_ASSERT (field_desc[x + 2] == '-', SCM_MAKINUM (x / 2), "missing dash field", FUNC_NAME);
	    x += 2;
	    goto recheck_ref;
	  }
#endif
      }
    new_sym = SCM_CAR (scm_intern_obarray (field_desc, len, SCM_BOOL_F));
  }
  return scm_return_first (new_sym, fields);
}
#undef FUNC_NAME





void
scm_struct_init (SCM handle, int tail_elts, SCM inits)
{
  SCM layout;
  SCM * data;
  unsigned char * fields_desc;
  unsigned char prot = 0;
  int n_fields;
  SCM * mem;
  int tailp = 0;
  
  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  fields_desc = (unsigned char *) SCM_CHARS (layout) - 2;
  n_fields = SCM_LENGTH (layout) / 2;
  mem = SCM_STRUCT_DATA (handle);
  while (n_fields)
    {
      if (!tailp)
	{
	  fields_desc += 2;
	  prot = fields_desc[1];
	  if (SCM_LAYOUT_TAILP (prot))
	    {
	      tailp = 1;
	      prot = prot == 'R' ? 'r' : prot == 'W' ? 'w' : 'o';
	      *mem++ = tail_elts;
	      n_fields += tail_elts - 1;
	      if (n_fields == 0)
		break;
	    }
	}
      
      switch (*fields_desc)
	{
#if 0
	case 'i':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = 0;
	  else
	    {
	      *mem = scm_num2long (SCM_CAR (inits), SCM_ARGn, "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  break;
#endif

	case 'u':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = 0;
	  else
	    {
	      *mem = scm_num2ulong (SCM_CAR (inits), SCM_ARGn, "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  break;

	case 'p':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *mem = SCM_BOOL_F;
	  else
	    {
	      *mem = SCM_CAR (inits);
	      inits = SCM_CDR (inits);
	    }
	      
	  break;

#if 0
	case 'd':
	  if ((prot != 'r' && prot != 'w') || inits == SCM_EOL)
	    *((double *)mem) = 0.0;
	  else
	    {
	      *mem = scm_num2dbl (SCM_CAR (inits), "scm_struct_init");
	      inits = SCM_CDR (inits);
	    }
	  fields_desc += 2;
	  break;
#endif

	case 's':
	  *mem = handle;
	  break;
	}

      n_fields--;
      mem++;
    }
}


SCM_DEFINE (scm_struct_p, "struct?", 1, 0, 0, 
            (SCM x),
	    "Return #t iff @var{obj} is a structure object, else #f.")
#define FUNC_NAME s_scm_struct_p
{
  return SCM_BOOL(SCM_STRUCTP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_struct_vtable_p, "struct-vtable?", 1, 0, 0, 
            (SCM x),
	    "Return #t iff obj is a vtable structure.")
#define FUNC_NAME s_scm_struct_vtable_p
{
  SCM layout;
  SCM * mem;

  if (SCM_IMP (x))
    return SCM_BOOL_F;

  if (!SCM_STRUCTP (x))
    return SCM_BOOL_F;

  layout = SCM_STRUCT_LAYOUT (x);

  if (SCM_LENGTH (layout) < SCM_LENGTH (required_vtable_fields))
    return SCM_BOOL_F;

  if (strncmp (SCM_CHARS (layout), SCM_CHARS (required_vtable_fields),
	       SCM_LENGTH (required_vtable_fields)))
    return SCM_BOOL_F;

  mem = SCM_STRUCT_DATA (x);

  if (mem[1] != 0)
    return SCM_BOOL_F;

  if (SCM_IMP (mem[0]))
    return SCM_BOOL_F;

  return SCM_BOOL(SCM_SYMBOLP (mem[0]));
}
#undef FUNC_NAME


/* All struct data must be allocated at an address whose bottom three
   bits are zero.  This is because the tag for a struct lives in the
   bottom three bits of the struct's car, and the upper bits point to
   the data of its vtable, which is a struct itself.  Thus, if the
   address of that data doesn't end in three zeros, tagging it will
   destroy the pointer.

   This function allocates a block of memory, and returns a pointer at
   least scm_struct_n_extra_words words into the block.  Furthermore,
   it guarantees that that pointer's least three significant bits are
   all zero.

   The argument n_words should be the number of words that should
   appear after the returned address.  (That is, it shouldn't include
   scm_struct_n_extra_words.)

   This function initializes the following fields of the struct:

     scm_struct_i_ptr --- the actual start of the block of memory; the
	 address you should pass to 'free' to dispose of the block.
	 This field allows us to both guarantee that the returned
	 address is divisible by eight, and allow the GC to free the
	 block.

     scm_struct_i_n_words --- the number of words allocated to the
         block, including the extra fields.  This is used by the GC.

     Ugh.  */


SCM *
scm_alloc_struct (int n_words, int n_extra, char *who)
{
  int size = sizeof (SCM) * (n_words + n_extra) + 7;
  SCM *block = (SCM *) scm_must_malloc (size, who);

  /* Adjust the pointer to hide the extra words.  */
  SCM *p = block + n_extra;

  /* Adjust it even further so it's aligned on an eight-byte boundary.  */
  p = (SCM *) (((SCM) p + 7) & ~7);

  /* Initialize a few fields as described above.  */
  p[scm_struct_i_free] = (SCM) scm_struct_free_standard;
  p[scm_struct_i_ptr] = (SCM) block;
  p[scm_struct_i_n_words] = (SCM) n_words;
  p[scm_struct_i_flags] = 0;

  return p;
}

scm_sizet
scm_struct_free_0 (SCM *vtable, SCM *data)
{
  return 0;
}

scm_sizet
scm_struct_free_light (SCM *vtable, SCM *data)
{
  free (data);
  return vtable[scm_struct_i_size] & ~SCM_STRUCTF_MASK;
}

scm_sizet
scm_struct_free_standard (SCM *vtable, SCM *data)
{
  size_t n = ((data[scm_struct_i_n_words] + scm_struct_n_extra_words)
	      * sizeof (SCM) + 7);
  free ((void *) data[scm_struct_i_ptr]);
  return n;
}

scm_sizet
scm_struct_free_entity (SCM *vtable, SCM *data)
{
  size_t n = ((data[scm_struct_i_n_words] + scm_struct_entity_n_extra_words)
	      * sizeof (SCM) + 7);
  free ((void *) data[scm_struct_i_ptr]);
  return n;
}

SCM_DEFINE (scm_make_struct, "make-struct", 2, 0, 1, 
            (SCM vtable, SCM tail_array_size, SCM init),
	    "Create a new structure.\n\n"
	    "@var{type} must be a vtable structure (@xref{Vtables}).\n\n"
	    "@var{tail-elts} must be a non-negative integer.  If the layout\n"
	    "specification indicated by @var{type} includes a tail-array,\n"
	    "this is the number of elements allocated to that array.\n\n"
	    "The @var{inits} are optional arguments describing how successive fields\n"
	    "of the structure should be initialized.  Only fields with protection 'r'\n"
	    "or 'w' can be initialized -- fields of protection 's' are automatically\n"
	    "initialized to point to the new structure itself;  fields of protection 'o'\n"
	    "can not be initialized by Scheme programs.")
#define FUNC_NAME s_scm_make_struct
{
  SCM layout;
  int basic_size;
  int tail_elts;
  SCM * data;
  SCM handle;

  SCM_VALIDATE_VTABLE (1,vtable);
  SCM_VALIDATE_INUM (2,tail_array_size);

  layout = SCM_STRUCT_DATA (vtable)[scm_vtable_index_layout];
  basic_size = SCM_LENGTH (layout) / 2;
  tail_elts = SCM_INUM (tail_array_size);
  SCM_NEWCELL (handle);
  SCM_DEFER_INTS;
  if (SCM_STRUCT_DATA (vtable)[scm_struct_i_flags] & SCM_STRUCTF_ENTITY)
    {
      data = scm_alloc_struct (basic_size + tail_elts,
			       scm_struct_entity_n_extra_words,
			       "make-struct");
      data[scm_struct_i_procedure] = SCM_BOOL_F;
      data[scm_struct_i_setter] = SCM_BOOL_F;
    }
  else
    data = scm_alloc_struct (basic_size + tail_elts,
			     scm_struct_n_extra_words,
			     "make-struct");
  SCM_SETCDR (handle, data);
  SCM_SETCAR (handle, ((SCM)SCM_STRUCT_DATA (vtable)) + scm_tc3_cons_gloc);
  scm_struct_init (handle, tail_elts, init);
  SCM_ALLOW_INTS;
  return handle;
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_vtable_vtable, "make-vtable-vtable", 2, 0, 1,
            (SCM extra_fields, SCM tail_array_size, SCM init),
	    "Return a new, self-describing vtable structure.\n\n"
	    "@var{new-fields} is a layout specification describing fields\n"
	    "of the resulting structure beginning at the position bound to\n"
	    "@code{vtable-offset-user}.\n\n"
	    "@var{tail-size} specifies the size of the tail-array (if any) of\n"
	    "this vtable.\n\n"
	    "@var{inits} initializes the fields of the vtable.  Minimally, one\n"
	    "initializer must be provided: the layout specification for instances\n"
	    "of the type this vtable will describe.  If a second initializer is\n"
	    "provided, it will be interpreted as a print call-back function.\n\n"
	    "@example\n"
	    ";;; loading ,a...\n"
	    "(define x\n"
	    "  (make-vtable-vtable (make-struct-layout (quote pw))\n"
	    "                      0\n"
	    "                      'foo))\n\n"
	    "(struct? x)\n"
	    "@result{} #t\n"
	    "(struct-vtable? x)\n"
	    "@result{} #t\n"
	    "(eq? x (struct-vtable x))\n"
	    "@result{} #t\n"
	    "(struct-ref x vtable-offset-user)\n"
	    "@result{} foo\n"
	    "(struct-ref x 0)\n"
	    "@result{} pruosrpwpw\n\n\n"
	    "(define y\n"
	    "  (make-struct x\n"
	    "               0\n"
	    "               (make-struct-layout (quote pwpwpw))\n"
	    "               'bar))\n\n"
	    "(struct? y)\n"
	    "@result{} #t\n"
	    "(struct-vtable? y)\n"
	    "@result{} #t\n"
	    "(eq? x y)\n"
	    "@result{} ()\n"
	    "(eq? x (struct-vtable y))\n"
	    "@result{} #t\n"
	    "(struct-ref y 0)\n"
	    "@result{} pwpwpw\n"
	    "(struct-ref y vtable-offset-user)\n"
	    "@result{} bar\n\n\n"
	    "(define z (make-struct y 0 'a 'b 'c))\n\n"
	    "(struct? z)\n"
	    "@result{} #t\n"
	    "(struct-vtable? z)\n"
	    "@result{} ()\n"
	    "(eq? y (struct-vtable z))\n"
	    "@result{} #t\n"
	    "(map (lambda (n) (struct-ref z n)) '(0 1 2))\n"
	    "@result{} (a b c)\n"
	    "@end example\n"
	    "")
#define FUNC_NAME s_scm_make_vtable_vtable
{
  SCM fields;
  SCM layout;
  int basic_size;
  int tail_elts;
  SCM * data;
  SCM handle;

  SCM_VALIDATE_ROSTRING (1,extra_fields);
  SCM_VALIDATE_INUM (2,tail_array_size);

  fields = scm_string_append (scm_listify (required_vtable_fields,
					   extra_fields,
					   SCM_UNDEFINED));
  layout = scm_make_struct_layout (fields);
  basic_size = SCM_LENGTH (layout) / 2;
  tail_elts = SCM_INUM (tail_array_size);
  SCM_NEWCELL (handle);
  SCM_DEFER_INTS;
  data = scm_alloc_struct (basic_size + tail_elts,
			   scm_struct_n_extra_words,
			   "make-vtable-vtable");
  SCM_SETCDR (handle, data);
  SCM_SETCAR (handle, ((SCM)data) + scm_tc3_cons_gloc);
  SCM_STRUCT_LAYOUT (handle) = layout;
  scm_struct_init (handle, tail_elts, scm_cons (layout, init));
  SCM_ALLOW_INTS;
  return handle;
}
#undef FUNC_NAME




SCM_DEFINE (scm_struct_ref, "struct-ref", 2, 0, 0,
            (SCM handle, SCM pos),
	    "@deffnx primitive struct-set! struct n value\n"
	    "Access (or modify) the @var{n}th field of @var{struct}.\n\n"
	    "If the field is of type 'p', then it can be set to an arbitrary value.\n\n"
	    "If the field is of type 'u', then it can only be set to a non-negative\n"
	    "integer value small enough to fit in one machine word.")
#define FUNC_NAME s_scm_struct_ref
{
  SCM answer = SCM_UNDEFINED;
  SCM * data;
  SCM layout;
  int p;
  int n_fields;
  unsigned char * fields_desc;
  unsigned char field_type = 0;
  

  SCM_VALIDATE_STRUCT (1,handle);
  SCM_VALIDATE_INUM (2,pos);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = SCM_INUM (pos);

  fields_desc = (unsigned char *) SCM_CHARS (layout);
  n_fields = data[scm_struct_i_n_words];
  
  SCM_ASSERT_RANGE(1,pos, p < n_fields);

  if (p * 2 < SCM_LENGTH (layout))
    {
      unsigned char ref;
      field_type = fields_desc[p * 2];
      ref = fields_desc[p * 2 + 1];
      if ((ref != 'r') && (ref != 'w'))
	{
	  if ((ref == 'R') || (ref == 'W'))
	    field_type = 'u';
	  else
	    SCM_ASSERT (0, pos, "ref denied", FUNC_NAME);
	}
    }
  else if (fields_desc[SCM_LENGTH (layout) - 1] != 'O')    
    field_type = fields_desc[SCM_LENGTH (layout) - 2];
  else
    {
      SCM_ASSERT (0, pos, "ref denied", FUNC_NAME);
      abort ();
    }
  
  switch (field_type)
    {
    case 'u':
      answer = scm_ulong2num (data[p]);
      break;

#if 0
    case 'i':
      answer = scm_long2num (data[p]);
      break;

    case 'd':
      answer = scm_makdbl (*((double *)&(data[p])), 0.0);
      break;
#endif

    case 's':
    case 'p':
      answer = data[p];
      break;


    default:
      SCM_ASSERT (0, SCM_MAKE_CHAR (field_type), "unrecognized field type", FUNC_NAME);
      break;
    }

  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_set_x, "struct-set!", 3, 0, 0,
            (SCM handle, SCM pos, SCM val),
	    "")
#define FUNC_NAME s_scm_struct_set_x
{
  SCM * data;
  SCM layout;
  int p;
  int n_fields;
  unsigned char * fields_desc;
  unsigned char field_type = 0;

  SCM_VALIDATE_STRUCT (1,handle);
  SCM_VALIDATE_INUM (2,pos);

  layout = SCM_STRUCT_LAYOUT (handle);
  data = SCM_STRUCT_DATA (handle);
  p = SCM_INUM (pos);

  fields_desc = (unsigned char *)SCM_CHARS (layout);
  n_fields = data[scm_struct_i_n_words];

  SCM_ASSERT_RANGE (1,pos, p < n_fields);

  if (p * 2 < SCM_LENGTH (layout))
    {
      unsigned char set_x;
      field_type = fields_desc[p * 2];
      set_x = fields_desc [p * 2 + 1];
      if (set_x != 'w')
	SCM_ASSERT (0, pos, "set_x denied", FUNC_NAME);
    }
  else if (fields_desc[SCM_LENGTH (layout) - 1] == 'W')    
    field_type = fields_desc[SCM_LENGTH (layout) - 2];
  else
    {
      SCM_ASSERT (0, pos, "set_x denied", FUNC_NAME);
      abort ();
    }
  
  switch (field_type)
    {
    case 'u':
      data[p] = SCM_NUM2ULONG (3,val);
      break;

#if 0
    case 'i':
      data[p] = SCM_NUM2LONG (3,val);
      break;

    case 'd':
      *((double *)&(data[p])) = scm_num2dbl (val, (char *)SCM_ARG3);
      break;
#endif

    case 'p':
      data[p] = val;
      break;

    case 's':
      SCM_ASSERT (0, SCM_MAKE_CHAR (field_type), "self fields immutable", FUNC_NAME);
      break;

    default:
      SCM_ASSERT (0, SCM_MAKE_CHAR (field_type), "unrecognized field type", FUNC_NAME);
      break;
    }

  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable, "struct-vtable", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable structure that describes the type of @var{struct}.")
#define FUNC_NAME s_scm_struct_vtable
{
  SCM_VALIDATE_STRUCT (1,handle);
  return SCM_STRUCT_VTABLE (handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable_tag, "struct-vtable-tag", 1, 0, 0, 
            (SCM handle),
	    "")
#define FUNC_NAME s_scm_struct_vtable_tag
{
  SCM_VALIDATE_VTABLE (1,handle);
  return scm_long2num ((long) SCM_STRUCT_DATA (handle) >> 3);
}
#undef FUNC_NAME

/* {Associating names and classes with vtables}
 *
 * The name of a vtable should probably be stored as a slot.  This is
 * a backward compatible solution until agreement has been achieved on
 * how to associate names with vtables.
 */

unsigned int
scm_struct_ihashq (SCM obj, unsigned int n)
{
  /* The length of the hash table should be a relative prime it's not
     necessary to shift down the address.  */
  return obj % n;
}

SCM
scm_struct_create_handle (SCM obj)
{
  SCM handle = scm_hash_fn_create_handle_x (scm_struct_table,
					    obj,
					    SCM_BOOL_F,
					    scm_struct_ihashq,
					    scm_sloppy_assq,
					    0);
  if (SCM_FALSEP (SCM_CDR (handle)))
    SCM_SETCDR (handle, scm_cons (SCM_BOOL_F, SCM_BOOL_F));
  return handle;
}

SCM_DEFINE (scm_struct_vtable_name, "struct-vtable-name", 1, 0, 0, 
            (SCM vtable),
	    "")
#define FUNC_NAME s_scm_struct_vtable_name
{
  SCM_VALIDATE_VTABLE (1,vtable);
  return SCM_STRUCT_TABLE_NAME (SCM_CDR (scm_struct_create_handle (vtable)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_struct_vtable_name_x, "set-struct-vtable-name!", 2, 0, 0, 
            (SCM vtable, SCM name),
	    "")
#define FUNC_NAME s_scm_set_struct_vtable_name_x
{
  SCM_VALIDATE_VTABLE (1,vtable);
  SCM_VALIDATE_SYMBOL (2,name);
  SCM_SET_STRUCT_TABLE_NAME (SCM_CDR (scm_struct_create_handle (vtable)),
			     name);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_print_struct (SCM exp, SCM port, scm_print_state *pstate)
{
  if (SCM_NFALSEP (scm_procedure_p (SCM_STRUCT_PRINTER (exp))))
    scm_printer_apply (SCM_STRUCT_PRINTER (exp), exp, port, pstate);
  else
    {
      SCM vtable = SCM_STRUCT_VTABLE (exp);
      SCM name = scm_struct_vtable_name (vtable);
      scm_puts ("#<", port);
      if (SCM_NFALSEP (name))
	scm_display (name, port);
      else
	scm_puts ("struct", port);
      scm_putc (' ', port);
      scm_intprint (vtable, 16, port);
      scm_putc (':', port);
      scm_intprint (exp, 16, port);
      scm_putc ('>', port);
    }
}

void
scm_init_struct ()
{
  scm_struct_table
    = scm_permanent_object (scm_make_weak_key_hash_table (SCM_MAKINUM (31)));
  required_vtable_fields = SCM_CAR (scm_intern_obarray ("pruosrpw", sizeof ("pruosrpw") - 1, SCM_BOOL_F));
  scm_permanent_object (required_vtable_fields);
  scm_sysintern ("vtable-index-layout", SCM_MAKINUM (scm_vtable_index_layout));
  scm_sysintern ("vtable-index-vtable", SCM_MAKINUM (scm_vtable_index_vtable));
  scm_sysintern ("vtable-index-printer", SCM_MAKINUM (scm_vtable_index_printer));
  scm_sysintern ("vtable-offset-user", SCM_MAKINUM (scm_vtable_offset_user));
#include "struct.x"
}
