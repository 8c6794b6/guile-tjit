/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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
#include "eq.h"

#include "validate.h"
#include "list.h"

#ifdef __STDC__
#include <stdarg.h>
#define var_start(x, y) va_start(x, y)
#else
#include <varargs.h>
#define var_start(x, y) va_start(x)
#endif


/* creating lists */

SCM
scm_listify (SCM elt, ...)
{
  va_list foo;
  SCM answer = SCM_EOL;
  SCM *pos = &answer;

  var_start (foo, elt);
  while (elt != SCM_UNDEFINED)
    {
      *pos = scm_cons (elt, SCM_EOL);
      pos = SCM_CDRLOC (*pos);
      elt = va_arg (foo, SCM);
    }
  return answer;
}


SCM_DEFINE (scm_list, "list", 0, 0, 1, 
           (SCM objs),
            "Return a list containing OBJS, the arguments to `list'.")
#define FUNC_NAME s_scm_list
{
  return objs;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_star, "list*", 1, 0, 1, 
            (SCM arg, SCM rest),
	    "Return an improper list of the arguments.")
#define FUNC_NAME s_scm_list_star
{
  if (SCM_NNULLP (rest))
    {
      SCM prev = arg = scm_cons (arg, rest);
      while (SCM_NNULLP (SCM_CDR (rest)))
	{
	  prev = rest;
	  rest = SCM_CDR (rest);
	}
      SCM_SETCDR (prev, SCM_CAR (rest));
    }
  return arg;
}
#undef FUNC_NAME



/* general questions about lists --- null?, list?, length, etc.  */

SCM_DEFINE (scm_null_p, "null?", 1, 0, 0, 
           (SCM x),
            "Return #t iff X is the empty list, else #f.")
#define FUNC_NAME s_scm_null_p
{
  return SCM_BOOL (SCM_NULLP (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_p, "list?", 1, 0, 0, 
           (SCM x),
            "Return #t iff X is a proper list, else #f.")
#define FUNC_NAME s_scm_list_p
{
  return SCM_BOOL (scm_ilength (x) >= 0);
}
#undef FUNC_NAME


/* Return the length of SX, or -1 if it's not a proper list.
   This uses the "tortoise and hare" algorithm to detect "infinitely
   long" lists (i.e. lists with cycles in their cdrs), and returns -1
   if it does find one.  */
long
scm_ilength(SCM sx)
{
  long i = 0;
  SCM tortoise = sx;
  SCM hare = sx;

  do {
    if (SCM_NULLP(hare)) return i;
    if (SCM_NCONSP(hare)) return -1;
    hare = SCM_CDR(hare);
    i++;
    if (SCM_NULLP(hare)) return i;
    if (SCM_NCONSP(hare)) return -1;
    hare = SCM_CDR(hare);
    i++;
    /* For every two steps the hare takes, the tortoise takes one.  */
    tortoise = SCM_CDR(tortoise);
  }
  while (hare != tortoise);

  /* If the tortoise ever catches the hare, then the list must contain
     a cycle.  */
  return -1;
}


SCM_DEFINE (scm_length, "length", 1, 0, 0, 
           (SCM lst),
            "Return the number of elements in list LST.")
#define FUNC_NAME s_scm_length
{
  int i;
  SCM_VALIDATE_LIST_COPYLEN (1,lst,i);
  return SCM_MAKINUM (i);
}
#undef FUNC_NAME



/* appending lists */

SCM_DEFINE (scm_append, "append", 0, 0, 1, 
            (SCM args),
            "Returns a list consisting of the elements of the first LIST\n"
            "followed by the elements of the other LISTs.\n"
            "\n"
            "  (append '(x) '(y))          =>  (x y)\n"
            "  (append '(a) '(b c d))      =>  (a b c d)\n"
            "  (append '(a (b)) '((c)))    =>  (a (b) (c))\n"
            "\n"
            "The resulting list is always newly allocated, except that it shares\n"
            "structure with the last LIST argument.  The last argument may\n"
            "actually be any object; an improper list results if the last\n"
            "argument is not a proper list.\n"

            "  (append '(a b) '(c . d))    =>  (a b c . d)\n"
            "  (append '() 'a)             =>  a\n")
#define FUNC_NAME s_scm_append
{
  SCM res = SCM_EOL;
  SCM *lloc = &res, arg;
  if (SCM_IMP(args)) {
    SCM_VALIDATE_NULL (SCM_ARGn, args);
    return res;
  }
  SCM_VALIDATE_CONS (SCM_ARGn, args);
  while (1) {
    arg = SCM_CAR(args);
    args = SCM_CDR(args);
    if (SCM_IMP(args)) {
      *lloc = arg;
      SCM_VALIDATE_NULL (SCM_ARGn, args);
      return res;
    }
    SCM_VALIDATE_CONS (SCM_ARGn, args);
    for (; SCM_CONSP(arg); arg = SCM_CDR(arg)) {
      *lloc = scm_cons(SCM_CAR(arg), SCM_EOL);
      lloc = SCM_CDRLOC(*lloc);
    }
    SCM_VALIDATE_NULL (SCM_ARGn, arg);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_append_x, "append!", 0, 0, 1, 
            (SCM args),
	    "A destructive version of @code{append} (@pxref{Pairs and Lists,,,r4rs,\n"
	    "The Revised^4 Report on Scheme}).  The cdr field of each list's final\n"
	    "pair is changed to point to the head of the next list, so no consing is\n"
	    "performed.  Return a pointer to the mutated list.")
#define FUNC_NAME s_scm_append_x
{
  SCM arg;
 tail:
  if (SCM_NULLP(args)) return SCM_EOL;
  arg = SCM_CAR(args);
  args = SCM_CDR(args);
  if (SCM_NULLP(args)) return arg;
  if (SCM_NULLP(arg)) goto tail;
  SCM_VALIDATE_CONS (SCM_ARG1,arg);
  SCM_SETCDR (scm_last_pair (arg), scm_append_x (args));
  return arg;
}
#undef FUNC_NAME


SCM_DEFINE (scm_last_pair, "last-pair", 1, 0, 0, 
           (SCM lst),
	    "Return a pointer to the last pair in @var{lst}, signalling an error if\n"
	    "@var{lst} is circular.")
#define FUNC_NAME s_scm_last_pair
{
  SCM tortoise = lst;
  SCM hare = lst;

  if (SCM_NULLP (lst))
    return SCM_EOL;

  SCM_VALIDATE_CONS (SCM_ARG1, lst);
  do {
    SCM ahead = SCM_CDR(hare);
    if (SCM_NCONSP(ahead)) return hare;
    hare = ahead;
    ahead = SCM_CDR(hare);
    if (SCM_NCONSP(ahead)) return hare;
    hare = ahead;
    tortoise = SCM_CDR(tortoise);
  }
  while (hare != tortoise);
  SCM_MISC_ERROR ("Circular structure in position 1: ~S", SCM_LIST1 (lst));
}
#undef FUNC_NAME


/* reversing lists */

SCM_DEFINE (scm_reverse, "reverse", 1, 0, 0,
            (SCM lst),
	    "Return a new list that contains the elements of LST but in reverse order.")
#define FUNC_NAME s_scm_reverse
{
  SCM result = SCM_EOL;
  SCM tortoise = lst;
  SCM hare = lst;

  do {
      if (SCM_NULLP(hare)) return result;
      SCM_ASSERT(SCM_CONSP(hare), lst, 1, FUNC_NAME);
      result = scm_cons (SCM_CAR (hare), result);
      hare = SCM_CDR (hare);
      if (SCM_NULLP(hare)) return result;
      SCM_ASSERT(SCM_CONSP(hare), lst, 1, FUNC_NAME);
      result = scm_cons (SCM_CAR (hare), result);
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  while (hare != tortoise);
  SCM_MISC_ERROR ("Circular structure in position 1: ~S", SCM_LIST1 (lst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_reverse_x, "reverse!", 1, 1, 0,
            (SCM lst, SCM new_tail),
	    "A destructive version of @code{reverse} (@pxref{Pairs and Lists,,,r4rs,\n"
	    "The Revised^4 Report on Scheme}).  The cdr of each cell in @var{lst} is\n"
	    "modified to point to the previous list element.  Return a pointer to the\n"
	    "head of the reversed list.\n\n"
	    "Caveat: because the list is modified in place, the tail of the original\n"
	    "list now becomes its head, and the head of the original list now becomes\n"
	    "the tail.  Therefore, the @var{lst} symbol to which the head of the\n"
	    "original list was bound now points to the tail.  To ensure that the head\n"
	    "of the modified list is not lost, it is wise to save the return value of\n"
	    "@code{reverse!}")
#define FUNC_NAME s_scm_reverse_x
{
  SCM_ASSERT (scm_ilength (lst) >= 0, lst, SCM_ARG1, FUNC_NAME);
  if (SCM_UNBNDP (new_tail))
    new_tail = SCM_EOL;
  else
    SCM_ASSERT (scm_ilength (new_tail) >= 0, new_tail, SCM_ARG2, FUNC_NAME);

  while (SCM_NNULLP (lst))
    {
      SCM old_tail = SCM_CDR (lst);
      SCM_SETCDR (lst, new_tail);
      new_tail = lst;
      lst = old_tail;
    }
  return new_tail;
}
#undef FUNC_NAME



/* indexing lists by element number */

SCM_DEFINE (scm_list_ref, "list-ref", 2, 0, 0,
           (SCM lst, SCM k),
	    "Return the Kth element from list LST.")
#define FUNC_NAME s_scm_list_ref
{
  register long i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (i-- > 0) {
    SCM_ASRTGO(SCM_CONSP(lst), erout);
    lst = SCM_CDR(lst);
  }
 erout:	
  SCM_ASSERT(SCM_CONSP(lst),
             SCM_NULLP(lst)?k:lst, SCM_NULLP(lst)?SCM_OUTOFRANGE:SCM_ARG1, FUNC_NAME);
  return SCM_CAR(lst);
}
#undef FUNC_NAME

SCM_DEFINE (scm_list_set_x, "list-set!", 3, 0, 0,
           (SCM lst, SCM k, SCM val),
	    "Set the @var{k}th element of @var{lst} to @var{val}.")
#define FUNC_NAME s_scm_list_set_x
{
  register long i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (i-- > 0) {
    SCM_ASRTGO(SCM_CONSP(lst), erout);
    lst = SCM_CDR(lst);
  }
 erout:	
  SCM_ASSERT(SCM_CONSP(lst),
             SCM_NULLP(lst)?k:lst, SCM_NULLP(lst)?SCM_OUTOFRANGE:SCM_ARG1, FUNC_NAME);
  SCM_SETCAR (lst, val);
  return val;
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_cdr_ref, "list-cdr-ref", 2, 0, 0, scm_list_tail);

SCM_DEFINE (scm_list_tail, "list-tail", 2, 0, 0,
           (SCM lst, SCM k),
	    "Return the \"tail\" of @var{lst} beginning with its @var{k}th element.\n"
	    "The first element of the list is considered to be element 0.\n\n"
	    "@code{list-cdr-ref} and @code{list-tail} are identical.  It may help to\n"
	    "think of @code{list-cdr-ref} as accessing the @var{k}th cdr of the list,\n"
	    "or returning the results of cdring @var{k} times down @var{lst}.")
#define FUNC_NAME s_scm_list_tail
{
  register long i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (i-- > 0) {
    SCM_VALIDATE_CONS (1,lst);
    lst = SCM_CDR(lst);
  }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_cdr_set_x, "list-cdr-set!", 3, 0, 0,
           (SCM lst, SCM k, SCM val),
	    "Set the @var{k}th cdr of @var{lst} to @var{val}.")
#define FUNC_NAME s_scm_list_cdr_set_x
{
  register long i;
  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  while (i-- > 0) {
    SCM_ASRTGO(SCM_CONSP(lst), erout);
    lst = SCM_CDR(lst);
  }
erout:
  SCM_ASSERT(SCM_CONSP(lst),
             SCM_NULLP(lst)?k:lst, SCM_NULLP(lst)?SCM_OUTOFRANGE:SCM_ARG1, FUNC_NAME);
  SCM_SETCDR (lst, val);
  return val;
}
#undef FUNC_NAME



/* copying lists, perhaps partially */

SCM_DEFINE (scm_list_head, "list-head", 2, 0, 0,
           (SCM lst, SCM k),
	    "Copy the first @var{k} elements from @var{lst} into a new list, and\n"
	    "return it.")
#define FUNC_NAME s_scm_list_head
{
  SCM answer;
  SCM * pos;
  register long i;

  SCM_VALIDATE_INUM_MIN_COPY (2,k,0,i);
  answer = SCM_EOL;
  pos = &answer;
  while (i-- > 0)
    {
      SCM_VALIDATE_CONS (1,lst);
      *pos = scm_cons (SCM_CAR (lst), SCM_EOL);
      pos = SCM_CDRLOC (*pos);
      lst = SCM_CDR(lst);
    }
  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_copy, "list-copy", 1, 0, 0, 
            (SCM lst),
	    "Return a (newly-created) copy of @var{lst}.")
#define FUNC_NAME s_scm_list_copy
{
  SCM newlst;
  SCM * fill_here;
  SCM from_here;

  newlst = SCM_EOL;
  fill_here = &newlst;
  from_here = lst;

  while (SCM_CONSP (from_here))
    {
      SCM c;
      c = scm_cons (SCM_CAR (from_here), SCM_CDR (from_here));
      *fill_here = c;
      fill_here = SCM_CDRLOC (c);
      from_here = SCM_CDR (from_here);
    }
  return newlst;
}
#undef FUNC_NAME


/* membership tests (memq, memv, etc.) */ 

SCM_DEFINE (scm_sloppy_memq, "sloppy-memq", 2, 0, 0,
            (SCM x, SCM lst),
	    "This procedure behaves like @code{memq}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_memq
{
  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_CAR(lst)==x)
	return lst;
    }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_memv, "sloppy-memv", 2, 0, 0,
            (SCM x, SCM lst),
 	    "This procedure behaves like @code{memv}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_memv
{
  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR(lst), x))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_sloppy_member, "sloppy-member", 2, 0, 0,
            (SCM x, SCM lst),
 	    "This procedure behaves like @code{member}, but does no type or error checking.\n"
	    "Its use is recommended only in writing Guile internals,\n"
            "not for high-level Scheme programs.")
#define FUNC_NAME s_scm_sloppy_member
{
  for(;  SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR(lst), x))
	return lst;
    }
  return lst;
}
#undef FUNC_NAME



SCM_DEFINE (scm_memq, "memq", 2, 0, 0,
           (SCM x, SCM lst),
            "Return the first sublist of LST whose car is `eq?' to X\n"
            "where the sublists of LST are the non-empty lists returned\n"
            "by `(list-tail LST K)' for K less than the length of LST.  If\n"
            "X does not occur in LST, then `#f' (not the empty list) is\n"
            "returned.")
#define FUNC_NAME s_scm_memq
{
  SCM answer;
  SCM_VALIDATE_LIST (2,lst);
  answer = scm_sloppy_memq (x, lst);
  return (answer == SCM_EOL) ? SCM_BOOL_F : answer;
}
#undef FUNC_NAME



SCM_DEFINE (scm_memv, "memv", 2, 0, 0,
           (SCM x, SCM lst),
            "Return the first sublist of LST whose car is `eqv?' to X\n"
            "where the sublists of LST are the non-empty lists returned\n"
            "by `(list-tail LST K)' for K less than the length of LST.  If\n"
            "X does not occur in LST, then `#f' (not the empty list) is\n"
            "returned.")
#define FUNC_NAME s_scm_memv
{
  SCM answer;
  SCM_VALIDATE_LIST (2,lst);
  answer = scm_sloppy_memv (x, lst);
  return (answer == SCM_EOL) ? SCM_BOOL_F : answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_member, "member", 2, 0, 0,
           (SCM x, SCM lst),
            "Return the first sublist of LST whose car is `equal?' to X\n"
            "where the sublists of LST are the non-empty lists returned\n"
            "by `(list-tail LST K)' for K less than the length of LST.  If\n"
            "X does not occur in LST, then `#f' (not the empty list) is\n"
            "returned.")
#define FUNC_NAME s_scm_member
{
  SCM answer;
  SCM_VALIDATE_LIST (2,lst);
  answer = scm_sloppy_member (x, lst);
  return (answer == SCM_EOL) ? SCM_BOOL_F : answer;
}
#undef FUNC_NAME



/* deleting elements from a list (delq, etc.) */

SCM_DEFINE (scm_delq_x, "delq!", 2, 0, 0,
           (SCM item, SCM lst),
	    "@deffnx primitive delv! item lst\n"
	    "@deffnx primitive delete! item lst\n"
	    "These procedures are destructive versions of @code{delq}, @code{delv}\n"
	    "and @code{delete}: they modify the pointers in the existing @var{lst}\n"
	    "rather than creating a new list.  Caveat evaluator: Like other\n"
	    "destructive list functions, these functions cannot modify the binding of\n"
	    "@var{lst}, and so cannot be used to delete the first element of\n"
	    "@var{lst} destructively.")
#define FUNC_NAME s_scm_delq_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_CAR (walk) == item)
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delv_x, "delv!", 2, 0, 0,
           (SCM item, SCM lst),
	    "Destructively remove all elements from LST that are `eqv?' to ITEM.")
#define FUNC_NAME s_scm_delv_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR (walk), item))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME



SCM_DEFINE (scm_delete_x, "delete!", 2, 0, 0,
           (SCM item, SCM lst),
	    "Destructively remove all elements from LST that are `equal?' to ITEM.")
#define FUNC_NAME s_scm_delete_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR (walk), item))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}
#undef FUNC_NAME





SCM_DEFINE (scm_delq, "delq", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements `eq?' to @var{item} removed.\n"
            "This procedure mirrors @code{memq}:\n"
	    "@code{delq} compares elements of @var{lst} against @var{item} with\n"
	    "@code{eq?}.")
#define FUNC_NAME s_scm_delq
{
  SCM copy = scm_list_copy (lst);
  return scm_delq_x (item, copy);
}
#undef FUNC_NAME

SCM_DEFINE (scm_delv, "delv", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements `eqv?' to @var{item} removed.\n"
            "This procedure mirrors @code{memv}:\n"
	    "@code{delv} compares elements of @var{lst} against @var{item} with\n"
	    "@code{eqv?}.")
#define FUNC_NAME s_scm_delv
{
  SCM copy = scm_list_copy (lst);
  return scm_delv_x (item, copy);
}
#undef FUNC_NAME

SCM_DEFINE (scm_delete, "delete", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements `equal?' to @var{item} removed.\n"
            "This procedure mirrors @code{member}:\n"
	    "@code{delete} compares elements of @var{lst} against @var{item} with\n"
	    "@code{equal?}.")
#define FUNC_NAME s_scm_delete
{
  SCM copy = scm_list_copy (lst);
  return scm_delete_x (item, copy);
}
#undef FUNC_NAME


SCM_DEFINE (scm_delq1_x, "delq1!", 2, 0, 0,
           (SCM item, SCM lst),
	    "Like `delq!', but only deletes the first occurrence of ITEM from LST.\n"
            "Tests for equality using `eq?'.  See also `delv1!' and `delete1!'.")
#define FUNC_NAME s_scm_delq1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_CAR (walk) == item)
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delv1_x, "delv1!", 2, 0, 0,
            (SCM item, SCM lst),
	    "Like `delv!', but only deletes the first occurrence of ITEM from LST.\n"
            "Tests for equality using `eqv?'.  See also `delq1!' and `delete1!'.")
#define FUNC_NAME s_scm_delv1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR (walk), item))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delete1_x, "delete1!", 2, 0, 0,
            (SCM item, SCM lst),
	    "Like `delete!', but only deletes the first occurrence of ITEM from LST.\n"
            "Tests for equality using `equal?'.  See also `delq1!' and `delv1!'.")
#define FUNC_NAME s_scm_delete1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       SCM_CONSP (walk);
       walk = SCM_CDR (walk))
    {
      if (SCM_BOOL_F != scm_equal_p (SCM_CAR (walk), item))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}
#undef FUNC_NAME



void
scm_init_list ()
{
#include "list.x"
}
