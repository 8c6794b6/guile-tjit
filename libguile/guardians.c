/* Copyright (C) 1998,1999,2000,2001 Free Software Foundation, Inc.
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




/* This is an implementation of guardians as described in
 * R. Kent Dybvig, Carl Bruggeman, and David Eby (1993) "Guardians in
 * a Generation-Based Garbage Collector" ACM SIGPLAN Conference on
 * Programming Language Design and Implementation, June 1993
 * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/guardians.ps.gz
 *
 * By this point, the semantics are actually quite different from
 * those described in the abovementioned paper.  The semantic changes
 * are there to improve safety and intuitiveness.  The interface is
 * still (mostly) the one described by the paper, however.
 *
 * Original design:         Mikael Djurfeldt
 * Original implementation: Michael Livshin
 * Hacked on since by:      everybody
 */


#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/hashtab.h"
#include "libguile/weaks.h"

#include "libguile/guardians.h"


/* The live and zombies FIFOs are implemented as tconcs as described
   in Dybvig's paper.  This decouples addition and removal of elements
   so that no synchronization between these needs to take place.
*/

typedef struct tconc_t
{
  SCM head;
  SCM tail;
} tconc_t;

#define TCONC_EMPTYP(tc) (SCM_EQ_P ((tc).head, (tc).tail))

#define TCONC_IN(tc, obj, pair) \
do { \
  SCM_SETCAR ((tc).tail, obj); \
  SCM_SET_CELL_WORD_0 (pair, SCM_BOOL_F); \
  SCM_SET_CELL_WORD_1 (pair, SCM_EOL); \
  SCM_SETCDR ((tc).tail, pair); \
  (tc).tail = pair; \
} while (0)

#define TCONC_OUT(tc, res) \
do { \
  (res) = SCM_CAR ((tc).head); \
  (tc).head = SCM_CDR ((tc).head); \
} while (0)


static scm_t_bits tc16_guardian;

typedef struct guardian_t
{
  tconc_t live;
  tconc_t zombies;
  struct guardian_t *next;
  unsigned long flags;
} guardian_t;

#define GUARDIAN_P(x) SCM_SMOB_PREDICATE(tc16_guardian, x)
#define GUARDIAN(x) ((guardian_t *) SCM_CELL_WORD_1 (x))

#define F_GREEDY 1L
#define F_LISTED (1L << 1)
#define F_DESTROYED (1L << 2)

#define GREEDY_P(x)   (((x)->flags & F_GREEDY) != 0)
#define SET_GREEDY(x) ((x)->flags |= F_GREEDY)

#define LISTED_P(x)   (((x)->flags & F_LISTED) != 0)
#define SET_LISTED(x) ((x)->flags |= F_LISTED)
#define CLR_LISTED(x) ((x)->flags &= ~F_LISTED)

#define DESTROYED_P(x)   (((x)->flags & F_DESTROYED) != 0)
#define SET_DESTROYED(x) ((x)->flags |= F_DESTROYED)

/* during the gc mark phase, live guardians are linked into the lists
   here. */
static guardian_t *greedy_guardians = NULL;
static guardian_t *sharing_guardians = NULL;

static SCM greedily_guarded_whash = SCM_EOL;

/* this is the list of guarded objects that are parts of cycles.  we
   don't know in which order to return them from guardians, so we just
   unguard them and whine about it in after-gc-hook */
static SCM self_centered_zombies = SCM_EOL;


static void
add_to_live_list (guardian_t *g)
{
  if (LISTED_P (g))
    return;

  if (GREEDY_P (g))
    {
      g->next = greedy_guardians;
      greedy_guardians = g;
    }
  else
    {
      g->next = sharing_guardians;
      sharing_guardians = g;
    }

  SET_LISTED (g);
}

/* mark a guardian by adding it to the live guardian list.  */
static SCM
guardian_mark (SCM ptr)
{
  add_to_live_list (GUARDIAN (ptr));

  /* the objects protected by the guardian are not marked here: that
     would prevent them from ever getting collected.  instead marking
     is done at the end of the mark phase by guardian_zombify.  */
  return SCM_BOOL_F;
}


static size_t
guardian_free (SCM ptr)
{
  scm_must_free (GUARDIAN (ptr));
  return sizeof (guardian_t);
}


static int
guardian_print (SCM guardian, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  guardian_t *g = GUARDIAN (guardian);
  
  scm_puts ("#<", port);
  
  if (DESTROYED_P (g))
    scm_puts ("destroyed ", port);

  if (GREEDY_P (g))
    scm_puts ("greedy", port);
  else
    scm_puts ("sharing", port);

  scm_puts (" guardian 0x", port);
  scm_intprint ((long) g, 16, port);

  if (! DESTROYED_P (g))
    {
      scm_puts (" (reachable: ", port);
      scm_display (scm_length (SCM_CDR (g->live.head)), port);
      scm_puts (" unreachable: ", port);
      scm_display (scm_length (SCM_CDR (g->zombies.head)), port);
      scm_puts (")", port);
    }

  scm_puts (">", port);

  return 1;
}


/* This is the Scheme entry point for each guardian: If OBJ is an
 * object, it's added to the guardian's live list.  If OBJ is unbound,
 * the next available unreachable object (or #f if none) is returned.
 *
 * If the second optional argument THROW_P is true (the default), then
 * an error is raised if GUARDIAN is greedy and OBJ is already greedily
 * guarded.  If THROW_P is false, #f is returned instead of raising the
 * error, and #t is returned if everything is fine.
 */ 
static SCM
guardian_apply (SCM guardian, SCM obj, SCM throw_p)
{
  if (DESTROYED_P (GUARDIAN (guardian)))
    scm_misc_error ("guard", "attempted use of destroyed guardian: ~A",
                    scm_list_1 (guardian));
  
  if (!SCM_UNBNDP (obj))
    return scm_guard (guardian, obj,
                      (SCM_UNBNDP (throw_p)
                       ? 1
                       : SCM_NFALSEP (throw_p)));
  else
    return scm_get_one_zombie (guardian);
}


SCM
scm_guard (SCM guardian, SCM obj, int throw_p)
{
  guardian_t *g = GUARDIAN (guardian);
  
  if (!SCM_IMP (obj))
    {
      SCM z;

      SCM_NEWCELL (z);
      
      /* This critical section barrier will be replaced by a mutex. */
      SCM_DEFER_INTS;

      if (GREEDY_P (g))
        {
          if (!SCM_FALSEP (scm_hashq_get_handle
                           (greedily_guarded_whash, obj)))
            {
              SCM_ALLOW_INTS;

              if (throw_p)
                scm_misc_error ("guard",
                                "object is already greedily guarded: ~A",
                                scm_list_1 (obj));
              else
                return SCM_BOOL_F;
            }
          else
            scm_hashq_create_handle_x (greedily_guarded_whash,
                                       obj, guardian);
        }

      TCONC_IN (g->live, obj, z);

      SCM_ALLOW_INTS;
    }

  return throw_p ? SCM_UNSPECIFIED : SCM_BOOL_T;

}


SCM
scm_get_one_zombie (SCM guardian)
{
  guardian_t *g = GUARDIAN (guardian);
  SCM res = SCM_BOOL_F;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_DEFER_INTS;

  if (!TCONC_EMPTYP (g->zombies))
    TCONC_OUT (g->zombies, res);

  if (SCM_NFALSEP (res)
      && GREEDY_P (g))
    scm_hashq_remove_x (greedily_guarded_whash, res);

  SCM_ALLOW_INTS;
  
  return res;
}


SCM_DEFINE (scm_make_guardian, "make-guardian", 0, 1, 0, 
            (SCM greedy_p),
            "Create a new guardian.\n"
	    "A guardian protects a set of objects from garbage collection,\n"
	    "allowing a program to apply cleanup or other actions.\n\n"

	    "@code{make-guardian} returns a procedure representing the guardian.\n"
	    "Calling the guardian procedure with an argument adds the\n"
	    "argument to the guardian's set of protected objects.\n"
	    "Calling the guardian procedure without an argument returns\n"
	    "one of the protected objects which are ready for garbage\n"
	    "collection, or @code{#f} if no such object is available.\n"
	    "Objects which are returned in this way are removed from\n"
	    "the guardian.\n\n"

            "@code{make-guardian} takes one optional argument that says whether the\n"
            "new guardian should be greedy or sharing.  If there is any chance\n"
            "that any object protected by the guardian may be resurrected,\n"
            "then you should make the guardian greedy (this is the default).\n\n"

            "See R. Kent Dybvig, Carl Bruggeman, and David Eby (1993)\n"
            "\"Guardians in a Generation-Based Garbage Collector\".\n"
            "ACM SIGPLAN Conference on Programming Language Design\n"
            "and Implementation, June 1993.\n\n"

            "(the semantics are slightly different at this point, but the\n"
            "paper still (mostly) accurately describes the interface).")
#define FUNC_NAME s_scm_make_guardian
{
  guardian_t *g = SCM_MUST_MALLOC_TYPE (guardian_t);
  SCM z1 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z2 = scm_cons (SCM_BOOL_F, SCM_EOL);
  SCM z;

  /* A tconc starts out with one tail pair. */
  g->live.head = g->live.tail = z1;
  g->zombies.head = g->zombies.tail = z2;

  g->next = NULL;
  g->flags = 0L;

  /* [cmm] the UNBNDP check below is redundant but I like it. */
  if (SCM_UNBNDP (greedy_p) || SCM_NFALSEP (greedy_p))
    SET_GREEDY (g);
  
  SCM_NEWSMOB (z, tc16_guardian, g);

  return z;
}
#undef FUNC_NAME


SCM_DEFINE (scm_guardian_destroyed_p, "guardian-destroyed?", 1, 0, 0, 
            (SCM guardian),
            "Return @code{#t} if @var{guardian} has been destroyed, otherwise @code{#f}.")
#define FUNC_NAME s_scm_guardian_destroyed_p       
{
  SCM res = SCM_BOOL_F;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_DEFER_INTS;

  res = SCM_BOOL (DESTROYED_P (GUARDIAN (guardian)));
  
  SCM_ALLOW_INTS;

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_guardian_greedy_p, "guardian-greedy?", 1, 0, 0,
            (SCM guardian),
            "Return @code{#t} if @var{guardian} is a greedy guardian, otherwise @code{#f}.\n")
#define FUNC_NAME s_scm_guardian_greedy_p  
{
  return SCM_BOOL (GREEDY_P (GUARDIAN (guardian)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_destroy_guardian_x, "destroy-guardian!", 1, 0, 0, 
            (SCM guardian),
            "Destroys @var{guardian}, by making it impossible to put any more\n"
            "objects in it or get any objects from it.  It also unguards any\n"
            "objects guarded by @var{guardian}.")
#define FUNC_NAME s_scm_destroy_guardian_x
{
  guardian_t *g = GUARDIAN (guardian);

  /* This critical section barrier will be replaced by a mutex. */
  SCM_DEFER_INTS;
  
  if (DESTROYED_P (g))
    {
      SCM_ALLOW_INTS;
      SCM_MISC_ERROR ("guardian is already destroyed: ~A",
		      scm_list_1 (guardian));
    }

  if (GREEDY_P (g))
    {
      /* clear the "greedily guarded" property of the objects */
      SCM pair;
      for (pair = g->live.head; pair != g->live.tail; pair = SCM_CDR (pair))
        scm_hashq_remove_x (greedily_guarded_whash, SCM_CAR (pair));
      for (pair = g->zombies.head; pair != g->zombies.tail; pair = SCM_CDR (pair))
        scm_hashq_remove_x (greedily_guarded_whash, SCM_CAR (pair));
    }

  /* empty the lists */
  g->live.head = g->live.tail;
  g->zombies.head = g->zombies.tail;
  
  SET_DESTROYED (g);
  
  SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
            
/* called before gc mark phase begins to initialise the live guardian list. */
static void *
guardian_gc_init (void *dummy1 SCM_UNUSED,
		  void *dummy2 SCM_UNUSED,
		  void *dummy3 SCM_UNUSED)
{
  greedy_guardians = sharing_guardians = NULL;

  return 0;
}

static void
mark_dependencies_in_tconc (tconc_t *tc)
{
  SCM pair, next_pair;
  SCM *prev_ptr;

  /* scan the list for unmarked objects, and mark their
     dependencies */
  for (pair = tc->head, prev_ptr = &tc->head;
       ! SCM_EQ_P (pair, tc->tail);
       pair = next_pair)
    {
      SCM obj = SCM_CAR (pair);
      next_pair = SCM_CDR (pair);
            
      if (! SCM_MARKEDP (obj))
        {
          /* a candidate for finalizing */
          scm_gc_mark_dependencies (obj);

          if (SCM_MARKEDP (obj))
            {
              /* uh oh.  a cycle.  transfer this object (the
                 spine cell, to be exact) to
                 self_centered_zombies, so we'll be able to
                 complain about it later. */
              *prev_ptr = next_pair;
              SCM_SETGCMARK (pair);
              SCM_SETCDR (pair, SCM_CDR (self_centered_zombies));
              SCM_SETCDR (self_centered_zombies, pair);
            }
          else
            {
              /* see if this is a guardian.  if yes, list it (but don't   
                 mark it yet). */
              if (GUARDIAN_P (obj))
                add_to_live_list (GUARDIAN (obj));

              prev_ptr = SCM_CDRLOC (pair);
            }
        }
    }
}

static void
mark_dependencies (guardian_t *g)
{
  mark_dependencies_in_tconc (&g->zombies);
  mark_dependencies_in_tconc (&g->live);
}

static void
mark_and_zombify (guardian_t *g)
{
  SCM tconc_tail = g->live.tail;
  SCM *prev_ptr = &g->live.head;
  SCM pair = g->live.head;

  while (! SCM_EQ_P (pair, tconc_tail))
    {
      SCM next_pair = SCM_CDR (pair);

      if (SCM_NMARKEDP (SCM_CAR (pair)))
        {
          /* got you, zombie! */

          /* out of the live list! */
          *prev_ptr = next_pair;

          if (GREEDY_P (g))
            /* if the guardian is greedy, mark this zombie now.  this
               way it won't be zombified again this time around. */
            SCM_SETGCMARK (SCM_CAR (pair));

          /* into the zombie list! */
          TCONC_IN (g->zombies, SCM_CAR (pair), pair);
        }
      else
        prev_ptr = SCM_CDRLOC (pair);

      pair = next_pair;
    }

  /* Mark the cells of the live list (yes, the cells in the list, we
     don't care about objects pointed to by the list cars, since we
     know they are already marked).  */
  for (pair = g->live.head; !SCM_NULLP (pair); pair = SCM_CDR (pair))
    SCM_SETGCMARK (pair);
}


/* this is called by the garbage collector between the mark and sweep
   phases.  for each marked guardian, it moves any unmarked object in
   its live list (tconc) to its zombie list (tconc).  */
static void *
guardian_zombify (void *dummy1 SCM_UNUSED,
		  void *dummy2 SCM_UNUSED,
		  void *dummy3 SCM_UNUSED)
{
  guardian_t *last_greedy_guardian = NULL;
  guardian_t *last_sharing_guardian = NULL;
  guardian_t *first_greedy_guardian = NULL;
  guardian_t *first_sharing_guardian = NULL;
  guardian_t *g;

  /* First, find all newly unreachable objects and mark their
     dependencies.
     
     Note that new guardians may be stuck on the end of the live
     guardian lists as we run this loop, since guardians might be
     guarded too.  When we mark a guarded guardian, its mark function
     sticks in the appropriate live guardian list.  The loop
     terminates when no new guardians are found. */

  do {
    first_greedy_guardian = greedy_guardians;
    first_sharing_guardian = sharing_guardians;

    for (g = greedy_guardians; g != last_greedy_guardian;
         g = g->next)
      mark_dependencies (g);
    for (g = sharing_guardians; g != last_sharing_guardian;
         g = g->next)
      mark_dependencies (g);

    last_greedy_guardian = first_greedy_guardian;
    last_sharing_guardian = first_sharing_guardian;
  } while (first_greedy_guardian != greedy_guardians
           || first_sharing_guardian != sharing_guardians);

  /* now, scan all the guardians that are currently known to be live
     and move their unmarked objects to zombie lists. */

  for (g = greedy_guardians; g; g = g->next)
    {
      mark_and_zombify (g);
      CLR_LISTED (g);
    }
  for (g = sharing_guardians; g; g = g->next)
    {
      mark_and_zombify (g);
      CLR_LISTED (g);
    }
  
  /* Preserve the zombies in their undead state, by marking to prevent
     collection. */
  for (g = greedy_guardians; g; g = g->next)
    scm_gc_mark (g->zombies.head);
  for (g = sharing_guardians; g; g = g->next)
    scm_gc_mark (g->zombies.head);

  return 0;
}

static void *
whine_about_self_centered_zombies (void *dummy1 SCM_UNUSED,
				   void *dummy2 SCM_UNUSED,
				   void *dummy3 SCM_UNUSED)
{
  if (! SCM_NULLP (SCM_CDR (self_centered_zombies)))
    {
      SCM pair;
      
      scm_puts ("** WARNING: the following guarded objects were unguarded due to cycles:",
                scm_cur_errp);
      scm_newline (scm_cur_errp);
      for (pair = SCM_CDR (self_centered_zombies);
           ! SCM_NULLP (pair); pair = SCM_CDR (pair))
        {
          scm_display (SCM_CAR (pair), scm_cur_errp);
          scm_newline (scm_cur_errp);
        }

      SCM_SETCDR (self_centered_zombies, SCM_EOL);
    }
  
  return 0;
}

void
scm_init_guardians ()
{
  tc16_guardian = scm_make_smob_type ("guardian", 0);
  scm_set_smob_mark (tc16_guardian, guardian_mark);
  scm_set_smob_free (tc16_guardian, guardian_free);
  scm_set_smob_print (tc16_guardian, guardian_print);
  scm_set_smob_apply (tc16_guardian, guardian_apply, 0, 2, 0);

  scm_c_hook_add (&scm_before_mark_c_hook, guardian_gc_init, 0, 0);
  scm_c_hook_add (&scm_before_sweep_c_hook, guardian_zombify, 0, 0);

  self_centered_zombies =
    scm_permanent_object (scm_cons (SCM_UNDEFINED, SCM_EOL));
  scm_c_hook_add (&scm_after_gc_c_hook,
                  whine_about_self_centered_zombies, 0, 0);

  greedily_guarded_whash =
    scm_permanent_object (scm_make_doubly_weak_hash_table (SCM_MAKINUM (31)));

#ifndef SCM_MAGIC_SNARFER
#include "libguile/guardians.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
