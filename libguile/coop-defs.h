/* classes: h_files */

#ifndef SCM_COOP_DEFS_H
#define SCM_COOP_DEFS_H

/* Copyright (C) 1996,1997,1998,1999,2000,2001, 2002, 2006 Free Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



#include "libguile/__scm.h"
#include "libguile/iselect.h"

#if SCM_HAVE_WINSOCK2_H
# include <winsock2.h>
#endif

#ifdef GUILE_PTHREAD_COMPAT
#include <pthread.h>
#endif

/* This file is included by threads.h, which, in turn, is included by
   libguile.h while coop-threads.h only is included by
   coop-threads.c. */

/* The coop_t struct must be declared here, since macros in this file
   refer to the data member. */

/* The notion of a thread is merged with the notion of a queue.
   Thread stuff: thread status (sp) and stuff to use during
   (re)initialization.  Queue stuff: next thread in the queue
   (next). */

struct qt_t;

typedef struct coop_t {
  struct qt_t *sp;       /* QuickThreads handle. */
  void *sto;             /* `malloc'-allocated stack. */

  struct coop_t *next;    /* Next thread in the queue. */

  struct coop_t *all_next;    
  struct coop_t *all_prev;    

  void *data;            /* Thread local data */
  void **specific;	 /* Data associated with keys */
  int n_keys;		 /* Upper limit for keys on this thread */
  
  void *base;            /* Base of stack */
  void *top;             /* Top of stack */

  void *joining;         /* A queue of threads waiting to join this
			    thread */

  SCM handle;            /* SCM handle, protected via scm_all_threads. */

  int nfds;
  SELECT_TYPE *readfds;
  SELECT_TYPE *writefds;
  SELECT_TYPE *exceptfds;
  int timeoutp;
  struct timeval wakeup_time;	/* Time to stop sleeping */
  int _errno;
  int retval;

#ifdef GUILE_PTHREAD_COMPAT
  pthread_t dummy_thread;
  pthread_mutex_t dummy_mutex;
#endif
} coop_t;

/* A queue is a circular list of threads.  The queue head is a
   designated list element.  If this is a uniprocessor-only
   implementation we can store the `main' thread in this, but in a
   multiprocessor there are several `heavy' threads but only one run
   queue.  A fancier implementation might have private run queues,
   which would lead to a simpler (trivial) implementation */

typedef struct coop_q_t {
  coop_t t;
  coop_t *tail;
} coop_q_t;

/* A Mutex variable is made up of a owner thread, and a queue of threads
   waiting on the mutex */

typedef struct coop_m {
  coop_t *owner;          /* Mutex owner */
  int level;              /* for recursive locks. */
  coop_q_t waiting;      /* Queue of waiting threads */
} coop_m;

typedef int coop_mattr;

SCM_API int coop_mutex_init (coop_m*);
SCM_API int coop_new_mutex_init (coop_m*, coop_mattr*);
SCM_API int coop_mutex_lock (coop_m*);
SCM_API int coop_mutex_trylock (coop_m*);
SCM_API int coop_mutex_unlock (coop_m*);
SCM_API int coop_mutex_destroy (coop_m*);

/* A Condition variable is made up of a list of threads waiting on the
   condition. */

typedef struct coop_c {
  coop_q_t waiting;      /* Queue of waiting threads */
} coop_c;

typedef int coop_cattr;

SCM_API int coop_condition_variable_init (coop_c*);
SCM_API int coop_new_condition_variable_init (coop_c*, coop_cattr*);
SCM_API int coop_condition_variable_wait_mutex (coop_c*, coop_m*);
SCM_API int coop_condition_variable_timed_wait_mutex (coop_c*,
						      coop_m*,
						      const scm_t_timespec *abstime);
SCM_API int coop_condition_variable_signal (coop_c*);
SCM_API int coop_condition_variable_broadcast (coop_c*);
SCM_API int coop_condition_variable_destroy (coop_c*);

typedef int coop_k;

typedef coop_k scm_t_key;

SCM_API int coop_key_create (coop_k *keyp, void (*destruktor) (void *value));
SCM_API int coop_setspecific (coop_k key, const void *value);
SCM_API void *coop_getspecific (coop_k key);
SCM_API int coop_key_delete (coop_k);
#define scm_key_create coop_key_create
#define scm_setspecific coop_setspecific
#define scm_getspecific coop_getspecific
#define scm_key_delete coop_key_delete

SCM_API coop_t *coop_global_curr;             /* Currently-executing thread. */

SCM_API void coop_join (coop_t *t);
SCM_API void coop_yield (void);

SCM_API size_t scm_switch_counter;
SCM_API size_t scm_thread_count;


/* Some iselect functions.  */ 

/* I'm not sure whether these three declarations should be here.
   They're really defined in iselect.c, so you'd think they'd go in
   iselect.h, but they use coop_t, defined above, which uses things
   defined in iselect.h.  Basically, we're making at best a flailing
   (and failing) attempt at modularity here, and I don't have time to
   rethink this at the moment.  This code awaits a Hero.  --JimB
 */
SCM_API void coop_timeout_qinsert (coop_q_t *, coop_t *);
SCM_API coop_t *coop_next_runnable_thread (void);
SCM_API coop_t *coop_wait_for_runnable_thread_now (struct timeval *);
SCM_API coop_t *coop_wait_for_runnable_thread (void);




/* Cooperative threads don't need to have these defined */

#define SCM_CRITICAL_SECTION_START 
#define SCM_CRITICAL_SECTION_END 



#define SCM_NO_CRITICAL_SECTION_OWNER 0
#define SCM_THREAD_SWITCH_COUNT       50 /* was 10 /mdj */



#if 0
#define SCM_THREAD_SWITCHING_CODE \
do { \
  if (scm_thread_count > 1) \
    coop_yield(); \
} while (0)

#else
#define SCM_THREAD_SWITCHING_CODE \
do { \
  if (scm_thread_count > 1) \
  { \
    scm_switch_counter--; \
    if (scm_switch_counter == 0) \
      { \
        scm_switch_counter = SCM_THREAD_SWITCH_COUNT; \
        coop_yield(); \
      } \
  } \
} while (0)

#endif

/* For pthreads, this is a value associated with a specific key.
 * For coop, we use a special field for increased efficiency.
 */
#define SCM_THREAD_LOCAL_DATA (coop_global_curr->data)
#define SCM_SET_THREAD_LOCAL_DATA(ptr) (coop_global_curr->data = (ptr))

#endif  /* SCM_COOP_DEFS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
