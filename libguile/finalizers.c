/* Copyright (C) 2012 Free Software Foundation, Inc.
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




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>

#include <full-write.h>

#include "libguile/bdw-gc.h"
#include "libguile/_scm.h"
#include "libguile/finalizers.h"
#include "libguile/gc.h"
#include "libguile/threads.h"



static size_t finalization_count;




void
scm_i_set_finalizer (void *obj, scm_t_finalizer_proc proc, void *data)
{
  GC_finalization_proc prev;
  GC_PTR prev_data;
  GC_REGISTER_FINALIZER_NO_ORDER (obj, proc, data, &prev, &prev_data);
}

struct scm_t_chained_finalizer
{
  int resuscitating_p;
  scm_t_finalizer_proc proc;
  void *data;
  scm_t_finalizer_proc prev;
  void *prev_data;
};

static void
chained_finalizer (void *obj, void *data)
{
  struct scm_t_chained_finalizer *chained_data = data;
  if (chained_data->resuscitating_p)
    {
      if (chained_data->prev)
        scm_i_set_finalizer (obj, chained_data->prev, chained_data->prev_data);
      chained_data->proc (obj, chained_data->data);
    }
  else
    {
      chained_data->proc (obj, chained_data->data);
      if (chained_data->prev)
        chained_data->prev (obj, chained_data->prev_data);
    }
}

void
scm_i_add_resuscitator (void *obj, scm_t_finalizer_proc proc, void *data)
{
  struct scm_t_chained_finalizer *chained_data;
  chained_data = scm_gc_malloc (sizeof (*chained_data), "chained finalizer");
  chained_data->resuscitating_p = 1;
  chained_data->proc = proc;
  chained_data->data = data;
  GC_REGISTER_FINALIZER_NO_ORDER (obj, chained_finalizer, chained_data,
                                  &chained_data->prev,
                                  &chained_data->prev_data);
}

static void
shuffle_resuscitators_to_front (struct scm_t_chained_finalizer *cd)
{
  while (cd->prev == chained_finalizer)
    {
      struct scm_t_chained_finalizer *prev = cd->prev_data;
      scm_t_finalizer_proc proc = cd->proc;
      void *data = cd->data;

      if (!prev->resuscitating_p)
        break;

      cd->resuscitating_p = 1;
      cd->proc = prev->proc;
      cd->data = prev->data;

      prev->resuscitating_p = 0;
      prev->proc = proc;
      prev->data = data;

      cd = prev;
    }
}

void
scm_i_add_finalizer (void *obj, scm_t_finalizer_proc proc, void *data)
{
  struct scm_t_chained_finalizer *chained_data;
  chained_data = scm_gc_malloc (sizeof (*chained_data), "chained finalizer");
  chained_data->resuscitating_p = 0;
  chained_data->proc = proc;
  chained_data->data = data;
  GC_REGISTER_FINALIZER_NO_ORDER (obj, chained_finalizer, chained_data,
                                  &chained_data->prev,
                                  &chained_data->prev_data);
  shuffle_resuscitators_to_front (chained_data);
}




static SCM finalizer_async_cell;

static SCM
run_finalizers_async_thunk (void)
{
  finalization_count += GC_invoke_finalizers ();
  return SCM_UNSPECIFIED;
}


/* The function queue_after_gc_hook is run by the scm_before_gc_c_hook
 * at the end of the garbage collection.  The only purpose of this
 * function is to mark the after_gc_async (which will eventually lead to
 * the execution of the after_gc_async_thunk).
 */
static void
queue_finalizer_async (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  static scm_i_pthread_mutex_t lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

  scm_i_pthread_mutex_lock (&lock);
  if (scm_is_false (SCM_CDR (finalizer_async_cell)))
    {
      SCM_SETCDR (finalizer_async_cell, t->active_asyncs);
      t->active_asyncs = finalizer_async_cell;
      t->pending_asyncs = 1;
    }
  scm_i_pthread_mutex_unlock (&lock);
}




#if SCM_USE_PTHREAD_THREADS

static int finalization_pipe[2];
static scm_i_pthread_mutex_t finalization_thread_lock =
  SCM_I_PTHREAD_MUTEX_INITIALIZER;
static SCM finalization_thread = SCM_BOOL_F;

static void
notify_finalizers_to_run (void)
{
  char byte = 0;
  full_write (finalization_pipe[1], &byte, 1);
}

static void
notify_about_to_fork (void)
{
  char byte = 1;
  full_write (finalization_pipe[1], &byte, 1);
}

struct finalization_pipe_data
{
  char byte;
  ssize_t n;
  int err;
};

static void*
read_finalization_pipe_data (void *data)
{
  struct finalization_pipe_data *fdata = data;
  
  fdata->n = read (finalization_pipe[0], &fdata->byte, 1);
  fdata->err = errno;

  return NULL;
}
  
static SCM
finalization_thread_proc (void *unused)
{
  while (1)
    {
      struct finalization_pipe_data data;

      scm_without_guile (read_finalization_pipe_data, &data);
      
      if (data.n <= 0 && data.err != EINTR) 
        {
          perror ("error in finalization delivery thread");
          return SCM_UNSPECIFIED;
        }

      switch (data.byte)
        {
        case 0:
          finalization_count += GC_invoke_finalizers ();
          break;
        case 1:
          return SCM_UNSPECIFIED;
        default:
          abort ();
        }
    }
}

static void
start_finalization_thread (void)
{
  scm_i_pthread_mutex_lock (&finalization_thread_lock);
  if (scm_is_false (finalization_thread))
    finalization_thread = scm_spawn_thread (finalization_thread_proc, NULL,
                                            scm_handle_by_message,
                                            "finalization thread");
  scm_i_pthread_mutex_unlock (&finalization_thread_lock);
}

static void
stop_finalization_thread (void)
{
  scm_i_pthread_mutex_lock (&finalization_thread_lock);
  if (scm_is_true (finalization_thread))
    {
      notify_about_to_fork ();
      scm_join_thread (finalization_thread);
      finalization_thread = SCM_BOOL_F;
    }
  scm_i_pthread_mutex_unlock (&finalization_thread_lock);
}

static void
spawn_finalizer_thread (void)
{
  GC_set_finalizer_notifier (notify_finalizers_to_run);
  start_finalization_thread ();
}

#endif /* SCM_USE_PTHREAD_THREADS */




void
scm_i_finalizer_pre_fork (void)
{
#if SCM_USE_PTHREAD_THREADS
  stop_finalization_thread ();
  GC_set_finalizer_notifier (spawn_finalizer_thread);
#endif
}




void
scm_init_finalizers (void)
{
  /* When the async is to run, the cdr of the pair gets set to the
     asyncs queue of the current thread.  */
  finalizer_async_cell =
    scm_cons (scm_c_make_gsubr ("%run-finalizers", 0, 0, 0,
                                run_finalizers_async_thunk),
              SCM_BOOL_F);
  GC_set_finalizer_notifier (queue_finalizer_async);
}

void
scm_init_finalizer_thread (void)
{
#if SCM_USE_PTHREAD_THREADS
  if (pipe2 (finalization_pipe, O_CLOEXEC) != 0)
    scm_syserror (NULL);
  GC_set_finalizer_notifier (spawn_finalizer_thread);
#endif
}
