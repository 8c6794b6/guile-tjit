/* classes: h_files */

#ifndef SOCKETH
#define SOCKETH
/*	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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







extern SCM scm_socket SCM_P ((SCM family, SCM style, SCM proto));
extern SCM scm_socketpair SCM_P ((SCM family, SCM style, SCM proto));
extern SCM scm_getsockopt SCM_P ((SCM sfd, SCM level, SCM optname));
extern SCM scm_setsockopt SCM_P ((SCM sfd, SCM level, SCM optname, SCM value));
extern SCM scm_shutdown SCM_P ((SCM sfd, SCM how));
extern SCM scm_connect SCM_P ((SCM sockfd, SCM fam, SCM address, SCM args));
extern SCM scm_bind SCM_P ((SCM sockfd, SCM fam, SCM address, SCM args));
extern SCM scm_listen SCM_P ((SCM sfd, SCM backlog));
extern SCM scm_accept SCM_P ((SCM sockfd));
extern SCM scm_getsockname SCM_P ((SCM sockfd));
extern SCM scm_getpeername SCM_P ((SCM sockfd));
extern SCM scm_recv SCM_P ((SCM sockfd, SCM buff_or_size, SCM flags));
extern SCM scm_send SCM_P ((SCM sockfd, SCM message, SCM flags));
extern SCM scm_recvfrom SCM_P ((SCM sockfd, SCM buff_or_size, SCM flags, SCM offset, SCM length));
extern SCM scm_sendto SCM_P ((SCM sockfd, SCM message, SCM fam, SCM address, SCM args_and_flags));
extern void scm_init_socket SCM_P ((void));

#endif  /* SOCKETH */
