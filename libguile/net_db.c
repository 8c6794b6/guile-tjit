/* "net_db.c" network database support
 * Copyright (C) 1995,1996,1997,1998,1999,2000,2001 Free Software Foundation, Inc.
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



/* Written in 1994 by Aubrey Jaffer.
 * Thanks to Hallvard.Tretteberg@si.sintef.no for inspiration and discussion.
 * Rewritten by Gary Houston to be a closer interface to the C socket library.
 * Split into net_db.c and socket.c.
 */


#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/net_db.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#if !defined (HAVE_H_ERRNO) && !defined (__MINGW32__)
/* h_errno not found in netdb.h, maybe this will help.  */
extern int h_errno;
#endif



SCM_SYMBOL (scm_host_not_found_key, "host-not-found");
SCM_SYMBOL (scm_try_again_key, "try-again");
SCM_SYMBOL (scm_no_recovery_key, "no-recovery");
SCM_SYMBOL (scm_no_data_key, "no-data");

static void scm_resolv_error (const char *subr, SCM bad_value)
{
#ifdef NETDB_INTERNAL
  if (h_errno == NETDB_INTERNAL)
    {
      /* errno supposedly contains a useful value.  */
      scm_syserror (subr);
    }
  else
#endif
    {
      SCM key;
      const char *errmsg;

      switch (h_errno)
	{
	case HOST_NOT_FOUND:
	  key = scm_host_not_found_key;
	  errmsg = "Unknown host"; 
	  break;
	case TRY_AGAIN:	
	  key = scm_try_again_key;
	  errmsg = "Host name lookup failure";
	  break;
	case NO_RECOVERY:
	  key = scm_no_recovery_key;
	  errmsg = "Unknown server error"; 
	  break;
	case NO_DATA:
	  key = scm_no_data_key;
	  errmsg = "No address associated with name";
	  break;
	default:
	  scm_misc_error (subr, "Unknown resolver error", SCM_EOL);
	  errmsg = NULL;
	}

#ifdef HAVE_HSTRERROR
      errmsg = (const char *) hstrerror (h_errno);
#endif
      scm_error (key, subr, errmsg, scm_cons (bad_value, SCM_EOL), SCM_EOL);
    }
}

/* Should take an extra arg for address format (will be needed for IPv6).
   Should use reentrant facilities if available.
 */

SCM_DEFINE (scm_gethost, "gethost", 0, 1, 0, 
            (SCM host),
	    "@deffnx procedure gethostbyname hostname\n"
	    "@deffnx procedure gethostbyaddr address\n"
	    "Look up a host by name or address, returning a host object.  The\n"
	    "@code{gethost} procedure will accept either a string name or an integer\n"
	    "address; if given no arguments, it behaves like @code{gethostent} (see\n"
	    "below).  If a name or address is supplied but the address can not be\n"
	    "found, an error will be thrown to one of the keys:\n"
	    "@code{host-not-found}, @code{try-again}, @code{no-recovery} or\n"
	    "@code{no-data}, corresponding to the equivalent @code{h_error} values.\n"
	    "Unusual conditions may result in errors thrown to the\n"
	    "@code{system-error} or @code{misc_error} keys.")
#define FUNC_NAME s_scm_gethost
{
  SCM ans = scm_c_make_vector (5, SCM_UNSPECIFIED);
  SCM *ve = SCM_VELTS (ans);
  SCM lst = SCM_EOL;
  struct hostent *entry;
  struct in_addr inad;
  char **argv;
  int i = 0;
  if (SCM_UNBNDP (host))
    {
#ifdef HAVE_GETHOSTENT
      entry = gethostent ();
#else
      entry = NULL;
#endif
      if (! entry)
	{
	  /* As far as I can tell, there's no good way to tell whether
             zero means an error or end-of-file.  The trick of
             clearing errno before calling gethostent and checking it
             afterwards doesn't cut it, because, on Linux, it seems to
             try to contact some other server (YP?) and fails, which
             is a benign failure.  */
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_STRINGP (host))
    {
      SCM_STRING_COERCE_0TERMINATION_X (host);
      entry = gethostbyname (SCM_STRING_CHARS (host));
    }
  else
    {
      inad.s_addr = htonl (SCM_NUM2ULONG (1, host));
      entry = gethostbyaddr ((char *) &inad, sizeof (inad), AF_INET);
    }
  if (!entry)
    scm_resolv_error (FUNC_NAME, host);
  
  ve[0] = scm_mem2string (entry->h_name, strlen (entry->h_name));
  ve[1] = scm_makfromstrs (-1, entry->h_aliases);
  ve[2] = SCM_MAKINUM (entry->h_addrtype + 0L);
  ve[3] = SCM_MAKINUM (entry->h_length + 0L);
  if (sizeof (struct in_addr) != entry->h_length)
    {
      ve[4] = SCM_BOOL_F;
      return ans;
    }
  for (argv = entry->h_addr_list; argv[i]; i++);
  while (i--)
    {
      inad = *(struct in_addr *) argv[i];
      lst = scm_cons (scm_ulong2num (ntohl (inad.s_addr)), lst);
    }
  ve[4] = lst;
  return ans;
}
#undef FUNC_NAME


/* In all subsequent getMUMBLE functions, when we're called with no
   arguments, we're supposed to traverse the tables entry by entry.
   However, there doesn't seem to be any documented way to distinguish
   between end-of-table and an error; in both cases the functions
   return zero.  Gotta love Unix.  For the time being, we clear errno,
   and if we get a zero and errno is set, we signal an error.  This
   doesn't seem quite right (what if errno gets set as part of healthy
   operation?), but it seems to work okay.  We'll see.  */

#if defined(HAVE_GETNETENT) && defined(HAVE_GETNETBYNAME) && defined(HAVE_GETNETBYADDR)
SCM_DEFINE (scm_getnet, "getnet", 0, 1, 0, 
            (SCM net),
	    "@deffnx procedure getnetbyname net-name\n"
	    "@deffnx procedure getnetbyaddr net-number\n"
	    "Look up a network by name or net number in the network database.  The\n"
	    "@var{net-name} argument must be a string, and the @var{net-number}\n"
	    "argument must be an integer.  @code{getnet} will accept either type of\n"
	    "argument, behaving like @code{getnetent} (see below) if no arguments are\n"
	    "given.")
#define FUNC_NAME s_scm_getnet
{
  SCM ans;
  SCM *ve;
  struct netent *entry;

  ans = scm_c_make_vector (4, SCM_UNSPECIFIED);
  ve = SCM_VELTS (ans);
  if (SCM_UNBNDP (net))
    {
      entry = getnetent ();
      if (! entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_STRINGP (net))
    {
      SCM_STRING_COERCE_0TERMINATION_X (net);
      entry = getnetbyname (SCM_STRING_CHARS (net));
    }
  else
    {
      unsigned long netnum;
      netnum = SCM_NUM2ULONG (1, net);
      entry = getnetbyaddr (netnum, AF_INET);
    }
  if (!entry)
    SCM_SYSERROR_MSG ("no such network ~A", scm_list_1 (net), errno);
  ve[0] = scm_mem2string (entry->n_name, strlen (entry->n_name));
  ve[1] = scm_makfromstrs (-1, entry->n_aliases);
  ve[2] = SCM_MAKINUM (entry->n_addrtype + 0L);
  ve[3] = scm_ulong2num (entry->n_net + 0L);
  return ans;
}
#undef FUNC_NAME
#endif

#ifdef HAVE_GETPROTOENT
SCM_DEFINE (scm_getproto, "getproto", 0, 1, 0, 
            (SCM protocol),
	    "@deffnx procedure getprotobyname name\n"
	    "@deffnx procedure getprotobynumber number\n"
	    "Look up a network protocol by name or by number.  @code{getprotobyname}\n"
	    "takes a string argument, and @code{getprotobynumber} takes an integer\n"
	    "argument.  @code{getproto} will accept either type, behaving like\n"
	    "@code{getprotoent} (see below) if no arguments are supplied.")
#define FUNC_NAME s_scm_getproto
{
  SCM ans;
  SCM *ve;
  struct protoent *entry;

  ans = scm_c_make_vector (3, SCM_UNSPECIFIED);
  ve = SCM_VELTS (ans);
  if (SCM_UNBNDP (protocol))
    {
      entry = getprotoent ();
      if (! entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
    }
  else if (SCM_STRINGP (protocol))
    {
      SCM_STRING_COERCE_0TERMINATION_X (protocol);
      entry = getprotobyname (SCM_STRING_CHARS (protocol));
    }
  else
    {
      unsigned long protonum;
      protonum = SCM_NUM2ULONG (1, protocol);
      entry = getprotobynumber (protonum);
    }
  if (!entry)
    SCM_SYSERROR_MSG ("no such protocol ~A", scm_list_1 (protocol), errno);
  ve[0] = scm_mem2string (entry->p_name, strlen (entry->p_name));
  ve[1] = scm_makfromstrs (-1, entry->p_aliases);
  ve[2] = SCM_MAKINUM (entry->p_proto + 0L);
  return ans;
}
#undef FUNC_NAME
#endif

static SCM
scm_return_entry (struct servent *entry)
{
  SCM ans;
  SCM *ve;

  ans = scm_c_make_vector (4, SCM_UNSPECIFIED);
  ve = SCM_VELTS (ans);
  ve[0] = scm_mem2string (entry->s_name, strlen (entry->s_name));
  ve[1] = scm_makfromstrs (-1, entry->s_aliases);
  ve[2] = SCM_MAKINUM (ntohs (entry->s_port) + 0L);
  ve[3] = scm_mem2string (entry->s_proto, strlen (entry->s_proto));
  return ans;
}

#ifdef HAVE_GETSERVENT
SCM_DEFINE (scm_getserv, "getserv", 0, 2, 0,
            (SCM name, SCM protocol),
	    "@deffnx procedure getservbyname name protocol\n"
	    "@deffnx procedure getservbyport port protocol\n"
	    "Look up a network service by name or by service number, and return a\n"
	    "network service object.  The @var{protocol} argument specifies the name\n"
	    "of the desired protocol; if the protocol found in the network service\n"
	    "database does not match this name, a system error is signalled.\n\n"
	    "The @code{getserv} procedure will take either a service name or number\n"
	    "as its first argument; if given no arguments, it behaves like\n"
	    "@code{getservent} (see below).")
#define FUNC_NAME s_scm_getserv
{
  struct servent *entry;
  if (SCM_UNBNDP (name))
    {
      entry = getservent ();
      if (!entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
      return scm_return_entry (entry);
    }
  SCM_VALIDATE_STRING (2, protocol);
  SCM_STRING_COERCE_0TERMINATION_X (protocol);
  if (SCM_STRINGP (name))
    {
      SCM_STRING_COERCE_0TERMINATION_X (name);
      entry = getservbyname (SCM_STRING_CHARS (name), SCM_STRING_CHARS (protocol));
    }
  else
    {
      SCM_VALIDATE_INUM (1,name);
      entry = getservbyport (htons (SCM_INUM (name)), SCM_STRING_CHARS (protocol));
    }
  if (!entry)
    SCM_SYSERROR_MSG("no such service ~A", scm_list_1 (name), errno);
  return scm_return_entry (entry);
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETHOSTENT) && defined(HAVE_ENDHOSTENT)
SCM_DEFINE (scm_sethost, "sethost", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endhostent}.\n"
	    "Otherwise it is equivalent to @code{sethostent stayopen}.")
#define FUNC_NAME s_scm_sethost
{
  if (SCM_UNBNDP (stayopen))
    endhostent ();
  else
    sethostent (!SCM_FALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETNETENT) && defined(HAVE_ENDNETENT) 
SCM_DEFINE (scm_setnet, "setnet", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endnetent}.\n"
	    "Otherwise it is equivalent to @code{setnetent stayopen}.")
#define FUNC_NAME s_scm_setnet
{
  if (SCM_UNBNDP (stayopen))
    endnetent ();
  else
    setnetent (!SCM_FALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETPROTOENT) && defined(HAVE_ENDPROTOENT)
SCM_DEFINE (scm_setproto, "setproto", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endprotoent}.\n"
	    "Otherwise it is equivalent to @code{setprotoent stayopen}.")
#define FUNC_NAME s_scm_setproto
{
  if (SCM_UNBNDP (stayopen))
    endprotoent ();
  else
    setprotoent (!SCM_FALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETSERVENT) && defined(HAVE_ENDSERVENT)
SCM_DEFINE (scm_setserv, "setserv", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endservent}.\n"
	    "Otherwise it is equivalent to @code{setservent stayopen}.")
#define FUNC_NAME s_scm_setserv
{
  if (SCM_UNBNDP (stayopen))
    endservent ();
  else
    setservent (!SCM_FALSEP (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


void 
scm_init_net_db ()
{
  scm_add_feature ("net-db");
#ifndef SCM_MAGIC_SNARFER
#include "libguile/net_db.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
