/* Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2006,
 *   2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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



/* This file contains POSIX file system access procedures.  Procedures
   essential to the compiler and run-time (`stat', `canonicalize-path',
   etc.) are compiled even with `--disable-posix'.  */


/* See stime.c for comments on why _POSIX_C_SOURCE is not always defined. */
#define _LARGEFILE64_SOURCE      /* ask for stat64 etc */
#ifdef __hpux
#define _POSIX_C_SOURCE 199506L  /* for readdir_r */
#endif

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <alloca.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/smob.h"
#include "libguile/feature.h"
#include "libguile/fports.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/filesys.h"


#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef LIBC_H_WITH_UNISTD_H
#include <libc.h>
#endif

#include <sys/select.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <dirent.h>

#define NAMLEN(dirent)  strlen ((dirent)->d_name)

#ifdef HAVE_SYS_SENDFILE_H
# include <sys/sendfile.h>
#endif

/* Glibc's `sendfile' function.  */
#define sendfile_or_sendfile64			\
  CHOOSE_LARGEFILE (sendfile, sendfile64)

#include <full-read.h>
#include <full-write.h>


/* Some more definitions for the native Windows port. */
#ifdef __MINGW32__
# define fsync(fd) _commit (fd)
#endif /* __MINGW32__ */




/* Two helper macros for an often used pattern */

#define STRING_SYSCALL(str,cstr,code)        \
  do {                                       \
    int eno;                                 \
    char *cstr = scm_to_locale_string (str); \
    SCM_SYSCALL (code);                      \
    eno = errno; free (cstr); errno = eno;   \
  } while (0)

#define STRING2_SYSCALL(str1,cstr1,str2,cstr2,code)  \
  do {                                               \
    int eno;                                         \
    char *cstr1, *cstr2;                             \
    scm_dynwind_begin (0);                             \
    cstr1 = scm_to_locale_string (str1);             \
    scm_dynwind_free (cstr1);                          \
    cstr2 = scm_to_locale_string (str2);             \
    scm_dynwind_free (cstr2);                          \
    SCM_SYSCALL (code);                              \
    eno = errno; scm_dynwind_end (); errno = eno;      \
  } while (0)


#define MAX(A, B) ((A) > (B) ? (A) : (B))
#define MIN(A, B) ((A) < (B) ? (A) : (B))



#ifdef HAVE_POSIX

/* {Permissions}
 */

#ifdef HAVE_CHOWN
SCM_DEFINE (scm_chown, "chown", 3, 0, 0, 
            (SCM object, SCM owner, SCM group),
	    "Change the ownership and group of the file referred to by @var{object} to\n"
	    "the integer values @var{owner} and @var{group}.  @var{object} can be\n"
	    "a string containing a file name or, if the platform\n"
	    "supports fchown, a port or integer file descriptor\n"
	    "which is open on the file.  The return value\n"
	    "is unspecified.\n\n"
	    "If @var{object} is a symbolic link, either the\n"
	    "ownership of the link or the ownership of the referenced file will be\n"
	    "changed depending on the operating system (lchown is\n"
	    "unsupported at present).  If @var{owner} or @var{group} is specified\n"
	    "as @code{-1}, then that ID is not changed.")
#define FUNC_NAME s_scm_chown
{
  int rv;

  object = SCM_COERCE_OUTPORT (object);

#ifdef HAVE_FCHOWN
  if (scm_is_integer (object) || (SCM_OPFPORTP (object)))
    {
      int fdes = (SCM_OPFPORTP (object)?
		  SCM_FPORT_FDES (object) : scm_to_int (object));

      SCM_SYSCALL (rv = fchown (fdes, scm_to_int (owner), scm_to_int (group)));
    }
  else
#endif
    {
      STRING_SYSCALL (object, c_object,
		      rv = chown (c_object,
				  scm_to_int (owner), scm_to_int (group)));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_CHOWN */



SCM_DEFINE (scm_open_fdes, "open-fdes", 2, 1, 0, 
            (SCM path, SCM flags, SCM mode),
	    "Similar to @code{open} but return a file descriptor instead of\n"
	    "a port.")
#define FUNC_NAME s_scm_open_fdes
{
  int fd;
  int iflags;
  int imode;

  iflags = SCM_NUM2INT (2, flags);
  imode = SCM_NUM2INT_DEF (3, mode, 0666);
  STRING_SYSCALL (path, c_path, fd = open_or_open64 (c_path, iflags, imode));
  if (fd == -1)
    SCM_SYSERROR;
  return scm_from_int (fd);
}
#undef FUNC_NAME

SCM_DEFINE (scm_open, "open", 2, 1, 0, 
            (SCM path, SCM flags, SCM mode),
	    "Open the file named by @var{path} for reading and/or writing.\n"
	    "@var{flags} is an integer specifying how the file should be opened.\n"
	    "@var{mode} is an integer specifying the permission bits of the file, if\n"
	    "it needs to be created, before the umask is applied.  The default is 666\n"
	    "(Unix itself has no default).\n\n"
	    "@var{flags} can be constructed by combining variables using @code{logior}.\n"
	    "Basic flags are:\n\n"
	    "@defvar O_RDONLY\n"
	    "Open the file read-only.\n"
	    "@end defvar\n"
	    "@defvar O_WRONLY\n"
	    "Open the file write-only.\n"
	    "@end defvar\n"
	    "@defvar O_RDWR\n"
	    "Open the file read/write.\n"
	    "@end defvar\n"
	    "@defvar O_APPEND\n"
	    "Append to the file instead of truncating.\n"
	    "@end defvar\n"
	    "@defvar O_CREAT\n"
	    "Create the file if it does not already exist.\n"
	    "@end defvar\n\n"
	    "See the Unix documentation of the @code{open} system call\n"
	    "for additional flags.")
#define FUNC_NAME s_scm_open
{
  SCM newpt;
  char *port_mode;
  int fd;
  int iflags;

  fd = scm_to_int (scm_open_fdes (path, flags, mode));
  iflags = SCM_NUM2INT (2, flags);

  if ((iflags & O_RDWR) == O_RDWR)
    {
      /* Opened read-write.  */
      if (iflags & O_APPEND)
	port_mode = "a+";
      else if (iflags & O_CREAT)
	port_mode = "w+";
      else
	port_mode = "r+";
    }
  else
    {
      /* Opened read-only or write-only.  */
      if (iflags & O_APPEND)
	port_mode = "a";
      else if (iflags & O_WRONLY)
	port_mode = "w";
      else
	port_mode = "r";
    }

  newpt = scm_fdes_to_port (fd, port_mode, path);
  return newpt;
}
#undef FUNC_NAME

SCM_DEFINE (scm_close, "close", 1, 0, 0, 
            (SCM fd_or_port),
	    "Similar to close-port (@pxref{Closing, close-port}),\n"
	    "but also works on file descriptors.  A side\n"
	    "effect of closing a file descriptor is that any ports using that file\n"
	    "descriptor are moved to a different file descriptor and have\n"
	    "their revealed counts set to zero.")
#define FUNC_NAME s_scm_close
{
  int rv;
  int fd;

  fd_or_port = SCM_COERCE_OUTPORT (fd_or_port);

  if (SCM_PORTP (fd_or_port))
    return scm_close_port (fd_or_port);
  fd = scm_to_int (fd_or_port);
  scm_evict_ports (fd);		/* see scsh manual.  */
  SCM_SYSCALL (rv = close (fd));
  /* following scsh, closing an already closed file descriptor is
     not an error.  */
  if (rv < 0 && errno != EBADF)
    SCM_SYSERROR;
  return scm_from_bool (rv >= 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_close_fdes, "close-fdes", 1, 0, 0, 
            (SCM fd),
	    "A simple wrapper for the @code{close} system call.\n"
	    "Close file descriptor @var{fd}, which must be an integer.\n"
	    "Unlike close (@pxref{Ports and File Descriptors, close}),\n"
	    "the file descriptor will be closed even if a port is using it.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_close_fdes
{
  int c_fd;
  int rv;

  c_fd = scm_to_int (fd);
  SCM_SYSCALL (rv = close (c_fd));
  if (rv < 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif /* HAVE_POSIX */


/* {Files}
 */

SCM_SYMBOL (scm_sym_regular, "regular");
SCM_SYMBOL (scm_sym_directory, "directory");
#ifdef S_ISLNK
SCM_SYMBOL (scm_sym_symlink, "symlink");
#endif
SCM_SYMBOL (scm_sym_block_special, "block-special");
SCM_SYMBOL (scm_sym_char_special, "char-special");
SCM_SYMBOL (scm_sym_fifo, "fifo");
SCM_SYMBOL (scm_sym_sock, "socket");
SCM_SYMBOL (scm_sym_unknown, "unknown");

static SCM 
scm_stat2scm (struct stat_or_stat64 *stat_temp)
{
  SCM ans = scm_c_make_vector (18, SCM_UNSPECIFIED);
  
  SCM_SIMPLE_VECTOR_SET(ans, 0, scm_from_ulong (stat_temp->st_dev));
  SCM_SIMPLE_VECTOR_SET(ans, 1, scm_from_ino_t_or_ino64_t (stat_temp->st_ino));
  SCM_SIMPLE_VECTOR_SET(ans, 2, scm_from_ulong (stat_temp->st_mode));
  SCM_SIMPLE_VECTOR_SET(ans, 3, scm_from_ulong (stat_temp->st_nlink));
  SCM_SIMPLE_VECTOR_SET(ans, 4, scm_from_ulong (stat_temp->st_uid));
  SCM_SIMPLE_VECTOR_SET(ans, 5, scm_from_ulong (stat_temp->st_gid));
#ifdef HAVE_STRUCT_STAT_ST_RDEV
  SCM_SIMPLE_VECTOR_SET(ans, 6, scm_from_ulong (stat_temp->st_rdev));
#else
  SCM_SIMPLE_VECTOR_SET(ans, 6, SCM_BOOL_F);
#endif
  SCM_SIMPLE_VECTOR_SET(ans, 7, scm_from_off_t_or_off64_t (stat_temp->st_size));
  SCM_SIMPLE_VECTOR_SET(ans, 8, scm_from_ulong (stat_temp->st_atime));
  SCM_SIMPLE_VECTOR_SET(ans, 9, scm_from_ulong (stat_temp->st_mtime));
  SCM_SIMPLE_VECTOR_SET(ans, 10, scm_from_ulong (stat_temp->st_ctime));
#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
  SCM_SIMPLE_VECTOR_SET(ans, 11, scm_from_ulong (stat_temp->st_blksize));
#else
  SCM_SIMPLE_VECTOR_SET(ans, 11, scm_from_ulong (4096L));
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLOCKS
  SCM_SIMPLE_VECTOR_SET(ans, 12, scm_from_blkcnt_t_or_blkcnt64_t (stat_temp->st_blocks));
#else
  SCM_SIMPLE_VECTOR_SET(ans, 12, SCM_BOOL_F);
#endif
  {
    int mode = stat_temp->st_mode;
    
    if (S_ISREG (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_regular);
    else if (S_ISDIR (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_directory);
#ifdef S_ISLNK
    /* systems without symlinks probably don't have S_ISLNK */
    else if (S_ISLNK (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_symlink);
#endif
    else if (S_ISBLK (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_block_special);
    else if (S_ISCHR (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_char_special);
    else if (S_ISFIFO (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_fifo);
#ifdef S_ISSOCK
    else if (S_ISSOCK (mode))
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_sock);
#endif
    else
      SCM_SIMPLE_VECTOR_SET(ans, 13, scm_sym_unknown);

    SCM_SIMPLE_VECTOR_SET(ans, 14, scm_from_int ((~S_IFMT) & mode));

    /* the layout of the bits in ve[14] is intended to be portable.
       If there are systems that don't follow the usual convention,
       the following could be used:

       tmp = 0;
       if (S_ISUID & mode) tmp += 1;
       tmp <<= 1;
       if (S_IRGRP & mode) tmp += 1;
       tmp <<= 1;
       if (S_ISVTX & mode) tmp += 1;
       tmp <<= 1;
       if (S_IRUSR & mode) tmp += 1;
       tmp <<= 1;
       if (S_IWUSR & mode) tmp += 1;
       tmp <<= 1;
       if (S_IXUSR & mode) tmp += 1;
       tmp <<= 1;
       if (S_IWGRP & mode) tmp += 1;
       tmp <<= 1;
       if (S_IXGRP & mode) tmp += 1;
       tmp <<= 1;
       if (S_IROTH & mode) tmp += 1;
       tmp <<= 1;
       if (S_IWOTH & mode) tmp += 1;
       tmp <<= 1;
       if (S_IXOTH & mode) tmp += 1; 

       SCM_SIMPLE_VECTOR_SET(ans, 14, scm_from_int (tmp));
       
       */
  }  
#ifdef HAVE_STRUCT_STAT_ST_ATIM
  SCM_SIMPLE_VECTOR_SET(ans, 15, scm_from_long (stat_temp->st_atim.tv_nsec));
#else
  SCM_SIMPLE_VECTOR_SET(ans, 15, SCM_I_MAKINUM (0));
#endif
#ifdef HAVE_STRUCT_STAT_ST_MTIM
  SCM_SIMPLE_VECTOR_SET(ans, 16, scm_from_long (stat_temp->st_mtim.tv_nsec));
#else
  SCM_SIMPLE_VECTOR_SET(ans, 16, SCM_I_MAKINUM (0));
#endif
#ifdef HAVE_STRUCT_STAT_ST_CTIM
  SCM_SIMPLE_VECTOR_SET(ans, 17, scm_from_ulong (stat_temp->st_ctim.tv_sec));
#else
  SCM_SIMPLE_VECTOR_SET(ans, 17, SCM_I_MAKINUM (0));
#endif

  return ans;
}

static int
is_file_name_separator (SCM c)
{
  if (scm_is_eq (c, SCM_MAKE_CHAR ('/')))
    return 1;
#ifdef __MINGW32__
  if (scm_is_eq (c, SCM_MAKE_CHAR ('\\')))
    return 1;
#endif
  return 0;
}

SCM_DEFINE (scm_stat, "stat", 1, 1, 0, 
            (SCM object, SCM exception_on_error),
	    "Return an object containing various information about the file\n"
	    "determined by @var{object}.  @var{object} can be a string containing\n"
	    "a file name or a port or integer file descriptor which is open\n"
	    "on a file (in which case @code{fstat} is used as the underlying\n"
	    "system call).\n"
	    "\n"
            "If the optional @var{exception_on_error} argument is true, which\n"
            "is the default, an exception will be raised if the underlying\n"
            "system call returns an error, for example if the file is not\n"
            "found or is not readable. Otherwise, an error will cause\n"
            "@code{stat} to return @code{#f}."
	    "\n"
	    "The object returned by a successful call to @code{stat} can be\n"
            "passed as a single parameter to the following procedures, all of\n"
            "which return integers:\n"
	    "\n"
	    "@table @code\n"
	    "@item stat:dev\n"
	    "The device containing the file.\n"
	    "@item stat:ino\n"
	    "The file serial number, which distinguishes this file from all\n"
	    "other files on the same device.\n"
	    "@item stat:mode\n"
	    "The mode of the file.  This includes file type information and\n"
	    "the file permission bits.  See @code{stat:type} and\n"
	    "@code{stat:perms} below.\n"
	    "@item stat:nlink\n"
	    "The number of hard links to the file.\n"
	    "@item stat:uid\n"
	    "The user ID of the file's owner.\n"
	    "@item stat:gid\n"
	    "The group ID of the file.\n"
	    "@item stat:rdev\n"
	    "Device ID; this entry is defined only for character or block\n"
	    "special files.\n"
	    "@item stat:size\n"
	    "The size of a regular file in bytes.\n"
	    "@item stat:atime\n"
	    "The last access time for the file.\n"
	    "@item stat:mtime\n"
	    "The last modification time for the file.\n"
	    "@item stat:ctime\n"
	    "The last modification time for the attributes of the file.\n"
	    "@item stat:blksize\n"
	    "The optimal block size for reading or writing the file, in\n"
	    "bytes.\n"
	    "@item stat:blocks\n"
	    "The amount of disk space that the file occupies measured in\n"
	    "units of 512 byte blocks.\n"
	    "@end table\n"
	    "\n"
	    "In addition, the following procedures return the information\n"
	    "from stat:mode in a more convenient form:\n"
	    "\n"
	    "@table @code\n"
	    "@item stat:type\n"
	    "A symbol representing the type of file.  Possible values are\n"
	    "regular, directory, symlink, block-special, char-special, fifo,\n"
	    "socket and unknown\n"
	    "@item stat:perms\n"
	    "An integer representing the access permission bits.\n"
	    "@end table")
#define FUNC_NAME s_scm_stat
{
  int rv;
  int fdes;
  struct stat_or_stat64 stat_temp;

  if (scm_is_integer (object))
    {
      SCM_SYSCALL (rv = fstat_or_fstat64 (scm_to_int (object), &stat_temp));
    }
  else if (scm_is_string (object))
    {
      char *file = scm_to_locale_string (object);
      SCM_SYSCALL (rv = stat_or_stat64 (file, &stat_temp));
      free (file);
    }
  else
    {
      object = SCM_COERCE_OUTPORT (object);
      SCM_VALIDATE_OPFPORT (1, object);
      fdes = SCM_FPORT_FDES (object);
      SCM_SYSCALL (rv = fstat_or_fstat64 (fdes, &stat_temp));
    }

  if (rv == -1)
    {
      if (SCM_UNBNDP (exception_on_error) || scm_is_true (exception_on_error))
        {
          int en = errno;
          SCM_SYSERROR_MSG ("~A: ~S",
                            scm_list_2 (scm_strerror (scm_from_int (en)),
                                        object),
                            en);
        }
      else
        return SCM_BOOL_F;
    }
  return scm_stat2scm (&stat_temp);
}
#undef FUNC_NAME

#ifdef HAVE_LSTAT
SCM_DEFINE (scm_lstat, "lstat", 1, 0, 0, 
            (SCM str),
	    "Similar to @code{stat}, but does not follow symbolic links, i.e.,\n"
	    "it will return information about a symbolic link itself, not the\n"
	    "file it points to.  @var{str} must be a string.")
#define FUNC_NAME s_scm_lstat
{
  int rv;
  struct stat_or_stat64 stat_temp;

  STRING_SYSCALL (str, c_str, rv = lstat_or_lstat64 (c_str, &stat_temp));
  if (rv != 0)
    {
      int en = errno;

      SCM_SYSERROR_MSG ("~A: ~S",
			scm_list_2 (scm_strerror (scm_from_int (en)), str),
			en);
    }
  return scm_stat2scm (&stat_temp);
}
#undef FUNC_NAME
#endif /* HAVE_LSTAT */


#ifdef HAVE_POSIX

/* {Modifying Directories}
 */

#ifdef HAVE_LINK
SCM_DEFINE (scm_link, "link", 2, 0, 0,
            (SCM oldpath, SCM newpath),
	    "Creates a new name @var{newpath} in the file system for the\n"
	    "file named by @var{oldpath}.  If @var{oldpath} is a symbolic\n"
	    "link, the link may or may not be followed depending on the\n"
	    "system.")
#define FUNC_NAME s_scm_link
{
  int val;

  STRING2_SYSCALL (oldpath, c_oldpath,
		   newpath, c_newpath,
		   val = link (c_oldpath, c_newpath));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_LINK */


/* {Navigating Directories}
 */


SCM_DEFINE (scm_chdir, "chdir", 1, 0, 0, 
            (SCM str),
	    "Change the current working directory to @var{str}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_chdir
{
  int ans;

  STRING_SYSCALL (str, c_str, ans = chdir (c_str));
  if (ans != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* check that element is a port or file descriptor.  if it's a port
   and its buffer is ready for use, add it to the ports_ready list.
   otherwise add its file descriptor to *set.  the type of list can be
   determined from pos: SCM_ARG1 for reads, SCM_ARG2 for writes,
   SCM_ARG3 for excepts.  */
static int
set_element (fd_set *set, SCM *ports_ready, SCM element, int pos)
{
  int fd;

  if (scm_is_integer (element))
    {
      fd = scm_to_int (element);
    }
  else
    {
      int use_buf = 0;

      element = SCM_COERCE_OUTPORT (element);
      SCM_ASSERT (SCM_OPFPORTP (element), element, pos, "select");
      if (pos == SCM_ARG1)
	{
	  /* check whether port has buffered input.  */
	  scm_t_port *pt = SCM_PTAB_ENTRY (element);
      
	  if (pt->read_pos < pt->read_end)
	    use_buf = 1;
	}
      else if (pos == SCM_ARG2)
	{
	  /* check whether port's output buffer has room.  */
	  scm_t_port *pt = SCM_PTAB_ENTRY (element);

	  /* > 1 since writing the last byte in the buffer causes flush.  */
	  if (pt->write_end - pt->write_pos > 1)
	    use_buf = 1;
	}
      fd = use_buf ? -1 : SCM_FPORT_FDES (element);
    }
  if (fd == -1)
    *ports_ready = scm_cons (element, *ports_ready);
  else
    FD_SET (fd, set);
  return fd;
}

/* check list_or_vec, a list or vector of ports or file descriptors,
   adding each member to either the ports_ready list (if it's a port
   with a usable buffer) or to *set.  the kind of list_or_vec can be
   determined from pos: SCM_ARG1 for reads, SCM_ARG2 for writes,
   SCM_ARG3 for excepts.  */
static int
fill_select_type (fd_set *set, SCM *ports_ready, SCM list_or_vec, int pos)
{
  int max_fd = 0;

  if (scm_is_vector (list_or_vec))
    {
      int i = SCM_SIMPLE_VECTOR_LENGTH (list_or_vec);
      
      while (--i >= 0)
	{
	  int fd = set_element (set, ports_ready,
				SCM_SIMPLE_VECTOR_REF (list_or_vec, i), pos);

	  if (fd > max_fd)
	    max_fd = fd;
	}
    }
  else
    {
      while (!SCM_NULL_OR_NIL_P (list_or_vec))
	{
	  int fd = set_element (set, ports_ready, SCM_CAR (list_or_vec), pos);

	  if (fd > max_fd)
	    max_fd = fd;
	  list_or_vec = SCM_CDR (list_or_vec);
	}
    }

  return max_fd;
}

/* if element (a file descriptor or port) appears in *set, cons it to
   list.  return list.  */
static SCM
get_element (fd_set *set, SCM element, SCM list)
{
  int fd;

  if (scm_is_integer (element))
    {
      fd = scm_to_int (element);
    }
  else
    {
      fd = SCM_FPORT_FDES (SCM_COERCE_OUTPORT (element));
    }
  if (FD_ISSET (fd, set))
    list = scm_cons (element, list);
  return list;
}

/* construct component of scm_select return value.
   set: pointer to set of file descriptors found by select to be ready
   ports_ready: ports ready due to buffering
   list_or_vec: original list/vector handed to scm_select.
   the return value is a list/vector of ready ports/file descriptors. 
   works by finding the objects in list which correspond to members of
   *set and appending them to ports_ready.  result is converted to a
   vector if list_or_vec is a vector.  */
static SCM 
retrieve_select_type (fd_set *set, SCM ports_ready, SCM list_or_vec)
{
  SCM answer_list = ports_ready;

  if (scm_is_vector (list_or_vec))
    {
      int i = SCM_SIMPLE_VECTOR_LENGTH (list_or_vec);

      while (--i >= 0)
	{
	  answer_list = get_element (set,
				     SCM_SIMPLE_VECTOR_REF (list_or_vec, i),
				     answer_list);
	}
      return scm_vector (answer_list);
    }
  else
    {
      /* list_or_vec must be a list.  */
      while (!SCM_NULL_OR_NIL_P (list_or_vec))
	{
	  answer_list = get_element (set, SCM_CAR (list_or_vec), answer_list);
	  list_or_vec = SCM_CDR (list_or_vec);
	}
      return answer_list;
    }
}

/* Static helper functions above refer to s_scm_select directly as s_select */
SCM_DEFINE (scm_select, "select", 3, 2, 0, 
            (SCM reads, SCM writes, SCM excepts, SCM secs, SCM usecs),
	    "This procedure has a variety of uses: waiting for the ability\n"
	    "to provide input, accept output, or the existence of\n"
	    "exceptional conditions on a collection of ports or file\n"
	    "descriptors, or waiting for a timeout to occur.\n"
	    "It also returns if interrupted by a signal.\n\n"
	    "@var{reads}, @var{writes} and @var{excepts} can be lists or\n"
	    "vectors, with each member a port or a file descriptor.\n"
	    "The value returned is a list of three corresponding\n"
	    "lists or vectors containing only the members which meet the\n"
	    "specified requirement.  The ability of port buffers to\n"
	    "provide input or accept output is taken into account.\n"
	    "Ordering of the input lists or vectors is not preserved.\n\n"
	    "The optional arguments @var{secs} and @var{usecs} specify the\n"
	    "timeout.  Either @var{secs} can be specified alone, as\n"
	    "either an integer or a real number, or both @var{secs} and\n"
	    "@var{usecs} can be specified as integers, in which case\n"
	    "@var{usecs} is an additional timeout expressed in\n"
	    "microseconds.  If @var{secs} is omitted or is @code{#f} then\n"
	    "select will wait for as long as it takes for one of the other\n"
	    "conditions to be satisfied.\n\n"
	    "The scsh version of @code{select} differs as follows:\n"
	    "Only vectors are accepted for the first three arguments.\n"
	    "The @var{usecs} argument is not supported.\n"
	    "Multiple values are returned instead of a list.\n"
	    "Duplicates in the input vectors appear only once in output.\n"
	    "An additional @code{select!} interface is provided.")
#define FUNC_NAME s_scm_select
{
  struct timeval timeout;
  struct timeval * time_ptr;
  fd_set read_set;
  fd_set write_set;
  fd_set except_set;
  int read_count;
  int write_count;
  int except_count;
  /* these lists accumulate ports which are ready due to buffering.
     their file descriptors don't need to be added to the select sets.  */
  SCM read_ports_ready = SCM_EOL;
  SCM write_ports_ready = SCM_EOL;
  int max_fd;

  if (scm_is_vector (reads))
    {
      read_count = SCM_SIMPLE_VECTOR_LENGTH (reads);
    }
  else
    {
      read_count = scm_ilength (reads);
      SCM_ASSERT (read_count >= 0, reads, SCM_ARG1, FUNC_NAME);
    }
  if (scm_is_vector (writes))
    {
      write_count = SCM_SIMPLE_VECTOR_LENGTH (writes);
    }
  else
    {
      write_count = scm_ilength (writes);
      SCM_ASSERT (write_count >= 0, writes, SCM_ARG2, FUNC_NAME);
    }
  if (scm_is_vector (excepts))
    {
      except_count = SCM_SIMPLE_VECTOR_LENGTH (excepts);
    }
  else
    {
      except_count = scm_ilength (excepts);
      SCM_ASSERT (except_count >= 0, excepts, SCM_ARG3, FUNC_NAME);
    }

  FD_ZERO (&read_set);
  FD_ZERO (&write_set);
  FD_ZERO (&except_set);

  max_fd = fill_select_type (&read_set, &read_ports_ready, reads, SCM_ARG1);

  {
    int write_max = fill_select_type (&write_set, &write_ports_ready, 
				      writes, SCM_ARG2);
    int except_max = fill_select_type (&except_set, NULL,
				       excepts, SCM_ARG3);

    if (write_max > max_fd)
      max_fd = write_max;
    if (except_max > max_fd)
      max_fd = except_max;
  }

  /* if there's a port with a ready buffer, don't block, just
     check for ready file descriptors.  */
  if (!scm_is_null (read_ports_ready) || !scm_is_null (write_ports_ready))
    {
      timeout.tv_sec = 0;
      timeout.tv_usec = 0;
      time_ptr = &timeout;
    }
  else if (SCM_UNBNDP (secs) || scm_is_false (secs))
    time_ptr = 0;
  else
    {
      if (scm_is_unsigned_integer (secs, 0, ULONG_MAX))
	{
	  timeout.tv_sec = scm_to_ulong (secs);
	  if (SCM_UNBNDP (usecs))
	    timeout.tv_usec = 0;
	  else
	    timeout.tv_usec = scm_to_long (usecs);
	}
      else
	{
	  double fl = scm_to_double (secs);

	  if (!SCM_UNBNDP (usecs))
	    SCM_WRONG_TYPE_ARG (4, secs);
	  if (fl > LONG_MAX)
	    SCM_OUT_OF_RANGE (4, secs);
	  timeout.tv_sec = (long) fl;
	  timeout.tv_usec = (long) ((fl - timeout.tv_sec) * 1000000);
	}
      time_ptr = &timeout;
    }

  {
    int rv = select (max_fd + 1,
                     &read_set, &write_set, &except_set,
                     time_ptr);
    if (rv < 0)
      SCM_SYSERROR;
  }
  return scm_list_3 (retrieve_select_type (&read_set, read_ports_ready, reads),
		     retrieve_select_type (&write_set, write_ports_ready, writes),
		     retrieve_select_type (&except_set, SCM_EOL, excepts));
}
#undef FUNC_NAME



#ifdef HAVE_FCNTL
SCM_DEFINE (scm_fcntl, "fcntl", 2, 1, 0,
            (SCM object, SCM cmd, SCM value),
	    "Apply @var{cmd} to the specified file descriptor or the underlying\n"
	    "file descriptor of the specified port.  @var{value} is an optional\n"
	    "integer argument.\n\n"
	    "Values for @var{cmd} are:\n\n"
	    "@table @code\n"
	    "@item F_DUPFD\n"
	    "Duplicate a file descriptor\n"
	    "@item F_GETFD\n"
	    "Get flags associated with the file descriptor.\n"
	    "@item F_SETFD\n"
	    "Set flags associated with the file descriptor to @var{value}.\n"
	    "@item F_GETFL\n"
	    "Get flags associated with the open file.\n"
	    "@item F_SETFL\n"
	    "Set flags associated with the open file to @var{value}\n"
	    "@item F_GETOWN\n"
	    "Get the process ID of a socket's owner, for @code{SIGIO} signals.\n"
	    "@item F_SETOWN\n"
	    "Set the process that owns a socket to @var{value}, for @code{SIGIO} signals.\n"
	    "@item FD_CLOEXEC\n"
	    "The value used to indicate the \"close on exec\" flag with @code{F_GETFL} or\n"
	    "@code{F_SETFL}.\n"
	    "@end table")
#define FUNC_NAME s_scm_fcntl
{
  int rv;
  int fdes;
  int ivalue;

  object = SCM_COERCE_OUTPORT (object);

  if (SCM_OPFPORTP (object))
    fdes = SCM_FPORT_FDES (object);
  else
    fdes = scm_to_int (object);

  if (SCM_UNBNDP (value))
    ivalue = 0;
  else
    ivalue = scm_to_int (value);

  SCM_SYSCALL (rv = fcntl (fdes, scm_to_int (cmd), ivalue));
  if (rv == -1)
    SCM_SYSERROR;
  return scm_from_int (rv);
}
#undef FUNC_NAME
#endif /* HAVE_FCNTL */

SCM_DEFINE (scm_fsync, "fsync", 1, 0, 0, 
            (SCM object),
	    "Copies any unwritten data for the specified output file\n"
	    "descriptor to disk.  If @var{object} is a port, its buffer is\n"
	    "flushed before the underlying file descriptor is fsync'd.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_fsync
{
  int fdes;

  object = SCM_COERCE_OUTPORT (object);

  if (SCM_OPFPORTP (object))
    {
      scm_flush_unlocked (object);
      fdes = SCM_FPORT_FDES (object);
    }
  else
    fdes = scm_to_int (object);

  if (fsync (fdes) == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef HAVE_SYMLINK
SCM_DEFINE (scm_symlink, "symlink", 2, 0, 0,
            (SCM oldpath, SCM newpath),
	    "Create a symbolic link named @var{oldpath} with the value\n"
	    "(i.e., pointing to) @var{newpath}.  The return value is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_symlink
{
  int val;

  STRING2_SYSCALL (oldpath, c_oldpath,
		   newpath, c_newpath,
		   val = symlink (c_oldpath, c_newpath));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_SYMLINK */

#ifdef HAVE_READLINK
SCM_DEFINE (scm_readlink, "readlink", 1, 0, 0, 
            (SCM path),
	    "Return the value of the symbolic link named by @var{path} (a\n"
	    "string), i.e., the file that the link points to.")
#define FUNC_NAME s_scm_readlink
{
  int rv;
  int size = 100;
  char *buf;
  SCM result;
  char *c_path;
  
  scm_dynwind_begin (0);

  c_path = scm_to_locale_string (path);
  scm_dynwind_free (c_path);

  buf = scm_malloc (size);

  while ((rv = readlink (c_path, buf, size)) == size)
    {
      free (buf);
      size *= 2;
      buf = scm_malloc (size);
    }
  if (rv == -1)
    {
      int save_errno = errno;
      free (buf);
      errno = save_errno;
      SCM_SYSERROR;
    }
  result = scm_take_locale_stringn (buf, rv);

  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_READLINK */

SCM_DEFINE (scm_copy_file, "copy-file", 2, 0, 0,
            (SCM oldfile, SCM newfile),
	    "Copy the file specified by @var{oldfile} to @var{newfile}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_copy_file
{
  char *c_oldfile, *c_newfile;
  int oldfd, newfd;
  int n, rv;
  char buf[BUFSIZ];
  struct stat_or_stat64 oldstat;

  scm_dynwind_begin (0);
  
  c_oldfile = scm_to_locale_string (oldfile);
  scm_dynwind_free (c_oldfile);
  c_newfile = scm_to_locale_string (newfile);
  scm_dynwind_free (c_newfile);

  oldfd = open_or_open64 (c_oldfile, O_RDONLY | O_BINARY);
  if (oldfd == -1)
    SCM_SYSERROR;

  SCM_SYSCALL (rv = fstat_or_fstat64 (oldfd, &oldstat));
  if (rv == -1)
    goto err_close_oldfd;

  /* use POSIX flags instead of 07777?.  */
  newfd = open_or_open64 (c_newfile, O_WRONLY | O_CREAT | O_TRUNC,
                          oldstat.st_mode & 07777);
  if (newfd == -1)
    {
    err_close_oldfd:
      close (oldfd);
      SCM_SYSERROR;
    }

  while ((n = read (oldfd, buf, sizeof buf)) > 0)
    if (write (newfd, buf, n) != n)
      {
	close (oldfd);
	close (newfd);
	SCM_SYSERROR;
      }
  close (oldfd);
  if (close (newfd) == -1)
    SCM_SYSERROR;

  scm_dynwind_end ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sendfile, "sendfile", 3, 1, 0,
	    (SCM out, SCM in, SCM count, SCM offset),
	    "Send @var{count} bytes from @var{in} to @var{out}, both of which "
	    "must be either open file ports or file descriptors.  When "
	    "@var{offset} is omitted, start reading from @var{in}'s current "
	    "position; otherwise, start reading at @var{offset}.  Return "
	    "the number of bytes actually sent.")
#define FUNC_NAME s_scm_sendfile
{
#define VALIDATE_FD_OR_PORT(cvar, svar, pos)	\
  if (scm_is_integer (svar))			\
    cvar = scm_to_int (svar);			\
  else						\
    {						\
      SCM_VALIDATE_OPFPORT (pos, svar);		\
      scm_flush (svar);				\
      cvar = SCM_FPORT_FDES (svar);		\
    }

  ssize_t result SCM_UNUSED;
  size_t c_count, total = 0;
  scm_t_off c_offset;
  int in_fd, out_fd;

  VALIDATE_FD_OR_PORT (out_fd, out, 1);
  VALIDATE_FD_OR_PORT (in_fd, in, 2);
  c_count = scm_to_size_t (count);
  c_offset = SCM_UNBNDP (offset) ? 0 : scm_to_off_t (offset);

#if defined HAVE_SYS_SENDFILE_H && defined HAVE_SENDFILE
  /* The Linux-style sendfile(2), which is different from the BSD-style.  */

  {
    off_t *offset_ptr;

    offset_ptr = SCM_UNBNDP (offset) ? NULL : &c_offset;

    /* On Linux, when OUT_FD is a file, everything is transferred at once and
       RESULT == C_COUNT.  However, when OUT_FD is a pipe or other "slow"
       device, fewer bytes may be transferred, hence the loop.  RESULT == 0
       means EOF on IN_FD, so leave the loop in that case.  */
    do
      {
	result = sendfile_or_sendfile64 (out_fd, in_fd, offset_ptr,
					 c_count - total);
	if (result > 0)
	  /* At this point, either OFFSET_PTR is non-NULL and it has been
	     updated to the current offset in IN_FD, or it is NULL and IN_FD's
	     offset has been updated.  */
	  total += result;
	else if (result < 0 && (errno == EINTR || errno == EAGAIN))
	  /* Keep going.  */
	  result = 1;
      }
    while (total < c_count && result > 0);
  }

  /* Quoting the Linux man page: "In Linux kernels before 2.6.33, out_fd
     must refer to a socket.  Since Linux 2.6.33 it can be any file."
     Fall back to read(2) and write(2) when such an error occurs.  */
  if (result < 0 && errno != EINVAL && errno != ENOSYS)
    SCM_SYSERROR;
  else if (result < 0)
#endif
  {
    char buf[8192];
    size_t left;
    int reached_eof = 0;

    if (!SCM_UNBNDP (offset))
      {
	if (SCM_PORTP (in))
	  scm_seek (in, scm_from_off_t (c_offset), scm_from_int (SEEK_SET));
	else
	  {
	    if (lseek_or_lseek64 (in_fd, c_offset, SEEK_SET) < 0)
	      SCM_SYSERROR;
	  }
      }

    for (total = 0, left = c_count; total < c_count && !reached_eof; )
      {
	size_t asked, obtained, written;

	asked = MIN (sizeof buf, left);
	obtained = full_read (in_fd, buf, asked);
	if (obtained < asked)
          {
            if (errno == 0)
              reached_eof = 1;
            else
              SCM_SYSERROR;
          }

	left -= obtained;

	written = full_write (out_fd, buf, obtained);
	if (written < obtained)
	  SCM_SYSERROR;

	total += written;
      }

  }

  return scm_from_size_t (total);

#undef VALIDATE_FD_OR_PORT
}
#undef FUNC_NAME

#endif /* HAVE_POSIX */


/* Essential procedures used in (system base compile).  */

#ifdef HAVE_GETCWD
SCM_DEFINE (scm_getcwd, "getcwd", 0, 0, 0,
            (),
	    "Return the name of the current working directory.")
#define FUNC_NAME s_scm_getcwd
{
  char *rv;
  size_t size = 100;
  char *wd;
  SCM result;

  wd = scm_malloc (size);
  while ((rv = getcwd (wd, size)) == 0 && errno == ERANGE)
    {
      free (wd);
      size *= 2;
      wd = scm_malloc (size);
    }
  if (rv == 0)
    {
      int save_errno = errno;
      free (wd);
      errno = save_errno;
      SCM_SYSERROR;
    }
  result = scm_from_locale_stringn (wd, strlen (wd));
  free (wd);
  return result;
}
#undef FUNC_NAME
#endif /* HAVE_GETCWD */

#ifdef HAVE_MKDIR
SCM_DEFINE (scm_mkdir, "mkdir", 1, 1, 0,
            (SCM path, SCM mode),
	    "Create a new directory named by @var{path}.  If @var{mode} is omitted\n"
	    "then the permissions of the directory file are set using the current\n"
	    "umask.  Otherwise they are set to the decimal value specified with\n"
	    "@var{mode}.  The return value is unspecified.")
#define FUNC_NAME s_scm_mkdir
{
  int rv;
  mode_t mask;

  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
      STRING_SYSCALL (path, c_path, rv = mkdir (c_path, 0777 ^ mask));
    }
  else
    {
      STRING_SYSCALL (path, c_path, rv = mkdir (c_path, scm_to_uint (mode)));
    }
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_MKDIR */

#ifdef HAVE_RMDIR
SCM_DEFINE (scm_rmdir, "rmdir", 1, 0, 0, 
            (SCM path),
	    "Remove the existing directory named by @var{path}.  The directory must\n"
	    "be empty for this to succeed.  The return value is unspecified.")
#define FUNC_NAME s_scm_rmdir
{
  int val;

  STRING_SYSCALL (path, c_path, val = rmdir (c_path));
  if (val != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#ifdef HAVE_RENAME
#define my_rename rename
#else
static int
my_rename (const char *oldname, const char *newname)
{
  int rv;

  SCM_SYSCALL (rv = link (oldname, newname));
  if (rv == 0)
    {
      SCM_SYSCALL (rv = unlink (oldname));
      if (rv != 0)
	/* unlink failed.  remove new name */
	SCM_SYSCALL (unlink (newname)); 
    }
  return rv;
}
#endif

SCM_DEFINE (scm_rename, "rename-file", 2, 0, 0,
            (SCM oldname, SCM newname),
	    "Renames the file specified by @var{oldname} to @var{newname}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_rename
{
  int rv;

  STRING2_SYSCALL (oldname, c_oldname,
		   newname, c_newname,
		   rv = my_rename (c_oldname, c_newname));
  if (rv != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delete_file, "delete-file", 1, 0, 0, 
           (SCM str),
	    "Deletes (or \"unlinks\") the file specified by @var{str}.")
#define FUNC_NAME s_scm_delete_file
{
  int ans;
  STRING_SYSCALL (str, c_str, ans = unlink (c_str));
  if (ans != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_access, "access?", 2, 0, 0,
            (SCM path, SCM how),
	    "Test accessibility of a file under the real UID and GID of the\n"
	    "calling process.  The return is @code{#t} if @var{path} exists\n"
	    "and the permissions requested by @var{how} are all allowed, or\n"
	    "@code{#f} if not.\n"
	    "\n"
	    "@var{how} is an integer which is one of the following values,\n"
	    "or a bitwise-OR (@code{logior}) of multiple values.\n"
	    "\n"
	    "@defvar R_OK\n"
	    "Test for read permission.\n"
	    "@end defvar\n"
	    "@defvar W_OK\n"
	    "Test for write permission.\n"
	    "@end defvar\n"
	    "@defvar X_OK\n"
	    "Test for execute permission.\n"
	    "@end defvar\n"
	    "@defvar F_OK\n"
	    "Test for existence of the file.  This is implied by each of the\n"
	    "other tests, so there's no need to combine it with them.\n"
	    "@end defvar\n"
	    "\n"
	    "It's important to note that @code{access?} does not simply\n"
	    "indicate what will happen on attempting to read or write a\n"
	    "file.  In normal circumstances it does, but in a set-UID or\n"
	    "set-GID program it doesn't because @code{access?} tests the\n"
	    "real ID, whereas an open or execute attempt uses the effective\n"
	    "ID.\n"
	    "\n"
	    "A program which will never run set-UID/GID can ignore the\n"
	    "difference between real and effective IDs, but for maximum\n"
	    "generality, especially in library functions, it's best not to\n"
	    "use @code{access?} to predict the result of an open or execute,\n"
	    "instead simply attempt that and catch any exception.\n"
	    "\n"
	    "The main use for @code{access?} is to let a set-UID/GID program\n"
	    "determine what the invoking user would have been allowed to do,\n"
	    "without the greater (or perhaps lesser) privileges afforded by\n"
	    "the effective ID.  For more on this, see ``Testing File\n"
	    "Access'' in The GNU C Library Reference Manual.")
#define FUNC_NAME s_scm_access
{
  int rv;
  char *c_path;

  c_path = scm_to_locale_string (path);
  rv = access (c_path, scm_to_int (how));
  free (c_path);

  return scm_from_bool (!rv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_chmod, "chmod", 2, 0, 0,
            (SCM object, SCM mode),
	    "Changes the permissions of the file referred to by\n"
	    "@var{object}.  @var{object} can be a string containing a file\n"
	    "name or a port or integer file descriptor which is open on a\n"
	    "file (in which case @code{fchmod} is used as the underlying\n"
	    "system call).  @var{mode} specifies the new permissions as a\n"
	    "decimal number, e.g., @code{(chmod \"foo\" #o755)}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_chmod
{
  int rv;

  object = SCM_COERCE_OUTPORT (object);

#if HAVE_FCHMOD
  if (scm_is_integer (object) || SCM_OPFPORTP (object))
    {
      int fdes;
      if (scm_is_integer (object))
	fdes = scm_to_int (object);
      else
	fdes = SCM_FPORT_FDES (object);
      SCM_SYSCALL (rv = fchmod (fdes, scm_to_int (mode)));
    }
  else
#endif
    {
      STRING_SYSCALL (object, c_object,
		      rv = chmod (c_object, scm_to_int (mode)));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_umask, "umask", 0, 1, 0, 
            (SCM mode),
	    "If @var{mode} is omitted, returns a decimal number representing the current\n"
	    "file creation mask.  Otherwise the file creation mask is set to\n"
	    "@var{mode} and the previous value is returned.\n\n"
	    "E.g., @code{(umask #o022)} sets the mask to octal 22, decimal 18.")
#define FUNC_NAME s_scm_umask
{
  mode_t mask;
  if (SCM_UNBNDP (mode))
    {
      mask = umask (0);
      umask (mask);
    }
  else
    {
      mask = umask (scm_to_uint (mode));
    }
  return scm_from_uint (mask);
}
#undef FUNC_NAME

#ifndef HAVE_MKSTEMP
extern int mkstemp (char *);
#endif

SCM_DEFINE (scm_mkstemp, "mkstemp!", 1, 0, 0,
	    (SCM tmpl),
	    "Create a new unique file in the file system and return a new\n"
	    "buffered port open for reading and writing to the file.\n"
	    "\n"
	    "@var{tmpl} is a string specifying where the file should be\n"
	    "created: it must end with @samp{XXXXXX} and those @samp{X}s\n"
	    "will be changed in the string to return the name of the file.\n"
	    "(@code{port-filename} on the port also gives the name.)\n"
	    "\n"
	    "POSIX doesn't specify the permissions mode of the file, on GNU\n"
	    "and most systems it's @code{#o600}.  An application can use\n"
	    "@code{chmod} to relax that if desired.  For example\n"
	    "@code{#o666} less @code{umask}, which is usual for ordinary\n"
	    "file creation,\n"
	    "\n"
	    "@example\n"
	    "(let ((port (mkstemp! (string-copy \"/tmp/myfile-XXXXXX\"))))\n"
	    "  (chmod port (logand #o666 (lognot (umask))))\n"
	    "  ...)\n"
	    "@end example")
#define FUNC_NAME s_scm_mkstemp
{
  char *c_tmpl;
  int rv;

  scm_dynwind_begin (0);

  c_tmpl = scm_to_locale_string (tmpl);
  scm_dynwind_free (c_tmpl);

  SCM_SYSCALL (rv = mkstemp (c_tmpl));
  if (rv == -1)
    SCM_SYSERROR;

  scm_substring_move_x (scm_from_locale_string (c_tmpl),
			SCM_INUM0, scm_string_length (tmpl),
			tmpl, SCM_INUM0);

  scm_dynwind_end ();
  return scm_fdes_to_port (rv, "w+", tmpl);
}
#undef FUNC_NAME


/* Filename manipulation */

SCM scm_dot_string;

#ifdef __MINGW32__
SCM_SYMBOL (sym_file_name_convention, "windows");
#else
SCM_SYMBOL (sym_file_name_convention, "posix");
#endif

SCM_INTERNAL SCM scm_system_file_name_convention (void);

SCM_DEFINE (scm_system_file_name_convention,
            "system-file-name-convention", 0, 0, 0, (void),
	    "Return either @code{posix} or @code{windows}, depending on\n"
            "what kind of system this Guile is running on.")
#define FUNC_NAME s_scm_system_file_name_convention
{
  return sym_file_name_convention;
}
#undef FUNC_NAME

SCM_DEFINE (scm_dirname, "dirname", 1, 0, 0, 
            (SCM filename),
	    "Return the directory name component of the file name\n"
	    "@var{filename}. If @var{filename} does not contain a directory\n"
	    "component, @code{.} is returned.")
#define FUNC_NAME s_scm_dirname
{
  long int i;
  unsigned long int len;

  SCM_VALIDATE_STRING (1, filename);

  len = scm_i_string_length (filename);

  i = len - 1;

  while (i >= 0 && is_file_name_separator (scm_c_string_ref (filename, i)))
    --i;
  while (i >= 0 && !is_file_name_separator (scm_c_string_ref (filename, i)))
    --i;
  while (i >= 0 && is_file_name_separator (scm_c_string_ref (filename, i)))
    --i;

  if (i < 0)
    {
      if (len > 0 && is_file_name_separator (scm_c_string_ref (filename, 0)))
	return scm_c_substring (filename, 0, 1);
      else
	return scm_dot_string;
    }
  else
    return scm_c_substring (filename, 0, i + 1);
}
#undef FUNC_NAME

SCM_DEFINE (scm_basename, "basename", 1, 1, 0, 
            (SCM filename, SCM suffix),
	    "Return the base name of the file name @var{filename}. The\n"
	    "base name is the file name without any directory components.\n"
	    "If @var{suffix} is provided, and is equal to the end of\n"
	    "@var{filename}, it is removed also.")
#define FUNC_NAME s_scm_basename
{
  int i, j, len, end;

  SCM_VALIDATE_STRING (1, filename);
  len = scm_i_string_length (filename);

  if (SCM_UNBNDP (suffix))
    j = -1;
  else
    {
      SCM_VALIDATE_STRING (2, suffix);
      j = scm_i_string_length (suffix) - 1;
    }
  i = len - 1;
  while (i >= 0 && is_file_name_separator (scm_c_string_ref (filename, i)))
    --i;
  end = i;
  while (i >= 0 && j >= 0 
	 && (scm_i_string_ref (filename, i)
	     == scm_i_string_ref (suffix, j)))
    {
      --i;
      --j;
    }
  if (j == -1)
    end = i;
  while (i >= 0 && !is_file_name_separator (scm_c_string_ref (filename, i)))
    --i;
  if (i == end)
    {
      if (len > 0 && is_file_name_separator (scm_c_string_ref (filename, 0)))
        return scm_c_substring (filename, 0, 1);
      else
	return scm_dot_string;
    }
  else
    return scm_c_substring (filename, i+1, end+1);
}
#undef FUNC_NAME

SCM_DEFINE (scm_canonicalize_path, "canonicalize-path", 1, 0, 0, 
            (SCM path),
	    "Return the canonical path of @var{path}. A canonical path has\n"
            "no @code{.} or @code{..} components, nor any repeated path\n"
            "separators (@code{/}) nor symlinks.\n\n"
            "Raises an error if any component of @var{path} does not exist.")
#define FUNC_NAME s_scm_canonicalize_path
{
  char *str, *canon;
  
  SCM_VALIDATE_STRING (1, path);

  str = scm_to_locale_string (path);
  canon = canonicalize_file_name (str);
  free (str);
  
  if (canon)
    return scm_take_locale_string (canon);
  else
    SCM_SYSERROR;
}
#undef FUNC_NAME

SCM
scm_i_relativize_path (SCM path, SCM in_path)
{
  char *str, *canon;
  SCM scanon;
  
  str = scm_to_locale_string (path);
  canon = canonicalize_file_name (str);
  free (str);
  
  if (!canon)
    return SCM_BOOL_F;

  scanon = scm_take_locale_string (canon);

  for (; scm_is_pair (in_path); in_path = scm_cdr (in_path))
    {
      SCM dir = scm_car (in_path);
      size_t len = scm_c_string_length (dir);

      /* When DIR is empty, it means "current working directory".  We
	 could set DIR to (getcwd) in that case, but then the
	 canonicalization would depend on the current directory, which
	 is not what we want in the context of `compile-file', for
	 instance.  */
      if (len > 0
	  && scm_is_true (scm_string_prefix_p (dir, scanon,
					       SCM_UNDEFINED, SCM_UNDEFINED,
					       SCM_UNDEFINED, SCM_UNDEFINED)))
	{
	  /* DIR either has a trailing delimiter or doesn't.  SCANON
	     will be delimited by single delimiters.  When DIR does not
	     have a trailing delimiter, add one to the length to strip
	     off the delimiter within SCANON.  */
	  if (!is_file_name_separator (scm_c_string_ref (dir, len - 1)))
	    len++;

	  if (scm_c_string_length (scanon) > len)
	    return scm_substring (scanon, scm_from_size_t (len), SCM_UNDEFINED);
	  else
	    return SCM_BOOL_F;
	}
    }

  return SCM_BOOL_F;
}


/* Examining directories.  These procedures are used by `check-guile'
   and thus compiled unconditionally.  */

scm_t_bits scm_tc16_dir;


SCM_DEFINE (scm_directory_stream_p, "directory-stream?", 1, 0, 0,
	    (SCM obj),
	    "Return a boolean indicating whether @var{obj} is a directory\n"
	    "stream as returned by @code{opendir}.")
#define FUNC_NAME s_scm_directory_stream_p
{
  return scm_from_bool (SCM_DIRP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_opendir, "opendir", 1, 0, 0,
	    (SCM dirname),
	    "Open the directory specified by @var{dirname} and return a directory\n"
	    "stream.")
#define FUNC_NAME s_scm_opendir
{
  DIR *ds;
  STRING_SYSCALL (dirname, c_dirname, ds = opendir (c_dirname));
  if (ds == NULL)
    SCM_SYSERROR;
  SCM_RETURN_NEWSMOB (scm_tc16_dir | (SCM_DIR_FLAG_OPEN<<16), ds);
}
#undef FUNC_NAME


/* FIXME: The glibc manual has a portability note that readdir_r may not
   null-terminate its return string.  The circumstances outlined for this
   are not clear, nor is it clear what should be done about it.  Lets use
   NAMLEN and worry about what else should be done if/when someone can
   figure it out.  */

SCM_DEFINE (scm_readdir, "readdir", 1, 0, 0,
	    (SCM port),
	    "Return (as a string) the next directory entry from the directory stream\n"
	    "@var{port}.  If there is no remaining entry to be read then the\n"
	    "end of file object is returned.")
#define FUNC_NAME s_scm_readdir
{
  struct dirent_or_dirent64 *rdent;

  SCM_VALIDATE_DIR (1, port);
  if (!SCM_DIR_OPEN_P (port))
    SCM_MISC_ERROR ("Directory ~S is not open.", scm_list_1 (port));

#if HAVE_READDIR_R
  /* As noted in the glibc manual, on various systems (such as Solaris)
     the d_name[] field is only 1 char and you're expected to size the
     dirent buffer for readdir_r based on NAME_MAX.  The MAX expressions
     below effectively give either sizeof(d_name) or NAME_MAX+1,
     whichever is bigger.

     On solaris 10 there's no NAME_MAX constant, it's necessary to use
     pathconf().  We prefer NAME_MAX though, since it should be a constant
     and will therefore save a system call.  We also prefer it since dirfd()
     is not available everywhere.

     An alternative to dirfd() would be to open() the directory and then use
     fdopendir(), if the latter is available.  That'd let us hold the fd
     somewhere in the smob, or just the dirent size calculated once.  */
  {
    struct dirent_or_dirent64 de; /* just for sizeof */
    DIR    *ds = (DIR *) SCM_SMOB_DATA_1 (port);
#ifdef NAME_MAX
    char   buf [MAX (sizeof (de),
                     sizeof (de) - sizeof (de.d_name) + NAME_MAX + 1)];
#else
    char   *buf;
    long   name_max = fpathconf (dirfd (ds), _PC_NAME_MAX);
    if (name_max == -1)
      SCM_SYSERROR;
    buf = alloca (MAX (sizeof (de),
                       sizeof (de) - sizeof (de.d_name) + name_max + 1));
#endif

    errno = 0;
    SCM_SYSCALL (readdir_r_or_readdir64_r (ds, (struct dirent_or_dirent64 *) buf, &rdent));
    if (errno != 0)
      SCM_SYSERROR;
    if (! rdent)
      return SCM_EOF_VAL;

    return (rdent ? scm_from_locale_stringn (rdent->d_name, NAMLEN (rdent))
	    : SCM_EOF_VAL);
  }
#else
  {
    SCM ret;
    scm_dynwind_begin (0);
    scm_i_dynwind_pthread_mutex_lock (&scm_i_misc_mutex);

    errno = 0;
    SCM_SYSCALL (rdent = readdir_or_readdir64 ((DIR *) SCM_SMOB_DATA_1 (port)));
    if (errno != 0)
      SCM_SYSERROR;

    ret = (rdent ? scm_from_locale_stringn (rdent->d_name, NAMLEN (rdent))
	   : SCM_EOF_VAL);

    scm_dynwind_end ();
    return ret;
  }
#endif
}
#undef FUNC_NAME


SCM_DEFINE (scm_rewinddir, "rewinddir", 1, 0, 0,
	    (SCM port),
	    "Reset the directory port @var{port} so that the next call to\n"
	    "@code{readdir} will return the first directory entry.")
#define FUNC_NAME s_scm_rewinddir
{
  SCM_VALIDATE_DIR (1, port);
  if (!SCM_DIR_OPEN_P (port))
    SCM_MISC_ERROR ("Directory ~S is not open.", scm_list_1 (port));

  rewinddir ((DIR *) SCM_SMOB_DATA_1 (port));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_closedir, "closedir", 1, 0, 0,
	    (SCM port),
	    "Close the directory stream @var{port}.\n"
	    "The return value is unspecified.")
#define FUNC_NAME s_scm_closedir
{
  SCM_VALIDATE_DIR (1, port);

  if (SCM_DIR_OPEN_P (port))
    {
      int sts;

      SCM_SYSCALL (sts = closedir ((DIR *) SCM_SMOB_DATA_1 (port)));
      if (sts != 0)
	SCM_SYSERROR;

      SCM_SET_SMOB_DATA_0 (port, scm_tc16_dir);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#ifdef HAVE_POSIX
static int
scm_dir_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts_unlocked ("#<", port);
  if (!SCM_DIR_OPEN_P (exp))
    scm_puts_unlocked ("closed: ", port);
  scm_puts_unlocked ("directory stream ", port);
  scm_uintprint (SCM_SMOB_DATA_1 (exp), 16, port);
  scm_putc_unlocked ('>', port);
  return 1;
}


static size_t
scm_dir_free (SCM p)
{
  if (SCM_DIR_OPEN_P (p))
    closedir ((DIR *) SCM_SMOB_DATA_1 (p));
  return 0;
}
#endif



void
scm_init_filesys ()
{
#ifdef HAVE_POSIX
  scm_tc16_dir = scm_make_smob_type ("directory", 0);
  scm_set_smob_free (scm_tc16_dir, scm_dir_free);
  scm_set_smob_print (scm_tc16_dir, scm_dir_print);

#ifdef O_RDONLY
  scm_c_define ("O_RDONLY", scm_from_int (O_RDONLY));
#endif 	       
#ifdef O_WRONLY
  scm_c_define ("O_WRONLY", scm_from_int (O_WRONLY));
#endif 	       
#ifdef O_RDWR
  scm_c_define ("O_RDWR", scm_from_int (O_RDWR));
#endif 	       
#ifdef O_CREAT
  scm_c_define ("O_CREAT", scm_from_int (O_CREAT));
#endif 	       
#ifdef O_EXCL  
  scm_c_define ("O_EXCL", scm_from_int (O_EXCL));
#endif 	       
#ifdef O_NOCTTY
  scm_c_define ("O_NOCTTY", scm_from_int (O_NOCTTY));
#endif 	       
#ifdef O_TRUNC 
  scm_c_define ("O_TRUNC", scm_from_int (O_TRUNC));
#endif 	       
#ifdef O_APPEND
  scm_c_define ("O_APPEND", scm_from_int (O_APPEND));
#endif 	       
#ifdef O_NONBLOCK
  scm_c_define ("O_NONBLOCK", scm_from_int (O_NONBLOCK));
#endif 	       
#ifdef O_NDELAY
  scm_c_define ("O_NDELAY", scm_from_int (O_NDELAY));
#endif 	       
#ifdef O_SYNC  
  scm_c_define ("O_SYNC", scm_from_int (O_SYNC));
#endif 
#ifdef O_LARGEFILE  
  scm_c_define ("O_LARGEFILE", scm_from_int (O_LARGEFILE));
#endif
#ifdef O_NOTRANS
  scm_c_define ("O_NOTRANS", scm_from_int (O_NOTRANS));
#endif

#ifdef F_DUPFD  
  scm_c_define ("F_DUPFD", scm_from_int (F_DUPFD));
#endif 
#ifdef F_GETFD  
  scm_c_define ("F_GETFD", scm_from_int (F_GETFD));
#endif 
#ifdef F_SETFD  
  scm_c_define ("F_SETFD", scm_from_int (F_SETFD));
#endif 
#ifdef F_GETFL  
  scm_c_define ("F_GETFL", scm_from_int (F_GETFL));
#endif 
#ifdef F_SETFL  
  scm_c_define ("F_SETFL", scm_from_int (F_SETFL));
#endif 
#ifdef F_GETOWN  
  scm_c_define ("F_GETOWN", scm_from_int (F_GETOWN));
#endif 
#ifdef F_SETOWN  
  scm_c_define ("F_SETOWN", scm_from_int (F_SETOWN));
#endif 
#ifdef FD_CLOEXEC  
  scm_c_define ("FD_CLOEXEC", scm_from_int (FD_CLOEXEC));
#endif
#endif /* HAVE_POSIX */

  /* `access' symbols.  */
  scm_c_define ("R_OK", scm_from_int (R_OK));
  scm_c_define ("W_OK", scm_from_int (W_OK));
  scm_c_define ("X_OK", scm_from_int (X_OK));
  scm_c_define ("F_OK", scm_from_int (F_OK));

  scm_dot_string = scm_from_locale_string (".");

#include "libguile/filesys.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
