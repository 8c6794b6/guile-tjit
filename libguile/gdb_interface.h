/* Simple interpreter interface for GDB, the GNU debugger.
   Copyright (C) 1996 Mikael Djurfeldt.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

The author can be reached at djurfeldt@nada.kth.se
Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN  */

#ifndef GDB_INTERFACE_H
#define GDB_INTERFACE_H

/* This is the header file for GDB's interpreter interface.  The
   interpreter must supply definitions of all symbols declared in this
   file.

   Before including this file, you must #define GDB_TYPE to be the
   data type used for communication with the interpreter. */

/* The following macro can be used to anchor the symbols of the
   interface in your main program.  This is necessary if the interface
   is defined in a library, such as Guile. */

#define GDB_INTERFACE \
void *gdb_interface[] = { \
  &gdb_options, \
  &gdb_language, \
  &gdb_result, \
  &gdb_output, \
  &gdb_output_length, \
  gdb_maybe_valid_type_p, \
  gdb_read, \
  gdb_eval, \
  gdb_print, \
  gdb_binding \
}; \


/* GDB_OPTIONS is a set of flags informing gdb what features are present
   in the interface.  Currently only one option is supported: */

/* GDB_HAVE_BINDINGS: Set this bit if your interpreter can create new
   top level bindings on demand (through gdb_top_level_binding) */

#define GDB_HAVE_BINDINGS 1

extern unsigned short gdb_options;

/* GDB_LANGUAGE holds the name of the preferred language mode for this
   interpreter.  For lisp interpreters, the suggested mode is "lisp/c". */

extern char *gdb_language;
   
/* GDB_RESULT is used for passing results from the interpreter to GDB */

extern GDB_TYPE gdb_result;

/* The interpreter passes strings to GDB in GDB_OUTPUT and
   GDB_OUTPUT_LENGTH.  GDB_OUTPUT should hold the pointer to the
   string.  GDB_OUTPUT_LENGTH should hold its length.  The string
   doesn't need to be terminated by '\0'. */

extern char *gdb_output;

extern int gdb_output_length;

/* Return TRUE if the interpreter regards VALUE's type as valid.  A
   lazy implementation is allowed to pass TRUE always.  FALSE should
   only be returned when it is certain that VALUE is not valid.

   In the "lisp/c" language mode, this is used to heuristically
   discriminate lisp values from C values during printing. */

extern int gdb_maybe_valid_type_p SCM_P ((GDB_TYPE value));

/* Parse expression in string STR.  Store result in GDB_RESULT, then
   return 0 to indicate success.  On error, return -1 to indicate
   failure.  An error string can be passed in GDB_OUTPUT and
   GDB_OUTPUT_LENGTH.  Be careful to set GDB_OUTPUT_LENGTH to zero if
   no message is passed.  Please note that the resulting value should
   be protected against garbage collection. */

extern int gdb_read SCM_P ((char *str));

/* Evaluate expression EXP.  Store result in GDB_RESULT, then return 0
   to indicate success.  On error, return -1 to indicate failure.  Any
   output (both on success and failure) can be passed in GDB_OUTPUT
   and GDB_OUTPUT_LENGTH.  Be careful to set GDB_OUTPUT_LENGTH to zero
   if no output is passed.  Please note that the resulting lisp object
   should be protected against garbage collection. */

extern int gdb_eval SCM_P ((GDB_TYPE exp));

/* Print VALUE.  Store output in GDB_OUTPUT and GDB_OUTPUT_LENGTH.
   Return 0 to indicate success.  On error, return -1 to indicate
   failure.  GDB will not look at GDB_OUTPUT or GDB_OUTPUT_LENGTH on
   failure.  Note that this function should be robust against strange
   values.  It could in fact be passed any kind of value. */

extern int gdb_print SCM_P ((GDB_TYPE value));

/* Bind NAME to VALUE in interpreter.  (GDB has previously obtained
   NAME by passing a string to gdb_read.)  Return 0 to indicate
   success or -1 to indicate failure.  This feature is optional.  GDB
   will only call this function if the GDB_HAVE_BINDINGS flag is set
   in gdb_options.  Note that GDB may call this function many times
   for the same name.

   For scheme interpreters, this function should introduce top-level
   bindings. */

extern int gdb_binding SCM_P ((GDB_TYPE name, GDB_TYPE value));

#endif /* GDB_INTERFACE_H */
