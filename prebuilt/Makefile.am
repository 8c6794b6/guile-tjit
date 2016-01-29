## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2016 Free Software Foundation, Inc.
##
##   This file is part of GNU Guile.
##
##   GNU Guile is free software; you can redistribute it and/or modify
##   it under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or (at
##   your option) any later version.
##
##   GNU Guile is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GNU Guile; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

# Guile Scheme is mostly written in Guile Scheme.  Its compiler is
# written in Guile Scheme, and its interpreter too.  However, it is not
# bootstrapped from itself: Guile includes a minimal interpreter written
# in C as well which can load the compiler, enough to compile the
# interpreter written in Scheme.  That compiled interpreter written in
# Scheme is then used to compile the rest of Guile, including the
# compiler itself.
#
# The problem is, this process takes a long time, and for people
# installing Guile from source, it's their first experience of Guile: an
# hour-long bootstrap.  It's not the nicest experience.  To avoid this,
# in our tarballs we pre-build object files for the essential parts of
# the compiler.
#
# In the future we will do native compilation and so we will need to
# precompile object files for all common host types.  Still, since we
# use ELF everywhere, there will be many host types whose compiled files
# are the same: because Guile incorporates its own linker and loader for
# compiled Scheme files, any AArch64 machine, for example, is going to
# have the same compiled files.  So, for the variants that will be the
# same, we compile one target triple, and symlink the similar targets to
# that directory.
#
# The current situation though is that we compile to bytecode, and there
# are only four variants of that bytecode: big- or little-endian, and
# 32- or 64-bit.  The strategy is the same, only that now
# arm64-unknown-linux-gnu will link to x86_64-unknown-linux-gnu, as they
# have the same word size and endianness.  A pending issue to resolve is
# how this wil deal with architectures where longs are 32 bits and
# pointers are 64 bits; we'll let the x32 people deal with that.

SUBDIRS = \
  x86_64-unknown-linux-gnu \
  i686-pc-linux-gnu \
  mips-unknown-linux-gnu
