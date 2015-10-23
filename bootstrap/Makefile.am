## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2009, 2010, 2011, 2012, 2013,
##        2014, 2015 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA


GOBJECTS = $(SOURCES:%.scm=%.go)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
GUILE_OPTIMIZATIONS = -O1
nobase_noinst_DATA = $(GOBJECTS) ice-9/eval.go
CLEANFILES = $(GOBJECTS) ice-9/eval.go

$(GOBJECTS): ice-9/eval.go

VM_TARGETS := system/vm/assembler.go system/vm/disassembler.go
$(VM_TARGETS): $(top_builddir)/libguile/vm-operations.h

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  BOOTSTRAP GUILEC" $@;

vpath %.scm @srcdir@/../module

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GUILEC)GUILE_AUTO_COMPILE=0			\
	$(top_builddir)/meta/uninstalled-env			\
	guild compile --target="$(host)"                        \
          $(GUILE_WARNINGS) $(GUILE_OPTIMIZATIONS)              \
	  -L "$(abs_srcdir)" -L "$(abs_builddir)"		\
	  -L "$(abs_top_srcdir)/guile-readline"			\
	  -o "$@" "$<"

# A subset of sources that are used by the compiler.  We can compile
# these in any order; the order below is designed to hopefully result in
# the lowest total compile time.
SOURCES =					\
  language/cps/intmap.scm			\
  language/cps/intset.scm			\
  language/cps/utils.scm			\
  ice-9/vlist.scm				\
  srfi/srfi-1.scm				\
						\
  language/tree-il.scm				\
  language/tree-il/analyze.scm			\
  language/tree-il/canonicalize.scm		\
  language/tree-il/compile-cps.scm		\
  language/tree-il/debug.scm			\
  language/tree-il/effects.scm			\
  language/tree-il/fix-letrec.scm		\
  language/tree-il/optimize.scm			\
  language/tree-il/peval.scm			\
  language/tree-il/primitives.scm		\
  language/tree-il/spec.scm			\
						\
  language/cps.scm				\
  language/cps/closure-conversion.scm		\
  language/cps/compile-bytecode.scm		\
  language/cps/constructors.scm			\
  language/cps/contification.scm		\
  language/cps/cse.scm				\
  language/cps/dce.scm				\
  language/cps/effects-analysis.scm		\
  language/cps/elide-values.scm			\
  language/cps/licm.scm				\
  language/cps/peel-loops.scm			\
  language/cps/primitives.scm			\
  language/cps/prune-bailouts.scm		\
  language/cps/prune-top-level-scopes.scm	\
  language/cps/reify-primitives.scm		\
  language/cps/renumber.scm			\
  language/cps/rotate-loops.scm			\
  language/cps/optimize.scm			\
  language/cps/simplify.scm			\
  language/cps/self-references.scm		\
  language/cps/slot-allocation.scm		\
  language/cps/spec.scm				\
  language/cps/specialize-primcalls.scm		\
  language/cps/split-rec.scm			\
  language/cps/type-checks.scm			\
  language/cps/type-fold.scm			\
  language/cps/types.scm			\
  language/cps/verify.scm			\
  language/cps/with-cps.scm			\
						\
  language/scheme/spec.scm			\
  language/scheme/compile-tree-il.scm		\
  language/scheme/decompile-tree-il.scm		\
						\
  language/bytecode.scm				\
  language/bytecode/spec.scm			\
						\
  language/value/spec.scm			\
						\
  system/base/pmatch.scm			\
  system/base/syntax.scm			\
  system/base/compile.scm			\
  system/base/language.scm			\
  system/base/lalr.scm				\
  system/base/message.scm			\
  system/base/target.scm			\
  system/base/types.scm				\
  system/base/ck.scm				\
						\
  ice-9/psyntax-pp.scm				\
  ice-9/boot-9.scm				\
  ice-9/r5rs.scm				\
  ice-9/deprecated.scm				\
  ice-9/binary-ports.scm			\
  ice-9/command-line.scm			\
  ice-9/control.scm				\
  ice-9/format.scm				\
  ice-9/getopt-long.scm				\
  ice-9/i18n.scm				\
  ice-9/match.scm				\
  ice-9/networking.scm				\
  ice-9/posix.scm				\
  ice-9/rdelim.scm				\
  ice-9/receive.scm				\
  ice-9/regex.scm				\
  ice-9/session.scm				\
  ice-9/pretty-print.scm			\
						\
  system/vm/assembler.scm			\
  system/vm/debug.scm				\
  system/vm/disassembler.scm			\
  system/vm/dwarf.scm				\
  system/vm/elf.scm				\
  system/vm/frame.scm				\
  system/vm/linker.scm				\
  system/vm/loader.scm				\
  system/vm/program.scm				\
  system/vm/vm.scm				\
  system/foreign.scm
