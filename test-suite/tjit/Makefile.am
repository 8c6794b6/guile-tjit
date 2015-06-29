## Process this file with automake to produce Makefile.in.
##
## Copyright 2005, 2006, 2008, 2009, 2010 Software Foundation, Inc.
##
## This file is part of GUILE.
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

if BUILD_VM_LIGHTNING

TESTS_ENVIRONMENT = \
	$(top_builddir)/meta/guile \
	-l $(srcdir)/run-tjit-tests.scm -e main

TESTS = \
	t-micro-mono.scm \
	t-micro-di.scm \
	t-micro-tri.scm \
	t-micro-tetra.scm \
	t-micro-penta.scm \
	t-micro-hexa.scm \
	t-micro-hepta.scm \
	t-micro-octa.scm \
	t-micro-nona.scm \
	t-micro-deca.scm \
	t-sum-tailcall.scm \
	t-ssa.scm \
	t-micro-flonum-01.scm \
	t-micro-flonum-02.scm \
	t-micro-twice.scm \
	t-box-01.scm \
	t-toplevel-box-01.scm \
	t-call-01.scm \
	t-call-02.scm \
	t-call-03.scm \
	t-nest-01.scm \
	t-nest-02.scm \
	t-nest-03.scm \
	t-nest-04.scm

EXTRA_DIST = run-tjit-tests.scm $(TESTS)

endif
