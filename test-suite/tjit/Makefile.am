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
	t-args-01.scm \
	t-args-02.scm \
	t-args-03.scm \
	t-args-04.scm \
	t-args-05.scm \
	t-args-06.scm \
	t-args-07.scm \
	t-args-08.scm \
	t-args-09.scm \
	t-args-10.scm \
	t-assign-01.scm \
	t-box-01.scm \
	t-box-02.scm \
	t-box-03.scm \
	t-call-01.scm \
	t-call-02.scm \
	t-call-03.scm \
	t-call-04.scm \
	t-deoptimize-01.scm \
	t-escape-01.scm \
	t-f64-01.scm \
	t-f64-02.scm \
	t-f64-03.scm \
	t-f64-04.scm \
	t-f64-05.scm \
	t-flonum-01.scm \
	t-flonum-02.scm \
	t-flonum-03.scm \
	t-flonum-04.scm \
	t-guard-01.scm \
	t-mov-01.scm \
	t-mov-02.scm \
	t-nest-01.scm \
	t-nest-02.scm \
	t-nest-03.scm \
	t-nest-04.scm \
	t-nest-05.scm \
	t-nest-06.scm \
	t-nest-07.scm \
	t-nest-07b.scm \
	t-nest-08.scm \
	t-nest-09.scm \
	t-nest-10.scm \
	t-nest-10b.scm \
	t-nest-11.scm \
	t-nest-12.scm \
	t-nest-13.scm \
	t-nest-14.scm \
	t-nest-15.scm \
	t-nest-16.scm \
	t-nest-17.scm \
	t-nest-18.scm \
	t-nest-19.scm \
	t-pairs-01.scm \
	t-pairs-02.scm \
	t-pairs-03.scm \
	t-pairs-04.scm \
	t-pairs-05.scm \
	t-pairs-06.scm \
	t-pairs-07.scm \
	t-pairs-08.scm \
	t-rec-01.scm \
	t-rec-02.scm \
	t-rec-03.scm \
	t-side-exit-01.scm \
	t-side-exit-02.scm \
	t-side-exit-03.scm \
	t-side-exit-04.scm \
	t-side-exit-05.scm \
	t-side-exit-06.scm \
	t-side-exit-07.scm \
	t-side-exit-08.scm \
	t-side-exit-09.scm \
	t-side-exit-10.scm \
	t-side-exit-11.scm \
	t-side-exit-12.scm \
	t-side-exit-13.scm \
	t-side-exit-14.scm \
	t-side-exit-15.scm \
	t-side-exit-16.scm \
	t-side-exit-17.scm \
	t-subr-call-01.scm \
	t-subr-call-02.scm \
	t-subr-call-03.scm \
	t-subr-call-04.scm \
	t-subr-call-05.scm \
	t-subr-call-06.scm \
	t-tail-call-01.scm \
	t-tail-call-02.scm \
	t-tail-call-03.scm \
	t-tail-call-04.scm \
	t-tail-call-label-01.scm \
	t-toplevel-box-01.scm \
	t-toplevel-box-02.scm \
	t-toplevel-box-03.scm \
	t-toplevel-box-04.scm \
	t-types-01.scm \
	t-u64-01.scm \
	t-u64-02.scm

EXTRA_DIST = run-tjit-tests.scm $(TESTS)

endif
