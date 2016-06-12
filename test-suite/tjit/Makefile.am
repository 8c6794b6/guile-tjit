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
	$(top_builddir)/meta/guile -e main $(srcdir)/run-tjit-tests.scm

TESTS = \
	t-add-immediate-01.scm \
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
	t-box-04.scm \
	t-bind-rest-01.scm \
	t-bind-rest-02.scm \
	t-bind-rest-03.scm \
	t-br-if-eq-01.scm \
	t-br-if-eq-02.scm \
	t-br-if-eqv-01.scm \
	t-br-if-eqv-02.scm \
	t-br-if-lt-01.scm \
	t-br-if-lt-02.scm \
	t-br-if-lt-03.scm \
	t-br-if-le-01.scm \
	t-br-if-le-02.scm \
	t-br-if-=-01.scm \
	t-br-if-char-01.scm \
	t-br-if-logtest-01.scm \
	t-br-if-nargs-ne-01.scm \
	t-br-if-nargs-lt-01.scm \
	t-br-if-null-01.scm \
	t-br-if-null-02.scm \
	t-br-if-pair-01.scm \
	t-br-if-struct-01.scm \
	t-br-if-tc7-01.scm \
	t-br-if-true-01.scm \
	t-br-if-u64-lt-scm-01.scm \
	t-br-if-u64-le-scm-01.scm \
	t-br-if-u64-=-scm-01.scm \
	t-br-if-u64-=-scm-02.scm \
	t-builtin-ref-01.scm \
	t-bv-length-01.scm \
	t-bv-u8-ref-01.scm \
	t-bv-u8-ref-02.scm \
	t-bv-u8-ref-03.scm \
	t-bv-u8-set-01.scm \
	t-bytecompile-01.scm \
	t-bytecompile-02.scm \
	t-bytecompile-03.scm \
	t-bytecompile-04.scm \
	t-call-01.scm \
	t-call-02.scm \
	t-call-03.scm \
	t-call-04.scm \
	t-call-05.scm \
	t-call-06.scm \
	t-call-07.scm \
	t-call-08.scm \
	t-call-label-01.scm \
	t-call-label-02.scm \
	t-call-label-03.scm \
	t-call-label-04.scm \
	t-call-cc-01.scm \
	t-call-cc-02.scm \
	t-call-cc-03.scm \
	t-div-01.scm \
	t-div-02.scm \
	t-div-03.scm \
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
	t-flonum-04b.scm \
	t-flonum-04c.scm \
	t-flonum-04d.scm \
	t-flonum-04e.scm \
	t-flonum-04f.scm \
	t-flonum-04g.scm \
	t-flonum-05.scm \
	t-free-ref-01.scm \
	t-free-ref-02.scm \
	t-free-ref-03.scm \
	t-free-ref-04.scm \
	t-free-ref-05.scm \
	t-guard-01.scm \
	t-load-f64-01.scm \
	t-logand-01.scm \
	t-logior-01.scm \
	t-make-closure-01.scm \
	t-make-long-long-immediate-01.scm \
	t-make-vector-01.scm \
	t-make-vector-02.scm \
	t-make-vector-immediate-01.scm \
	t-make-vector-immediate-02.scm \
	t-mod-01.scm \
	t-mod-02.scm \
	t-mod-03.scm \
	t-mov-01.scm \
	t-mov-02.scm \
	t-mul-01.scm \
	t-mul-02.scm \
	t-mul-03.scm \
	t-mul-04.scm \
	t-mul-05.scm \
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
	t-nest-19b.scm \
	t-nest-20.scm \
	t-nest-21.scm \
	t-nest-22.scm \
	t-nest-23.scm \
	t-nest-23b.scm \
	t-nest-23c.scm \
	t-nest-24.scm \
	t-nest-25.scm \
	t-nest-26.scm \
	t-nest-26b.scm \
	t-nest-26c.scm \
	t-nest-26d.scm \
	t-nest-27.scm \
	t-nest-28.scm \
	t-nest-29.scm \
	t-nest-30.scm \
	t-nest-30b.scm \
	t-nest-30c.scm \
	t-nest-30d.scm \
	t-nest-30e.scm \
	t-nest-30f.scm \
	t-nest-31.scm \
	t-pairs-01.scm \
	t-pairs-02.scm \
	t-pairs-03.scm \
	t-pairs-04.scm \
	t-pairs-05.scm \
	t-pairs-06.scm \
	t-pairs-07.scm \
	t-pairs-08.scm \
	t-pairs-09.scm \
	t-pairs-10.scm \
	t-pairs-11.scm \
	t-pairs-12.scm \
	t-pairs-13.scm \
	t-pic-01.scm \
	t-pic-02.scm \
	t-pic-03.scm \
	t-pic-04.scm \
	t-pic-05.scm \
	t-quo-01.scm \
	t-rem-01.scm \
	t-rem-02.scm \
	t-rec-01.scm \
	t-rec-02.scm \
	t-rec-03.scm \
	t-rec-04.scm \
	t-rec-05.scm \
	t-rec-06.scm \
	t-rec-07.scm \
	t-reset-frame-01.scm \
	t-reset-frame-02.scm \
	t-repl-01.scm \
	t-repl-02.scm \
	t-scm-to-f64-01.scm \
	t-scm-to-u64-truncate-01.scm \
	t-set-car-01.scm \
	t-set-cdr-01.scm \
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
	t-side-exit-18.scm \
	t-subr-call-01.scm \
	t-subr-call-02.scm \
	t-subr-call-03.scm \
	t-subr-call-04.scm \
	t-subr-call-05.scm \
	t-subr-call-06.scm \
	t-subr-call-07.scm \
	t-subr-call-08.scm \
	t-subr-call-09.scm \
	t-subr-call-10.scm \
	t-string-length-01.scm \
	t-string-ref-01.scm \
	t-string-to-number-01.scm \
	t-string-to-symbol-01.scm \
	t-tail-call-01.scm \
	t-tail-call-02.scm \
	t-tail-call-03.scm \
	t-tail-call-04.scm \
	t-tail-call-05.scm \
	t-tail-call-05b.scm \
	t-tail-call-05c.scm \
	t-tail-call-05d.scm \
	t-tail-call-06.scm \
	t-tail-call-07.scm \
	t-tail-call-08.scm \
	t-tail-call-09.scm \
	t-tail-call-10.scm \
	t-tail-call-11.scm \
	t-tail-call-label-01.scm \
	t-tail-call-label-02.scm \
	t-toplevel-box-01.scm \
	t-toplevel-box-02.scm \
	t-toplevel-box-03.scm \
	t-toplevel-box-04.scm \
	t-types-01.scm \
	t-u64-01.scm \
	t-u64-02.scm \
	t-uadd-01.scm \
	t-ulogand-01.scm \
	t-ulogior-01.scm \
	t-umul-01.scm \
	t-usub-01.scm \
	t-vector-length-01.scm \
	t-vector-ref-01.scm \
	t-vector-ref-02.scm \
	t-vector-ref-03.scm \
	t-vector-ref-04.scm \
	t-vector-ref-05.scm \
	t-vector-set-01.scm \
	t-vector-set-02.scm \
	t-vector-set-03.scm \
	x-collatz-q.scm \
	x-conform.scm \
	x-cpstak.scm \
	x-destruct.scm \
	x-div.scm \
	x-dynamic.scm \
	x-dynamic2.scm \
	x-earley.scm \
	x-fft.scm \
	x-graphs.scm \
	x-lattice.scm \
	x-maze.scm \
	x-maze2.scm \
	x-mazefun.scm \
	x-nboyer.scm \
	x-nucleic2.scm \
	x-nqueens.scm \
	x-paraffins.scm \
	x-peval.scm \
	x-puzzle.scm \
	x-scheme.scm \
	x-scheme2.scm \
	x-sboyer.scm \
	x-sort1.scm \
	x-takl.scm \
	x-triangle.scm

EXTRA_DIST = run-tjit-tests.scm $(TESTS)

TJIT_TESTS_INFO = tjit-tests.info

coverage:
	$(TESTS_ENVIRONMENT) -c -o $(TJIT_TESTS_INFO) $(TESTS)

endif
