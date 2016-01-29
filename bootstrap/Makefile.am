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


GUILE_WARNINGS =
GUILE_OPTIMIZATIONS = -O1

include $(top_srcdir)/am/bootstrap.am

# We must build the evaluator first, so that we can be sure to control
# the stack.
$(filter-out ice-9/eval.go, $(GOBJECTS)): ice-9/eval.go
