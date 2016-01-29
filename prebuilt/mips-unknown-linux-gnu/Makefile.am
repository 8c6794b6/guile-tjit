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

GUILE_TARGET = $(shell basename $(subdir))
GUILE_BUILD_TAG = PREBUILD $(GUILE_TARGET)

include $(top_srcdir)/am/bootstrap.am

EXTRA_DIST = $(GOBJECTS)
