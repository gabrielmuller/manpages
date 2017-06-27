# Copyright (C) 2007, 2010 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111, USA.
#
# As a special exception, the Free Software Foundation gives permission
# for additional uses of the text contained in its release of WB.
#
# The exception is that, if you link the WB library with other files
# to produce an executable, this does not by itself cause the
# resulting executable to be covered by the GNU General Public License.
# Your use of that executable is in no way restricted on account of
# linking the WB library code into it.
#
# This exception does not however invalidate any other reasons why
# the executable file might be covered by the GNU General Public License.
#
# This exception applies only to the code released by the
# Free Software Foundation under the name WB.  If you copy
# code from other Free Software Foundation releases into a copy of
# WB, as the General Public License permits, the exception does
# not apply to the code that you add in this way.  To avoid misleading
# anyone as to the status of such modified files, you must delete
# this exception notice from them.
#
# If you write modifications of your own for WB, it is your choice
# whether to permit this exception to apply to your modifications.
# If you do not wish that, delete this exception notice.

# Makefile for Java WB-tree File Based Associative String Data Base.
# Author: Aubrey Jaffer

javafiles = Blink.java Blkio.java Del.java Handle.java Segs.java	\
	Prev.java Scan.java Stats.java Wbdefs.java Ents.java Blk.java	\
	SchlepRT.java Db.java Ent.java Han.java Lck.java Pkt.java	\
	Seg.java Wbsys.java

classfiles = Blink.class Blkio.class Del.class Segs.class Handle.class	\
	Prev.class Scan.class Stats.class Wbdefs.class Ents.class	\
	Blk.class SchlepRT.class Db.class Ent.class Han.class		\
	Lck.class Pkt.class Seg.class Wbsys.class

JAVAC = javac -source 5

$(classfiles): $(javafiles)
	$(JAVAC) $(javafiles)
