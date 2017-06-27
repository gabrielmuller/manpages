# Copyright (C) 2007 Free Software Foundation, Inc.
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

JAVAC = javac -source 5

classfiles = wb/Blink.class wb/Blkio.class wb/Del.class wb/Segs.class	\
	wb/Handle.class wb/Prev.class wb/Scan.class wb/Stats.class	\
	wb/Wbdefs.class wb/Ents.class wb/Blk.class wb/SchlepRT.class	\
	wb/Db.class wb/Ent.class wb/Han.class wb/Lck.class		\
	wb/Pkt.class wb/Seg.class wb/Wbsys.class

javafiles = wb/Blink.java wb/Blkio.java wb/Del.java wb/Segs.java	\
	wb/Handle.java wb/Prev.java wb/Scan.java wb/Stats.java	\
	wb/Wbdefs.java wb/Ents.java wb/Blk.java wb/SchlepRT.java	\
	wb/Db.java wb/Ent.java wb/Han.java wb/Lck.java		\
	wb/Pkt.java wb/Seg.java wb/Wbsys.java

wb.jar: $(classfiles)
	jar cf wb.jar $(classfiles)

$(classfiles): $(javafiles)
	$(JAVAC) $(javafiles)

#To link against anyother file X.java that uses wb,
# $(JAVAC) -cp wb.jar:. X.java
# java -cp wb.jar:. X
#To run Example one can say,
# $(JAVAC) -cp wb.jar:. Example.java
# java -cp wb.jar:. Example

Test.class: wb.jar Test.java
	$(JAVAC) -cp wb.jar:. Test.java
Example.class: wb.jar Example.java
	$(JAVAC) -cp wb.jar:. Example.java
check:	Example.class Test.class
	java -cp wb.jar:. Test
	../c/wbcheck z
	java -cp wb.jar:. Example
	../c/wbcheck mydata

stress: Stress.class
	java -cp wb.jar:. Stress
	../c/wbcheck mydata
Stress.class: wb.jar Stress.java
	$(JAVAC) -cp wb.jar:. Stress.java

testmt: wb.jar TestMT.java
	$(JAVAC) -cp wb.jar:. TestMT.java
	java -cp wb.jar:. TestMT
	../c/wbcheck empty.wb
