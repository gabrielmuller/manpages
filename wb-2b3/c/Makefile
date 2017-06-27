# Makefile for WB-tree File Based Associative String Data Base.
# Copyright (C) 1991, 1992, 1993, 2000, 2008 Free Software Foundation, Inc.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

# Author: Aubrey Jaffer

#CC = cc -Aa
#CFLAGS = -p	#for profiling
# Ubuntu needs -fPIC
CFLAGS = -O2 -Wall -fPIC	#-g
CC = gcc
LD = $(CC)
LDSO = $(LD) -shared

# generated C include files
s2hfiles = wbdefs.h ents.h blink.h handle.h segs.h prev.h \
	del.h stats.h scan.h db.h
# generated C source files
s2cfiles = ents.c blink.c handle.c segs.c prev.c \
	del.c stats.c scan.c
# all C include files
hfiles = $(s2hfiles) wbsys.h schleprt.h
# (compiled) object files
ofiles = handle.o segs.o blink.o prev.o ents.o wbsys.o \
	del.o stats.o blkio.o scan.o schleprt.o db.o

all: libwb.so libwb.a wbcheck wbscm.so

install:
	cd ../; $(MAKE) installc

wbscm.so:	$(ofiles) wbscm.o Makefile
	$(LDSO) -o wbscm.so $(ofiles) wbscm.o -lm -lc -lpthread
#	-ln -fs wbscm.so db.so
libwb.so:	$(ofiles) Makefile
	$(LDSO) -o libwb.so $(ofiles) -lm -lc -lpthread
wbscm.a:	$(ofiles) wbscm.o Makefile
	ar crv wbscm.a wbscm.o $(ofiles)
libwb.a:	$(ofiles) Makefile
	ar crv libwb.a $(ofiles)
dbscm:	wbscm.a
	cd ../../scm;$(MAKE) dbscm
	-rm -f dbscm
	mv ../../scm/dbscm ./dbscm
	rm wbscm.a
# db:	$(ofiles) wbscm.o
# 	$(LD) -r -nostdlib wbscm.o $(ofiles) -lpthread -o db

wbcheck:	wbcheck.o $(ofiles)
	$(LD) -o wbcheck wbcheck.o $(ofiles) -lpthread
	strip wbcheck

schleprt.o:	schleprt.c schleprt.h
wbcheck.o:	wbcheck.c $(hfiles)
scan.o:		scan.c $(hfiles)
blkio.o:	blkio.c $(hfiles)
stats.o:	stats.c $(hfiles)
del.o:		del.c $(hfiles)
ents.o:		ents.c $(hfiles)
wbsys.o:	wbsys.c $(hfiles)
blink.o:	blink.c $(hfiles)
handle.o:	handle.c $(hfiles)
segs.o:		segs.c $(hfiles)
db.o:		db.c $(hfiles)

wbscm.o:	wbscm.c $(hfiles)
	$(CC) $(CFLAGS) -I../../scm -c $<
