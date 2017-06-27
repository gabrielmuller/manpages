# Makefile for WB-tree File Based Associative String Data Base.
# Copyright (C) 1991, 1992, 1993, 2000, 2008, 2010 Free Software Foundation, Inc.
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

# These are normally set in "config.status"; defaults are here so that
# "make" won't complain about target redefinitions.
snapdir=$(HOME)/pub/
infodir=$(HOME)/info/
htmldir=$(HOME)/public_html/

SHELL = /bin/sh
INSTALL = install
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = ginstall-info

SCM2JAVA = scm -e"(define *script* \#t)" -l./scm2java
SCM2CS = scm -e"(define *script* \#t)" -l./scm2cs
SCM2C = scm -e"(define *script* \#t)" -l./scm2c -p scm

TEXI2HTML = /usr/local/bin/texi2html -split -verbose
TEXI2PDF = texi2pdf
MAKEDEV = make -f $(HOME)/makefile.dev
CHPAT = $(HOME)/bin/chpat
RSYNC = rsync -av
Uploadee = csail

VERSION = 2b3
RELEASE = 1

# ./configure --distdir=${HOME}/dist/ --snapdir=${HOME}/pub/ --htmldir=${HOME}/public_html/ --pdfdir=${HOME}/public_html/

config.status:
	./configure
Makefile: config.status
include config.status

jardir = $(prefix)share/java/
libwbdir = $(libdir)wb/
includewbdir = $(includedir)wb/
windistdir = /c/Voluntocracy/dist/
rpm_prefix = $(HOME)/rpmbuild/
scmdir = $(HOME)/scm/
prevdocsdir = wbprev/
waterdevdir = $(HOME)/workspace/dev/components/water_dev/engine/

smakfiles = configure Makefile all.scm wb.spec
makfiles = $(smakfiles) c/VMSBUILD.COM c/Makefile java/Makefile	\
	java/wb/Makefile csharp/Makefile
# Scheme files which get translated
srcfiles = handle.scm segs.scm blink.scm wbdefs.scm \
	prev.scm del.scm ents.scm scan.scm stats.scm db.scm
# Scheme files which don't get translated
scmfiles = wbsys.scm blkio.scm schleprt.scm
typfiles = scm2c.typ scm2java.typ scm2cs.typ
# Scheme test files
ststfiles = example.scm test.scm test2.scm \
	rel.scm wbview wbtab.scm rwb-isam.scm
# Scheme and C test files
tstfiles = $(ststfiles) c/wbcheck.c c/wbscm.c
# Generated TeXInfo files
txifiles = c/wbdefs.txi c/scan.txi c/handle.txi c/segs.txi	\
	java/wb/Wbdefs.txi java/wb/Scan.txi java/wb/Handle.txi	\
	java/wb/Segs.txi java/wb/Db.txi csharp/Wbdefs.txi	\
	csharp/Scan.txi csharp/Handle.txi csharp/Segs.txi
docfiles = ANNOUNCE README COPYING COPYING.LESSER wb.info wb.texi \
	fdl.texi
# generated C include files
s2hfiles = c/wbdefs.h c/ents.h c/blink.h c/handle.h c/segs.h c/prev.h \
	c/del.h c/stats.h c/scan.h
# all C include files
ahfiles = $(s2hfiles) c/wbsys.h c/blkio.h c/schleprt.h
# generated C source files
s2cfiles = c/ents.c c/blink.c c/handle.c c/segs.c c/prev.c \
	c/del.c c/stats.c c/scan.c
# generated Java files
s2jfiles = java/wb/Ents.java java/wb/Blink.java java/wb/Handle.java	\
	java/wb/Segs.java java/wb/Prev.java java/wb/Del.java		\
	java/wb/Stats.java java/wb/Scan.java java/wb/Wbdefs.java	\
	java/wb/Db.java
# Java infrastructure (not generated)
ujfiles = java/wb/Seg.java java/wb/Blkio.java java/wb/Wbsys.java	\
	java/wb/Blk.java java/wb/SchlepRT.java java/wb/Ent.java		\
	java/wb/Han.java java/wb/Lck.java java/wb/Pkt.java
# Java test files
jtfiles = java/Example.java java/Test.java java/TestMT.java
# C# infrastructure
csfiles = SchlepRT.cs Blkio.cs Lck.cs Ent.cs Han.cs Seg.cs Pkt.cs	\
	 Blk.cs Db.cs
# csharp/C# infrastructure
csifiles = csharp/SchlepRT.cs csharp/Blkio.cs csharp/Lck.cs	\
	csharp/Ent.cs csharp/Han.cs csharp/Seg.cs csharp/Pkt.cs	\
	csharp/Blk.cs csharp/Db.cs
# Schlep translator programs
trnfiles = scm2c scm2java scm2cs
allfiles = $(ststfiles) $(srcfiles) $(scmfiles) $(makfiles)		\
	 $(docfiles) $(trnfiles) c/wbsys.c c/blkio.c c/schleprt.c	\
	 $(ahfiles) $(s2cfiles) $(csifiles) csharp/Wb.cs ChangeLog	\
	 version.txi c/db.texi

cdeliverables = c/libwb.so c/libwb.a c/wbcheck c/wbscm.so
deliverables = $(cdeliverables) csharp/Wb.dll java/wb.jar

tagfiles = $(srcfiles) $(scmfiles) $(tstfiles) $(makfiles) $(docfiles)	\
	$(ujfiles) $(csifiles) c/wbsys.c c/blkio.c c/blkio.h c/wbsys.h	\
	c/schleprt.c c/schleprt.h wb.texi c/db.texi

installdirs:
	-mkdir -p $(DESTDIR)$(infodir)
	-mkdir -p $(DESTDIR)$(dvidir)
	-mkdir -p $(DESTDIR)$(pdfdir)
	-mkdir -p $(DESTDIR)$(includedir)
	-mkdir -p $(DESTDIR)$(includewbdir)
	-mkdir -p $(DESTDIR)$(libdir)
	-mkdir -p $(DESTDIR)$(bindir)
	-mkdir -p $(DESTDIR)$(libwbdir)
	-mkdir -p $(DESTDIR)$(jardir)

wb.dvi:	wb.texi version.txi $(txifiles) c/db.texi
	$(TEXI2DVI) -b -c $<
dvi:	wb.dvi
xdvi:	wb.dvi
	xdvi $<
install-dvi: wb.dvi installdirs
	$(INSTALL_DATA) $< $(DESTDIR)$(dvidir)

wb.pdf: wb.texi version.txi $(txifiles) c/db.texi
	$(TEXI2PDF) -b -c $<
pdf:	wb.pdf
xpdf:	wb.pdf
	xpdf $<
install-pdf: wb.pdf installdirs
	$(INSTALL_DATA) $< $(DESTDIR)$(pdfdir)

# wb_toc.html: wb.texi version.txi $(txifiles) c/db.texi
# 	$(TEXI2HTML) $<
# $(DESTDIR)$(htmldir)wb_toc.html: wb_toc.html
# 	-rm -f wb_stoc.html
# 	if [ -f $(prevdocsdir)wb_toc.html ]; \
# 	  then hitch $(prevdocsdir)wb_\*.html wb_\*.html \
# 		$(DESTDIR)$(htmldir); \
# 	  else $(INSTALL_DATA) wb_*.html $(DESTDIR)$(htmldir);fi
# install-html: $(DESTDIR)$(htmldir)wb_toc.html
# html: install-html

html/wb: wb.texi version.txi $(txifiles) c/db.texi
	mkdir -p html
	rm -rf html/wb
	makeinfo --html $< -o html/wb
html: html/wb
$(DESTDIR)$(htmldir)wb: html/wb
	-rm -rf $(DESTDIR)$(htmldir)wb
	mkdir -p $(DESTDIR)$(htmldir)wb
	$(INSTALL_DATA) html/wb/*.html $(DESTDIR)$(htmldir)wb
install-html: $(DESTDIR)$(htmldir)wb

wb-$(VERSION).info: wb.texi version.txi $(txifiles) c/db.texi
	$(MAKEINFO) $< --no-split -o wb.info
	mv wb.info $@
wb.info: wb-$(VERSION).info
	if [ -f $(prevdocsdir)wb.info ]; \
	  then infobar $(prevdocsdir)wb.info $< $@; \
	  else $(INSTALL_DATA) $< $@;fi

$(DESTDIR)$(infodir)wb.info: wb.info
	$(INSTALL_DATA) $< $@;
	-rm $(DESTDIR)$(infodir)wb.info.gz
	$(POST_INSTALL)    # Post-install commands follow.
	-$(INSTALL_INFO) $@ $(DESTDIR)$(infodir)dir
install-info: $(DESTDIR)$(infodir)wb.info
info:	install-info

$(DESTDIR)$(infodir)wb.info.gz:	$(DESTDIR)$(infodir)wb.info
	gzip -f $<
install-infoz:	$(DESTDIR)$(infodir)wb.info.gz
infoz:	install-infoz

README: wb-$(VERSION).info Makefile
	(info -f wb-$(VERSION).info -n 'Description' -o -;\
	 info -f wb-$(VERSION).info -n 'License' -o -;\
	 info -f wb-$(VERSION).info -n 'History' -o -;\
	 info -f wb-$(VERSION).info -n 'File Organization' -o -;\
	 info -f wb-$(VERSION).info -n 'Installation' -o -) \
	| grep -v '^File: wb.info,  Node:' > README

docs: README install-infoz install-html install-dvi install-pdf install-infoz

all:	$(deliverables)

c/libwb.so: $(s2hfiles) $(s2cfiles)
	cd c; $(MAKE) libwb.so
c/libwb.a: $(s2hfiles) $(s2cfiles)
	cd c; $(MAKE) libwb.a
c/wbcheck: $(s2hfiles) $(s2cfiles) c/wbcheck.c
	cd c; $(MAKE) wbcheck
c/wbscm.so: $(s2hfiles) $(s2cfiles)
	cd c; $(MAKE) wbscm.so

java/wb.jar: $(s2jfiles) $(ujfiles)
	cd java; $(MAKE)
jall:	java/wb.jar
jcheck:	java/wb.jar
	cd java; $(MAKE) check

csharp/Wb.cs: $(srcfiles) $(csifiles) scm2cs scm2cs.typ
	$(SCM2CS) $(srcfiles) $(csfiles) csharp/
csharp/Wb.dll: csharp/Wb.cs
	cd csharp; $(MAKE) Wb.dll
csall:	csharp/Wb.dll
cscheck: csharp/Wb.dll
	cd csharp; $(MAKE) check

c/scan.c c/scan.h c/scan.txi:	scan.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/stats.c c/stats.h:	stats.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/blink.c c/blink.h:	blink.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/ents.c c/ents.h:	ents.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/del.c c/del.h:	del.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/prev.c c/prev.h:	prev.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/handle.c c/handle.h c/handle.txi:	handle.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/segs.c c/segs.h c/segs.txi:	segs.scm scm2c.typ scm2c
	$(SCM2C) $< c/
c/wbdefs.h c/wbdefs.txi:	wbdefs.scm scm2c
	$(SCM2C) wbdefs.h c/

java/wb/Scan.java java/wb/Scan.txi:	scan.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Stats.java:	stats.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Blink.java:	blink.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Db.java java/wb/Db.txi:	db.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Ents.java:	ents.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Del.java:	del.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Prev.java:	prev.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Handle.java java/wb/Handle.txi:	handle.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Segs.java java/wb/Segs.txi:	segs.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/
java/wb/Wbdefs.java java/wb/Wbdefs.txi:	wbdefs.scm scm2java.typ scm2java
	$(SCM2JAVA) $< java/wb/	# -p constant

scheck:
	scm -l all -l test -l test2 -e '(main) (rmain)'
check:
	$(MAKE) scheck
	$(MAKE) $(s2cfiles) $(s2hfiles)
	cd c; $(MAKE) wbcheck
	c/wbcheck z
	cd c; $(MAKE) wbscm.so
	scm -l c/wbscm.so -l example.scm
	c/wbcheck mydata
	$(MAKE) jcheck
	$(MAKE) cscheck

################ INSTALL DEFINITIONS ################
installc: $(ahfiles) $(cdeliverables) installdirs
	$(INSTALL_DATA) $(ahfiles) $(DESTDIR)$(includewbdir)
	-rm $(DESTDIR)$(includedir)wbsys.h
	ln -s wb/wbsys.h $(DESTDIR)$(includedir)wbsys.h
	-$(INSTALL_PROGRAM) c/wbcheck $(DESTDIR)$(bindir)
	-$(INSTALL_DATA) c/libwb.so c/libwb.a c/wbscm.so $(DESTDIR)$(libwbdir)
	-rm $(DESTDIR)$(libdir)/libwb.so \
	    $(DESTDIR)$(libdir)/libwb.a
	-ln -s wb/libwb.so $(DESTDIR)$(libdir)libwb.so
	-ln -s wb/libwb.a  $(DESTDIR)$(libdir)libwb.a
	-$(INSTALL_DATA) wbtab.scm rwb-isam.scm c/wbscm.so \
	    $(DESTDIR)$(libwbdir)
#	 -rm $(DESTDIR)$(libwbdir)wbscm.so
#	 -ln -s wbscm.so $(DESTDIR)$(libwbdir)wbscm.so

installdll: csharp/Wb.dll installdirs
	-$(INSTALL_DATA) csharp/Wb.dll $(DESTDIR)$(libwbdir)
	-rm $(DESTDIR)$(libdir)/Wb.dll
	-ln -s wb/Wb.dll $(DESTDIR)$(libdir)Wb.dll

installjar: java/wb.jar installdirs
	-$(INSTALL_DATA) java/wb.jar $(DESTDIR)$(jardir)

install: installc installdll installjar install-info

install-ww: install
	$(INSTALL_DATA) java/wb.jar $(DESTDIR)$(waterdevdir)whitewater/java/
	$(INSTALL_DATA) csharp/Wb.dll $(DESTDIR)$(waterdevdir)whitewater/csharp/
	cd $(DESTDIR)$(waterdevdir)wb; cvs update;

install-scm: c/wbscm.so wbtab.scm rwb-isam.scm
	$(INSTALL_DATA) c/wbscm.so rwb-isam.scm wbtab.scm $(DESTDIR)$(scmdir)

myinstalls: install-ww install-scm

uninstall:
	$(PRE_UNINSTALL)     # Pre-uninstall commands follow.
	-$(INSTALL_INFO) --delete $(DESTDIR)$(infodir)wb.info $(DESTDIR)$(infodir)dir
	$(NORMAL_UNINSTALL)  # Normal commands follow.
	-rm $(DESTDIR)$(bindir)wbcheck
	-rm $(DESTDIR)$(includedir)wbsys.h
	-rm $(DESTDIR)$(libdir)libwb.so \
	    $(DESTDIR)$(libdir)libwb.a
	-rm $(DESTDIR)$(libwbdir)wbtab.scm \
	    $(DESTDIR)$(libwbdir)rwb-isam.scm \
	    $(DESTDIR)$(libwbdir)wbscm.so
	-rm $(DESTDIR)$(libwbdir)db.so
	-cd $(DESTDIR)$(includewbdir); rm $(ahfiles)
#	$(POST_UNINSTALL)     # Post-uninstall commands follow.
	-rmdir $(DESTDIR)$(includewbdir)

#### Stuff for maintaining WB below ####

ver = $(VERSION)

temp/wb: $(allfiles) java/wb.jar
#
	-rm -rf temp
	mkdir -p temp/wb/
	ln $(ststfiles) $(smakfiles) $(docfiles) $(scmfiles) ChangeLog	\
	 $(trnfiles) $(srcfiles) $(typfiles) version.txi temp/wb/
#
	mkdir -p temp/wb/c/
	ln c/VMSBUILD.COM c/Makefile c/wbcheck.c c/wbscm.c c/wbdefs.txi	\
	 c/scan.txi c/handle.txi c/segs.txi $(s2hfiles) $(s2cfiles)	\
	 c/wbsys.h c/blkio.h c/blkio.c c/wbsys.c c/schleprt.c		\
	 c/schleprt.h c/db.c c/db.h c/db.texi temp/wb/c/
#
	mkdir -p temp/wb/java/
	ln java/Makefile java/wb.jar $(jtfiles) temp/wb/java/
#
	mkdir -p temp/wb/java/wb/
	ln java/wb/Wbdefs.txi java/wb/Scan.txi java/wb/Handle.txi	\
	 java/wb/Segs.txi java/wb/Makefile $(s2jfiles) $(ujfiles)	\
	 temp/wb/java/wb/
#
	mkdir -p temp/wb/csharp/
	ln csharp/Wbdefs.txi csharp/Scan.txi csharp/Handle.txi		\
	 csharp/Segs.txi csharp/Makefile $(csifiles) csharp/Test.cs	\
	 csharp/Example.cs csharp/TestMT.cs csharp/Wb.dll		\
	 temp/wb/csharp/

release: dist pdf		# rpm
	cvs tag -F wb-$(VERSION)
	cp ANNOUNCE $(htmldir)WB_ANNOUNCE.txt
	$(RSYNC) $(htmldir)WB.html $(htmldir)WB_ANNOUNCE.txt \
	  $(Uploadee):public_html/
	$(RSYNC) $(distdir)README $(distdir)wb-$(VERSION).zip \
	  $(distdir)wb-$(VERSION)-$(RELEASE).src.rpm \
	  $(distdir)wb-$(VERSION)-$(RELEASE).*.rpm \
	  $(Uploadee):dist/
#	upload $(distdir)README $(distdir)wb-$(VERSION).zip ftp.gnu.org:gnu/jacal/

upzip:	$(snapdir)wb.zip
	$(RSYNC) $(snapdir)wb.zip $(Uploadee):pub/

dist:	$(distdir)wb-$(VERSION).zip
$(distdir)wb-$(VERSION).zip:	temp/wb
	$(MAKEDEV) DEST=$(distdir) PROD=wb ver=-$(VERSION) zip

upgnu:	$(distdir)wb-$(VERSION).tar.gz
	cd $(distdir); gnupload --to ftp.gnu.org:wb wb-$(VERSION).tar.gz
tar.gz:	$(distdir)wb-$(VERSION).tar.gz
$(distdir)wb-$(VERSION).tar.gz:	temp/wb/
	$(MAKEDEV) DEST=$(distdir) PROD=wb ver=-$(VERSION) tar.gz

rpm:	pubzip
	cp -f $(snapdir)wb.zip $(rpm_prefix)SOURCES/wb-$(VERSION).zip
	rpmbuild -ba wb.spec	# --clean --target x86_64
	rm $(rpm_prefix)SOURCES/wb-$(VERSION).zip
	mv $(rpm_prefix)RPMS/*/wb-$(VERSION)-$(RELEASE).*.rpm	\
	   $(rpm_prefix)SRPMS/wb-$(VERSION)-$(RELEASE).src.rpm	\
	   $(distdir)

shar:	wb.shar
wb.shar:	temp/wb
	$(MAKEDEV) PROD=wb shar
dclshar:	wb.com
com:	wb.com
wb.com:	temp/wb
	$(MAKEDEV) PROD=wb com
zip:	wb.zip
wb.zip:	temp/wb
	$(MAKEDEV) PROD=wb zip
doszip:	$(windistdir)wb-$(VERSION).zip
$(windistdir)wb-$(VERSION).zip:	temp/wb
	$(MAKEDEV) DEST=$(windistdir) PROD=wb ver=-$(VERSION) zip
	zip -d $(windistdir)wb-$(VERSION).zip wb/wb.info
pubzip:	temp/wb
	$(MAKEDEV) DEST=$(snapdir) PROD=wb zip

diffs:	pubdiffs
pubdiffs:	temp/wb
	$(MAKEDEV) DEST=$(snapdir) PROD=wb pubdiffs
distdiffs:	temp/wb
	$(MAKEDEV) DEST=$(distdir) PROD=wb ver=$(ver) distdiffs
announcediffs:	temp/wb
	$(MAKEDEV) DEST=$(distdir) PROD=wb ver=-$(VERSION) announcediffs

CITERS = ANNOUNCE segs.scm $(windistdir)unzipall.bat			\
	$(windistdir)buildall $(htmldir)README.html			\
	$(htmldir)FreeSnell/index.html $(htmldir)FreeSnell/ANNOUNCE	\
	$(htmldir)FreeSnell/FreeSnell.texi $(distdir)README	\
	$(scmdir)ANNOUNCE
CITES = Makefile wb.spec $(htmldir)WB.html

new:
	$(CHPAT) wb-$(VERSION) wb-$(ver) $(CITERS)
	$(CHPAT) $(VERSION) $(ver) $(CITES)
	echo @set WBVERSION $(ver) > version.txi
	echo @set WBDATE `date +"%B %Y"` >> version.txi
	echo `date -I` \ Aubrey Jaffer \ \<`whoami`@`hostname`\>> change
	echo>> change
	echo '	* Makefile (VERSION): Bumped from $(VERSION) to $(ver).'>>change
	echo>> change
	cat ChangeLog >> change
	mv -f change ChangeLog
	cvs commit -m '(VERSION): Bumped from $(VERSION) to $(ver).'
	cvs tag -F wb-$(ver)

TAGS:
tags:	$(tagfiles)
	etags $(tagfiles)
clean:
	-rm -f *~ \#* c/*.o c/*.so *.orig c/a.out core TAGS tmp* c/wbscm.a db
	-rf -f java/*.class java/wb/*.class csharp/*.exe csharp/*.dll
	-rm -rf temp
dclean:
	-rm $(s2cfiles) $(s2jfiles) java/*.jar csharp/Wb.cs $(txifiles)
distclean:
	-rm -f *~ \#* c/*.o c/*.so *.orig c/a.out core TAGS tmp* c/wbscm.a db
	-rm -f $(s2hfiles) $(scfiles) $(deliverables)
	-rm -f *.bv *.db *.rec
	-rm -rf temp
