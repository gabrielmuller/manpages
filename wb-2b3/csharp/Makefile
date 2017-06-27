#CS is csharp compiler.
CS = mcs
csincs = SchlepRT.cs Blkio.cs Lck.cs Ent.cs Han.cs Seg.cs Pkt.cs	\
	 Blk.cs Db.cs
depends = ../scm2cs ../scm2cs.typ ../wbdefs.scm ../scan.scm \
	  ../stats.scm ../blink.scm ../ents.scm ../del.scm ../prev.scm	\
	  ../handle.scm ../segs.scm ../db.scm $(csincs)

Wb.dll:	Wb.cs
	$(CS) /target:library Wb.cs

Wb.cs:	$(depends)
	cd ..; make csharp/Wb.cs

check: Wb.dll
	mcs Test.cs /reference:Wb.dll
	mono Test.exe
	-../c/wbcheck z
	mcs Example.cs /reference:Wb.dll
	mono Example.exe
	../c/wbcheck mydata

cscheck: check

testmt: Wb.dll
	mcs TestMT.cs /reference:Wb.dll
	mono TestMT.exe
#	../c/wbcheck empty.wb
