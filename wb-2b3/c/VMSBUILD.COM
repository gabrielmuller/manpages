$ !"VMSBUILD.COM": File to build the wb-tree database executable.
$ !Copyright (C) 1991, 1992, 1993, 2000 Free Software Foundation, Inc.
$ !
$ !This program is free software: you can redistribute it and/or modify
$ !it under the terms of the GNU Lesser General Public License as
$ !published by the Free Software Foundation, either version 3 of the
$ !License, or (at your option) any later version.
$ !
$ !This program is distributed in the hope that it will be useful, but
$ !WITHOUT ANY WARRANTY; without even the implied warranty of
$ !MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
$ !Lesser General Public License for more details.
$ !
$ !You should have received a copy of the GNU Lesser General Public
$ !License along with this program.  If not, see
$ !<http://www.gnu.org/licenses/>.

$ cc wbcheck,blkio,stats,del,ent,wbsys,blink,handle,prev,scan
$   link wbcheck,blkio,stats,del,ent,wbsys,blink,handle,prev,scan,sys$input/opt
	sys$share:vaxcrtl/share

$ cc /INCLUDE_DIRECTORY="[-.scm]" /define=("RTL") db
$ cc /INCLUDE_DIRECTORY="[-.scm]"-
     /define=("INITS=init_sc2();init_db()") [-.scm]scm
$   link db,scm,blkio,stats,del,ent,wbsys,blink,handle,prev,scan,-
	[-.scm]time,[-.scm]repl,[-.scm]scl,-
	[-.scm]sys,[-.scm]eval,[-.scm]subr,-
	[-.scm]sc2,[-.scm]unif,[-.scm]setjump,sys$input/opt
	sys$share:vaxcrtl/share
