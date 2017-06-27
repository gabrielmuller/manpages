%define name wb
%define version 2b3
%define release 1

Name:         %{name}
Release:      %{release}
Version:      %{version}
Packager:     Aubrey Jaffer <agj @ alum.mit.edu>

License:      GPL
Vendor:       Aubrey Jaffer <agj @ alum.mit.edu>
Group:        Development/Libraries
Provides:     wb
Requires:     scm >= 5d6

Summary:      The WB B-Tree Database for C, C#, Java, and Scheme
Source:       http://groups.csail.mit.edu/mac/ftpdir/scm/wb-%{version}.zip
URL:          http://people.csail.mit.edu/jaffer/WB.html
BuildRoot:    %{_tmppath}/%{name}-%{version}
Prefix:       /usr

%description
WB is a disk based, sorted associative array library for C, C#, and
Java.  WB has an optional interface to the SCM Scheme implementation
which supports SLIB relational databases.

# %define __os_install_post /usr/lib/rpm/brp-compress

%prep
rm -rf /var/tmp/%{name}-%{version}
%setup -n wb -c -T
cd ..
unzip $RPM_SOURCE_DIR/wb-%{version}.zip

%build
make clean
make all

%install
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/bin
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/lib/wb
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/share/java
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/include
make	prefix=${RPM_BUILD_ROOT}%{prefix}/ \
	bindir=${RPM_BUILD_ROOT}%{_bindir}/ \
	infodir=${RPM_BUILD_ROOT}%{_infodir}/ \
	javadir=${RPM_BUILD_ROOT}%{prefix}/share/java/ \
	install # install-scm

%clean
rm -rf $RPM_BUILD_ROOT

%post
%{prefix}/bin/scm -c "(require 'new-catalog)"

%files
%defattr(-, root, root)
%{_bindir}/wbcheck
%dir %{prefix}/lib/wb
%{prefix}/lib/libwb.a
%{prefix}/lib/wb/libwb.a
%{prefix}/lib/libwb.so
%{prefix}/lib/wb/libwb.so
%{prefix}/lib/wb/wbscm.so
#%{prefix}/lib/wb/db.so
%{prefix}/lib/wb/wbtab.scm
%{prefix}/lib/wb/rwb-isam.scm
%{prefix}/lib/Wb.dll
%{prefix}/lib/wb/Wb.dll
%{prefix}/share/java/wb.jar
%dir %{prefix}/include/wb
%{prefix}/include/wb/wbdefs.h 
%{prefix}/include/wb/ents.h 
%{prefix}/include/wb/blink.h 
%{prefix}/include/wb/handle.h 
%{prefix}/include/wb/segs.h 
%{prefix}/include/wb/prev.h 
%{prefix}/include/wb/del.h 
%{prefix}/include/wb/stats.h 
%{prefix}/include/wb/scan.h 
%{prefix}/include/wb/wbsys.h 
%{prefix}/include/wb/blkio.h 
%{prefix}/include/wb/schleprt.h
%{prefix}/include/wbsys.h
%{_infodir}/wb.info.gz
%{_infodir}/dir

%doc ANNOUNCE COPYING README ChangeLog

%changelog
