/* "blkio.c" WB-tree File Based Associative String Data Base System. */
/* Copyright (C) 1991, 1992, 1993, 2000, 2003, 2008, 2009 Free Software Foundation, Inc. */
/* */
/* This program is free software: you can redistribute it and/or modify */
/* it under the terms of the GNU Lesser General Public License as */
/* published by the Free Software Foundation, either version 3 of the */
/* License, or (at your option) any later version. */
/* */
/* This program is distributed in the hope that it will be useful, but */
/* WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU */
/* Lesser General Public License for more details. */
/* */
/* You should have received a copy of the GNU Lesser General Public */
/* License along with this program.  If not, see */
/* <http://www.gnu.org/licenses/>. */

#include "wbsys.h"

#ifdef unix
# define _XOPEN_SOURCE 500
# include <unistd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <errno.h>
#endif

#ifdef _MSC_VER
# include <sys/types.h>
# include <sys/stat.h>
# include <io.h>
# include <sys/locking.h>
# include <Windows.h>
# include <string.h>
#endif

#include <fcntl.h>

#ifdef _MSC_VER
_invalid_parameter_handler oldHandler;
void blkio_iph(const wchar_t* expression,
	       const wchar_t* function,
	       const wchar_t* file,
	       unsigned int line,
	       uintptr_t pReserved)
{
  wprintf(L"Invalid parameter detected in function %s."
	  L" File: %s Line: %d\n", function, file, line);
  wprintf(L"Expression: %s\n", expression);
}
#endif

/* CriticalSection serializes the _lseek() and _read() or _write()
   calls of blkio_read() and blkio_write() on MS-Windows. */
#ifdef _MSC_VER
CRITICAL_SECTION CriticalSection;
#endif

/* !0L to enable diagnostic line for every read and write. */
int io_diag_P = 0L;

/* http://msdn.microsoft.com/en-us/library/ms683476%28VS.85%29.aspx */
/* BOOL WINAPI InitializeCriticalSectionAndSpinCount(__out  LPCRITICAL_SECTION lpCriticalSection, __in   DWORD dwSpinCount); */

void blkio_init()
{
#ifdef _MSC_VER
  _invalid_parameter_handler newHandler = blkio_iph;
  oldHandler = _set_invalid_parameter_handler(newHandler);
  InitializeCriticalSectionAndSpinCount(&CriticalSection, 0x00000000L); /* 0x80000400 */
#endif
  return;
}

void blkio_final()
{
#ifdef _MSC_VER
  DeleteCriticalSection(&CriticalSection);
  _set_invalid_parameter_handler(oldHandler);
#endif
  return;
}

/* Test whether the value returned by blkio_create_file(),
   blkio_open_modify_file(), or blkio_open_read_only_file() is a valid
   file-descriptor. */
int blkio_port_open_P (int fd, int writable_P)
{
  return -1 != fd;
}

/* _locking() described by
   http://msdn.microsoft.com/en-us/library/8054ew2f(VS.90).aspx */

int blkio_create_file(unsigned char *name,int bsiz)
{
#ifdef _MSC_VER
  int fd = _open((char *)name, O_BINARY | O_RDWR | O_CREAT, S_IWRITE | S_IREAD);
#else
# ifdef unix
  int fd = open((char *)name, O_RDWR | O_CREAT | O_TRUNC, 0666);
# else
  /* MinGW */
  int fd = open((char *)name, O_BINARY | O_RDWR | O_CREAT | O_TRUNC, 0666);
# endif
#endif
  if (-1==fd) {
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    return fd;
  }
  {
#ifdef _MSC_VER
    int lret = _locking(fd, _LK_NBLCK, (long)bsiz);
#else
# ifdef unix
    int lret = lockf(fd, F_TLOCK, 0L);
# else
    int lret = 0;
# endif
#endif
    if (0==lret) return fd;
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
#ifdef _MSC_VER
    if (-1==_close(fd)) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
#else
    if (-1==close(fd)) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
#endif
    return -1L;
  }
}

int blkio_open_modify_file(unsigned char *name, int bsiz)
{
#ifdef _MSC_VER
  int fd = _open((char *)name, O_BINARY | O_RDWR);
#else
# ifdef unix
  int fd = open((char *)name, O_RDWR);
# else
  /* MinGW */
  int fd = open((char *)name, O_BINARY | O_RDWR);
# endif
#endif
  if (-1==fd) {
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    return fd;
  }
  {
#ifdef _MSC_VER
    int lret = _locking(fd, _LK_NBLCK, (long)bsiz);
#else
# ifdef unix
    int lret = lockf(fd, F_TLOCK, 0L);
# else
    int lret = 0;
# endif
#endif
    if (0==lret) return fd;
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
#ifdef _MSC_VER
    if (-1==_close(fd)) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
#else
    if (-1==close(fd)) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
#endif
    return -1L;
  }
}

int blkio_open_read_only_file(unsigned char *name, int bsiz)
{
#ifdef _MSC_VER
  int fd = _open((char *)name, O_BINARY | O_RDONLY);
#else
# ifdef unix
  int fd = open((char *)name, O_RDONLY);
# else
  /* MinGW */
  int fd = open((char *)name, O_BINARY | O_RDONLY);
# endif  
#endif
  if (-1==fd) {
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
  }
  return fd;
}

/* This automatically releases any locks on fd */
void blkio_file_close(int fd, int bsiz, int mutable_P)
{
  int sts = 0L;
#ifdef _MSC_VER
  if (mutable_P) {
    long lsts;
    EnterCriticalSection(&CriticalSection);
    lsts = _lseek(fd, (0ULL)*bsiz, SEEK_SET);
    if (-1L!= lsts) sts = _locking(fd, _LK_UNLCK, (long)bsiz);
    LeaveCriticalSection(&CriticalSection);
    if (-1L==lsts) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
    if (-1==sts) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
  }
  sts = _close(fd);
#else
  sts = close(fd);
#endif
  if (-1==sts) {
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
  }
  return;
}

void blkio_flush_to_file(int fd, int metadata_P)
{
#ifdef _MSC_VER
  _commit(fd);
#else
# ifdef unix
  if (metadata_P) fsync(fd);
  else fdatasync(fd);
# endif
#endif
  return;
}

/* http://msdn.microsoft.com/en-us/library/ms682608%28VS.85%29.aspx */
/* void WINAPI EnterCriticalSection(__inout  LPCRITICAL_SECTION lpCriticalSection); */

/* http://msdn.microsoft.com/en-us/library/ms684169%28VS.85%29.aspx */
/* void WINAPI LeaveCriticalSection(__inout  LPCRITICAL_SECTION lpCriticalSection); */

int blkio_read(int fd, unsigned char *blk, int bsiz, long blknum)
{
#ifdef unix
  int rsiz = pread(fd, blk, bsiz, (blknum+0ULL)*bsiz);
#else
# ifdef _MSC_VER
  long lsts;
  int rsiz = -1;
  if (!blk) {
    dprintf((diagout, ">>>>ERROR<<<<  blkio_read: NULL blk argument\n"));
    return 0L;
  }
  EnterCriticalSection(&CriticalSection);
  errno = 0L;
  lsts = _lseek(fd, (blknum+0ULL)*bsiz, SEEK_SET);
  if (-1L != lsts) rsiz = _read(fd, blk, bsiz);
  LeaveCriticalSection(&CriticalSection);
# else
  /* >>>> not thread-safe <<<< */
  lseek(fd, (blknum+0ULL)*bsiz, SEEK_SET);
  int rsiz = read(fd, blk, bsiz);
# endif
#endif
  if (bsiz==rsiz) {
    if (io_diag_P) {
      dprintf((diagout, "rd:%*s%c %10ld\n", 1+((blk_level(blk))-(leaf)), "", blk_typ(blk), blknum));
    }
    if (blk_typ_P(blk, frl_typ)) {
      read_fl_ct = (read_fl_ct)+0x1L;
    }
    else read_ct = (read_ct)+0x1L;
    return 1L;
  }
  else {
#ifdef _MSC_VER
    if (-1L==lsts) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
    else
#endif
      if (-1==rsiz) {
	dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
      }
    dprintf((diagout, ">>>>ERROR<<<<  couldn't read blk %ld (read %d B)\n",
	     blknum, rsiz));
    return 0L;
  }
}

int blkio_write(int fd, unsigned char *blk, int bsiz, long blknum)
{
#ifdef unix
  int rsiz = pwrite(fd, blk, bsiz, (blknum+0ULL)*bsiz);
#else
# ifdef _MSC_VER
  long lsts;
  int rsiz = -1;
  if (!blk) {
    dprintf((diagout, ">>>>ERROR<<<<  blkio_write: NULL blk argument\n"));
    return 0L;
  }
  EnterCriticalSection(&CriticalSection);
  errno = 0L;
  lsts = _lseek(fd, (blknum+0ULL)*bsiz, SEEK_SET);
  if (-1L != lsts) { rsiz = _write(fd, blk, bsiz); }
  LeaveCriticalSection(&CriticalSection);
# else
  /* >>>> not thread-safe <<<< */
  lseek(fd, (blknum+0ULL)*bsiz, SEEK_SET);
  int rsiz = write(fd, blk, bsiz);
# endif
#endif
  if (bsiz==rsiz) {
    if (io_diag_P) {
      dprintf((diagout, "wr:%*s%c %10ld\n", 1+((blk_level(blk))-(leaf)), "", blk_typ(blk), blknum));
    }
    if (blk_typ_P(blk, frl_typ)) {
      write_fl_ct = (write_fl_ct)+0x1L;
    }
    else write_ct = (write_ct)+0x1L;
    return 1L;
  }
  else {
#ifdef _MSC_VER
    if (-1L==lsts) {
      dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    }
    else
#endif
      if (-1==rsiz) {
	dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
      }
    dprintf((diagout, ">>>>ERROR<<<<  couldn't write blk %ld; (wrote %d B)\n",
	     blknum, rsiz));
    return 0L;
  }
}

/*  Don't need to write block when extending. */

int blkio_file_extend(int fd, int bsiz, long blknum)
{
#ifdef _MSC_VER
  long lsts = _lseek(fd, (blknum+0ULL)*bsiz, SEEK_SET);
#else
  long lsts = lseek(fd, (blknum+0ULL)*bsiz, SEEK_SET);
#endif
  if (-1L != lsts) {
    if (io_diag_P) {
      dprintf((diagout, "Extended file to blk %ld\n", blknum));
    }
    return 1L;
  } else {
    dprintf((diagout, "%s: %d: %s\n", __FILE__, __LINE__, strerror(errno)));
    dprintf((diagout, ">>>>ERROR<<<<  couldn't extend file to blk %ld\n", blknum));
    return 0L;
  }
}
