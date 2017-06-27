/*  "blkio.c" WB-tree File Based Associative String Data Base System. */
/*  Copyright (C) 1991, 1992, 1993, 2000, 2003, 2008, 2009 Free Software Foundation, Inc. */
/*  */
/*  This program is free software: you can redistribute it and/or modify */
/*  it under the terms of the GNU Lesser General Public License as */
/*  published by the Free Software Foundation, either version 3 of the */
/*  License, or (at your option) any later version. */
/*  */
/*  This program is distributed in the hope that it will be useful, but */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU */
/*  Lesser General Public License for more details. */
/*  */
/*  You should have received a copy of the GNU Lesser General Public */
/*  License along with this program.  If not, see */
/*  <http://www.gnu.org/licenses/>. */

extern int io_diag_P;

SCM_EXPORT void blkio_init P((void));

SCM_EXPORT void blkio_final P((void));

SCM_EXPORT int blkio_create_file P((unsigned char *name,int bsiz));

SCM_EXPORT int blkio_open_modify_file P((unsigned char *name,int bsiz));

SCM_EXPORT int blkio_open_read_only_file P((unsigned char *name,int bsiz));

SCM_EXPORT int blkio_port_open_P P((int fd, int writable_P));

SCM_EXPORT void blkio_file_close P((int fd, int bsiz, int mutable_P));

SCM_EXPORT void blkio_flush_to_file P((int fd, int metadata_P));

SCM_EXPORT int blkio_read P((int fd,unsigned char *blk,int bsiz,long blknum));

SCM_EXPORT int blkio_write P((int fd,unsigned char *blk,int bsiz,long blknum));

SCM_EXPORT int blkio_file_extend P((int fd,int bsiz,long blknum));
