// "Blkio.java" WB-tree File Based Associative String Data Base System.
// Copyright (C) 1991, 1992, 1993, 2000, 2003, 2008, 2009 Free Software Foundation, Inc.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.

// http://java.sun.com/j2se/1.4.2/docs/api/java/io/RandomAccessFile.html#getChannel%28%29
// http://java.sun.com/j2se/1.4.2/docs/api/java/nio/ByteBuffer.html

// http://java.sun.com/j2se/1.4.2/docs/api/java/nio/channels/FileChannel.html
//
// File channels are safe for use by multiple concurrent threads.  The
// close method may be invoked at any time, as specified by the
// Channel interface.  Only one operation that involves the channel's
// position or can change its file's size may be in progress at any
// given time; attempts to initiate a second such operation while the
// first is still in progress will block until the first operation
// completes.  Other operations, in particular those that take an
// explicit position, may proceed concurrently; whether they in fact
// do so is dependent upon the underlying implementation and is
// therefore unspecified.
//
// The view of a file provided by an instance of this class is
// guaranteed to be consistent with other views of the same file
// provided by other instances in the same program.  The view provided
// by an instance of this class may or may not, however, be consistent
// with the views seen by other concurrently-running programs due to
// caching performed by the underlying operating system and delays
// induced by network-filesystem protocols.  This is true regardless
// of the language in which these other programs are written, and
// whether they are running on the same machine or on some other
// machine.  The exact nature of any such inconsistencies are
// system-dependent and are therefore unspecified.

// http://java.sun.com/j2se/1.4.2/docs/api/java/nio/channels/FileLock.html
//
// File locks are held on behalf of the entire Java virtual machine.
// They are not suitable for controlling access to a file by multiple
// threads within the same virtual machine.
//
// File-lock objects are safe for use by multiple concurrent threads.
// ...
// On some systems, closing a channel releases all locks held by the
// Java virtual machine on the underlying file regardless of whether
// the locks were acquired via that channel or via another channel
// open on the same file.  It is strongly recommended that, within a
// program, a unique channel be used to acquire all locks on any given
// file.
// 
// Some network filesystems permit file locking to be used with
// memory-mapped files only when the locked regions are page-aligned
// and a whole multiple of the underlying hardware's page size.  Some
// network filesystems do not implement file locks on regions that
// extend past a certain position, often 2^30 or 2^31.  In general,
// great care should be taken when locking files that reside on
// network filesystems.

package wb;

import static wb.Stats.*;
import static wb.Wbdefs.*;
import static wb.Wbsys.*;
import static wb.Blk.*;
import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.ByteBuffer.*;

public class Blkio {

// Set to true for diagnostic output for every block read and write.
public static boolean ioDiag_P = false;

public static void blkio_Init()
{
  return;
}

public static void blkio_Final()
{
  return;
}

// check to see if the file stream returned by blkio_CreateFile,
// blkio_OpenModifyFile, or blkio_OpenReadOnlyFile is valid.
public static boolean blkio_PortOpen_P(RandomAccessFile file, boolean mutable_P)
{
    if (file == null) return false;
    try {
	file.getFD();
    } catch (IOException e) {
	return false;
    }
    return true;
}

public static java.io.RandomAccessFile blkio_CreateFile(String name, int bsiz)
{
    RandomAccessFile file = null;
    try {
	file = new RandomAccessFile(name, "rws");
    } catch (FileNotFoundException e) {
	return null;
    }
    try {
	FileLock fl = file.getChannel().tryLock(0, Long.MAX_VALUE, false);
    } catch (IOException ex) {
	try {file.close();} catch (IOException e2) {}
	return null;
    } catch (OverlappingFileLockException e) {
	try {file.close();} catch (IOException e2) {}
	return null;
    }
    try {
	file.setLength(0L);
    } catch (IOException e) {
	try {file.close();} catch (IOException e2) {}
	return null;
    }
    return file;
}

public static java.io.RandomAccessFile blkio_OpenModifyFile(String name, int bsiz)
{
    RandomAccessFile file = null;
    try {
	file = new RandomAccessFile(name, "rws");
    } catch (FileNotFoundException e) {
	return null;
    }
    try {
	FileLock fl = file.getChannel().tryLock(0, Long.MAX_VALUE, false);
    } catch (IOException ex) {
	try {file.close();} catch (IOException e2) {}
	return null;
    } catch (OverlappingFileLockException e) {
	try {file.close();} catch (IOException e2) {}
	return null;
    }
    return file;
}

public static java.io.RandomAccessFile blkio_OpenReadOnlyFile(String name, int bsiz)
{
    RandomAccessFile file = null;
    try {
	file =  new RandomAccessFile(name, "r");
    } catch (FileNotFoundException e) {
	return null;
    }
    try {
	FileLock fl = file.getChannel().tryLock(0, Long.MAX_VALUE, true);
    } catch (IOException ex) {
	try {file.close();} catch (IOException e2) {}
	return null;
    } catch (OverlappingFileLockException e) {
	try {file.close();} catch (IOException e2) {}
	return null;
    }
    return file;
}

public static void blkio_FileClose(java.io.RandomAccessFile file, int bsiz,  boolean mutable_P)
{
    try {file.close();} catch (IOException e) {}
}

public static void blkio_FlushToFile(java.io.RandomAccessFile file, boolean metadata_P)
{
    try {file.getChannel().force(metadata_P);} catch (IOException e) {}
}

//file read and write
public static boolean blkio_Read(java.io.RandomAccessFile file,byte []blk, int bsiz, int blknum)
{
    long pos = bsiz*(long)blknum;
    //try {file.seek(pos);} catch (IOException e) {return false;}
    try {
	if (bsiz==file.getChannel().read(ByteBuffer.wrap(blk, 0, bsiz), pos)) {
	    if (ioDiag_P)
		System.err.print("rd:"+(1+((blk_Level(blk))-(leaf)))+""+""+" "+(blk_Typ(blk))+"\n");
	    if (blk_Typ_P(blk, frlTyp)) {
		readFlCt = (readFlCt)+1;
	    }
	    else readCt = (readCt)+1;
	    return true;
	}
	System.err.print(">>>>ERROR<<<<  couldn't read all of blk "+(blknum)+"\n");
	return false;
    } catch (IOException e) {
	System.err.print(">>>>ERROR<<<<  couldn't read blk "+(blknum)+"\n");
	return false;
    }
}

public static boolean blkio_Write(java.io.RandomAccessFile file, byte[]blk, int bsiz, int blknum)
{
    long pos = bsiz*(long)blknum;
    //try {file.seek(pos);} catch (IOException e) {return false;}
    try {
	file.getChannel().write(ByteBuffer.wrap(blk, 0, bsiz), pos);
	if (ioDiag_P)
	    System.err.print("wr:"+(1+((blk_Level(blk))-(leaf)))+""+""+" "+(blk_Typ(blk))+"\n");
	if (blk_Typ_P(blk, frlTyp)) {
	    writeFlCt = (writeFlCt)+1;
	}
	else writeCt = (writeCt)+1;
	return true;
    } catch (IOException e) {
	System.err.print(">>>>ERROR<<<<  couldn't write blk "+(blknum)+"\n");
	return false;
    }
}

/*  Don't need to write block when extending. */

public static boolean blkio_FileExtend(java.io.RandomAccessFile file, int bsiz, int blknum)
{
    long pos = bsiz*(long)blknum;
    try {
	file.seek(pos);
	if (ioDiag_P)
	    System.err.print("Extending file to blk "+(blknum)+"\n");
	return true;
    } catch (IOException e) {
	System.err.print(">>>>ERROR<<<<  couldn't extend file to blk "+(blknum)+"\n");
	return false;
    }
}

}
