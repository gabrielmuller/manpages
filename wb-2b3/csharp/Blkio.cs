/*  "Blkio.cs" WB-tree File Based Associative String Data Base System.
 * Copyright (C) 1991, 1992, 1993, 2000, 2003, 2007, 2009 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

// Set to true for diagnostic output for every block read and write.
public static bool ioDiag_P = false;

public static void blkio_Init()
{
    return;
}

public static void blkio_Final()
{
    return;
}

// Test whether the value returned by blkio_CreateFile(),
// blkio_OpenModifyFile(), or blkio_OpenReadOnlyFile() is a valid
// file-descriptor.
public static bool blkio_PortOpen_P(FileStream file, bool mutable_P)
{
    if (file == null) return false;
    return true;
}

// http://msdn.microsoft.com/en-us/library/system.io.filestream.lock.aspx
// Prevents other processes from changing the FileStream.
// public virtual void Lock(long position, long length)

public static FileStream blkio_CreateFile(String name,int bsiz)
{
    FileStream file = null;
    try {
	file = new FileStream(name, FileMode.Create, FileAccess.ReadWrite);
    } catch (IOException) {}
    if (null==file) return file;
    {
	byte []blk = new byte[bsiz];
	blkio_Write(file, blk, bsiz, 0);
    }
    try {
	file.Lock(0, bsiz);
    } catch (IOException) {
	file.Close();
	return null;
    }
    return file;
}

public static FileStream blkio_OpenModifyFile(String name,int bsiz)
{
    FileStream file = null;
    try {	// was FileMode.OpenOrCreate
	file = new FileStream(name, FileMode.Open,FileAccess.ReadWrite);
    } catch (IOException) {}
    if (null==file) return file;
    try {
	file.Lock(0, file.Length);
    } catch (IOException) {
	file.Close();
	return null;
    }
    return file;
}

public static FileStream blkio_OpenReadOnlyFile(String name,int bsiz)
{
    FileStream file = null;
    try {
	file = new FileStream(name, FileMode.Open, FileAccess.Read);
    } catch (IOException) {}
    return file;
}

// http://msdn.microsoft.com/en-us/library/system.io.filestream.unlock.aspx
// Allows access by other processes to all or part of a file that was
// previously locked.
// public virtual void Unlock(long position, long length)

public static void blkio_FileClose(FileStream file, int bsiz, bool mutable_P)
{
    lock (file) {
	file.Flush();
	if (mutable_P)
	    try {
		file.Unlock(0, bsiz); // was file.Length
	    } catch (IOException) {
		Console.Error.Write(">>>>ERROR<<<<  couldn't Unlock file.\n");
		//Console.Error.Write(">>>>ERROR<<<<  couldn't Unlock file \""+(File.Name)+"\".\n");
	    }
	file.Close();
    }
    return;
}

// http://msdn.microsoft.com/en-us/library/system.io.stream.flush.aspx
// public abstract void Flush()

public static void blkio_FlushToFile(FileStream file, bool metadata_P)
{
    lock (file) {
	file.Flush();
    }
    return;
}

// http://msdn.microsoft.com/en-us/library/c5kehkcz%28VS.71%29.aspx
// lock Statement
//
// The lock keyword marks a statement block as a critical section by
// obtaining the mutual-exclusion lock for a given object, executing a
// statement, and then releasing the lock. This statement takes the
// following form:
//
// lock(expression) statement_block
//
// where:
// expression
//     Specifies the object that you want to lock on. expression must
//     be a reference type.
//
//     Typically, expression will either be this, if you want to
//     protect an instance variable, or typeof(class), if you want to
//     protect a static variable (or if the critical section occurs in
//     a static method in the given class).
//
// statement_block
//     The statements of the critical section. 

// http://msdn.microsoft.com/en-us/library/system.io.stream.position.aspx
// public abstract long Position { get; set; }

// http://msdn.microsoft.com/en-us/library/system.io.stream.read.aspx
// public abstract int Read(byte[] buffer, int offset, int count)

public static bool blkio_Read(FileStream file,byte []blk,int bsiz,int blknum)
{
    long pos = bsiz*((long)blknum);
    int lrd;
    lock (file) {
	file.Position = pos;
	lrd = file.Read(blk, 0, bsiz);
    }
    if (bsiz==lrd) {
	if (ioDiag_P)
	    Console.Error.Write("rd:"+(1+blk_Level(blk)-leaf)+" "+(blk_Typ(blk))+"\n");
	if (blk_Typ_P(blk, frlTyp)) {
	    readFlCt = (readFlCt)+1;
	}
	else readCt = (readCt)+1;
	return (true);
    }
    else {
	Console.Error.Write(">>>>ERROR<<<<  couldn't read blk "+(blknum)+"\n");
	return false;
    }
}

// http://msdn.microsoft.com/en-us/library/system.io.stream.write.aspx
// public abstract void Write(byte[] buffer, int offset, int count)

public static bool blkio_Write(FileStream file,byte []blk,int bsiz,int blknum)
{
    long pos = bsiz*((long)blknum);
    try {
	lock (file) {
	    file.Position = pos;
	    file.Write(blk, 0, bsiz);
	}
	if (ioDiag_P)
	    Console.Error.Write("wr:"+(1+blk_Level(blk)-leaf)+" "+(blk_Typ(blk))+"\n");
	if (blk_Typ_P(blk, frlTyp)) {
	    writeFlCt = (writeFlCt)+1;
	}
	else writeCt = (writeCt)+1;
	return true;
    } catch (IOException)
	{
	    Console.Error.Write(">>>>ERROR<<<<  couldn't write blk "+(blknum)+"\n");
	    return false;
	}
}

/*  Don't need to write block when extending. */

public static bool blkio_FileExtend(FileStream file,int bsiz,int blknum)
{
    long pos = bsiz*((long)blknum);
    try {
	lock (file) {
	    file.Position = pos;
	    if (pos==file.Position) {
		if (ioDiag_P)
		    Console.Error.Write("Extending file to blk "+(blknum)+"\n");
	    }
	}
	return true;
    } catch (IOException) {
	Console.Error.Write(">>>>ERROR<<<<  couldn't extend file to blk "+(blknum)+"\n");
	return false;
    }
}
