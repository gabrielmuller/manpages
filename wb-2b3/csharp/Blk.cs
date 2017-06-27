/* "Blk.cs" WB-tree File Based Associative String Data Base System.
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

/*  BLK parameters */
/*  The IDs are 4 byte numbers identifying this block, the root of */
/*  this tree, and the next in the chain. */
public static readonly int blkIdPos = 0;

public static readonly int blkTopIdPos = 4;

public static readonly int blkNxtIdPos = 8;

public static readonly int blkTimePos = 0xc;
/*  blk-end-pos is position (stored in 2 bytes) of first free byte */

public static readonly int blkEndPos = 0x10;

public static readonly int blkLevelPos = 0x12;

public static readonly int blkTypPos = 0x13;

public static readonly int blkDataStart = 0x14;

public static int blk_Id(byte[] blk)
{
    return str2long(blk, blkIdPos);
}

public static int blk_TopId(byte[] blk)
{
    return str2long(blk, blkTopIdPos);
}

public static int blk_NxtId(byte[] blk)
{
    return str2long(blk, blkNxtIdPos);
}

public static int blk_Time(byte[] blk)
{
    return str2long(blk, blkTimePos);
}

public static int blk_End(byte[] blk)
{
    return str2short(blk, blkEndPos);
}

public static int blk_Level(byte[] b)
{
    return (b[blkLevelPos]);
}

public static int blk_Typ(byte[] b)
{
    return (b[blkTypPos]);
}

public static bool  blk_Typ_P(byte[] b,int typ)
{
    return ((b[blkTypPos]))==(typ);
}

public static void blk_SetId(byte[] blk,int id)
{
    long2str(blk, blkIdPos, id);
}

public static void blk_SetTopId(byte[] blk,int id)
{
    long2str(blk, blkTopIdPos, id);
}

public static void blk_SetNxtId(byte[] blk,int id)
{
    long2str(blk, blkNxtIdPos, id);
}

public static void blk_SetTime(byte[] blk,int time)
{
    long2str(blk, blkTimePos, time);
}

public static void blk_SetEnd(byte[] blk,int pos)
{
    short2str(blk, blkEndPos, pos);
}

public static void blk_SetLevel(byte[] b,int level)
{
    //b[blkTypPos] = -128 + typ;
	b[blkLevelPos] = (byte) (0xFF & level);
}

public static void blk_SetTyp(byte[] b,int typ)
{
    //b[blkTypPos] = -128+typ;
	b[blkTypPos] = (byte) (0xFF & typ);
}
