/* "Seg.java" WB-tree File Based Associative String Data Base System.
 * Copyright (C) 1991, 1992, 1993, 2000, 2003 Free Software Foundation, Inc.
 * Copyright 2007 Clear Methods, Inc.
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
package wb;

import java.io.RandomAccessFile;
import static wb.Wbdefs.*;
import static wb.Lck.*;
import static wb.Han.*;

public class Seg {
    /* The SEG (segment) methods*/
    public static RandomAccessFile seg_Port(Seg seg)
    {
	return seg.PORT;
    }

    public static int seg_Bsiz(Seg seg)
    {
	return seg.BSIZ;
    }

    public static int seg_Used(Seg seg)
    {
	return seg.USED;
    }

    public static String seg_Str(Seg seg)
    {
	return seg.STRN;
    }

    public static Han seg_RtHan(Seg seg)
    {
	return seg.RT_HAN;
    }

    public static Han seg_FlHan(Seg seg)
    {
	return seg.FL_HAN;
    }

    public static Lck seg_Lck(Seg seg)
    {
	return seg.FLCK;
    }

    public static Lck seg_Fck(Seg seg)
    {
	return seg.FFCK;
    }

    public static int seg_FlcLen(Seg seg)
    {
	return seg.FLC_LEN;
    }

    public static int[] seg_Flc(Seg seg)
    {
	return seg.FLC;
    }

    public static int seg_Id(Seg seg)
    {
	return (null==seg) ? -1 : seg.ID;
    }

    public static boolean seg_Mutable_P(Seg seg)
    {
	return seg.FLC_LEN != -2;
    }

    public static Seg seg_Prv(Seg seg)
    {
	return seg.PRV;
    }

    public static Seg seg_Nxt(Seg seg)
    {
	return seg.NXT;
    }

    public static void seg_SetPort(Seg seg, RandomAccessFile port)
    {
	seg.PORT = port;
    }

    public static void seg_SetBsiz(Seg seg, int bsiz)
    {
	seg.BSIZ = bsiz;
    }

    public static void seg_SetUsed(Seg seg, int used)
    {
	seg.USED = used;
    }

    public static void seg_SetStr(Seg seg, String str)
    {
	seg.STRN = str;
    }

    public static void seg_SetFlcLen(Seg seg, int flcLen)
    {
	seg.FLC_LEN = flcLen;
    }

    public static void seg_SetFlc(Seg seg, int[] flc)
    {
	seg.FLC = flc;
    }

    public static void seg_SetPrv(Seg seg, Seg prv)
    {
	seg.PRV = prv;
    }

    public static void seg_SetNxt(Seg seg, Seg nxt)
    {
	seg.NXT = nxt;
    }

    public static Seg newSegd(int idx){
	Seg seg = new Seg();
	seg.RT_HAN = makeHand();
	seg.FL_HAN = makeHand();
	seg.FLCK = makeLck( 0-idx);
	seg.FFCK = makeLck(-1-idx);
	seg.ID = idx;
	return seg;
    }

    //	Segd members
    RandomAccessFile PORT;
    int BSIZ;
    int USED;
    String STRN;
    Han RT_HAN, FL_HAN;
    Lck FLCK;			//T - tentative; look at locking
    Lck FFCK;			//T
    int FLC_LEN;
    int []FLC;
    Seg PRV, NXT;
    int ID;
}
