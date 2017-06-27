/* "Ent.cs" WB-tree File Based Associative String Data Base System.
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

public class Ent {
    public Ent NEXT;
    public int ID;
    public byte[] BLK;
    public bool DTY;
    public int TAG, AGE, PUS, ACC, REF;
    public Seg SEG;
}

public static Ent makeEntry()
{
    return new Ent();
}

public static Ent ent_MakeEnt(int tag)
{
    //System.out.println("blkSize = " + blkSize);
    Ent ent = new Ent();
    ent.TAG = tag;
    //ent.SEG = null;
    ent.ID  = -1;
    ent.BLK = new byte[blkSize];
    return ent;
}

//The getter methods in C take struct entry as a parameter.
// Here we use object methods, thus the function ent_tag(ent)
//in C appears as ent.entTag() in Java.
public static int ent_Tag(Ent entry)
{
    return entry.TAG;
}

public  static Ent  ent_Next(Ent entry)
{
    return entry.NEXT;
}

public static Seg ent_Seg(Ent entry)
{
    return entry.SEG;
}

public static int ent_Id(Ent entry)
{
    return entry.ID;
}

public static byte[] ent_Blk(Ent entry)
{
    return entry.BLK;
}

public static int ent_Age(Ent entry)
{
    return entry.AGE;
}

public static bool  ent_Dty_P(Ent entry)
{
    return entry.DTY;
}

public static int ent_Pus(Ent entry)
{
    return entry.PUS;
}

public static int ent_Acc(Ent entry)
{
    return entry.ACC;
}

public static int ent_Ref(Ent entry)
{
    return entry.REF;
}
//As with getter methods, we do not take  ry object as parameter,
//but invoke methods on the objects.

public static void ent_SetTag(Ent entry, int tag)
{
    entry.TAG = tag;
}

public static void ent_SetNext(Ent entry, Ent next)
{
    entry.NEXT = next;
}

public static void ent_SetSeg(Ent entry, Seg seg)
{
    entry.SEG = seg;
}

public static void ent_SetId(Ent entry, int num)
{
    entry.ID = num;
}

public static void ent_SetAge(Ent entry, int age)
{
    entry.AGE = age;
}

public static void ent_SetDty(Ent entry, Object A) //for handling nulls.
{
    entry.DTY = ((A==null)? false:true);
}

public static void ent_SetDty(Ent entry, bool dty)
{
    entry.DTY = dty;
}

public static void ent_SetPus(Ent entry, int pus)
{
    entry.PUS = pus;
}

public static void ent_SetAcc(Ent entry, int acc)
{
    entry.ACC = acc;
}

public static void ent_SetRef(Ent entry, int Ref)
{
    entry.REF = Ref;
}
