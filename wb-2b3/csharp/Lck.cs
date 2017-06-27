/* "Lck.cs" WB-tree File Based Associative String Data Base System.
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

public class Lck {
    public Lck next;
    public int flg;
    public int name;
}

public static Lck lastLck;

public static Lck makeLck(int name)
{
    Lck lk = new Lck();
    lk.next = lastLck;
    Interlocked.Exchange(ref lk.flg, 0);
    lk.name = name;
    lastLck = lk;
    return lk;
}

public static bool tryLck_P(Lck lk)
{
    if (1==Interlocked.Exchange(ref lk.flg, 1)) return false;
    return true;
}

public static void lck(Lck lk)
{
    if (1==Interlocked.Exchange(ref lk.flg, 1))
	Console.Error.Write(">>>>ERROR<<<< spinning on lck " + lk.name);
}

public static void unlck(Lck lk)
{
    if (0==Interlocked.Exchange(ref lk.flg, 0))
	Console.Error.Write(">>>>ERROR<<<< unlcking unlck "+ lk.name);
}

public static void checkLcks()
{
    Lck lck = lastLck;
    while (lck!=null) {
	if (lck.flg != 0) {
	    if (lck.name < 0)
		switch(lck.name) {
		case -1:
		    Console.Error.Write(">>>>ERROR<<<< free-ent-lck left lcked\n");
		    break;
		case -2:
		    Console.Error.Write(">>>>ERROR<<<< flush-buk-lck left lcked\n");
		    break;
		default:
		    Console.Error.Write(">>>>ERROR<<<< unknown lck left lcked\n");
		    break;
		}
	    else
		Console.Error.Write(">>>>ERROR<<<< lck " + lck.name + "  left lcked\n");
	    Interlocked.Exchange(ref lck.flg, 0);
	}
	lck = lck.next;
    }
}
