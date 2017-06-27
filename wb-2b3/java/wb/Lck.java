/* "Lck.java" WB-tree File Based Associative String Data Base System.
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
import java.util.concurrent.atomic.*;
import java.lang.Thread;
import static wb.Wbsys.*;

//This implementation works only with Java 5 and beyond.
public class Lck {

    public static Lck makeLck(int name)
    {
	Lck lk = new Lck();
	lk.next = lastLck;
	lk.flg =  new AtomicBoolean();
	lk.name = name;
	lastLck = lk;
	return lk;
    }

    public static boolean tryLck_P(Lck lk)
    {
	if (lk.flg.getAndSet(true)) return false;
	return true;
    }

    public static void lck(Lck lk)
    {
	while (lk.flg.getAndSet(true)) {
	    Thread.yield();
	    // System.err.print(">>>>ERROR<<<< spinning on lck " + lk.name + "\n");
	}
    }

    public static void unlck(Lck lk)
    {
	if (!lk.flg.getAndSet(false))
	    System.err.print(">>>>ERROR<<<< unlcking unlck "+ lk.name + "\n");
    }

    public static void checkLcks()
    {
	Lck lck = lastLck;
	while (lck!=null) {
	    if (lck.flg.get()) {
		if (lck.name < 0)
		    switch (lck.name) {
		    case -1:
			System.err.print(">>>>ERROR<<<< free-ent-lck left lcked\n");
			break;
		    case -2:System.err.print(">>>>ERROR<<<< flush-buk-lck left lcked\n");
			break;
		    default:
			System.err.print(">>>>ERROR<<<< unknown lck left lcked\n");
		    }
		else
		    System.err.print(">>>>ERROR<<<< lck " + lck.name + "  left lcked\n");
		lck.flg.set(false);
	    }
	    lck = lck.next;
	}
    }

    Lck next;
    static Lck lastLck;
    AtomicBoolean flg;
    int name;
}
