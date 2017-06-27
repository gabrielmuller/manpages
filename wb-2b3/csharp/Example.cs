/* "Example.cs" WB-tree File Based Associative String Data Base System.
 * Copyright (C) 1991, 1992, 1993, 2000 Free Software Foundation, Inc.
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

using System;
using wb;

class Example
{
    public static void  Main(String[] args)
    {
	wb.wb.initWb(75, 150, 2048);
	wb.wb.Seg currentSeg = wb.wb.makeSeg("mydata", 2048);
	//wb.wb.closeSeg(currentSeg, true);
	//currentSeg = wb.wb.openSeg("mydata", true);

	wb.wb.Han pb = wb.wb.createDb(currentSeg, 'T', "phone-book");

	wb.wb.bt_Put(pb,  "LN",      "Doe");
	wb.wb.bt_Put(pb,  "FN",      "Joe");
	wb.wb.bt_Put(pb,  "PN",      "5551212");

	String val = wb.wb.bt_Next(pb, "LN");
	Console.WriteLine("val is " + val);

	val = wb.wb.bt_Get(pb,  "FN");
	Console.WriteLine("val is " + val);

	val = wb.wb.bt_Get(pb,   "PN");
	Console.WriteLine("val is " + val);

	wb.wb.closeSeg(currentSeg, false);
	wb.wb.finalWb();
    }
}
