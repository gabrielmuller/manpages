/* "TestMT.java" WB-tree File Based Associative String Data Base System.
 * Copyright (C) 1991, 1992, 1993, 2000 Free Software Foundation, Inc.
 * Copyright 2010 Clear Methods, Inc.
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

//import static wb.Db.*;
import static wb.Segs.*;
import static wb.Ents.initWb;
import static wb.Ents.finalWb;
import wb.Han;

public class TestMT {

    public static void main(String[] args)
    {
	initWb(75, 150, 2048);
	wb.Seg seg = makeSeg("empty.wb", 2048);
	if (null != seg) {
	    closeSeg(seg, false);
	    seg = openSeg("unclean", true);
	    if (null != seg) closeSeg(seg, false);
	    seg = openSeg("mydata", true);
	    if (null != seg) closeSeg(seg, false);
	}
	finalWb();
    }
}
