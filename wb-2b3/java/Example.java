/* "Example.java" WB-tree File Based Associative String Data Base System.
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

/*	       MUMPS Style Database Phone Book Example */

//package wb;

import static wb.Db.*;
import static wb.Segs.*;
import wb.Han;

public class Example {

    public static String makeName(int arg1,String args)
    {
	return arg1 + args;
    }

    public static String makeName(int arg1, int args)
    {
	return arg1 + "" +  args;
    }

    public static String makeName(String arg1, int args)
    {
	return arg1 + args;
    }

    public static String makeName(String arg1, String args)
    {
	return arg1 + args;
    }

    public static void runExample()
    {
	initWb(75, 150, 2048);
	wb.Seg seg = makeSeg("mydata", 2048);
//	closeSeg(seg, true);
//	seg = openSeg("mydata", true);
	// "phone-index" which we will use for indexing by phone number.
	Han pi = createDb(seg, 'T', "phone-index");
	// "phone-book" which will contain the phone book records.
	Han pb = createDb(seg, 'T', "phone-book");
	// create an array called  "lastname-index" which we will
	// use for indexing by last name
	Han lni = createDb(seg, 'T', "lastname-index");
	int recordNumber = 0;

	bt_Put(pb, makeName(recordNumber, "LN"), "Doe");
	bt_Put(pb, makeName(recordNumber, "FN"), "Joe");
	bt_Put(pb, makeName(recordNumber, "PN"), "5551212");
	bt_Put(pb, makeName(recordNumber, "AD1"), "13 Hi St.");
	bt_Put(pb, makeName(recordNumber, "CITY"), "Podunk");
	bt_Put(pb, makeName(recordNumber, "ST"), "NY");
	bt_Put(pb, makeName(recordNumber, "ZIP"), "10000");
	bt_Put(lni, makeName("Doe", recordNumber), "");
	bt_Put(pi, makeName("5551212", recordNumber), "ravi");

        String val = bt_Get(pb, makeName(recordNumber, "LN"));
	System.out.println("val is " + val);
	val = bt_Get(pb, makeName(recordNumber, "ZIP"));
	System.out.println("val is " + val);
	val = bt_Get(lni, makeName("Doe", recordNumber));
	System.out.println("val is " + val);
	val = bt_Get(pi, makeName("5551212", recordNumber));
	System.out.println("val is " + val);
	//bt_Scan(pb, 0, "", "zz", lambda(k(v), print(k, v), true),  -1);
	closeSeg(seg, false);
    }

    public static void  main(String[] args)
    {
	runExample();
    }
}
