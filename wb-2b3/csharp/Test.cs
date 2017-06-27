/* "Test.cs" WB-tree File Based Associative String Data Base System.
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

/* (require 'stdio) */
/* (slib:load-source (in-vicinity (program-vicinity) "wbsys.scm")) */

/* (define diagout stdout) */

/*   call (SCANHAN root-handle [test-prev? #f] [verbose? #t]) */
//package wb;
using System;
using System.Collections;
using wb;

namespace wb
{
    class Test
    {
	private static bool stringEql_P(String a, String b) {
	    return a.Equals(b);
	}

	private static ArrayList cons(object elem, ArrayList list) {
	    list.Insert(0, elem);
	    return list;
	}

	private static int Length(ArrayList v) {
	    return v.Count;
	}

	public static ArrayList scanhan(wb.Han han, params Int32[] args)
	{
	    int errors = 0;
	    bool testPrev_P = true; //(args.Length > 0)? args[0] : 0;
	    bool verbose_P = (args.Length > 1) ? wb.a2b(args[1]) : false;

	    Console.Error.Write("FORWARD key scan\n");
	    {
		String key = null;
		String prior = null;
		bool init_P = true;
		ArrayList list = new ArrayList();

		Lloop: while (true) {
		    if (verbose_P) {
			Console.Error.Write("NEXT KEY IS ");
			Console.Error.Write(key);
			Console.Error.Write("\n");
		    }
		    if (testPrev_P && !init_P) {
			String prevKey = wb.bt_Prev(han, key);
			if ((wb.a2b(prevKey)
			     && wb.a2b(prior)
			     && (!(stringEql_P(prevKey, prior))))
			    || (wb.a2b(prevKey)
				&& (!wb.a2b(prior)))
			    || (wb.a2b(prior)
				&& (!wb.a2b(prevKey))
				&& (!stringEql_P(prior, ""))))
			    {
				errors = 1+(errors);
				Console.Error.Write("NEXT/PREV error: key= ");
				Console.Error.Write(prior);
				Console.Error.Write("\n");
				Console.Error.Write("   next= ");
				Console.Error.Write(key);
				Console.Error.Write(" prev= ");
				Console.Error.Write(prevKey);
				Console.Error.Write("\n");
			    }
		    }
		    if ((wb.a2b(key) && (""!=(key))) || init_P) {
			String Tkey = wb.bt_Next(han, key);
			prior = key;
			init_P = false;
			list = wb.a2b(key)
			    ?cons(key, list)
			    :list;
			key = Tkey;
			goto Lloop;
		    }
		    else {
			if (testPrev_P)
			    Console.Error.Write("scanhan: " + (errors) + " next/prev errors found.");
			Console.Error.Write("scanhan: " + (Length(list)) + " items found.\n");
			return list;
		    }
		}
	    }
	}


	public static int countKeys(wb.Han han)
	{
	    {
		String key = "";
		int ct = 0;
		bool init_P = true;
		Lloop: while (true) {
		    if ((wb.a2b(key) && (""!=(key))) || init_P) {
			key = wb.bt_Next(han, key);
			ct = (ct)+1;
			init_P = false;
			goto Lloop;
		    }
		    else return ct;
		}
	    }
	}


	public static ArrayList scanb(wb.Han han)
	{
	    Console.Error.Write("REVERSE key scan\n");
	    {
		String key = null;
		bool init_P = true;
		ArrayList list = new ArrayList();

		Lloop: while (true) {
		    Console.Error.Write("PREV KEY IS ");
		    Console.Error.Write(key);
		    Console.Error.Write("\n");
		    if ((wb.a2b(key) && (""!=(key))) || init_P) {
			String Tkey = wb.bt_Prev(han, key);
			init_P = false;
			list = wb.a2b(key)
			    ?cons(key, list)
			    :list;
			key = Tkey;
			goto Lloop;
		    }
		    else {
			Console.Error.Write("%s: %d items found.\n"+("scanb")+(Length(list)));
			return list;
		    }
		}
	    }
	}

	public static int dbSize(wb.Han han)
	{
	    return (wb.seg_Used(wb.han_Seg(han)))+1;
	}

	public static wb.Han currentBt = null;
	public static wb.Seg currentSeg;
	public static int addKeyNum = 0;
	public static String addStr = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";

	/*  ADD! count [first-key add-key-num] [key-increment 1]  [value-string "abc...xyz"x2] */


	public static void add(int n, params int[] args)
	{
	    int curKeyNum =(args.Length > 0)? args[0] : addKeyNum;
	    int incr = (args.Length > 1)?  args[1] : 1;
	    String addstr = (args.Length > 2)?  args[2] + "" : addStr;
	    int i = 1;
	    while (!(i > n)) {
		String keyStr = curKeyNum + "";
		curKeyNum = (incr)+(curKeyNum);
		wb.bt_Put(currentBt, keyStr, addstr);
		i = (i)+1;
	    }
	    addKeyNum = Math.Max(addKeyNum, curKeyNum);
	}

	/*  REMOVE! count first-key [key-increment 1] [unused-arg] */
	public static void remove(int n, int curKey, params int[] args)
	{
	    int incr = (args.Length > 0) ? args[0] : 1;
	    //int startTrace = (args.Length > 1) ? args[1] : 0;
	    int i = 1;
	    while (!(i > n)) {
		String keyStr = curKey + "";
		curKey = (incr)+(curKey);
		wb.bt_Rem(currentBt, keyStr);
		i = (i)+1;
	    }
	    return;
	}

	/*  old test code, still useful! */


	public static void test0(wb.Han bHan)
	{
	    wb.bt_Put(bHan, "foo", "bar");
	    wb.bt_Put(bHan, "foz", "oof");
	    wb.bt_Put(bHan, "fonz", "zonf");
	    wb.bt_Put(bHan, "foo", "raboof");
	    wb.bt_Put(bHan, "food", "thought");
	    scanhan(bHan);
	    wb.bt_Rem(bHan, "foo");
	    wb.bt_Rem(bHan, "fonz");
	    wb.bt_Put(bHan, "foo", "foo=bar");
	    wb.bt_Put(bHan, "foz", "foz=oof");
	    wb.bt_Put(bHan, "fonz", "fonz=zonf");
	    wb.bt_Put(bHan, "foo", "foo=raboof");
	    wb.bt_Put(bHan, "food", "food=for thought");
	    wb.bt_Put(bHan, "very very very long key field", "very very very longer value field");
	    scanhan(bHan);
	}

	/*  THIS IS TEST CASE #1. */
	/*  It is used to test the addition of data elements with similar names */
	/*  to see if the repeat count routine is working correctly */
	public static void test1()
	{
	    wb.Han aHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(aHan, "cat", "6");
	    wb.bt_Put(aHan, "caddy", "4");
	    wb.bt_Put(aHan, "catalytic", "7");
	    wb.bt_Put(aHan, "cadalack", "3");
	    wb.bt_Put(aHan, "catastrophy", "10");
	    wb.bt_Put(aHan, "catastrophic", "9");
	    wb.bt_Put(aHan, "cad", "2");
	    wb.bt_Put(aHan, "cadjole", "5");
	    wb.bt_Put(aHan, "cataract", "8");
	    wb.bt_Put(aHan, "cacky", "1");
	    wb.bt_Put(aHan, "d", "12");
	    scanhan(aHan);
	    Console.Error.Write("   TEST 1: nexting on block 'a' index 'cadz'\n");
	    {
		String ansStr = wb.bt_Next(aHan, "cadz");
		Console.Error.Write(ansStr);
	    }
	    Console.Error.Write("   TEST 1: removing 'd' from 'a'\n");
	    wb.bt_Rem(aHan, "d");
	    scanhan(aHan);
	    Console.Error.Write("   TEST 1: removing 'cad' from 'a'\n");
	    wb.bt_Rem(aHan, "cad");
	    scanhan(aHan);
	    wb.closeBt(aHan);
	}

	/*  THIS IS TEST #2 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block completely then adds a new data element and value */
	/*  to the block to make it split */
	public static void test2()
	{
	    wb.Han bHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(bHan, "foo", "bar");
	    wb.bt_Put(bHan, "foz", "oof");
	    wb.bt_Put(bHan, "fonz", "zonf");
	    wb.bt_Put(bHan, "foo", "raboof");
	    wb.bt_Put(bHan, "food", "thought");
	    scanhan(bHan);
	    wb.bt_Rem(bHan, "foo");
	    wb.bt_Rem(bHan, "fonz");
	    scanhan(bHan);
	    Console.Error.Write("   TEST 2: creating a full block named 'b'\n");
	    wb.bt_Put(bHan, "foo", "1234567");
	    wb.bt_Put(bHan, "foz", "12345678901");
	    wb.bt_Put(bHan, "fonz", "123456789");
	    wb.bt_Put(bHan, "foo", "1234567890");
	    wb.bt_Put(bHan, "food", "1234567890123456");
	    wb.bt_Put(bHan, "test", "123456789012345678901234");
	    Console.Error.Write("   TEST 2: doing a PUT to fill the block\n");
	    wb.bt_Put(bHan, "a", "12345678");
	    scanhan(bHan);
	    wb.closeBt(bHan);
	}

	/*  THIS IS TEST #3 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block , then it tries to make the block split by making */
	/*  the first data element 1 char bigger by replacement */
	public static void test3()
	{
	    wb.Han cHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(cHan, "foo", "1234567");
	    wb.bt_Put(cHan, "foz", "12345678901");
	    wb.bt_Put(cHan, "fonz", "123456789");
	    wb.bt_Put(cHan, "foo", "1234567890");
	    wb.bt_Put(cHan, "food", "1234567890123456");
	    wb.bt_Put(cHan, "test", "123456789012345678901234");
	    wb.bt_Put(cHan, "a", "12345678");
	    scanhan(cHan);
	    Console.Error.Write("   TEST 3: split block by increasing first element by one char with PUT\n");
	    wb.bt_Put(cHan, "a", "123456789");
	    scanhan(cHan);
	    wb.closeBt(cHan);
	}

	/*  THIS IS TEST #4 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block , then it tries to make the block split by making */
	/*  a middle data element 1 char bigger by replacement */
	public static void test4()
	{
	    wb.Han dHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(dHan, "foo", "1234567");
	    wb.bt_Put(dHan, "foz", "12345678901");
	    wb.bt_Put(dHan, "fonz", "123456789");
	    wb.bt_Put(dHan, "foo", "1234567890");
	    wb.bt_Put(dHan, "food", "1234567890123456");
	    wb.bt_Put(dHan, "test", "123456789012345678901234");
	    wb.bt_Put(dHan, "a", "12345678");
	    scanhan(dHan);
	    Console.Error.Write("   TEST 4: split block by incresing value of a middle element by one with PUT\n");
	    wb.bt_Put(dHan, "foz", "123456789012");
	    scanhan(dHan);
	    wb.closeBt(dHan);
	}

	/*  THIS IS TEST #5 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block , then it tries to make the block split by making */
	/*  the last data element 1 char bigger by replacement */
	public static void test5()
	{
	    wb.Han eHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(eHan, "foo", "1234567");
	    wb.bt_Put(eHan, "foz", "12345678901");
	    wb.bt_Put(eHan, "fonz", "123456789");
	    wb.bt_Put(eHan, "foo", "1234567890");
	    wb.bt_Put(eHan, "food", "1234567890123456");
	    wb.bt_Put(eHan, "test", "123456789012345678901234");
	    wb.bt_Put(eHan, "a", "12345678");
	    scanhan(eHan);
	    Console.Error.Write("TEST 5: splitblock by increasing value of the \n"
				+ "last data element by one with PUT\n");
	    wb.bt_Put(eHan, "test", "12345678901234567890123456");
	    scanhan(eHan);
	    wb.closeBt(eHan);
	}

	/*  THIS IS TEST #6 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block , then it deletes the first element. It then */
	/*  tries to make the block split by reentering the first data element */
	/*  with 1 more char than the original */
	public static void test6()
	{
	    wb.Han fHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(fHan, "foo", "1234567");
	    wb.bt_Put(fHan, "foz", "12345678901");
	    wb.bt_Put(fHan, "fonz", "123456789");
	    wb.bt_Put(fHan, "foo", "1234567890");
	    wb.bt_Put(fHan, "food", "1234567890123456");
	    wb.bt_Put(fHan, "test", "123456789012345678901234");
	    wb.bt_Put(fHan, "a", "12345678");
	    scanhan(fHan);
	    Console.Error.Write("   TEST 6: split block by deleting the first data element with REM\n");
	    Console.Error.Write("   TEST 6: then reentering the first data element with 1 more char\n");
	    wb.bt_Rem(fHan, "a");
	    Console.Error.Write("   TEST 6: doing a PUT\n");
	    wb.bt_Put(fHan, "a", "123456789");
	    scanhan(fHan);
	    wb.closeBt(fHan);
	}

	/*  THIS IS TEST #7 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block , then it deletes a middle element. It then */
	/*  tries to make the block split by reentering a middle element with 1 */
	/*  more char than the original */
	public static void test7()
	{
	    wb.Han gHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(gHan, "foo", "1234567");
	    wb.bt_Put(gHan, "foz", "12345678901");
	    wb.bt_Put(gHan, "fonz", "123456789");
	    wb.bt_Put(gHan, "foo", "1234567890");
	    wb.bt_Put(gHan, "food", "1234567890123456");
	    wb.bt_Put(gHan, "test", "123456789012345678901234");
	    wb.bt_Put(gHan, "a", "12345678");
	    scanhan(gHan);
	    Console.Error.Write("   TEST 7: split block by deleting a middle data element\n");
	    Console.Error.Write("   TEST 7: then reentering a middle data element with 1 more char\n");
	    Console.Error.Write("   TEST 7: than the original value.\n");
	    wb.bt_Rem(gHan, "foo");
	    Console.Error.Write("   TEST 7: doing a PUT\n");
	    wb.bt_Put(gHan, "foo", "12345678901");
	    scanhan(gHan);
	    wb.closeBt(gHan);
	}

	/*  THIS IS TEST #8 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block , then it deletes the last element. It then */
	/*  tries to make the block split by reentering the last element with 1 */
	/*  more char than the original */
	public static void test8()
	{
	    wb.Han hHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(hHan, "foo", "1234567");
	    wb.bt_Put(hHan, "foz", "12345678901");
	    wb.bt_Put(hHan, "fonz", "123456789");
	    wb.bt_Put(hHan, "foo", "1234567890");
	    wb.bt_Put(hHan, "food", "1234567890123456");
	    wb.bt_Put(hHan, "test", "123456789012345678901234");
	    wb.bt_Put(hHan, "a", "12345678");
	    scanhan(hHan);
	    Console.Error.Write("   TEST 8: split block by deleting the last data element\n");
	    Console.Error.Write("   TEST 8: then reentering the last data element with 1 more char\n");
	    Console.Error.Write("   TEST 8: than the original value.  Block name is 'h'\n");
	    wb.bt_Rem(hHan, "test");
	    Console.Error.Write("   TEST 8: doing a PUT\n");
	    wb.bt_Put(hHan, "test", "12345678901234567890123456");
	    scanhan(hHan);
	    wb.closeBt(hHan);
	}

	/*  THIS IS TEST #9 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block completely then adds a new data element and value */
	/*  to the block to make it split */
	public static void test9()
	{
	    wb.Han iHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(iHan, "foz", "12345678901");
	    wb.bt_Put(iHan, "fonz", "123456789");
	    wb.bt_Put(iHan, "foo", "1234567890");
	    wb.bt_Put(iHan, "food", "1234567890123456");
	    wb.bt_Put(iHan, "test", "123456789012345678901234");
	    wb.bt_Put(iHan, "a", "12345678");
	    Console.Error.Write("   TEST 9: The block 'i' is full. Now we are adding a new index to the begining\n");
	    Console.Error.Write("   TEST 9: of the block with a value that should make the block split\n");
	    wb.bt_Put(iHan, "1", "123456789");
	    scanhan(iHan);
	    wb.closeBt(iHan);
	}

	/*  THIS IS TEST #10 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block completely then adds a new data element and value */
	/*  to the block to make it split */
	public static void test10()
	{
	    wb.Han jHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(jHan, "foz", "12345678901");
	    wb.bt_Put(jHan, "fonz", "123456789");
	    wb.bt_Put(jHan, "foo", "1234567890");
	    wb.bt_Put(jHan, "food", "1234567890123456");
	    wb.bt_Put(jHan, "test", "123456789012345678901234");
	    wb.bt_Put(jHan, "a", "12345678");
	    Console.Error.Write("   TEST 10: The block 'j' is full. Now we are adding a new index to the middle\n");
	    Console.Error.Write("   TEST 10: of the block with a value that should make the block split\n");
	    wb.bt_Put(jHan, "fooa", "123456789");
	    scanhan(jHan);
	    wb.closeBt(jHan);
	}

	/*  THIS IS TEST #11 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block completely then adds a new data element and value */
	/*  to the block to make it split */
	public static void test11()
	{
	    wb.Han kHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(kHan, "foz", "12345678901");
	    wb.bt_Put(kHan, "fonz", "123456789");
	    wb.bt_Put(kHan, "foo", "1234567890");
	    wb.bt_Put(kHan, "food", "1234567890123456");
	    wb.bt_Put(kHan, "test", "123456789012345678901234");
	    wb.bt_Put(kHan, "a", "12345678");
	    Console.Error.Write("   TEST 11: The block 'k' is full. Now we are adding a new index to the end\n");
	    Console.Error.Write("   TEST 11: of the block with a value that should make the block split\n");
	    wb.bt_Put(kHan, "zzz", "123456789");
	    scanhan(kHan);
	    wb.closeBt(kHan);
	}

	/*  THIS IS TEST #12 FOR BLOCK SPLITTING STUFF */
	/*  this test fills a block completely then adds a new data element and value */
	/*  to the block to make it split */
	public static void test12()
	{
	    wb.Han kHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(kHan, "132", "12345678901");
	    wb.bt_Put(kHan, "1233", "1234567890");
	    wb.bt_Put(kHan, "26", "1234567890123456");
	    wb.bt_Put(kHan, "275", "123456789012345678901234");
	    wb.bt_Put(kHan, "84", "12345678");
	    Console.Error.Write("TEST 12: The block 'k' is full. Now we are \n"+
				"adding a new index pastp after b-pos\n");
	    Console.Error.Write("   TEST 12: of the block with a value that should make the block split\n");
	    wb.bt_Put(kHan, "82", "123456789");
	    scanhan(kHan);
	    wb.closeBt(kHan);
	}


	public static void test13()
	{
	    wb.Han kHan = wb.createBt(currentSeg, wb.indTyp, 0);
	    wb.bt_Put(kHan, "0132", "12345678901");
	    wb.bt_Put(kHan, "01233", "1234567890");
	    wb.bt_Put(kHan, "026", "1234567890123456");
	    wb.bt_Put(kHan, "0275", "123456789012345678901234");
	    wb.bt_Put(kHan, "084", "12345678");
	    Console.Error.Write("TEST 13: The block 'k' is full. Now we are adding"+
				" a new index pastp after b-pos\n");
	    Console.Error.Write("   TEST 13: of the block with a value that should make the block split\n");
	    wb.bt_Put(kHan, "082", "123456789");
	    scanhan(kHan);
	    wb.closeBt(kHan);
	}



	public static void test128()
	{
	    test1();
	    test2();
	    test3();
	    test4();
	    test5();
	    test6();
	    test7();
	    test8();
	    test9();
	    test10();
	    test11();
	    test12();
	    test13();
	}


	public static void Main(String[] args)
	{
	    wb.initWb(75, 150, 2048);
	    Console.Error.Write("make-seg\n");
	    currentSeg = wb.makeSeg("z", 0x80);
	    if (null != currentSeg) {
		//Console.Error.Write("close-seg\n");
		//wb.closeSeg(currentSeg, true);
		//Console.Error.Write("open-seg\n");
		//currentSeg = wb.openSeg("z", true);
		Console.Error.Write("create-bt\n");
		currentBt = wb.createBt(currentSeg, wb.indTyp, 0);
		Console.Error.Write("test0!\n");
		test0(currentBt);
		Console.Error.Write("test!\n");
		test128();
		Console.Error.Write("add! 10\n");
		add(0xa);
		wb.closeSeg(currentSeg, false);
	    } else {
		Console.Error.Write("make-seg failed\n");
	    }
	    currentSeg = wb.openSeg("z", true);
	    if (null != currentSeg) {
		currentBt = wb.createBt(currentSeg, wb.indTyp, 0);
		scanhan(currentBt);
		wb.closeSeg(currentSeg, false);
	    } else {
		Console.Error.Write("open-seg failed\n");
	    }
	    wb.finalWb();
	}
    }
}
