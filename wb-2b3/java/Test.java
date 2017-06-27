/* "Test.java" WB-tree File Based Associative String Data Base System.
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

import static wb.Seg.*;
import static wb.Segs.*;
import static wb.Ents.*;
import static wb.Wbdefs.*;
import static wb.SchlepRT.*;
import static wb.Wbsys.*;
import static wb.Db.*;
import static wb.Han.*;
import wb.Han;
import java.util.Vector;

public class Test {
    private static boolean stringEqual_P(String a, String b)
    {
        return a.equals(b);
    }

    private static <E> Vector<E> cons(E elem, Vector<E> list)
    {
        list.add(0, elem);
        return list;
    }

    private static int length(Vector<?> v)
    {
	return v.size();
    }

    public static Vector scanhan(Han han, int ... args)
    {
	int errors = 0;
	boolean testPrev_P = true; //(args.length > 0)? args[0] : 0;
	boolean verbose_P = (args.length > 1) ? a2b(args[1]) : false;

	System.err.print("FORWARD key scan\n");
	{
	    String key = null;
	    String prior = null;
	    boolean init_P = true;
	    Vector<String> list = new Vector();

	    Lloop: while (true) {
		if (verbose_P) {
		    System.err.print("NEXT KEY IS ");
		    System.err.print(key);
		    System.err.print("\n");
		}
		if (testPrev_P && !(init_P)) {
		    String prevKey = bt_Prev(han, key);
		    if (a2b(a2b(a2b(prevKey)
				&& a2b(prior)
				&& a2b(!stringEqual_P(prevKey, prior)))
			    || a2b(a2b(prevKey)
				   && a2b(!a2b(prior)))
			    || a2b(a2b(prior)
				   && a2b(!a2b(prevKey))
				   && a2b(!stringEqual_P(prior, "")))))
			{
			    errors = 1+(errors);
			    System.err.print("NEXT/PREV error: key= ");
			    System.err.print(prior);
			    System.err.print("\n");
			    System.err.print("   next= ");
			    System.err.print(key);
			    System.err.print(" prev= ");
			    System.err.print(prevKey);
			    System.err.print("\n");
			}
		}
		if (a2b(a2b(a2b(key) && (""!=(key))) || init_P)) {
		    String Tkey = bt_Next(han, key);
		    prior = key;
		    init_P = false;
		    list = a2b(key)
			?cons(key, list)
			:list;
		    key = Tkey;
		    continue Lloop;
		}
		else {
		    if (testPrev_P)
			System.err.print("scanhan: " + (errors) + " next/prev errors found.");
		    System.err.print("scanhan: " + (length(list)) + " items found.\n");
		    return list;
		}
	    }
	}
    }

    public static int countKeys(Han han)
    {
	{
	    String key = "";
	    int ct = 0;
	    boolean init_P = true;
	    Lloop: while (true) {
		if (a2b(a2b(a2b(key) && (""!=(key))) || init_P)) {
		    key = bt_Next(han, key);
		    ct = (ct)+1;
		    init_P = false;
		    continue Lloop;
		}
		else return ct;
	    }
	}
    }

    public static Vector scanb(Han han)
    {
	System.err.print("REVERSE key scan\n");
	{
	    String key = null;
	    boolean init_P = true;
	    Vector<String> list = new Vector();

	    Lloop: while (true) {
		System.err.print("PREV KEY IS ");
		System.err.print(key);
		System.err.print("\n");
		if (a2b(a2b(a2b(key) && (""!=(key))) || init_P)) {
		    String Tkey = bt_Prev(han, key);
		    init_P = false;
		    list = a2b(key)
			?cons(key, list)
			:list;
		    key = Tkey;
		    continue Lloop;
		}
		else {
		    System.err.print("%s: %d items found.\n"+("scanb")+(length(list)));
		    return list;
		}
	    }
	}
    }

    public static int dbSize(Han han)
    {
	return (seg_Used(han_Seg(han)))+1;
    }

    public static Han currentBt = null;
    public static wb.Seg currentSeg;
    public static int addKeyNum = 0;
    public static String addStr = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";

    /*  ADD! count [first-key add-key-num] [key-increment 1] [value-string "abc...xyz"x2] */
    public static void add(int n, int ... args)
    {
	int curKeyNum =(args.length > 0)? args[0] : addKeyNum;
	int incr = (args.length > 1)?  args[1] : 1;
	String addstr = (args.length > 2)?  args[2] + "" : addStr;
        int i = 1;
	while (!(i > n)) {
	    String keyStr = curKeyNum + "";
	    curKeyNum = (incr)+(curKeyNum);
	    bt_Put(currentBt, keyStr, addstr);
	    i = (i)+1;
	}
	addKeyNum = Math.max(addKeyNum, curKeyNum);
    }

    /*  REMOVE! count first-key [key-increment 1] [unused-arg] */
    public static void remove(int n, int curKey, int ... args)
    {
	int incr = a2b(args.length > 0)? args[0]:1;
	//int startTrace = (args.length > 1) ? args[1] : 0;
        int i = 1;
	while (!(i > n)) {
	    String keyStr = curKey + "";
	    curKey = (incr)+(curKey);
	    bt_Rem(currentBt, keyStr);
	    i = (i)+1;
	}
	return;
    }

    /*  old test code, still useful! */
    public static void test0(Han bHan)
    {
	bt_Put(bHan, "foo", "bar");
	bt_Put(bHan, "foz", "oof");
	bt_Put(bHan, "fonz", "zonf");
	bt_Put(bHan, "foo", "raboof");
	bt_Put(bHan, "food", "thought");
	scanhan(bHan);
	bt_Rem(bHan, "foo");
	bt_Rem(bHan, "fonz");
	bt_Put(bHan, "foo", "foo=bar");
	bt_Put(bHan, "foz", "foz=oof");
	bt_Put(bHan, "fonz", "fonz=zonf");
	bt_Put(bHan, "foo", "foo=raboof");
	bt_Put(bHan, "food", "food=for thought");
	bt_Put(bHan, "very very very long key field", "very very very longer value field");
	scanhan(bHan);
    }

    /*  THIS IS TEST CASE #1. */
    /*  It is used to test the addition of data elements with similar names */
    /*  to see if the repeat count routine is working correctly */
    public static void test1()
    {
	Han aHan = createBt(currentSeg, indTyp, 0);
	bt_Put(aHan, "cat", "6");
	bt_Put(aHan, "caddy", "4");
	bt_Put(aHan, "catalytic", "7");
	bt_Put(aHan, "cadalack", "3");
	bt_Put(aHan, "catastrophy", "10");
	bt_Put(aHan, "catastrophic", "9");
	bt_Put(aHan, "cad", "2");
	bt_Put(aHan, "cadjole", "5");
	bt_Put(aHan, "cataract", "8");
	bt_Put(aHan, "cacky", "1");
	bt_Put(aHan, "d", "12");
	scanhan(aHan);
	System.err.print("   TEST 1: nexting on block 'a' index 'cadz'\n");
	{
	    String ansStr = bt_Next(aHan, "cadz");
	    System.err.print(ansStr);
	}
	System.err.print("   TEST 1: removing 'd' from 'a'\n");
	bt_Rem(aHan, "d");
	scanhan(aHan);
	System.err.print("   TEST 1: removing 'cad' from 'a'\n");
	bt_Rem(aHan, "cad");
	scanhan(aHan);
	closeBt(aHan);
    }

    /*  THIS IS TEST #2 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block completely then adds a new data
     *  element and value to the block to make it split */
    public static void test2()
    {
	Han bHan = createBt(currentSeg, indTyp, 0);
	bt_Put(bHan, "foo", "bar");
	bt_Put(bHan, "foz", "oof");
	bt_Put(bHan, "fonz", "zonf");
	bt_Put(bHan, "foo", "raboof");
	bt_Put(bHan, "food", "thought");
	scanhan(bHan);
	bt_Rem(bHan, "foo");
	bt_Rem(bHan, "fonz");
	scanhan(bHan);
	System.err.print("   TEST 2: creating a full block named 'b'\n");
	bt_Put(bHan, "foo", "1234567");
	bt_Put(bHan, "foz", "12345678901");
	bt_Put(bHan, "fonz", "123456789");
	bt_Put(bHan, "foo", "1234567890");
	bt_Put(bHan, "food", "1234567890123456");
	bt_Put(bHan, "test", "123456789012345678901234");
	System.err.print("   TEST 2: doing a PUT to fill the block\n");
	bt_Put(bHan, "a", "12345678");
	scanhan(bHan);
	closeBt(bHan);
    }

    /*  THIS IS TEST #3 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block , then it tries to make the block split by making */
    /*  the first data element 1 char bigger by replacement */
    public static void test3()
    {
	Han cHan = createBt(currentSeg, indTyp, 0);
	bt_Put(cHan, "foo", "1234567");
	bt_Put(cHan, "foz", "12345678901");
	bt_Put(cHan, "fonz", "123456789");
	bt_Put(cHan, "foo", "1234567890");
	bt_Put(cHan, "food", "1234567890123456");
	bt_Put(cHan, "test", "123456789012345678901234");
	bt_Put(cHan, "a", "12345678");
	scanhan(cHan);
	System.err.print("   TEST 3: split block by increasing first element by one char with PUT\n");
	bt_Put(cHan, "a", "123456789");
	scanhan(cHan);
	closeBt(cHan);
    }

    /*  THIS IS TEST #4 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block , then it tries to make the block split by making */
    /*  a middle data element 1 char bigger by replacement */
    public static void test4()
    {
	Han dHan = createBt(currentSeg, indTyp, 0);
	bt_Put(dHan, "foo", "1234567");
	bt_Put(dHan, "foz", "12345678901");
	bt_Put(dHan, "fonz", "123456789");
	bt_Put(dHan, "foo", "1234567890");
	bt_Put(dHan, "food", "1234567890123456");
	bt_Put(dHan, "test", "123456789012345678901234");
	bt_Put(dHan, "a", "12345678");
	scanhan(dHan);
	System.err.print("   TEST 4: split block by incresing value of a middle element by one with PUT\n");
	bt_Put(dHan, "foz", "123456789012");
	scanhan(dHan);
	closeBt(dHan);
    }

    /*  THIS IS TEST #5 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block , then it tries to make the block split by making */
    /*  the last data element 1 char bigger by replacement */
    public static void test5()
    {
	Han eHan = createBt(currentSeg, indTyp, 0);
	bt_Put(eHan, "foo", "1234567");
	bt_Put(eHan, "foz", "12345678901");
	bt_Put(eHan, "fonz", "123456789");
	bt_Put(eHan, "foo", "1234567890");
	bt_Put(eHan, "food", "1234567890123456");
	bt_Put(eHan, "test", "123456789012345678901234");
	bt_Put(eHan, "a", "12345678");
	scanhan(eHan);
	System.err.print("TEST 5: splitblock by increasing value of the \n"
		+ "last data element by one with PUT\n");
	bt_Put(eHan, "test", "12345678901234567890123456");
	scanhan(eHan);
	closeBt(eHan);
    }

    /*  THIS IS TEST #6 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block , then it deletes the first element. It then */
    /*  tries to make the block split by reentering the first data element */
    /*  with 1 more char than the original */
    public static void test6()
    {
	Han fHan = createBt(currentSeg, indTyp, 0);
	bt_Put(fHan, "foo", "1234567");
	bt_Put(fHan, "foz", "12345678901");
	bt_Put(fHan, "fonz", "123456789");
	bt_Put(fHan, "foo", "1234567890");
	bt_Put(fHan, "food", "1234567890123456");
	bt_Put(fHan, "test", "123456789012345678901234");
	bt_Put(fHan, "a", "12345678");
	scanhan(fHan);
	System.err.print("   TEST 6: split block by deleting the first data element with REM\n");
	System.err.print("   TEST 6: then reentering the first data element with 1 more char\n");
	bt_Rem(fHan, "a");
	System.err.print("   TEST 6: doing a PUT\n");
	bt_Put(fHan, "a", "123456789");
	scanhan(fHan);
	closeBt(fHan);
    }

    /*  THIS IS TEST #7 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block , then it deletes a middle element. It then */
    /*  tries to make the block split by reentering a middle element with 1 */
    /*  more char than the original */
    public static void test7()
    {
	Han gHan = createBt(currentSeg, indTyp, 0);
	bt_Put(gHan, "foo", "1234567");
	bt_Put(gHan, "foz", "12345678901");
	bt_Put(gHan, "fonz", "123456789");
	bt_Put(gHan, "foo", "1234567890");
	bt_Put(gHan, "food", "1234567890123456");
	bt_Put(gHan, "test", "123456789012345678901234");
	bt_Put(gHan, "a", "12345678");
	scanhan(gHan);
	System.err.print("   TEST 7: split block by deleting a middle data element\n");
	System.err.print("   TEST 7: then reentering a middle data element with 1 more char\n");
	System.err.print("   TEST 7: than the original value.\n");
	bt_Rem(gHan, "foo");
	System.err.print("   TEST 7: doing a PUT\n");
	bt_Put(gHan, "foo", "12345678901");
	scanhan(gHan);
	closeBt(gHan);
    }

    /*  THIS IS TEST #8 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block , then it deletes the last element. It then */
    /*  tries to make the block split by reentering the last element with 1 */
    /*  more char than the original */
    public static void test8()
    {
	Han hHan = createBt(currentSeg, indTyp, 0);
	bt_Put(hHan, "foo", "1234567");
	bt_Put(hHan, "foz", "12345678901");
	bt_Put(hHan, "fonz", "123456789");
	bt_Put(hHan, "foo", "1234567890");
	bt_Put(hHan, "food", "1234567890123456");
	bt_Put(hHan, "test", "123456789012345678901234");
	bt_Put(hHan, "a", "12345678");
	scanhan(hHan);
	System.err.print("   TEST 8: split block by deleting the last data element\n");
	System.err.print("   TEST 8: then reentering the last data element with 1 more char\n");
	System.err.print("   TEST 8: than the original value.  Block name is 'h'\n");
	bt_Rem(hHan, "test");
	System.err.print("   TEST 8: doing a PUT\n");
	bt_Put(hHan, "test", "12345678901234567890123456");
	scanhan(hHan);
	closeBt(hHan);
    }

    /*  THIS IS TEST #9 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block completely then adds a new data
     *  element and value to the block to make it split */
    public static void test9()
    {
	Han iHan = createBt(currentSeg, indTyp, 0);
	bt_Put(iHan, "foz", "12345678901");
	bt_Put(iHan, "fonz", "123456789");
	bt_Put(iHan, "foo", "1234567890");
	bt_Put(iHan, "food", "1234567890123456");
	bt_Put(iHan, "test", "123456789012345678901234");
	bt_Put(iHan, "a", "12345678");
	System.err.print("   TEST 9: The block 'i' is full. Now we are adding a new index to the begining\n");
	System.err.print("   TEST 9: of the block with a value that should make the block split\n");
	bt_Put(iHan, "1", "123456789");
	scanhan(iHan);
	closeBt(iHan);
    }

    /*  THIS IS TEST #10 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block completely then adds a new data
     *  element and value to the block to make it split */
    public static void test10()
    {
	Han jHan = createBt(currentSeg, indTyp, 0);
	bt_Put(jHan, "foz", "12345678901");
	bt_Put(jHan, "fonz", "123456789");
	bt_Put(jHan, "foo", "1234567890");
	bt_Put(jHan, "food", "1234567890123456");
	bt_Put(jHan, "test", "123456789012345678901234");
	bt_Put(jHan, "a", "12345678");
	System.err.print("   TEST 10: The block 'j' is full. Now we are adding a new index to the middle\n");
	System.err.print("   TEST 10: of the block with a value that should make the block split\n");
	bt_Put(jHan, "fooa", "123456789");
	scanhan(jHan);
	closeBt(jHan);
    }

    /*  THIS IS TEST #11 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block completely then adds a new data
     *  element and value to the block to make it split */
    public static void test11()
    {
	Han kHan = createBt(currentSeg, indTyp, 0);
	bt_Put(kHan, "foz", "12345678901");
	bt_Put(kHan, "fonz", "123456789");
	bt_Put(kHan, "foo", "1234567890");
	bt_Put(kHan, "food", "1234567890123456");
	bt_Put(kHan, "test", "123456789012345678901234");
	bt_Put(kHan, "a", "12345678");
	System.err.print("   TEST 11: The block 'k' is full. Now we are adding a new index to the end\n");
	System.err.print("   TEST 11: of the block with a value that should make the block split\n");
	bt_Put(kHan, "zzz", "123456789");
	scanhan(kHan);
	closeBt(kHan);
    }

    /*  THIS IS TEST #12 FOR BLOCK SPLITTING STUFF */
    /*  this test fills a block completely then adds a new data
     *  element and value to the block to make it split */
    public static void test12()
    {
	Han kHan = createBt(currentSeg, indTyp, 0);
	bt_Put(kHan, "132", "12345678901");
	bt_Put(kHan, "1233", "1234567890");
	bt_Put(kHan, "26", "1234567890123456");
	bt_Put(kHan, "275", "123456789012345678901234");
	bt_Put(kHan, "84", "12345678");
	System.err.print("TEST 12: The block 'k' is full. Now we are \n" +
		"adding a new index pastp after b-pos\n");
	System.err.print("   TEST 12: of the block with a value that should make the block split\n");
	bt_Put(kHan, "82", "123456789");
	scanhan(kHan);
	closeBt(kHan);
    }

    public static void test13()
    {
	Han kHan = createBt(currentSeg, indTyp, 0);
	bt_Put(kHan, "0132", "12345678901");
	bt_Put(kHan, "01233", "1234567890");
	bt_Put(kHan, "026", "1234567890123456");
	bt_Put(kHan, "0275", "123456789012345678901234");
	bt_Put(kHan, "084", "12345678");
	System.err.print("TEST 13: The block 'k' is full. Now we are adding" +
		" a new index pastp after b-pos\n");
	System.err.print("   TEST 13: of the block with a value that should make the block split\n");
	bt_Put(kHan, "082", "123456789");
	scanhan(kHan);
	closeBt(kHan);
    }

    //     public static void test14()
    //     {
    //	Han kHan = createBt(currentSeg, indTyp, 0);
    //	bt_Put(kHan, "0132", "12345678901");
    //	bt_Put(kHan, "01233", "1234567890");
    //	bt_Put(kHan, "026", "1234567890123456");
    //	bt_Put(kHan, "04", "123456789012345678901234");
    //	bt_Put(kHan, "041", "1234567890");
    //	System.err.print("\nTEST 14: The  block 'k' is full. Now we are adding\n" +
    //             "a new index pastp after b-pos\n");
    //	System.err.print("   TEST 14: of the block with a value that should make the block split\n");
    //	tscanBlk(id(kHan));
    //	bt_Put(kHan, "040", "123456789");
    //	tscanBlk(id(kHan));
    //	closeBt(kHan);
    //     }

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

    public static void main(String[] args)
    {
	initWb(75, 150, 2048);
	System.err.print("make-seg\n");
	currentSeg = makeSeg("z", 0x80);
	if (null != currentSeg) {
	    // 	System.err.print("close-seg\n");
	    // 	closeSeg(currentSeg, true);
	    // 	System.err.print("open-seg\n");
	    // 	currentSeg = openSeg("z", true);
	    System.err.print("create-bt\n");
	    currentBt = createBt(currentSeg, indTyp, 0);
	    System.err.print("test0!\n");
	    test0(currentBt);
	    System.err.print("test!\n");
	    test128();
	    System.err.print("add! 10\n");
	    add(0xa);
	    closeSeg(currentSeg, false);
	} else {
	    System.err.print("make-seg failed\n");
	}
	currentSeg = openSeg("z", true);
	if (null != currentSeg) {
	    currentBt = createBt(currentSeg, indTyp, 0);
	    scanhan(currentBt);
	    closeSeg(currentSeg, false);
	} else {
	    System.err.print("open-seg failed\n");
	}
	finalWb();
    }
}
