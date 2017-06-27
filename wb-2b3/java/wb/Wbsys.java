/* "Wbsys.java" WB-tree File Based Associative String Data Base System.
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

import static wb.Wbdefs.*;
import java.io.*;
import java.lang.reflect.*;

public class Wbsys {

// should implement root_P and end_of_chain_P.

// Method that returns a method - should actually be defined in
// "blink.java".
public static java.lang.reflect.Method selectSplitFun(int type)
{
    java.lang.Class blinkClass = null;
    java.lang.reflect.Method dummy = null;
    try {
	blinkClass = java.lang.Class.forName("Blink");
    } catch (ClassNotFoundException e) {
	System.out.println("Class Blink not found");
    }
    //All methods here take same types of arguments.
    java.lang.Class[] argClass =
	new Class[] {byte[].class,
		     byte[].class,
		     int.class,
		     byte[].class,
		     int.class,
		     int.class,
		     byte[].class,
		     int.class};
    try {
	switch(type) {
	case pastp: return blinkClass.getMethod("pastpLeafSplit", argClass);
	case qpastp: return blinkClass.getMethod("qpastpLeafSplit", argClass);
	case match: return blinkClass.getMethod("valLeafSplit", argClass);
	default: return blinkClass.getMethod("dummyLeafSplit", argClass);
	}
    } catch (NoSuchMethodException e) {
	System.out.println("No method `"+type+"' found in class `Blink'");
    }
    return dummy;
}

/* Returns a Method object when given the method signature and the
 * class it was defined in.
 */
public static Method getMethod(String className, String methodName, Class[] args)
{
    Class cls = null;
    Method mtd = null;
    try {
	cls = Class.forName(className);
	mtd = cls.getMethod(methodName, args);
    } catch (Exception e) {
	System.err.print("Exception " + e);
    }
    return mtd;
}

/** Wrapper for invoking methods that return integers. The wrapper is only for
 * handling exceptions.
 * @param func Method
 * @param obj Object of the class the method was defined in.  A null
 *             is sufficient for static methods.
 * @param args  Method arguments
 * @return  integer
 */
public static int intFunInvoke(Method func, Object obj, Object[] args)
{
    int result = -1;
    try {
	result = (Integer) func.invoke(obj, args);
    } catch (Exception e) {
	System.out.println(e);
    }
    return result;
}
}
