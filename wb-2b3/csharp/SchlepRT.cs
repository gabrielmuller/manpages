/* "SchlepRT.cs" WB-tree File Based Associative String Data Base System.
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

/* Boolean Methods*/
public static bool a2b(bool b) {return b;}
public static bool a2b(Object i) {return (i!= null);}

public static byte[] bytes(params byte[] bArray)
{
    return bArray;
}

public static void subbytesMove(byte[] src, int srcStart, int srcEnd, byte[] dest, int destStart)
{
    System.Array.Copy(src, srcStart, dest, destStart, (srcEnd - srcStart));
}

public static void subbytesMoveLeft(byte[] src, int srcStart, int srcEnd, byte[] dest, int destStart)
{
    System.Array.Copy(src, srcStart, dest, destStart, (srcEnd - srcStart));
}

public static void subbytesMoveRight(byte[] src, int srcStart, int srcEnd, byte[] dest, int destStart)
{
    System.Array.Copy(src, srcStart, dest, destStart, (srcEnd - srcStart));
}

//Get elements from start to end in a new array.
public static byte[] subbytes(byte[]byts, int start, int end)
{
    byte[]subArray = new byte[end - start];
    System.Array.Copy (byts, start, subArray, 0, (end - start));
    return subArray;
}

public static byte[] stringToBytes(string str)
{
    if(str == null) return null;
    return Encoding.UTF8.GetBytes(str);
}

public static string bytesToString(byte[] bytes)
{
    if (bytes == null) return null;
    return Encoding.UTF8.GetString(bytes);
}

public static Object resizeArray(Object[] old, int newLength)
{
    int upto = (old.Length < newLength) ? old.Length : newLength;
    Object[] newArray = new Object[upto];

    System.Array.Copy(old, newArray, upto);
    return newArray;
}

// The invokeFunc definition is generated by scm2cs.
// It looks like this:
//
// public static int invokeFunc(String funcName, params Object[] args)
//	 {
//		 MethodInfo method = typeof(wb).GetMethod(funcName);
//		 return (int) method.Invoke(null, args);//first parameter is null for all static methods.
//	 }
