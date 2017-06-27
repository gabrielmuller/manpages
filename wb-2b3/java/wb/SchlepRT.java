package wb;

import java.io.*;
import java.lang.reflect.*;

public class SchlepRT {

    // Method a2b(anything-to-boolean) to convert any type to boolean.
    // This is needed when translating from the Schlep-Dialect of Scheme.
public static boolean a2b(boolean b) {return b;}
public static boolean a2b(Object i) {return (i!= null);}

    //Byte array constructor.
public static byte[] bytes(byte ... bArray)
{
    return bArray;
}

// Substring move operations
public static void subbytesMove(byte[] src, int srcStart, int srcEnd, byte[] dest, int desStart)
{
    System.arraycopy(src, srcStart, dest, desStart, (srcEnd - srcStart));
}

public static void subbytesMoveLeft(byte[] src, int srcStart, int srcEnd, byte[] dest, int desStart)
{
    System.arraycopy(src, srcStart, dest, desStart, (srcEnd - srcStart));
}

public static void subbytesMoveRight(byte[] src, int srcStart, int srcEnd, byte[] dest, int desStart)
{
    System.arraycopy(src, srcStart, dest, desStart, (srcEnd - srcStart));
}

//Get elements from start to end in a new array.
public static byte[] subbytes(byte[] byts, int start, int end)
{
    byte[]subArray = new byte[end - start];
    System.arraycopy(byts, start, subArray, 0, end - start);
    return subArray;
}

//String Utility Methods.
public static byte[] stringToBytes(String str)
{
    try {
	return str.getBytes("UTF-8");
    } catch (UnsupportedEncodingException e) {
	System.out.println("stringToBytes: " + e);
	System.exit(-1);
    } catch (NullPointerException e) {}
    return (byte[]) null;
}

public static String bytesToString(byte[] byts)
{
    try {
	return new String(byts, "UTF-8");
    } catch (UnsupportedEncodingException e) {
	System.out.println("bytesToString: " + e);
	System.exit(-1);
    } catch (NullPointerException e) {}
    return (String) null;
}

public static Object resizeArray(Object old, int newLength)
{
    int oldLength = Array.getLength(old);
    Class elementType = old.getClass().getComponentType();
    Object newArray = Array.newInstance(elementType, newLength);
    int upto = (oldLength < newLength)? oldLength: newLength;
    System.arraycopy(old, 0, newArray, 0, upto);
    return newArray;
}
}
