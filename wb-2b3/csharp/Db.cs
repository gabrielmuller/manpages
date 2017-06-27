
/* Wrapper functions for db access that take strings instead
   of byte arrays */
// public static int bt_Del(Han han, String keyStr)
// {
//     return bt_Del(han, stringToBytes(keyStr));
// }

public static Han createDb(Seg seg, int typ, String nameStr)
{
    return createDb(seg, typ, stringToBytes(nameStr));
}

public static Han createDb(Seg seg, char typ, String nameStr)
{
    return createDb(seg, (int)typ, stringToBytes(nameStr));
}

public static Han openDb(Seg seg, String nameStr)
{
    return openDb(seg, stringToBytes(nameStr));
}

public static void bt_Delete(Han han, String keyStr, String key2Str)
{
    bt_Delete(han, stringToBytes(keyStr), stringToBytes(key2Str));
}

public static String bt_Get(Han han, String keyStr)
{
    byte[] byts = bt_Get(han, stringToBytes(keyStr));
    return bytesToString(byts);
}

public static String bt_Next(Han han, String keyStr)
{
    byte[] byts = bt_Next(han, stringToBytes(keyStr));
    return bytesToString(byts);
}


public static String bt_Prev(Han han, String keyStr)
{
    byte[] byts = bt_Prev(han, stringToBytes(keyStr));
    return bytesToString(byts);
}

public static void bt_Put(Han han, String keyStr, String key2Str)
{
    bt_Put(han, stringToBytes(keyStr), stringToBytes(key2Str));
}

public static String bt_Rem(Han han, String keyStr)
{
    byte[] byts = bt_Rem(han, stringToBytes(keyStr));
    return bytesToString(byts);
}
