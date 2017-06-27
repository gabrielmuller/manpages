/* "Seg.cs" WB-tree File Based Associative String Data Base System. */

public class Seg {
    public FileStream PORT;
    public int BSIZ;
    public int USED;
    public String STRN;
    public Han RT_HAN, FL_HAN;
    public Lck FLCK;
    public Lck FFCK;
    public int FLC_LEN;
    public int []FLC;
    public Seg PRV, NXT;
    public int ID;
}

/*  The SEG (segment) methods*/
public static Seg newSegd(int idx)
{
    Seg seg = new Seg();
    seg.RT_HAN = makeHand();
    seg.FL_HAN = makeHand();
    seg.FLCK = makeLck( 0-idx);
    seg.FFCK = makeLck(-1-idx);
    seg.ID = idx;
    return seg;
}

public static FileStream seg_Port(Seg seg)
{
    return seg.PORT;
}

public static int seg_Bsiz(Seg seg)
{
    return seg.BSIZ;
}

public static int seg_Used(Seg seg)
{
    return seg.USED;
}

public static String seg_Str(Seg seg)
{
    return seg.STRN;
}

public static Han seg_RtHan(Seg seg)
{
    return seg.RT_HAN;
}

public static Han seg_FlHan(Seg seg)
{
    return seg.FL_HAN;
}

public static Lck seg_Lck(Seg seg)
{
    return seg.FLCK;
}

public static Lck seg_Fck(Seg seg)
{
    return seg.FFCK;
}

public static int seg_FlcLen(Seg seg)
{
    return seg.FLC_LEN;
}

public static int[] seg_Flc(Seg seg)
{
    return seg.FLC;
}

public static int seg_Id(Seg seg)
{
    if (null==seg) return -1;
    else return seg.ID;
}

public static bool seg_Mutable_P(Seg seg)
{
    return seg.FLC_LEN != -2;
}

public static Seg seg_Prv(Seg seg)
{
    return seg.PRV;
}

public static Seg seg_Nxt(Seg seg)
{
    return seg.NXT;
}

public static void seg_SetPort(Seg seg,FileStream port)
{
    seg.PORT = port;
}

public static void seg_SetBsiz(Seg seg,int bsiz)
{
    seg.BSIZ = bsiz;
}

public static void seg_SetUsed(Seg seg,int used)
{
    seg.USED = used;
}

public static void seg_SetStr(Seg seg,String str)
{
    seg.STRN = str;
}

public static void seg_SetFlcLen(Seg seg,int flcLen)
{
    seg.FLC_LEN = flcLen;
}

public static void seg_SetFlc(Seg seg,int[] flc)
{
    seg.FLC = flc;
}

public static void seg_SetPrv(Seg seg, Seg prv)
{
    seg.PRV = prv;
}

public static void seg_SetNxt(Seg seg, Seg nxt)
{
    seg.NXT = nxt;
}
